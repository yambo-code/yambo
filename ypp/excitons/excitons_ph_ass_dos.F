!
!        Copyright (C) 2000-2023 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA
!
! This file is distributed under the terms of the GNU
! General Public License. You can redistribute it and/or
! modify it under the terms of the GNU General Public
! License as published by the Free Software Foundation;
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will
! be useful, but WITHOUT ANY WARRANTY; without even the
! implied warranty of MERCHANTABILITY or FITNESS FOR A
! PARTICULAR PURPOSE.  See the GNU General Public License
! for more details.
!
! You should have received a copy of the GNU General Public
! License along with this program; if not, write to the Free
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston,
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine excitons_ph_ass_dos(k,Xk,en,Xen,q_exc)
 !
 use pars,          ONLY:SP,pi,rZERO,schlen
 use units,         ONLY:HA2EV
 use R_lattice,     ONLY:bz_samp,bz_samp_reset
 use electrons,     ONLY:levels,spin_occ
 use vec_operate,   ONLY:c2a
 use com,           ONLY:msg
 use D_lattice,     ONLY:n_atoms,alat
 use interpolate,   ONLY:INTERPOLATION_BZ
 use YPPm,          ONLY:EXCITONS_n_user_states,DOS_E_step,DOS_E_range,DOS_E_steps,DOS_broadening
 use stderr,        ONLY:real2ch
 use YPP_ELPH,      ONLY:ph_modes_file,ph_freqs_file,ELPH_databases_IO_freqs,ELPH_databases_IO_modes
 use LIVE_t,        ONLY:live_timing
 use parallel_m,    ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,  ONLY:PARALLEL_index
 use ELPH,          ONLY:FAN_deltaE_treshold,elph_branches
 use functions,     ONLY:Lorentzian_func,boltzman_f,bose_f
 use stderr,        ONLY:STRING_same,intc
 !
#include<memory.h> 
 !
 type(bz_samp) ::Xk,k,q_exc
 type(levels)  ::Xen,en
 !
 logical, external     :: file_exists
 integer               :: i_q,i_c,i_E,ph_modes,i_idx,i_l
 integer               :: ID_INTERP_EXC,n_full_q
 type(bz_samp)         :: q_matdyn
 real(SP), allocatable :: ph_freqs(:,:)
 integer, allocatable  :: iq_indx(:)
 real(SP), allocatable :: BSE_interp_E(:,:)
 real(SP)              :: dos_E(DOS_E_steps),exc_ph_DOS(DOS_E_steps),exc_OCC,Boltz_F_exc,Bose_F_ph,min_E 
 real(SP)              :: occ_threshold=1E-5
 type(PP_indexes)      :: px
 complex(SP), allocatable  :: ph_pol(:,:,:,:)
 !
 ph_modes=3*n_atoms
 !
 ! Check input files from matdyn.x
 !
 if(.not.STRING_same(ph_modes_file,'none').and..not.STRING_same(ph_freqs_file,'none')) &
    call error("Please specify the Frequency file or the Matdyn Mode file, not both of them!")
 !
 call bz_samp_reset(q_matdyn)
 !
 if(.not.STRING_same(ph_freqs_file,'none')) then
    if(.not.file_exists(trim(ph_freqs_file))) call error("Phonon frequencies file not found!")
    call msg('s',"Phonon frequencies read from: "//trim(ph_freqs_file))
    call ELPH_databases_IO_freqs(ph_file=ph_freqs_file,n_q=q_matdyn%nibz)
 elseif(.not.STRING_same(ph_modes_file,'none')) then
    if(.not.file_exists(trim(ph_freqs_file))) call error("Phonon frequencies file not found!")
    call msg('s',"Phonon frequencies read from: "//trim(ph_modes_file))
    call ELPH_databases_IO_modes(ph_file=ph_modes_file,n_q=q_matdyn%nibz)
 endif
 call msg('s',"Number of phonon q-points in the IBZ : ",q_matdyn%nibz)
 call msg('s',"Alat in yambo                 : ",alat(1))
 !
 YAMBO_ALLOC(q_matdyn%pt,(q_matdyn%nibz,3))
 YAMBO_ALLOC(ph_freqs,(ph_modes,q_matdyn%nibz))
 !
 if(.not.STRING_same(ph_freqs_file,'none')) then
   call ELPH_databases_IO_freqs(ph_file=ph_freqs_file,n_q=q_matdyn%nibz,q_pt=q_matdyn%pt,ph_freqs=ph_freqs)
 elseif(.not.STRING_same(ph_modes_file,'none')) then
   YAMBO_ALLOC(ph_pol,(q_matdyn%nibz,ph_modes,n_atoms,3))
   call ELPH_databases_IO_modes(ph_file=ph_modes_file,n_q=q_matdyn%nibz,q_pt=q_matdyn%pt,ph_freqs=ph_freqs,ph_pol=ph_pol)
   YAMBO_FREE(ph_pol)
 endif
 !
 q_matdyn%pt=q_matdyn%pt*(2.*pi/alat(1))    ! From QuantumEspresso Alat to CC
 !
 ! Converto to iku units
 !
 do i_q=1,q_matdyn%nibz
   call c2a(v_in=q_matdyn%pt(i_q,:),mode="kc2i")
 enddo
 !
 call k_expand(q_matdyn)
 call msg('s',"Number of phonon q-points in the BZ : ",q_matdyn%nbz)
 !
 ! Read and interpolate excitons dispersion
 !
 ID_INTERP_EXC=1
 call excitons_interpolate_setup(k,Xk,en,Xen,q_exc,ID_INTERP_EXC,.TRUE.)
 YAMBO_ALLOC(BSE_interp_E,(EXCITONS_n_user_states,q_matdyn%nibz))
 call INTERPOLATION_BZ(K=q_matdyn,NK=q_matdyn%nibz,R1D=BSE_interp_E,ID=ID_INTERP_EXC)
 !
 ! Calculate the phonon assisted density of states
 !
 min_E=minval(BSE_interp_E(1,:))
 !
 call msg('s',"Minumum Excitonn Energy (chemical potential)      : ",min_E*HA2EV)
 !
 ! Find occupaied excitons
 ! 
 call live_timing("Find occupied excitons",q_matdyn%nibz)
 !
 n_full_q=0 ! Number Q-points when excitonic occupation different from zero 
 YAMBO_ALLOC(iq_indx,(q_matdyn%nibz))
 iq_indx=0
 !
 do i_q=1,q_matdyn%nibz
   do i_c=1,EXCITONS_n_user_states
     Boltz_f_exc=boltzman_f(BSE_interp_E(i_c,i_q)-min_E)
     if(Boltz_f_exc>occ_threshold) then
       n_full_q=n_full_q+1
       iq_indx(n_full_q)=i_q
       exit
     endif
   enddo
   call live_timing(steps=1)
 enddo
 call live_timing( )
 !
 call msg('sr',"Number of filled q-points "//trim(real2ch(real(n_full_q,SP)/real(q_matdyn%nibz,SP)*100))//"% ")
 if(n_full_q==0) call error("Zero excitons, decrease the chemical potential! ")
 !
 exc_OCC   =rZERO
 exc_ph_DOS=rZERO
 !
 ! Setup DOS
 !
 DOS_E_step=(DOS_E_range(2)-DOS_E_range(1))/DOS_E_steps
 !
 do i_E=1,DOS_E_steps
   dos_E(i_E)=DOS_E_range(1)+real(i_E,SP)*DOS_E_step
 enddo
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/n_full_q/))
 !
 call live_timing("EXC PH-DOS",px%n_of_elements(myid+1))
 !
 !  ph_ass_dos(w) = \sum_{Q,l,n} Bose(E^exc_{l,Q}, T) * Bose(w_{n,q}, T) \delta { E^exc_{l,Q} - w_{n,q} - w } \delta {Q,q}
 !
 if(elph_branches(1)<=0) elph_branches(1)=1
 if(elph_branches(1)>ph_modes) call error('Max number of phonon modes '//trim(intc(ph_modes)))
 if(elph_branches(2)==-1) elph_branches(2)=ph_modes
 !
 do i_idx=1,n_full_q
   !  
   i_q=iq_indx(i_idx)
   !
   do i_c=1,EXCITONS_n_user_states
     !
     Boltz_F_exc=boltzman_f(BSE_interp_E(i_c,i_q)-min_E)
     !
     exc_OCC=exc_OCC+Boltz_F_exc*q_matdyn%weights(i_q)
     !
     do i_l=elph_branches(1),elph_branches(2)
       !
       if(ph_freqs(i_l,i_q)<FAN_deltaE_treshold) cycle
       !
       Bose_F_ph=(1.0+bose_f(ph_freqs(i_l,i_q))/spin_occ)
       !
       do i_E=1,DOS_E_steps
          exc_ph_DOS(i_E)=exc_ph_DOS(i_E)+Boltz_F_exc*Bose_F_ph*q_matdyn%weights(i_q)*  &
    &                     Lorentzian_func((BSE_interp_E(i_c,i_q)-ph_freqs(i_l,i_q)-dos_E(i_E)),DOS_broadening)    
       enddo
       !
     enddo
     !
   enddo
   ! 
   call live_timing(steps=1)
   !
 enddo
 !
 call msg('sr',"Nomber of excited excitons ",exc_OCC)
 !
 call print_DOS()
 !
 contains
   !
   subroutine print_DOS()
     !
     use com,           ONLY:msg,of_open_close
     implicit none
     !
     character(schlen)     :: file_name
     !
     file_name='excitons_ph_dos'
     call of_open_close(trim(file_name),'ot')
     call msg('o '//trim(file_name),'#',' Phonon-assisted exciton DOS',INDENT=0)
     call msg('o '//trim(file_name),'#','',INDENT=0)
     call msg('o '//trim(file_name),'#',(/'E(meV)       ','EXC-PH-DOS(w)'/),USE_TABS=.true.)
     call msg('o '//trim(file_name),'#','',INDENT=0)
     do i_E=1,DOS_E_steps
       call msg('o '//trim(file_name),' ',(/dos_E(i_E)*HA2EV,exc_ph_dos(i_E)/),USE_TABS=.true.)
     enddo
     !
     call of_open_close(trim(file_name))
     !
     end subroutine
   !
end subroutine
   !
