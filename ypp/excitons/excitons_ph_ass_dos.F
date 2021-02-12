!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use pars,          ONLY:SP,pi,rZERO
 use units,         ONLY:HA2EV
 use R_lattice,     ONLY:bz_samp,bz_samp_reset
 use electrons,     ONLY:levels
 use vec_operate,   ONLY:c2a
 use com,           ONLY:msg
 use D_lattice,     ONLY:n_atoms,alat,Bose_Temp
 use interpolate,   ONLY:INTERPOLATION_BZ
 use YPPm,          ONLY:EXCITONS_n_user_states,DOS_E_step,DOS_E_range,DOS_E_steps,ChemPot,DOS_broadening
 use stderr,        ONLY:real2ch
 use YPP_ELPH,      ONLY:ph_freqs_file,ELPH_databases_IO_freqs
 use LIVE_t,        ONLY:live_timing
 use parallel_m,    ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,  ONLY:PP_redux_wait,PARALLEL_index
 use ELPH,          ONLY:FAN_deltaE_treshold
 use functions,     ONLY:Fermi_fnc_derivative
 !
#include<memory.h> 
 !
 type(bz_samp) ::Xk,k,q_exc
 type(levels)  ::Xen,en
 !
 logical, external     :: file_exists
 integer               :: i_q,i_c,i_E,ph_modes,i_idx,i_l
 type(bz_samp)         :: q_matdyn
 real(SP), allocatable :: ph_freqs(:,:)
 integer, allocatable  :: iq_indx(:)
 integer               :: ID_INTERP_EXC,n_full_q
 real(SP), allocatable :: BSE_interp_E(:,:)
 real(SP)              :: dos_E(DOS_E_steps),exc_ph_DOS(DOS_E_steps),exc_OCC,Bose_F_exc,Bose_F_ph 
 type(PP_indexes)      :: px
 !
 ph_modes=3*n_atoms
 !
 if(trim(ph_freqs_file)=='none')            call error("Phonon frequencies file not specified!")      
 !
 call msg('s',"Alat in yambo                 : ",alat(1))
 !
 call bz_samp_reset(q_matdyn)
 !
 if(.not.file_exists(trim(ph_freqs_file))) call error("Phonon frequencies file not found!")      
 !
 call msg('s',"Phonon frequencies read from: "//trim(ph_freqs_file))
 call ELPH_databases_IO_freqs(ph_freqs_file,q_matdyn%nibz)
 call msg('s',"Number of phonon q-points in the IBZ : ",q_matdyn%nibz)
 !
 YAMBO_ALLOC(q_matdyn%pt,(q_matdyn%nibz,3))
 YAMBO_ALLOC(ph_freqs,(q_matdyn%nibz,ph_modes))
 !
 call ELPH_databases_IO_freqs(ph_freqs_file,q_matdyn%nibz,q_matdyn%pt,ph_freqs)
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
 ChemPot=ChemPot+minval(BSE_interp_E(1,:))
 !
 call msg('s',"Chemical Potential                 : ",ChemPot)
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
     Bose_f_exc=1.0/(exp((BSE_interp_E(i_c,i_q)-ChemPot)/Bose_Temp)-1._SP)
     if(Bose_f_exc>epsilon(1.)) then
       n_full_q=n_full_q+1
       iq_indx(n_full_q)=i_q
       exit
     endif
   enddo
   call live_timing(steps=1)
 enddo
 call live_timing( )
 !
 call msg('sr',"Number of filled q-points "//real2ch(real(n_full_q)/real(q_matdyn%nibz)*100.0)//" % ")
 !
 exc_OCC   =rZERO
 exc_ph_DOS=rZERO
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/n_full_q/))
 !
 call live_timing("EXC PH-DOS",px%n_of_elements(myid+1))
 !
 !  ph_ass_dos(w) = \sum_{Q,l,n} Bose(E^exc_{l,Q}, T) * Bose(w_{n,q}, T) \delta { E^exc_{l,Q} - w_{n,q} - w } \delta {Q,q}
 !
 do i_idx=1,n_full_q
   !  
   i_q=iq_indx(i_idx)
   !
   do i_c=1,EXCITONS_n_user_states
     !
     Bose_F_exc=1.0/(exp((BSE_interp_E(i_c,i_q)-ChemPot)/Bose_Temp)-1._SP)
     !
     exc_OCC=exc_OCC+Bose_F_exc*q_matdyn%weights(i_q)
     !
     do i_l=1,ph_modes
       !
       if(ph_freqs(i_q,i_l)<FAN_deltaE_treshold) cycle
       !
       Bose_F_ph=(1.0+1.0/(exp(ph_freqs(i_q,i_l)/Bose_Temp)-1._SP))
       !
       forall(i_E=1:DOS_E_steps)
          exc_ph_DOS(i_E)=exc_ph_DOS(i_E)+Bose_F_exc*Bose_F_ph*q_matdyn%weights(i_q)*  &
    &                     Fermi_fnc_derivative((BSE_interp_E(i_c,i_q)-ph_freqs(i_q,i_l)-dos_E(i_E))*HA2EV,DOS_broadening)    
       end forall
       !
     enddo
     !
   enddo
   ! 
   call live_timing(steps=1)
   !
 enddo
 !
 call msg('s',"Nomber of excited excitons ",exc_OCC)
 !
end subroutine
