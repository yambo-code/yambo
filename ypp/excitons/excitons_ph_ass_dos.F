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
 use R_lattice,     ONLY:bz_samp,bz_samp_reset
 use electrons,     ONLY:levels
 use vec_operate,   ONLY:c2a
 use com,           ONLY:msg
 use D_lattice,     ONLY:n_atoms,alat
 use interpolate,   ONLY:INTERPOLATION_BZ
 use YPPm,          ONLY:EXCITONS_n_user_states,DOS_E_step,DOS_E_range,DOS_E_steps
 use YPP_ELPH,      ONLY:ph_freqs_file,ELPH_databases_IO_freqs
 use LIVE_t,        ONLY:live_timing
 use parallel_m,    ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,  ONLY:PP_redux_wait,PARALLEL_index
 !
#include<memory.h> 
 !
 type(bz_samp) ::Xk,k,q_exc
 type(levels)  ::Xen,en
 !
 logical, external     :: file_exists
 integer               :: i_q,i_c,i_E,ph_modes
 type(bz_samp)         :: q_matdyn
 real(SP), allocatable :: ph_freqs(:,:)
 integer               :: ID_INTERP_EXC
 real(SP), allocatable :: BSE_interp_E(:,:)
 real(SP)              :: dos_E(DOS_E_steps),exc_ph_DOS(DOS_E_steps) 
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
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/q_matdyn%nibz/))
 !
 exc_ph_DOS=rZERO
 !
 call live_timing("EXC PH-DOS",px%n_of_elements(myid+1))
 !
 do i_q=1,q_matdyn%nibz
   if (.not.px%element_1D(i_q)) cycle
     do i_c=1,EXCITONS_n_user_states
!       Bolz_exc=
       do i_E=1,DOS_E_steps
!        exc_dos(i_E)=exc_dos(i_E)+Fermi_fnc_derivative((BSE_interp_E(i_c,iq)-dos_E(i_E))*HA2EV,DOS_broadening)*GRID_q%weights(iq)
       enddo
   enddo
   call live_timing(steps=1)
 enddo
 !
 !  ph_ass_dos(w) = \sum_{Q,l,n} Bose(E^exc_{l,Q}, T) * Bose(w_{n,q}, T) \delta { E^exc_{l,Q} - w_{n,q} - w } \delta {Q,q}
 ! 
end subroutine
