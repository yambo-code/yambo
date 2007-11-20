!
! Copyright (C) 2000-2005 A. Marini, M. Gruning and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
subroutine DFT_driver(E,k,q)
 !
 use pars,          ONLY:SP
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use QP_m,          ONLY:QP_state,QP_nb,QP_nk,QP_Vxc,QP_n_states
 use DFT,           ONLY:DFT_SC,DFT_bands
 use wave_func,     ONLY:wf_load
 use memory_m,      ONLY:mem_est
 !
 implicit none
 type(levels) ::E
 type(bz_samp)::k,q
 ! 
 ! Work Space
 !
 complex(SP), allocatable :: V_xc_GS(:)
 !
 call section('*','DFT '//trim(DFT_SC)//' potentials')
 !
 ! Table for QP indexes
 !
 QP_nb=DFT_bands
 QP_nk=k%nibz
 if (allocated(QP_state)) deallocate(QP_state)
 allocate(QP_state(QP_nb,QP_nk))
 QP_state=.TRUE.
 !
 call QP_state_table_setup(E)
 !
 ! WF
 !
 call wf_load(0,1,(/1,DFT_bands/),(/1,k%nibz/),title=' ')
 !
 ! Sigma_x
 !
 !call QP_XX_Vxc(E,k,k,q,.FALSE.,.FALSE.) 
 !
 ! GS Vxc
 !
 allocate(QP_Vxc(QP_n_states))
 call mem_est("QP_Vxc",(/QP_n_states/))
 call DFT_Vxc(E,k)
 !
end subroutine
