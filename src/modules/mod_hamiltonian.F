!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
module hamiltonian
 !
 use pars,       ONLY:SP,schlen
 !
#include<memory.h>
 !
 integer           :: H_ref_bands(2)
 !
 character(schlen) :: H_potential
 !
 ! Possible Kinds, functionals and external perturbations
 !
 integer           :: H_kind
 integer           :: H_xc_functional
 integer           :: H_perturbation
 !
 complex(SP), allocatable :: Ho(:,:,:,:)
 !
 ! Non-Local part (sigma_x, sigma_cohsex)
 !
 complex(SP), allocatable :: H_nl_sc(:,:)
 !
 real(SP),    allocatable :: V_xc_sc(:,:)
 real(SP),    allocatable :: V_hartree_sc(:,:)
 !
 logical           :: l_sc_V_is_local=.TRUE.
 logical           :: l_sc_XC_is_local=.TRUE.
 !
 ! Kinetic Energy and Ionic potential energy
 !
 real(SP),    allocatable :: E_bare(:,:,:)
 !
 real(SP),    allocatable :: rho_n(:)
 real(SP),    allocatable :: magn_n(:,:)
 !
 ! Wavefunction indexes
 !
 integer :: WF_G_max,WF_Go_indx
 !
 contains
   !
   integer function B_mat_index(ib1,ib2,nb)
     integer :: ib1,ib2,nb(2)
     !
     B_mat_index=(ib1-nb(1))*(nb(2)-nb(1)+1)+ib2-nb(1)+1
     !
   end function
   !
   subroutine H_QP_table_setup(E)
     use R_lattice,      ONLY:nkibz
     use electrons,      ONLY:levels
     use QP_m,           ONLY:QP_nk,QP_state,QP_nb
     !
     type(levels) :: E
     !
     ! Build-up QP table
     !
     QP_nb=H_ref_bands(2)
     QP_nk=nkibz
     ! 
     ! QP_state my be read from DBs at start-up
     !
     YAMBO_FREE(QP_state)
     !
     YAMBO_ALLOC(QP_state,(QP_nb,QP_nk))
     QP_state=.TRUE.
     call QP_state_table_setup(E)
     !
   end subroutine
   !
   subroutine H_alloc(E)
     use pars,           ONLY:SP
     use drivers,        ONLY:l_sc_sex,l_sc_coh,l_sc_srpa
     use electrons,      ONLY:n_spin,n_sp_pol,levels
     use FFT_m,          ONLY:fft_size
     use QP_m,           ONLY:QP_n_states,QP_nk,QP_Sc
     use xc_functionals, ONLY:magn,V_xc
     !
     type(levels) :: E
     !
     YAMBO_ALLOC(rho_n,(fft_size))
     !
     if(n_spin>1) then
       YAMBO_ALLOC(magn,(fft_size,3))
       YAMBO_ALLOC(magn_n,(fft_size,3))
     endif
     !
     YAMBO_ALLOC(Ho,(H_ref_bands(2),H_ref_bands(2),E%nk,n_sp_pol))
     !
     YAMBO_ALLOC(V_xc_sc,(fft_size,n_spin))
     !
     YAMBO_ALLOC(V_xc,(fft_size,n_spin))
     !
     YAMBO_ALLOC(V_hartree_sc,(fft_size,n_spin))
     !
     YAMBO_ALLOC(H_nl_sc,(H_ref_bands(2),H_ref_bands(2)))
     !
   end subroutine
   !
   subroutine H_free()
     use drivers,        ONLY:l_real_time
     use QP_m,           ONLY:QP_Sc
     use xc_functionals, ONLY:magn,V_xc
     use X_m,            ONLY:X_alloc
     !
     if(allocated(rho_n)) then
       YAMBO_FREE(rho_n)
     endif
     !
     if(allocated(magn_n)) then
       YAMBO_FREE(magn_n)
     endif
     !
     if(allocated(magn)) then
       YAMBO_FREE(magn)
     endif
     !
     YAMBO_FREE(Ho)
     !
     YAMBO_FREE(H_nl_sc)
     !
     if(allocated(V_xc)) then 
       YAMBO_FREE(V_xc)
     endif
     !
     YAMBO_FREE(V_xc_sc)
     !
     YAMBO_FREE(V_hartree_sc)
     !
   end subroutine
   !
end module
