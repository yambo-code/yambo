!
!        Copyright (C) 2000-2017 the YAMBO team
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
 ! Rotation matrix which diagonalize the hamiltonian
 !
 complex(SP), allocatable :: SC_R(:,:,:,:)
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
     use memory_m,       ONLY:mem_est
     !
     implicit none
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
     if (allocated(QP_state)) deallocate(QP_state)
     !
     allocate(QP_state(QP_nb,QP_nk))
     call mem_est("QP_state",(/QP_nb*QP_nk/))
     QP_state=.TRUE.
     call QP_state_table_setup(E)
     !
   end subroutine
   !
   subroutine H_alloc(E,l_real_space,l_rotation_matrix)
     use pars,           ONLY:SP
     use electrons,      ONLY:n_spin,n_sp_pol,levels
     use FFT_m,          ONLY:fft_size
     use QP_m,           ONLY:QP_n_states,QP_nk,QP_Sc
     use memory_m,       ONLY:mem_est
     use xc_functionals, ONLY:magn,V_xc
     !
     implicit none
     !
     type(levels), intent(in) :: E
     logical,      intent(in) :: l_real_space,l_rotation_matrix
     !
     allocate(Ho(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2),E%nk,n_sp_pol))
     call mem_est("Ho",(/size(Ho)/),(/2*SP/))
     !
     allocate(H_nl_sc(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2)))
     call mem_est("H_nl_sc",(/size(H_nl_sc)/),(/2*SP/))
     !
     if(l_rotation_matrix) then
       allocate(SC_R(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2),E%nk,n_sp_pol))
       call mem_est("SC_R",(/size(SC_R)/))
     endif
     !
     if (.not.l_real_space) return
     !
     allocate(rho_n(fft_size))
     call mem_est("rho",(/fft_size/),(/SP/))
     !
     if(n_spin>1) then
       allocate(magn(fft_size,3))
       allocate(magn_n(fft_size,3))
       call mem_est("magn",(/2*fft_size*3/),(/SP/))
     endif
     !
     allocate(V_xc_sc(fft_size,n_spin))
     call mem_est('V_xc_sc',(/size(V_xc_sc)/),(/SP/))
     !
     allocate(V_xc(fft_size,n_spin))
     call mem_est('V_xc',(/size(V_xc)/),(/SP/))
     !
     allocate(V_hartree_sc(fft_size,n_spin))
     call mem_est("Hartree_Potential",(/size(V_hartree_sc)/))
     !
   end subroutine
   !
   subroutine H_free()
     use drivers,        ONLY:l_real_time
     use QP_m,           ONLY:QP_Sc
     use memory_m,       ONLY:mem_est
     use xc_functionals, ONLY:magn,V_xc
     use X_m,            ONLY:X_alloc
     !
     implicit none
     !
     deallocate(Ho)
     call mem_est("Ho")
     !
     deallocate(H_nl_sc)
     call mem_est("H_nl_sc")
     !
     if(allocated(SC_R)) then
       deallocate(SC_R)
       call mem_est("SC_R")
     endif
     !
     if(allocated(rho_n)) then
       deallocate(rho_n)
       call mem_est("rho")
     endif
     !
     if(allocated(magn_n)) then
       deallocate(magn_n)
       call mem_est("magn")
     endif
     !
     if(allocated(magn)) then
       deallocate(magn)
       call mem_est("magn")
     endif
     !
     if(allocated(V_xc)) then 
       deallocate(V_xc)
       call mem_est('V_xc')
     endif
     !
     if(allocated(V_xc_Sc)) then 
       deallocate(V_xc_sc)
       call mem_est('V_xc_sc')
     endif
     !
     if(allocated(V_hartree_sc)) then 
       deallocate(V_hartree_sc)
       call mem_est("Hartree_Potential")
     endif
     !
   end subroutine
   !
end module
