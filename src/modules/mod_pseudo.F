!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
module pseudo
 !
 use pars,      ONLY:SP,LP,IP
 use electrons, ONLY:n_spin,n_spinor
 use D_lattice, ONLY:n_atomic_species
 implicit none
 !
 ! *** Non-Local pseudo. The [r,Vnl] commutator ***
 !
 logical :: l_many_proj
 integer :: pp_n_l_max
 integer :: pp_n_l_times_proj_max
 integer :: pp_kbv_dim
 integer,  allocatable:: pp_table(:,:,:)
 integer,  allocatable:: pp_n_l_comp(:)
 real(SP), allocatable:: pp_factor(:,:,:,:,:)
 !
 ! ABINIT and PWSCF KB
 !=====================
 real(SP), allocatable:: pp_kbs(:,:)
 real(SP), allocatable:: pp_kb(:,:,:,:)
 real(SP), allocatable:: pp_kbd(:,:,:,:)
 complex(SP), allocatable:: pp_rho_nlcc(:)
 integer(LP), allocatable:: pp_has_nlcc(:)
 !
 ! PW (OLD)
 !=========
 complex(SP), allocatable :: Vnl(:,:,:,:,:)
 !
 contains
   !
   ! Abinit
   subroutine PP_alloc_abinit()
     use memory_m,   ONLY:mem_est
     use R_lattice,  ONLY:ng_vec
     integer  :: alloc_err(3)
     allocate(pp_kbs(n_atomic_species,pp_n_l_times_proj_max),stat=alloc_err(1))
     allocate(pp_kb(ng_vec,n_atomic_species,pp_n_l_times_proj_max,n_spin),stat=alloc_err(2))
     allocate(pp_kbd(ng_vec,n_atomic_species,pp_n_l_times_proj_max,n_spin),stat=alloc_err(3))
     call mem_est("pp_kbs pp_kb pp_kbd",(/size(pp_kbs),size(pp_kb),size(pp_kbd)/),&
&                 elements_kind=(/SP,SP,SP/),errors=alloc_err)
     pp_kbs =0.
     pp_kb  =0.
     pp_kbd =0.
   end subroutine
   !
   ! PWscf
   subroutine PP_alloc_pwscf()
     use memory_m,   ONLY:mem_est
     use wave_func,  ONLY:wf_ncx
     integer  :: alloc_err(3)
     allocate(pp_kbs(n_atomic_species,pp_n_l_times_proj_max),stat=alloc_err(1))
     allocate(pp_kb(wf_ncx,n_atomic_species,pp_n_l_times_proj_max,1),stat=alloc_err(2))
     allocate(pp_kbd(wf_ncx,n_atomic_species,pp_n_l_times_proj_max,1),stat=alloc_err(3))
     call mem_est("pp_kbs pp_kb pp_kbd",(/size(pp_kbs),size(pp_kb),size(pp_kbd)/),&
&                 elements_kind=(/SP,SP,SP/),errors=alloc_err)
     pp_kbs =0.
     pp_kb  =0.
     pp_kbd =0.
   end subroutine
   !
   subroutine PP_free()
     use memory_m,   ONLY:mem_est
     if (.not.allocated(pp_kbs)) return
     deallocate(pp_kbs,pp_kb,pp_kbd)
     call mem_est("pp_kbs pp_kb pp_kbd")
   end subroutine
   !
   subroutine PP_nlcc_alloc()
     use memory_m,   ONLY:mem_est
     use R_lattice,  ONLY:ng_vec
     implicit none
     integer  :: alloc_err(2)
     allocate(pp_rho_nlcc(ng_vec),stat=alloc_err(1))
     allocate(pp_has_nlcc(n_atomic_species),stat=alloc_err(2))
     call mem_est("pp_rho_nlcc pp_has_nlcc",(/size(pp_rho_nlcc),size(pp_has_nlcc)/),&
&                 elements_kind=(/SP,SP/),errors=alloc_err)
     pp_rho_nlcc =0.
     pp_has_nlcc =0
   end subroutine 
   !
   subroutine PP_nlcc_free()
     use memory_m,   ONLY:mem_est
     if (.not.allocated(pp_rho_nlcc)) return
     deallocate(pp_rho_nlcc)
     deallocate(pp_has_nlcc)
     call mem_est("pp_rho_nlcc pp_has_nlcc")
   end subroutine
   !
end module pseudo
