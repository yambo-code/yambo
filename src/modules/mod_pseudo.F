!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS, AF
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
 use pars,        ONLY:SP,DP,LP
 use electrons,   ONLY:n_sp_pol
 use R_lattice,   ONLY:nkibz
 use D_lattice,   ONLY:n_atomic_species,n_atoms_species
 use qe_pseudo_m, ONLY:qe_pseudo_deallocate, qe_pseudo_allocate,&
&                      bec_type, deallocate_bec_type
#include <memory.h>
 !
 ! *** Non-Local pseudo. The [r,Vnl] commutator ***
 !
 logical :: l_many_proj
 integer :: pp_n_l_max
 ! max number of angular momentum channel

 integer :: pp_n_l_times_proj_max
 ! max number of angular momentum channel times number of projectors

 integer :: pp_kbv_dim
 integer :: pp_kbv_dim_yambo
 integer,  allocatable:: pp_kbv_table(:)
 integer,  allocatable:: pp_table(:,:,:)
 ! (3,n_atomic_species,pp_n_l_times_proj_max)
 ! the first dimension contains (angular_momentum,nprojectors,spin)

 integer,  allocatable:: pp_n_l_comp(:)
 !

 real(SP), allocatable:: pp_factor(:,:,:,:,:)
 !
 ! ABINIT (old) KB
 !=====================
 real(SP), allocatable:: pp_kbs_old(:,:)
 real(SP), allocatable:: pp_kb_old(:,:,:,:)
 real(SP), allocatable:: pp_kbd_old(:,:,:,:)
 !
 ! ABINIT and PWSCF KB
 !=====================
 real(SP), allocatable:: pp_kbs(:,:)
 real(SP), allocatable:: pp_kb(:,:,:,:)
 real(SP), allocatable:: pp_kbd(:,:,:,:)
 ! 
 ! NLCC 
 !=====================
 logical                 :: use_nlcc=.FALSE.
 logical                 :: pp_has_nlcc=.FALSE. 
 complex(SP), allocatable:: pp_rhog_nlcc(:)
 complex(DP), allocatable:: pp_rho_nlcc(:)
 integer(LP), allocatable:: pp_has_nlcc_atom(:)
 ! 
 ! USPP
 !=====================
 logical                 :: pp_is_uspp=.FALSE. 
 logical                 :: qe_pseudo_alloc=.FALSE. 
 integer,     allocatable:: qe_atoms_map(:,:)
 type(bec_type), allocatable:: becp(:,:)
 !
 ! PW (OLD)
 !=========
 complex(SP), allocatable :: Vnl(:,:,:,:,:)
 !
 contains
   !
   ! Abinit (OLD)
   subroutine PP_alloc_abinit_old()
     use R_lattice,  ONLY:ng_vec
     YAMBO_ALLOC(pp_kbs_old,(n_atomic_species,pp_n_l_times_proj_max))
     YAMBO_ALLOC(pp_kb_old,(ng_vec,n_atomic_species,pp_n_l_times_proj_max,n_sp_pol))
     YAMBO_ALLOC(pp_kbd_old,(ng_vec,n_atomic_species,pp_n_l_times_proj_max,n_sp_pol))
     pp_kbs_old =0._SP
     pp_kb_old  =0._SP
     pp_kbd_old =0._SP
   end subroutine
   !
   ! Abinit
   subroutine PP_alloc_abinit()
     use wave_func,  ONLY:wf_ncx
     YAMBO_ALLOC(pp_kbs,(n_atomic_species,pp_n_l_times_proj_max))
     YAMBO_ALLOC(pp_kb,(wf_ncx,n_atomic_species,pp_n_l_times_proj_max,n_sp_pol))
     YAMBO_ALLOC(pp_kbd,(wf_ncx,n_atomic_species,pp_n_l_times_proj_max,n_sp_pol))
     pp_kbs =0._SP
     pp_kb  =0._SP
     pp_kbd =0._SP
   end subroutine
   !
   ! PWscf
   subroutine PP_alloc_pwscf()
     use wave_func,  ONLY:wf_ncx
     YAMBO_ALLOC(pp_kbs,(n_atomic_species,pp_n_l_times_proj_max))
     YAMBO_ALLOC(pp_kb,(wf_ncx,n_atomic_species,pp_n_l_times_proj_max,1))
     YAMBO_ALLOC(pp_kbd,(wf_ncx,n_atomic_species,pp_n_l_times_proj_max,1))
     pp_kbs =0._SP
     pp_kb  =0._SP
     pp_kbd =0._SP
   end subroutine
   !
   subroutine PP_free_old()
     YAMBO_FREE(pp_kbs_old)
     YAMBO_FREE(pp_kb_old)
     YAMBO_FREE(pp_kbd_old)
   end subroutine
   !
   subroutine PP_free()
     YAMBO_FREE(pp_kbs)
     YAMBO_FREE(pp_kb)
     YAMBO_FREE(pp_kbd)
   end subroutine
   !
   subroutine PP_nlcc_alloc()
     use R_lattice,  ONLY:ng_vec
     use FFT_m,      ONLY:fft_size
     implicit none
     YAMBO_ALLOC(pp_rhog_nlcc,(ng_vec))
     YAMBO_ALLOC(pp_has_nlcc_atom,(n_atomic_species))
     pp_rhog_nlcc =0.
     pp_has_nlcc_atom =0
   end subroutine 
   !
   subroutine PP_nlcc_free()
     YAMBO_FREE(pp_rhog_nlcc)
     YAMBO_FREE(pp_rho_nlcc)
     YAMBO_FREE(pp_has_nlcc_atom)
     pp_has_nlcc=.FALSE.
   end subroutine
   !
   subroutine PP_uspp_alloc()
     implicit none
     if (qe_pseudo_alloc) call PP_uspp_free()
     call qe_pseudo_allocate() 
     allocate(becp(nkibz,n_sp_pol))
     allocate(qe_atoms_map(maxval(n_atoms_species),n_atomic_species))
     qe_pseudo_alloc=.true.
   end subroutine
   !
   subroutine PP_uspp_free(becp_only)
     implicit none
     logical, optional, intent(in) :: becp_only
     integer :: ik,is
     logical :: becp_only_
     !
     becp_only_=.false.
     if (present(becp_only)) becp_only_=becp_only 
     !
     if (.not.qe_pseudo_alloc) return
     !
     if (.not.becp_only_) then
       call qe_pseudo_deallocate()
       if(allocated(qe_atoms_map)) deallocate(qe_atoms_map)
     endif
     !
     if (allocated(becp)) then
       do is = 1, n_sp_pol
       do ik = 1, nkibz
         call deallocate_bec_type(becp(ik,is))
       enddo
       enddo
       deallocate(becp)
     endif
     !
     if (.not.becp_only_) qe_pseudo_alloc=.false.
   end subroutine
   !
end module pseudo
