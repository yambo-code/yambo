!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA, AM
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
module interpolate
 !
 use pars,       ONLY:SP,DP,schlen
 use com,        ONLY:error
 use R_lattice,  ONLY:bz_samp
 !
 ! Interpolation according to PRB 38 p2721
 ! Code copied from BolzTraP 
 !
#include<memory.h>
 !
 integer               :: nshells
 real(SP), allocatable :: int_sop(:,:,:)
 real(SP), pointer     :: lattice_vectors(:,:)
 real(SP) :: INTERP_shell_factor
 real(SP) :: metric(3,3)
 !
 character(schlen) :: INTERPOLATION_name = " "
 !
 ! ID's
 !
 integer :: INTERP_E_id =0
 integer :: INTERP_dE_id=0
 integer :: INTERP_f_id=0
 integer :: INTERP_RT_id=0
 !
 ! K-grids
 !
 type(bz_samp):: GRID_k
 !
 ! Interpolation type
 !
 character(schlen), parameter :: REAL1D   = "REAL1D"
 character(schlen), parameter :: REAL2D   = "REAL2D"
 character(schlen), parameter :: CMPLX1D  = "CMPLX1D"
 character(schlen), parameter :: CMPLX2D  = "CMPLX2D"
 ! 
 type INTERP_coeff
   character(schlen)        :: interp_type = " "
   complex(DP), allocatable :: engre(:,:)
   integer                  :: ndim = 0
 end type INTERP_coeff
 ! 
 type INTERP_element
   integer                  :: nb(2)=0
   integer                  :: D=0  ! 3rd dimension of SEED
   integer                  :: ID=0
   logical                  :: in_use=.FALSE.
   logical                  :: use_NN=.FALSE.
   character(schlen)        :: what=" "
   real(SP),    allocatable :: SEED(:,:,:)
   real(SP),    allocatable :: INTERP(:,:,:)
 end type INTERP_element
 !
 integer, parameter :: max_interpls=10
 !
 type(INTERP_coeff)   :: interpls(max_interpls)
 type(INTERP_element) :: INTERP_obj(max_interpls)
 !
 interface
   !
   subroutine INTERPOLATION_coefficients(R1D,R2D,C1D,C2D,E,k,Nk,ID,ID_obj)
     use pars,       ONLY:SP
     use R_lattice,  ONLY:bz_samp
     use electrons,  ONLY:levels
     implicit none
     type(bz_samp), intent(in)           :: k
     real(SP),      intent(in), optional :: R1D(:,:),R2D(:,:,:)
     complex(SP),   intent(in), optional :: C1D(:,:),C2D(:,:,:)
     type(levels),  intent(in), optional :: E
     integer,       intent(in), optional :: Nk
     integer,       intent(in)           :: ID_obj
     integer,       intent(out)          :: ID
   end subroutine  INTERPOLATION_coefficients
   !
   subroutine INTERPOLATION_BZ(R1D,R2D,C1D,C2D,E,K,ID)
     use pars,       ONLY:SP
     use R_lattice,  ONLY:bz_samp
     use electrons,  ONLY:levels
     implicit none
     type(bz_samp), intent(in)            :: K
     real(SP),      intent(out), optional :: R1D(:,:),R2D(:,:,:)
     complex(SP),   intent(out), optional :: C1D(:,:),C2D(:,:,:)
     type(levels),  intent(out), optional :: E
     integer,       intent(in)            :: ID
   end subroutine INTERPOLATION_BZ
   !
   subroutine electrons_bands_interpolate(Xen,Xk,BANDS_range,CIRCUIT_E,GRID_E,CIRCUIT_dE)
     use pars,       ONLY:SP
     use electrons,  ONLY:levels
     use R_lattice,  ONLY:bz_samp
     implicit none
     type(bz_samp), intent(in)  :: Xk
     type(levels),  intent(in)  :: Xen
     integer      , intent(in)  :: BANDS_range(2)
     type(levels),  intent(out) :: CIRCUIT_E
     type(levels),  intent(out), optional  :: GRID_E
     type(levels),  intent(out), optional  :: CIRCUIT_dE
   end subroutine electrons_bands_interpolate
   !
   subroutine INTERPOLATION_driver(bands,SEED_k,SEED_E,SEED_RT,SEED_R3,OUT_k,OUT_E,operation,QUIET,ID,NEAREST)
     use pars,        ONLY:SP
     use real_time,   ONLY:RT_t
     use R_lattice,   ONLY:bz_samp
     use electrons,   ONLY:levels
     implicit none
     integer,       intent(in),    optional :: bands(2)
     type(bz_samp), intent(in),    optional :: SEED_k
     type(bz_samp), intent(inout), optional :: OUT_k
     real(SP),      intent(in)   , optional :: SEED_R3(:,:,:)
     type(RT_t),    intent(in)   , optional :: SEED_RT
     type(levels),  intent(in)   , optional :: SEED_E
     type(levels),  intent(inout), optional :: OUT_E
     character(*),  intent(in)              :: operation
     logical,       intent(in)   , optional :: QUIET,NEAREST
     integer,       intent(out)  , optional :: ID
   end subroutine INTERPOLATION_driver
   !
 end interface  
 !  
 contains
   !
   subroutine INTERPOLATION_reset(ID)
     integer, intent(in) :: ID
     interpls(ID)%ndim=0
     interpls(ID)%interp_type=""
     if (allocated(interpls(ID)%engre)) deallocate(interpls(ID)%engre)
     INTERP_obj(ID)%nb=0
     INTERP_obj(ID)%D=0
     INTERP_obj(ID)%ID=0
     INTERP_obj(ID)%in_use=.FALSE.
     INTERP_obj(ID)%what=" "
     YAMBO_FREE(INTERP_obj(ID)%SEED)
     YAMBO_FREE(INTERP_obj(ID)%INTERP)
   end subroutine
   !
   subroutine get_ID(ID)
     implicit none
     integer, intent(out) :: ID
     integer :: i1
     logical :: done
     !
     done=.false.
     i1=1
     !
     do while(.not.done.and.i1<max_interpls)
       if(interpls(i1)%ndim==0) then
         ID=i1
         done=.true.
       endif
       i1=i1+1
     enddo
     !
     if(.not.done) call error("Error too many simultaneous interpolations.")
     !
   end subroutine get_ID
   !
   subroutine make_star(R_vec,nsym,symop,nstar,star_vec)
     use vec_operate,    ONLY:v_is_zero
     !
     implicit none
     !
     integer,  intent(in)   :: nsym
     real(SP), intent(in)   :: R_vec(3),symop(3,3,nsym) ! input vector and symmetry operations
     real(SP), intent(out)  :: star_vec(3,nsym)         ! star vectors - maximum number is nsym
     integer,  intent(out)  :: nstar                    ! number of vectors in star
     !
     ! Work Space
     !
     integer  :: i1,is
     logical  :: add_vec
     real(SP) :: try(3)
     !
     nstar=1
     star_vec(1:3,1)=R_vec(1:3)
     !
     do is=1,nsym 
       try(1:3)=matmul(symop(:,:,is),R_vec(:))
       add_vec=.TRUE.
       do i1=1,nstar
         if(v_is_zero(try(:)-star_vec(:,i1))) add_vec=.FALSE.
       enddo
       !
       if(add_vec) then
         nstar=nstar+1
         star_vec(1:3,nstar)=try(1:3)
       endif
     enddo
  end subroutine make_star
  !
end module interpolate
