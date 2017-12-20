!
!        Copyright (C) 2000-2017 the YAMBO team
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
module interpolate
 !
 use pars,       ONLY:SP,DP,schlen
 use com,        ONLY:error
 !
 ! Interpolation according to PRB 38 p2721
 ! Code copied from BolzTraP 
 !
 implicit none
 !
 integer               :: nshells
 real(SP), allocatable :: int_sop(:,:,:)
 real(SP), pointer     :: lattice_vectors(:,:)
 real(SP) :: INTERP_shell_factor
 real(SP) :: metric(3,3)
 !
 character(schlen) :: INTERPOLATION_name
 !
 ! Interpolation type
 !
 character(schlen), parameter :: REAL1D   = "REAL1D"
 character(schlen), parameter :: REAL2D   = "REAL2D"
 character(schlen), parameter :: CMPLX1D  = "CMPLX1D"
 character(schlen), parameter :: CMPLX2D  = "CMPLX2D"
 ! 
 type interp_coeff
   character(schlen)     :: interp_type
   complex(DP), allocatable :: engre(:,:)
   integer               :: ndim
 end type interp_coeff
 !
 integer, parameter :: max_interpls=10
 !
 type(interp_coeff) :: interpls(max_interpls)
 !
 interface
   !
   subroutine eval_interpolation_coeff(R1D,R2D,C1D,C2D,E,k,Nk,ID)
     use pars,       ONLY:SP
     use R_lattice,  ONLY:bz_samp
     use electrons,  ONLY:levels
     implicit none
     type(bz_samp), intent(in)           :: k
     real(SP),      intent(in), optional :: R1D(:,:),R2D(:,:,:)
     complex(SP),   intent(in), optional :: C1D(:,:),C2D(:,:,:)
     type(levels),  intent(in), optional :: E
     integer,       intent(in), optional :: Nk
     integer,       intent(out)          :: ID
   end subroutine  eval_interpolation_coeff
   !
   subroutine bz_interpolation(R1D,R2D,C1D,C2D,E,K,ID)
     use pars,       ONLY:SP
     use R_lattice,  ONLY:bz_samp
     use electrons,  ONLY:levels
     implicit none
     type(bz_samp), intent(in)            :: K
     real(SP),      intent(out), optional :: R1D(:,:),R2D(:,:,:)
     complex(SP),   intent(out), optional :: C1D(:,:),C2D(:,:,:)
     type(levels),  intent(out), optional :: E
     integer,       intent(in)            :: ID
   end subroutine bz_interpolation
   !
   subroutine electrons_bands_interpolate(Xen,Xk,ID_interp,BANDS_range,CIRCUIT_E,INTERP_E,CIRCUIT_dE)
     use pars,       ONLY:SP
     use electrons,  ONLY:levels
     use R_lattice,  ONLY:bz_samp
     type(bz_samp), intent(in)  :: Xk
     type(levels),  intent(in)  :: Xen
     integer      , intent(in)  :: BANDS_range(2)
     integer      , intent(out) :: ID_interp
     type(levels),  intent(out) :: CIRCUIT_E
     type(levels),  intent(out), optional  :: INTERP_E
     type(levels),  intent(out), optional  :: CIRCUIT_dE
   end subroutine electrons_bands_interpolate
   !
 end interface  
 !  
 contains
   !
   subroutine reset_interpolation(ID)
     implicit none
     integer, intent(in) :: ID
     !
     interpls(ID)%ndim=0
     interpls(ID)%interp_type=""
     if(allocated(interpls(ID)%engre)) deallocate(interpls(ID)%engre)
     !
   end subroutine reset_interpolation
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
     if(.not.done) call error("Error too many interpolations!!! ")
     !
   end subroutine get_ID
   !
   subroutine make_star(R_vec,nsym,symop,nstar,star_vec)
     use vec_operate,    ONLY:v_is_zero
     use D_lattice,      ONLY:i_time_rev
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
       if(is>nsym/(i_time_rev+1)) cycle
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
