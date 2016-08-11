!
!        Copyright (C) 2000-2016 the YAMBO team
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
module linear_algebra
 !
 use pars, ONLY:SP
 !
 implicit none
 !
 integer, parameter :: USE_LK=1
 integer, parameter :: USE_SLK=2
 integer, parameter :: ALREADY_FILLED=3
 integer, parameter :: DIAGO=4
 integer, parameter :: INV=5
 integer, parameter :: PARTIAL_DIAGO=6
 integer, parameter :: USE_SVD=7
 integer, parameter :: min_cpu_block_size=100
 !
 type LLAGEBRA_WS
   !
   real(SP),    allocatable :: v_real(:)
   real(SP),    allocatable :: vp_real(:)
   complex(SP), allocatable :: v_cmplx(:)
   complex(SP), allocatable :: m1_cmplx(:,:)
   complex(SP), allocatable :: m2_cmplx(:,:)
   integer    , allocatable :: v_int(:)
   integer                  :: i_fail
   integer                  :: dim
   !
 end type LLAGEBRA_WS
 !
 type(LLAGEBRA_WS) :: WS
 !
 interface 
   !
   subroutine LINEAR_ALGEBRA_driver(idriver,M,lib_in,V_left,V_right,E_real,E_cmpl,SVD_digits,det)
     !
     use pars,           ONLY:SP
     !
     integer                :: idriver
     complex(SP)            :: M(:,:)
     integer,      optional :: lib_in
     complex(SP),  optional :: V_left(:,:)
     complex(SP),  optional :: V_right(:,:)
     real(SP),     optional :: E_real(:)
     complex(SP),  optional :: E_cmpl(:)
     integer,      optional :: SVD_digits 
     complex(SP),  optional :: det
     !
   end subroutine
   !
 end interface
 !
 contains
   !
   subroutine LINEAR_ALGEBRA_error(calling_subr,message_)
     use stderr,  ONLY:string_pack
     use com,     ONLY:error
     character(*) calling_subr,message_
     call error( trim( string_pack('Matrix Ops. driver [',calling_subr,']:',message_) ))
   end subroutine
   !
end module linear_algebra
