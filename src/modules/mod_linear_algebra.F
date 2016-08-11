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
 interface 
   !
   subroutine MATRIX_driver(idriver,M,lib_in,V_left,V_right,E_real,E_cmpl,SVD_digits,det)
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
end module linear_algebra
