!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
module pointers
 !
 use iso_c_binding,      ONLY:C_LOC,C_F_POINTER
 use pars,               ONLY:SP
 !
 implicit none
 !
 interface linkC2R
   module procedure linkC2R_0D,linkC2R_1D,linkC2R_2D,linkC2R_3D,linkC2R_4D
 end interface
 !
 contains
   !
   function linkC2R_0D(M,N) result(REAL_pointer)
     complex(SP), target  :: M
     integer, intent(in)  :: N(:)
     real(SP), pointer :: REAL_pointer(:)
     call C_F_POINTER(C_LOC(M), REAL_pointer, N )
   end function linkC2R_0D
   !
   function linkC2R_1D(M,N) result(REAL_pointer)
     complex(SP), target  :: M(1)
     integer, intent(in)  :: N(:)
     real(SP), pointer :: REAL_pointer(:,:)
     call C_F_POINTER(C_LOC(M), REAL_pointer, N )
   end function linkC2R_1D
   !
   function linkC2R_2D(M,N) result(REAL_pointer)
     complex(SP), target  :: M(1,1)
     integer, intent(in)  :: N(:)
     real(SP), pointer :: REAL_pointer(:,:,:)
     call C_F_POINTER(C_LOC(M), REAL_pointer, N )
   end function linkC2R_2D
   !
   function linkC2R_3D(M,N) result(REAL_pointer)
     complex(SP), target  :: M(1,1,1)
     integer, intent(in)  :: N(:)
     real(SP), pointer :: REAL_pointer(:,:,:,:)
     call C_F_POINTER(C_LOC(M), REAL_pointer, N )
   end function linkC2R_3D
   !
   function linkC2R_4D(M,N) result(REAL_pointer)
     complex(SP), target  :: M(1,1,1,1)
     integer, intent(in)  :: N(:)
     real(SP), pointer :: REAL_pointer(:,:,:,:,:)
     call C_F_POINTER(C_LOC(M), REAL_pointer, N )
   end function linkC2R_4D
   !
end module
