!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use pars,               ONLY:SP,DP
 !
 implicit none
 !
 interface linkM2V
   module procedure linkM2V_real,linkM2V_complex
 end interface
 !
 interface linkC2R
   module procedure linkC2R_0D,linkC2R_1D,linkC2R_2D,linkC2R_3D,linkC2R_4D,linkC2R_5D
 end interface
 !
 interface linkZ2D
   module procedure linkZ2D_0D,linkZ2D_1D,linkZ2D_2D,linkZ2D_3D,linkZ2D_4D,linkZ2D_5D
 end interface
 !
 contains
   !
   ! Interface which links Matrix to Vector
   !
   function linkM2V_real(M,N) result(VEC_pointer)
     real(SP), target  :: M(1,1)
     integer, intent(in)  :: N(:)
     real(SP), pointer :: VEC_pointer(:)
     call C_F_POINTER(C_LOC(M), VEC_pointer, N )
   end function linkM2V_real
   !
   function linkM2V_complex(M,N) result(VEC_pointer)
     complex(SP), target  :: M(1,1)
     integer, intent(in)  :: N(:)
     complex(SP), pointer :: VEC_pointer(:)
     call C_F_POINTER(C_LOC(M), VEC_pointer, N )
   end function linkM2V_complex
   !
   ! Interface which links Complex to Real
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
   function linkC2R_5D(M,N) result(REAL_pointer)
     complex(SP), target  :: M(1,1,1,1,1)
     integer, intent(in)  :: N(:)
     real(SP), pointer :: REAL_pointer(:,:,:,:,:,:)
     call C_F_POINTER(C_LOC(M), REAL_pointer, N )
   end function linkC2R_5D
   !
   ! Interface which links Z-Complex to Double Prec
   !
   function linkZ2D_0D(M,N) result(REAL_pointer)
     complex(DP), target  :: M
     integer, intent(in)  :: N(:)
     real(DP), pointer :: REAL_pointer(:)
     call C_F_POINTER(C_LOC(M), REAL_pointer, N )
   end function linkZ2D_0D
   !
   function linkZ2D_1D(M,N) result(REAL_pointer)
     complex(DP), target  :: M(1)
     integer, intent(in)  :: N(:)
     real(DP), pointer :: REAL_pointer(:,:)
     call C_F_POINTER(C_LOC(M), REAL_pointer, N )
   end function linkZ2D_1D
   !
   function linkZ2D_2D(M,N) result(REAL_pointer)
     complex(DP), target  :: M(1,1)
     integer, intent(in)  :: N(:)
     real(DP), pointer :: REAL_pointer(:,:,:)
     call C_F_POINTER(C_LOC(M), REAL_pointer, N )
   end function linkZ2D_2D
   !
   function linkZ2D_3D(M,N) result(REAL_pointer)
     complex(DP), target  :: M(1,1,1)
     integer, intent(in)  :: N(:)
     real(DP), pointer :: REAL_pointer(:,:,:,:)
     call C_F_POINTER(C_LOC(M), REAL_pointer, N )
   end function linkZ2D_3D
   !
   function linkZ2D_4D(M,N) result(REAL_pointer)
     complex(DP), target  :: M(1,1,1,1)
     integer, intent(in)  :: N(:)
     real(DP), pointer :: REAL_pointer(:,:,:,:,:)
     call C_F_POINTER(C_LOC(M), REAL_pointer, N )
   end function linkZ2D_4D
   !
   function linkZ2D_5D(M,N) result(REAL_pointer)
     complex(DP), target  :: M(1,1,1,1,1)
     integer, intent(in)  :: N(:)
     real(DP), pointer :: REAL_pointer(:,:,:,:,:,:)
     call C_F_POINTER(C_LOC(M), REAL_pointer, N )
   end function linkZ2D_5D
   !
end module
