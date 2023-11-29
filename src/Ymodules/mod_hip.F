!
!        Copyright (C) 2000-2023 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF
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
! Copyright (C) Quantum ESPRESSO group
! Copyright (C) 2022 Advanced Micro Devices, Inc. All Rights Reserved.
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! AF: part of this module has been adapted from
!     fft_scalar.hipFFT.f90 from the Quantum ESPRESSO distribution
!
module hip_m

#if defined(_HIP)
  !
  use iso_c_binding
  implicit none
  !
  enum, bind(C) 
      enumerator :: HIP_SUCCESS = 0 
  end enum

  interface  
     !
     function HipDeviceSynchronize() &
        BIND(C,name="hipDeviceSynchronize")
        use iso_c_binding
        !use enums
        implicit none
        integer(kind(HIP_SUCCESS)) :: HipDeviceSynchronize
     end function HipDeviceSynchronize
     !
     function HipGetLastError() &
        BIND(C,name="hipGetLastError")
        use iso_c_binding
        !use enums
        implicit none
        integer(kind(HIP_SUCCESS)) :: HipGetLastError
     end function hipDeviceSynchronize
     !
   end interface

contains

  subroutine hipcheck(hiperror)
      IMPLICIT NONE
      integer(kind(HIP_SUCCESS)) :: hiperror
      if (hiperror /= HIP_SUCCESS) then
         write (*, *) "HIP ERROR: ERROR CODE = ", hiperror
         call exit(hiperror)
      endif
  end subroutine hipCheck

#else

contains
  !
  ! dummy subroutine to avoid having an empty module
  subroutine hip_dummy__(i)
     integer :: i
     i=0 
  end subroutine


#endif

end module

