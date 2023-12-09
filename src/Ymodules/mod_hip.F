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

#if defined _HIP 
  !
  use iso_c_binding
  implicit none
  !
  enum, bind(C) 
      enumerator :: HIP_SUCCESS = 0 
  end enum

  integer, parameter :: hipStreamDefault = 0
  integer, parameter :: hipStreamNonBlocking = 1

  interface  
     !
     function hipDeviceSynchronize() &
        BIND(C,name="hipDeviceSynchronize")
        use iso_c_binding
        import HIP_SUCCESS
        implicit none
        integer(kind(HIP_SUCCESS)) :: hipDeviceSynchronize
     end function hipDeviceSynchronize
     !
     function hipGetDeviceCount(ncount) &
        BIND(C,name="hipGetDeviceCount")
        use iso_c_binding
        import HIP_SUCCESS
        implicit none
        integer(c_int) :: ncount
        integer(kind(HIP_SUCCESS)) :: hipGetDeviceCount
     end function hipGetDeviceCount
     !
     function hipGetDevice(deviceId) &
              bind(C,name="hipGetDevice")
        use iso_c_binding
        import HIP_SUCCESS
        implicit none
        integer(c_int) :: deviceId
        integer(kind(HIP_SUCCESS)) :: hipGetDevice
     end function hipGetDevice
     !
     function hipSetDevice(deviceId) &
              bind(C,name="hipSetDevice")
        use iso_c_binding
        import HIP_SUCCESS
        implicit none
        integer(c_int), value :: deviceId
        integer(kind(HIP_SUCCESS)) :: hipSetDevice
     end function hipSetDevice
     !
     function hipGetLastError() &
              bind(C,name="hipGetLastError")
        use iso_c_binding
        import HIP_SUCCESS
        implicit none
        integer(kind(HIP_SUCCESS)) :: hipGetLastError
     end function hipGetLastError
     !
     function hipStreamCreate(stream) bind(c, name="hipStreamCreate")
        use iso_c_binding
        import HIP_SUCCESS
        implicit none
        integer(kind(HIP_SUCCESS)) :: hipStreamCreate
        !type(c_ptr) :: stream
        integer(c_intptr_t) :: stream
      end function
      !
      function hipStreamCreateWithFlags(stream,flags) &
               bind(c, name="hipStreamCreateWithFlags")
        use iso_c_binding
        import HIP_SUCCESS
        implicit none
        integer(kind(HIP_SUCCESS)) :: hipStreamCreateWithFlags
        !type(c_ptr) :: stream
        integer(c_intptr_t) :: stream
        integer(c_int),value :: flags
      end function
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

