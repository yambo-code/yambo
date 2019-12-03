!
! Copyright (C) 2002-2018 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Utility functions to perform memcpy and memset on the device with CUDA Fortran
! cuf_memXXX contains a CUF KERNEL to perform the selected operation
! cu_memsync are wrappers for cuda_memcpy functions
!
#if defined(_CUDA)
#  define __CUDA
#endif
!
module deviceXlib_m

  use device_memcpy_m
  use device_auxfunc_m
  use device_fbuff_m
  implicit none

end module deviceXlib_m

