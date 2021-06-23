
#include<dev_defs.h>
!
module cufft_m
  use iso_c_binding
  implicit none

  integer(c_int), parameter :: CUFFT_FORWARD = -1
  integer(c_int), parameter :: CUFFT_INVERSE = 1
  ! CUFFT Status
  enum, bind(C)
      enumerator :: CUFFT_SUCCESS        = 0
      enumerator :: CUFFT_INVALID_PLAN   = 1
      enumerator :: CUFFT_ALLOC_FAILED   = 2
      enumerator :: CUFFT_INVALID_TYPE   = 3
      enumerator :: CUFFT_INVALID_VALUE  = 4
      enumerator :: CUFFT_INTERNAL_ERROR = 5
      enumerator :: CUFFT_EXEC_FAILED    = 6
      enumerator :: CUFFT_SETUP_FAILED   = 7
      enumerator :: CUFFT_INVALID_SIZE   = 8
      enumerator :: CUFFT_UNALIGNED_DATA = 9
  end enum
  ! CUFFT Transform Types
  enum, bind(C)
      enumerator :: CUFFT_R2C = z'2a'     ! Real to Complex (interleaved)
      enumerator :: CUFFT_C2R = z'2c'     ! Complex (interleaved) to Real
      enumerator :: CUFFT_C2C = z'29'     ! Complex to Complex, interleaved
      enumerator :: CUFFT_D2Z = z'6a'     ! Double to Double-Complex
      enumerator :: CUFFT_Z2D = z'6c'     ! Double-Complex to Double
      enumerator :: CUFFT_Z2Z = z'69'     ! Double-Complex to Double-Complex
  end enum
  ! CUFFT Data Layouts
  enum, bind(C)
      enumerator :: CUFFT_COMPATIBILITY_NATIVE          = 0
      enumerator :: CUFFT_COMPATIBILITY_FFTW_PADDING    = 1
      enumerator :: CUFFT_COMPATIBILITY_FFTW_ASYMMETRIC = 2
      enumerator :: CUFFT_COMPATIBILITY_FFTW_ALL        = 3
  end enum
  integer(c_int), parameter :: CUFFT_COMPATIBILITY_DEFAULT = CUFFT_COMPATIBILITY_FFTW_PADDING

  interface

    integer(c_int) function cufftPlan3d( plan, nx, ny, nz, ffttype ) &
        bind(C, name="cufftPlan3d")
      use iso_c_binding
      implicit none
      integer(c_int) :: plan
      integer(c_int) :: nx, ny, nz
      integer(c_int) :: ffttype
    end function cufftPlan3d
  
    integer(c_int) function cufftDestroy( plan ) &
        bind(C, name="cufftDestroy")
      use iso_c_binding
      implicit none
      integer(c_int) :: plan
    end function cufftDestroy
    
    integer(c_int) function cufftExecZ2Z( plan, idata, odata, direction ) &
        bind(C, name="cufftExecZ2Z")
      use iso_c_binding
      implicit none
      integer(c_int) :: plan
      complex(c_double) DEV_ATTR, dimension(*) :: idata, odata
      integer(c_int) :: direction
    end function cufftExecZ2Z

  end interface

end module cufft_m
