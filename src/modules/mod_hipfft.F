!
! Copyright (C) Quantum ESPRESSO group
! Copyright (C) 2022 Advanced Micro Devices, Inc. All Rights Reserved.
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! AF: the following module has been freely adapted from
!     fft_scalar.hipFFT.f90 from the Quantum ESPRESSO distribution
!
MODULE hipfft_m
  USE iso_c_binding
  IMPLICIT NONE

  INTEGER(C_INT), PARAMETER, PUBLIC :: HIPFFT_FORWARD = -1, HIPFFT_BACKWARD = 1

#if defined(_HIP)
  ENUM, BIND(C) 
      ENUMERATOR :: HIPFFT_SUCCESS = 0 
  END ENUM

  ENUM, BIND(C)
    ENUMERATOR :: HIPFFT_R2C = 42
    ENUMERATOR :: HIPFFT_C2R = 44
    ENUMERATOR :: HIPFFT_C2C = 41
    ENUMERATOR :: HIPFFT_D2Z = 106 
    ENUMERATOR :: HIPFFT_Z2D = 108 
    ENUMERATOR :: HIPFFT_Z2Z = 105 
  END ENUM

  interface  

     function hipfftPlan1d(plan,nx,myType,batch) &
              bind(C, name="hipfftPlan1d")
       use iso_c_binding
       import HIPFFT_SUCCESS, HIPFFT_R2C
       implicit none
       integer(kind(HIPFFT_SUCCESS)) :: hipfftPlan1d
       type(c_ptr) :: plan
       integer(c_int),value :: nx
       integer(kind(HIPFFT_R2C)),value :: myType
       integer(c_int),value :: batch
     end function

     function hipfftPlan3d(plan,nx,ny,nz,myType) &
              bind(c, name="hipfftPlan3d")
       use iso_c_binding
       import HIPFFT_SUCCESS, HIPFFT_R2C
       implicit none
       integer(kind(HIPFFT_SUCCESS)) :: hipfftPlan3d
       type(c_ptr) :: plan
       integer(c_int),value :: nx
       integer(c_int),value :: ny
       integer(c_int),value :: nz
       integer(kind(HIPFFT_R2C)),value :: myType
     end function

     FUNCTION hipfftExecZ2Z(plan,idata,odata,direction) BIND(C, name="hipfftExecZ2Z")
       USE iso_c_binding
       import HIPFFT_SUCCESS
       IMPLICIT NONE
       INTEGER(kind(HIPFFT_SUCCESS)) :: hipfftExecZ2Z
       TYPE(C_PTR),VALUE :: plan
       TYPE(C_PTR),VALUE :: idata
       TYPE(C_PTR),VALUE :: odata
       INTEGER(C_INT),VALUE :: direction
     END FUNCTION

     FUNCTION hipfftPlanMany(plan,rank,n,inembed,istride,idist,onembed,ostride,odist,myType,batch) &
         bind(c, name="hipfftPlanMany")
       USE iso_c_binding
       import HIPFFT_SUCCESS, HIPFFT_R2C
       IMPLICIT NONE
       INTEGER(KIND(HIPFFT_SUCCESS)) :: hipfftPlanMany
       TYPE(C_PTR) :: plan
       INTEGER(C_INT),VALUE :: rank
       TYPE(C_PTR),VALUE :: n
       TYPE(C_PTR),VALUE :: inembed
       INTEGER(C_INT),VALUE :: istride
       INTEGER(C_INT),VALUE :: idist
       TYPE(C_PTR),VALUE :: onembed
       INTEGER(C_INT),VALUE :: ostride
       INTEGER(C_INT),VALUE :: odist
       INTEGER(KIND(HIPFFT_R2C)),VALUE :: myType
       INTEGER(C_INT),VALUE :: batch
     END FUNCTION

     FUNCTION hipfftDestroy(plan) BIND(C, name="hipfftDestroy")
       USE iso_c_binding
       import HIPFFT_SUCCESS
       IMPLICIT NONE
       INTEGER(kind(HIPFFT_SUCCESS)) :: hipfftDestroy
       TYPE(C_PTR),VALUE :: plan
     END FUNCTION

  END INTERFACE

CONTAINS

  SUBROUTINE hipfftcheck(hiperror)
      IMPLICIT NONE

      INTEGER(KIND(HIPFFT_SUCCESS)) :: hiperror

      IF (hiperror /= HIPFFT_SUCCESS) THEN
         WRITE (*, *) "HIPFFT ERROR: ERROR CODE = ", hiperror
         CALL EXIT(hiperror)
      END IF
  END SUBROUTINE hipfftCheck
  
#else

contains

  !
  ! dummy subroutine to avoid having an empty module
  subroutine hip_dummy__(i)
     integer :: i
     i=0 
  end subroutine

#endif

END MODULE

