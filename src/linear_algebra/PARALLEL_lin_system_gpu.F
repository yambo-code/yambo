!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): NS SO MM
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
subroutine PARALLEL_lin_system_gpu(A,B,n,nrhs)
 !
 !  Solve the linear system
 !                   A X = B
 !  where A is a n-x-n matrix and B is n-x-nrhs
 !
 use pars,           ONLY:SP,IPL
 use SLK_m,          ONLY:SLK_ORTHO
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error
 use matrix,         ONLY:PAR_matrix
 use timing_m,       ONLY:timing
 use gpu_m
#if defined _CUDA
 use mpi
 use cudafor
 use cublas,         ONLY:CUBLAS_OP_N
 use cusolvermp
#endif
 use gpu_m,          ONLY:device_id,stream_h2d
 !
#include<dev_defs.h>
#include<y_memory.h>
 !
 integer           :: n,nrhs
 type(PAR_matrix)  :: A, B
 !
 !ws
 character(64) :: subname="PARALLEL_lin_system_gpu"

 ! working buffers

 complex(SP), allocatable, contiguous          :: h_work_trf(:)
 complex(SP), allocatable, contiguous          :: h_work_trs(:)
 complex(SP), allocatable, contiguous DEV_ATTR :: d_work_trf(:)
 complex(SP), allocatable, contiguous DEV_ATTR :: d_work_trs(:)

 integer(IPL) :: h_bytes_trf, h_bytes_trs
 integer(IPL) :: d_bytes_trf, d_bytes_trs

 integer(IPL), allocatable DEV_ATTR :: DEV_VAR(ipiv)(:)
 integer,      allocatable DEV_ATTR :: DEV_VAR(info)(:)

 integer :: info(2)

 integer :: ierr

 integer(IPL) :: one
 parameter ( one=1 )

 complex(SP) :: c_value = 1.0

 integer(kind=cuda_stream_kind) :: stream

 character(len=512) :: error_msg
 !
 if (SLK_ORTHO%coordinate(1)==-1) return
 !
#if ! defined _CUDA
 call error(trim(subname)//" not yet ported on non-CUDA GPUs")
#endif
 !
 call timing(subname,OPR='start')
 !
#ifdef _CUSOLVERMP
 !
 ! create communicator handle with MPI communicator
 ierr = cal_comm_create_mpi(SLK_ORTHO%COMM, SLK_ORTHO%task, SLK_ORTHO%n_CPU, device_id, A%comm_mp)

 ierr = cudaSetDevice(device_id)
 ierr = cudaStreamCreate(stream)

 ! initialize the cuSOLVERMp environment
 ierr = cusolverMpCreate(A%cusolvermp_h, device_id, stream)

 ierr = cusolverMpCreateDeviceGrid(A%cusolvermp_h, A%grid_mp, A%comm_mp, SLK_ORTHO%grid(1), SLK_ORTHO%grid(2), CUSOLVERMP_GRID_MAPPING_ROW_MAJOR)

 if(ierr/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverMpCreate failed')

 ierr = cusolverMpCreateMatrixDesc(A%desc_mp, A%grid_mp, &
#if defined _DOUBLE
     & CUDADATATYPE(CUDA_C_64F), &
#else
     & CUDADATATYPE(CUDA_C_32F), &
#endif
     & n, nrhs, A%BLCnrows, A%BLCncols, 0, 0, A%BLCnrows)

 if(ierr/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverMpCreateMatrixDesc failed')

 ierr = cusolverMpCreateMatrixDesc(B%desc_mp, A%grid_mp, &
#if defined _DOUBLE
     & CUDADATATYPE(CUDA_C_64F), &
#else
     & CUDADATATYPE(CUDA_C_32F), &
#endif
     & n, nrhs, B%BLCnrows, B%BLCncols, 0, 0, B%BLCnrows)

 if(ierr/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverMpCreateMatrixDesc failed')

 YAMBO_ALLOC_GPU(DEV_VAR(ipiv),(A%BLCnrows + A%BLCnrows))
 YAMBO_ALLOC_GPU(DEV_VAR(info),(2))
 DEV_VAR(info) = 0
 !
 !DEV_ACC_DEBUG data present( A%blc, B%blc, ipiv )
 !DEV_ACC host_data use_device( A%blc, B%blc, ipiv )
 !
 ! query and allocating workspaces
 !
 ierr = cusolverMpGetrf_buffersize(A%cusolvermp_h, &
        n, nrhs, DEV_VAR(A%blc)(:,:,A%I), one, one, A%desc_mp, DEV_VAR(ipiv), &
#if defined _DOUBLE
        CUDADATATYPE(CUDA_C_64F), &
#else
        CUDADATATYPE(CUDA_C_32F), &
#endif
        d_bytes_trf, h_bytes_trf)

 if(ierr/=CUSOLVER_STATUS_SUCCESS) then
   write(error_msg,"(A,I1") "cusolverMpGetrf_buffersize launch failed with error ", ierr
   call LINEAR_ALGEBRA_error(subname,error_msg)
 endif

 ierr = cusolverMpGetrs_buffersize(A%cusolvermp_h, CUBLAS_OP_N, &
        n, nrhs, &
        DEV_VAR(A%blc)(:,:,A%I), one, one, A%desc_mp, DEV_VAR(ipiv), &
        DEV_VAR(B%blc)(:,:,B%I), one, one, B%desc_mp, &
#if defined _DOUBLE
        CUDADATATYPE(CUDA_C_64F), &
#else
        CUDADATATYPE(CUDA_C_32F), &
#endif
        d_bytes_trs, h_bytes_trs)

 if(ierr/=CUSOLVER_STATUS_SUCCESS) then
   write(error_msg,"(A,I1") "cusolverMpGetrs_buffersize launch failed with error ", ierr
   call LINEAR_ALGEBRA_error(subname,error_msg)
 endif
 !
 !DEV_ACC end host_data
 !DEV_ACC_DEBUG end data
 !
 YAMBO_ALLOC(h_work_trf,(h_bytes_trf/c_sizeof(c_value)))
 YAMBO_ALLOC(h_work_trs,(h_bytes_trs/c_sizeof(c_value)))

 YAMBO_ALLOC_GPU(d_work_trf,(d_bytes_trf/c_sizeof(c_value)))
 YAMBO_ALLOC_GPU(d_work_trs,(d_bytes_trs/c_sizeof(c_value)))
 !
 !DEV_ACC_DEBUG data present(A%blc, B%blc)
 !DEV_ACC host_data use_device(A%blc, B%blc, ipiv, d_work_trf, d_work_trs, info)
 !
 call timing("getrf-getrs",OPR='start')

 ierr = cusolverMpGetrf(A%cusolvermp_h, &
        n, nrhs, DEV_VAR(A%blc)(:,:,A%I), one, one, A%desc_mp, DEV_VAR(ipiv), &
#if defined _DOUBLE
        CUDADATATYPE(CUDA_C_64F), &
#else
        CUDADATATYPE(CUDA_C_32F), &
#endif
        d_work_trf, d_bytes_trf, &
        h_work_trf, h_bytes_trf, &
        DEV_VAR(info)(1))

 if(ierr/=CUSOLVER_STATUS_SUCCESS) then
   write(error_msg,"(A,I1") "cusolverMpGetrf launch failed with error ", ierr
   call LINEAR_ALGEBRA_error(subname,error_msg)
 endif

 !ierr = cal_stream_sync(A%comm_mp, stream)

 ierr = cusolverMpGetrs(A%cusolvermp_h, CUBLAS_OP_N, &
        n, nrhs, DEV_VAR(A%blc)(:,:,A%I), one, one, A%desc_mp, DEV_VAR(ipiv), &
        DEV_VAR(B%blc)(:,:,B%I), one, one, B%desc_mp, &
#if defined _DOUBLE
        CUDADATATYPE(CUDA_C_64F), &
#else
        CUDADATATYPE(CUDA_C_32F), &
#endif
        d_work_trs, d_bytes_trs, &
        h_work_trs, h_bytes_trs, &
        DEV_VAR(info)(2))

 if(ierr/=CUSOLVER_STATUS_SUCCESS) then
   write(error_msg,"(A,I1") "cusolverMpGetrs launch failed with error ", ierr
   call LINEAR_ALGEBRA_error(subname,error_msg)
 endif 

 call timing("getrf-getrs",OPR='stop')
 !
 !DEV_ACC end host_data
 !DEV_ACC_DEBUG end data
 !
 call devxlib_memcpy_d2h(info,DEV_VAR(info))

 if(info(1)/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverMpGetrf failed')
 if(info(2)/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverMpGetrs failed')

 YAMBO_FREE(h_work_trf)
 YAMBO_FREE(h_work_trs)

 YAMBO_FREE_GPU(d_work_trf)
 YAMBO_FREE_GPU(d_work_trs)

#endif
 !
 call timing(subname,OPR='stop')
 !
end subroutine PARALLEL_lin_system_gpu
