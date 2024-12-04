!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine SERIAL_lin_system_gpu(n,nrhs,A_d,B_d)
 !
 !  Solve the linear system
 !                   A X = B
 !  where A is a n-x-n matrix and B is n-x-nrhs
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_nl_optics,l_real_time
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error
 use timing_m,       ONLY:timing
 use gpu_m
#if defined _CUDA
 use cublas,         ONLY:CUBLAS_OP_N
#endif
 !
#include<dev_defs.h>
#include<y_memory.h>
 !
 integer     :: n,nrhs
 complex(SP) DEV_ATTR :: A_d(n,n),B_d(n,nrhs)
 !
 !ws
 character(64) :: subname="SERIAL_lin_system_gpu"
 integer       :: lwork
 complex(SP), allocatable DEV_ATTR :: v_cmplx(:)
 integer,     allocatable DEV_ATTR :: v_int(:), vp_int(:)
 integer     :: i_fail
 !
 if(.not.(l_nl_optics.or.l_real_time)) call timing(trim(subname),OPR='start')
 !
#if ! defined _CUDA
 call error(trim(subname)//" not yet ported on non-CUDA GPUs")
#endif
 !
 YAMBO_ALLOC_GPU(v_int,(n))
 YAMBO_ALLOC_GPU(vp_int,(1))
 !
 ! init
 if (.not.gpu_linalg_init) call gpu_linalg_setup()
 !
#if defined _CUDA
 !DEV_ACC_DEBUG data present(A_d)
 !DEV_ACC host_data use_device(A_d)
 !
#  if defined _DOUBLE
 i_fail = cusolverDnZgetrf_bufferSize(cusolv_h,n,n,A_d,n,lwork)
#  else
 i_fail = cusolverDnCgetrf_bufferSize(cusolv_h,n,n,A_d,n,lwork)
#  endif
 if(i_fail/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverDnZgetrf_bufferSize failed')
 !
 !DEV_ACC end host_data
 !DEV_ACC_DEBUG end data
#endif
 !
 YAMBO_ALLOC_GPU(v_cmplx,(lwork))
 !
#if defined _CUDA
 !DEV_ACC_DEBUG data present(A_d,B_d,v_cmplx,v_int,vp_int)
 !DEV_ACC host_data use_device(A_d,B_d,v_cmplx,v_int,vp_int)
 !
#  if defined _DOUBLE
 i_fail = cusolverDnZgetrf(cusolv_h,n,n,A_d,n,v_cmplx,v_int,vp_int(1))
#  else
 i_fail = cusolverDnCgetrf(cusolv_h,n,n,A_d,n,v_cmplx,v_int,vp_int(1))
#  endif
 if(i_fail/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverDnZgetrf failed')
 !
#  if defined _DOUBLE
 i_fail = cusolverDnZgetrs(cusolv_h,CUBLAS_OP_N,n,nrhs,A_d,n,v_int,B_d,n,vp_int(1))
#  else
 i_fail = cusolverDnCgetrs(cusolv_h,CUBLAS_OP_N,n,nrhs,A_d,n,v_int,B_d,n,vp_int(1))
#  endif
 if(i_fail/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverDnZgetrs failed')
 !
 !DEV_ACC end host_data
 !DEV_ACC_DEBUG end data
#endif
 !
 YAMBO_FREE_GPU(v_int)
 YAMBO_FREE_GPU(vp_int)
 YAMBO_FREE_GPU(v_cmplx)
 !
 if(.not.(l_nl_optics.or.l_real_time)) call timing(trim(subname),OPR='stop')
 !
end subroutine SERIAL_lin_system_gpu
