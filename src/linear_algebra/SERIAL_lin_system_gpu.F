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
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,LINEAR_ALGEBRA_WS_reset,LALGEBRA_WS
 use timing_m,       ONLY:timing
 use gpu_m
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
 type(LALGEBRA_WS),     target :: WS
 complex(SP), pointer DEV_ATTR :: v_cmplx_p(:)
 integer,     pointer DEV_ATTR :: v_int_p(:), vp_int_p(:)

 !
 if(.not.(l_nl_optics.or.l_real_time)) call timing(trim(subname),OPR='start')
 !
#if ! defined _CUDA
 call error(trim(subname)//" not yet ported on non-CUDA GPUs")
#endif
 !
 call LINEAR_ALGEBRA_WS_reset(WS)
 WS%dim=n
 !
 YAMBO_ALLOC_GPU(DEV_VAR(WS%v_int),(WS%dim))
 YAMBO_ALLOC_GPU(DEV_VAR(WS%vp_int),(WS%dim))
 !
 v_int_p => DEV_VAR(WS%v_int)
 vp_int_p => DEV_VAR(WS%vp_int)

 !
 ! init
 if (.not.gpu_linalg_init) call gpu_linalg_setup()
 !
#if defined _DOUBLE
 !
 !DEV_ACC data present(A_d)
 !DEV_ACC host_data use_device(A_d)
 !
#if defined _CUDA
 WS%i_fail = cusolverDnZgetrf_bufferSize(cusolv_h,WS%dim,WS%dim,A_d,WS%dim,lwork)
 if(WS%i_fail/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverDnZgetrf_bufferSize failed')
#endif
 !
 !DEV_ACC end host_data
 !DEV_ACC end data
 !
 YAMBO_ALLOC_GPU(DEV_VAR(WS%v_cmplx),(lwork))
 !
 !DEV_ACC data present(A_d,B_d,WS%v_cmplx,WS%v_int,WS%vp_int)
 !DEV_ACC host_data use_device(A_d,B_d,WS%v_cmplx,WS%v_int,WS%vp_int)
 !
#if defined _CUDA
 WS%i_fail = cusolverDnZgetrf(cusolv_h,WS%dim,WS%dim,A_d,WS%dim,DEV_VAR(WS%v_cmplx),DEV_VAR(WS%v_int),DEV_VAR(WS%vp_int))
 if(WS%i_fail/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverDnZgetrf failed')
#endif
 !
#if defined _CUDA
 WS%i_fail = cusolverDnZgetrs(cusolv_h,CUBLAS_OP_N,WS%dim,nrhs,A_d,WS%dim,DEV_VAR(WS%v_int),B_d,WS%dim,DEV_VAR(WS%vp_int))
 if(WS%i_fail/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverDnZgetrs failed')
#endif
 !
 !DEV_ACC end host_data
 !DEV_ACC end data
 !
#else
 !
 !DEV_ACC data present(A_d)
 !DEV_ACC host_data use_device(A_d)
 !
#if defined _CUDA
 WS%i_fail = cusolverDnCgetrf_bufferSize(cusolv_h,WS%dim,WS%dim,A_d,WS%dim,lwork)
 if(WS%i_fail/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverDnZgetrf_bufferSize failed')
#endif
 !
 !DEV_ACC end host_data
 !DEV_ACC end data
 !
 YAMBO_ALLOC_GPU(DEV_VAR(WS%v_cmplx),(lwork))
 v_cmplx_p => DEV_VAR(WS%v_cmplx)
 !
 !DEV_ACC data present(A_d,B_d,v_cmplx_p,v_int_p,vp_int_p)
 !DEV_ACC host_data use_device(A_d,B_d,v_cmplx_p,v_int_p,vp_int_p)
 !
write(*,*) "before cusolverDnCgetrf"
#if defined _CUDA
 WS%i_fail = cusolverDnCgetrf(cusolv_h,WS%dim,WS%dim,A_d,WS%dim,v_cmplx_p,v_int_p,vp_int_p)
 if(WS%i_fail/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverDnCgetrf failed')
#endif
write(*,*) "after cusolverDnCgetrf"
 !
write(*,*) "before cusolverDnCgetrs"
#if defined _CUDA
 WS%i_fail = cusolverDnCgetrs(cusolv_h,CUBLAS_OP_N,WS%dim,nrhs,A_d,WS%dim,v_int_p,B_d,WS%dim,vp_int_p)
 if(WS%i_fail/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverDnCgetrs failed')
#endif
write(*,*) "after cusolverDnCgetrs"
 !
 !DEV_ACC end host_data
 !DEV_ACC end data
 !
#endif
 !
 call LINEAR_ALGEBRA_WS_reset(WS)
 !
 if(.not.(l_nl_optics.or.l_real_time)) call timing(trim(subname),OPR='stop')
 !
end subroutine SERIAL_lin_system_gpu

