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
subroutine SERIAL_inversion_gpu(n,M_d,det,eval_det)
 !
 use pars,           ONLY:SP,cONE
 use drivers,        ONLY:l_nl_optics,l_real_time
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,LU_factorization,LU_inversion,&
&                         LINEAR_ALGEBRA_WS_reset,LALGEBRA_WS
 use timing_m,       ONLY:timing
 use cuda_m
#include<memory.h>
 !
 integer     :: n
 complex(SP), device :: M_d(n,n)
 complex(SP) :: det,caux
 logical     :: eval_det
 !
 !ws
 character(64)     :: subname="SERIAL_inversion_gpu"
 integer           :: i,j,lwork
 type(LALGEBRA_WS),    target :: WS
 complex(SP), pointer, device :: m1_p(:,:)
 !
 !
 if(.not.(l_nl_optics.or.l_real_time)) call timing(trim(subname),OPR='start')
 !
 call LINEAR_ALGEBRA_WS_reset(WS)
 WS%dim=n
 YAMBO_ALLOC(WS%v_int_d,(WS%dim))
 YAMBO_ALLOC(WS%vp_int_d,(WS%dim))
 YAMBO_ALLOC(WS%m1_cmplx,(WS%dim,WS%dim))
 YAMBO_ALLOC(WS%m1_cmplx_d,(WS%dim,WS%dim))
 !
 ! init
 if (.not.cuda_linalg_init) call cuda_linalg_setup()
 !
 ! init identity
 ! Since the interface to GETRI is not provided by cusolver
 ! here we use GETRS setting the rhs B=Id
 ! B is represetned m1_cmplx
 !
 WS%m1_cmplx=0.0_SP
 do i=1,WS%dim
   WS%m1_cmplx(i,i)=1.0_SP
 enddo
 WS%m1_cmplx_d=WS%m1_cmplx
 !
#if defined _DOUBLE
 !
 WS%i_fail = cusolverDnZgetrf_bufferSize(cusolv_h,WS%dim,WS%dim,M_d,WS%dim,lwork)
 if(WS%i_fail/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverDnZgetrf_bufferSize failed')
 !
 YAMBO_ALLOC(WS%v_cmplx_d,(lwork))
 !
 WS%i_fail = cusolverDnZgetrf(cusolv_h,WS%dim,WS%dim,M_d,WS%dim,WS%v_cmplx_d,WS%v_int_d,WS%vp_int_d)
 if(WS%i_fail/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverDnZgetrf failed')
 !
 WS%i_fail = cusolverDnZgetrs(cusolv_h,CUBLAS_OP_N,WS%dim,WS%dim,M_d,WS%dim,WS%v_int_d,&
&                             WS%m1_cmplx_d,WS%dim,WS%vp_int_d)
 if(WS%i_fail/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverDnZgetrs failed')
 !
#else
 !
 WS%i_fail = cusolverDnCgetrf_bufferSize(cusolv_h,WS%dim,WS%dim,M_d,WS%dim,lwork)
 if(WS%i_fail/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverDnZgetrf_bufferSize failed')
 !
 YAMBO_ALLOC(WS%v_cmplx_d,(lwork))
 !
 WS%i_fail = cusolverDnCgetrf(cusolv_h,WS%dim,WS%dim,M_d,WS%dim,WS%v_cmplx_d,WS%v_int_d,WS%vp_int_d)
 if(WS%i_fail/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverDnCgetrf failed')
 !
 WS%i_fail = cusolverDnCgetrs(cusolv_h,CUBLAS_OP_N,WS%dim,WS%dim,M_d,WS%dim,WS%v_int_d,&
&                             WS%m1_cmplx_d,WS%dim,WS%vp_int_d)
 if(WS%i_fail/=CUSOLVER_STATUS_SUCCESS) call LINEAR_ALGEBRA_error(subname,'cusolverDnCgetrs failed')
 !
#endif
 !
 m1_p=> WS%m1_cmplx_d
 !
 !$cuf kernel do(2) <<<*,*>>>
 do j = 1, n
 do i = 1, n
   M_d(i,j)=m1_p(i,j)
 enddo
 enddo
 !
 det=cONE
 if (eval_det) then
   !
   caux=0.0
   !$cuf kernel do <<<*,*>>>
   do i=1,n
     caux=caux+log(M_d(i,i))
   enddo
   det=exp(caux)
   !
   YAMBO_ALLOC(WS%v_int,(WS%dim))
   WS%v_int=WS%v_int_d
   !
   do i=1,n
     if (WS%v_int(i)/=i) det=-det
   enddo
   ! 
 endif
 !
 call LINEAR_ALGEBRA_WS_reset(WS)
 !
 if(.not.(l_nl_optics.or.l_real_time)) call timing(trim(subname),OPR='stop')
 !
end subroutine SERIAL_inversion_gpu

