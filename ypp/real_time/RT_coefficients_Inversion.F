!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM MG
!
!> @brief Polarization coefficient inversion see Sec. III in PRB 88, 235113 (2013) 
!!
!! @param[in]       NW          order of the response functions 
!! @param[in]       NX          numer of coefficents required
!! @param[in]       P           real-time polarization 
!! @param[in]       W           multiples of the laser frequency
!! @param[in]       T_prediod   shorted cicle period
!! @param[out]      X           coefficents of the response functions X1,X2,X3... 
!
subroutine RT_coefficients_Inversion(NW,NX,P,X,W,T_period)
 !
 use pars,            ONLY:SP,cI,cONE,cZERO,rZERO
 use YPP_real_time,   ONLY:X_order,RT_conf
 use real_time,       ONLY:RT_step,NE_steps
 use fields,          ONLY:Efield
 use interfaces,      ONLY:LINEAR_ALGEBRA_driver
 use linear_algebra,  ONLY:INV
#include<memory.h>
 !
 integer,     intent(in)   :: NW,NX
 real(SP),    intent(in)   :: P(NE_steps),W(NW),T_period
 complex(SP), intent(out)  :: X(NX)
 !
 ! Work space
 !
 integer                 :: i_t,i_n,i_deltaT,nP_components,M_size,i_t_start
 real(SP)   ,allocatable :: P_i(:),T_i(:)
 complex(SP),allocatable :: M(:,:),X_here(:)
 !
 M_size        =2*X_order+1
 nP_components =X_order+1 
 !
 i_t_start=nint(RT_conf%T_range(1)/RT_step)+1
 i_deltaT =nint(T_period/RT_step)/M_size
 !
 YAMBO_ALLOC(M,(M_size,M_size))
 YAMBO_ALLOC(P_i,(M_size))
 YAMBO_ALLOC(T_i,(M_size))
 YAMBO_ALLOC(X_here,(nP_components))
 !
 M     =cZERO
 P_i   =rZERO
 T_i   =rZERO
 X_here=cZERO
 !
 do i_t=1,M_size
   T_i(i_t)= ((i_t_start-1._SP)+i_deltaT*(i_t-1._SP))*RT_step-Efield(1)%t_initial
   P_i(i_t)=P(i_t_start+i_deltaT*(i_t-1))
 enddo
 !
 do i_t=1,M_size
   M(i_t,1) = cONE
 enddo
 !
 do i_t=1,M_size
   do i_n=2,nP_components
     M(i_t,i_n        ) = exp(-cI*W(i_n)*T_i(i_t))
     M(i_t,i_n+X_order) = exp(+cI*W(i_n)*T_i(i_t))
   end do
 end do
 !
 call LINEAR_ALGEBRA_driver(INV,M=M)
 !
 X = cZERO
 X_here = cZERO
 do i_n =1, nP_components
   do i_t = 1,M_size
     X_here(i_n)=X_here(i_n)+M(i_n,i_t)*P_i(i_t) 
   end do
 end do
 !
 X(:)=X_here(:)
 !
 YAMBO_FREE(M)
 YAMBO_FREE(P_i)
 YAMBO_FREE(T_i)
 YAMBO_FREE(X_here)
 ! 
end subroutine RT_coefficients_Inversion

