!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine RT_coefficients_Fourier(nt,P,nw,W,X,X_err,deltaW)
 !
 ! Here I calculate the coefficients of a given Fourier series
 !
 use pars,           ONLY:SP,cI,pi
 use YPP_real_time,  ONLY:i_t_start,RT_conf
 use fields,         ONLY:EtoT
 use real_time,      ONLY:RT_step
#include<memory.h>
 !
 integer,     intent(in)   :: nw,nt
 real(SP),    intent(in)   :: P(nt),W(nw),deltaW
 complex(SP), intent(out)  :: X(nw),X_err(nw)
 !
 ! Work Space 
 !
 real(SP)    :: T_range,T(nt)
 complex(SP) :: P_times_exp(nt),X_square
 integer     :: iT_range,i_f,i_t,N_periods
 integer     :: i_p,P_length
 complex(SP) ,allocatable :: xvalue(:)
 !
 complex(SP),    external    :: CIntegrate
 !
 N_periods=int( (RT_conf%T_range(2) - RT_conf%T_range(1))/EtoT(E=deltaW) )
 P_length=int(EtoT(E=deltaW)/RT_step)
 T_range=N_periods*EtoT(E=deltaW)
 iT_range=T_range/RT_step
 !
 YAMBO_ALLOC(xvalue,(N_periods))
 !
 do i_f=1,nw
   !
   X(i_f)=(0.,0.)
   !
   do i_p=1,N_periods
     !	   
     do i_t=1,P_length
       !
       T(i_t+(i_p-1)*P_length)=(i_t+i_t_start-1+(i_p-1)*P_length)*RT_step
       !
       P_times_exp(i_t+(i_p-1)*P_length)=exp(-cI*W(i_f)*T(i_t+(i_p-1)*P_length))*P(i_t+i_t_start-1+(i_p-1)*P_length)
       !
     enddo
     !
     xvalue(i_p)=deltaW/2./pi*CIntegrate(P_times_exp(1+(i_p-1)*P_length:i_p*P_length),T(1+(i_p-1)*P_length:i_p*P_length),P_length)
     !
   enddo
   !
   X(i_f)        =sum(xvalue)/N_periods
   X_square      =cmplx(sum(real(xvalue(:)-X(i_f))**2),sum(aimag(xvalue(:)-X(i_f))**2))/N_periods
   X_err(i_f)    =cmplx(sqrt(real(X_square)),sqrt(aimag((X_square))))/sqrt(real(N_periods))
   !
 enddo
 !
end subroutine RT_coefficients_Fourier
