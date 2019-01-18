!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
