!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM MG
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
subroutine RT_coefficients_Inversion(NW,NX,P,X,W,T_period)
 !
 ! BROKEN AS i_t_start not in YPP module anymore
 !
 use pars,            ONLY:SP,cI,cONE,cZERO
 use YPP_real_time,   ONLY:X_order,RT_conf
 use real_time,       ONLY:RT_step,NE_steps
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
 i_deltaT=nint(T_period/RT_step)/M_size
 !
 YAMBO_ALLOC(M,(M_size,M_size))
 YAMBO_ALLOC(P_i,(M_size))
 YAMBO_ALLOC(T_i,(M_size))
 YAMBO_ALLOC(X_here,(nP_components))
 !
 M=cZERO
 !
 do i_t=1,M_size
   T_i(i_t)= ((i_t_start-1._SP)+i_deltaT*(i_t-1._SP))*RT_step
   P_i(i_t)=P(i_t_start+i_deltaT*(i_t-1))
 enddo
 !
 do i_t=1,M_size
   M(i_t,1) = cONE
   do i_n=2,nP_components
     M(i_t,i_n        ) = exp( cI*W(i_n)*T_i(i_t))
     M(i_t,i_n+X_order) = exp(-cI*W(i_n)*T_i(i_t))
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

