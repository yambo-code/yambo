!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine RT_coefficients_Fourier(nt,P,nw,W,X,deltaW)
 !
 ! Here I calculate the coefficients of a given Fourier series
 !
 use pars,           ONLY:SP,cI,pi
 use YPP,            ONLY:i_t_start,X_rt
 use units,          ONLY:FS2AUT
 use fields,         ONLY:EtoT
 use real_time,      ONLY:RT_step
 implicit none
 !
 integer,     intent(in)   :: nw,nt
 real(SP),    intent(in)   :: P(nt),W(nw),deltaW
 complex(SP), intent(out)  :: X(nw)
 !
 ! Work Space 
 !
 real(SP)    :: T_range,T(nt)
 complex(SP) :: P_times_exp(nt)
 integer     :: iT_range,i_f,i_t,N_periods
 !
 complex(SP),    external    :: CIntegrate
 !
 N_periods=int( (X_rt%time_range(2) - X_rt%time_range(1))/EtoT(E=deltaW) )
 T_range=N_periods*EtoT(E=deltaW)
 iT_range=T_range/RT_step
 !
 do i_f=1,nw
   !
   do i_t=1,iT_range
     !
     T(i_t)=(i_t+i_t_start-1)*RT_step
     !
     P_times_exp(i_t)=exp(-cI*W(i_f)*T(i_t))*P(i_t+i_t_start-1)
     !
   enddo
   !
   X(i_f)=deltaW/2./pi/N_periods*CIntegrate(P_times_exp(:iT_range),T(:iT_range),iT_range)
   !
 enddo
 !
end subroutine RT_coefficients_Fourier
