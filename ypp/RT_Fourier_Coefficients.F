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
subroutine RT_Fourier_Coefficients(nt,P,nw,W,X)
 !
 ! Here I calculate the coefficients of a given Fourier series
 !
 use pars,           ONLY:SP,cI,pi
 use YPP,            ONLY:i_t_start
 use units,          ONLY:FS2AUT,HARTREE,HBAR_eVfs
 use timing,         ONLY:live_timing
 use real_time,      ONLY:RT_step,NE_steps
 use fields,         ONLY:Efield,i_Probe
 use com,            ONLY:msg
 implicit none
 !
 integer,     intent(in)   :: nw,nt
 real(SP),    intent(in)   :: P(nt),W(nw)
 complex(SP), intent(out)  :: X(nw)
 !
 ! Work Space 
 !
 real(SP)    :: T_range,deltaW,P_derivative,TIME,T(nt)
 complex(SP) :: P_times_exp(nt)
 integer     :: iT_range,i_f,i_t
 !
 complex(SP),    external    :: CIntegrate
 !
 if (nw==1) then
   deltaW=W(1)
 else
   deltaW=W(2)-W(1)
 endif
 !
 T_range=2.*pi*HBAR_eVfs/(deltaW*HARTREE)*FS2AUT
 iT_range=T_range/RT_step
 !
 if (nw==1) then
   !
   P_derivative=(P(i_t_start+1)-P(i_t_start))/RT_step
   X(1)=exp(-cI*W(1)*i_t_start*RT_step)/2./Efield(i_Probe)%amplitude*( P(i_t_start)-cI/W(1)*P_derivative)
   !
 else
   !
   call msg('s','[RT] Elemental energy step [eV]:',deltaW*HARTREE)
   call msg('s','[RT] Time range            [fs]:',(i_t_start*RT_step+(/0.,T_range/))/FS2AUT)
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
     X(i_f)=deltaW/2./pi*CIntegrate(P_times_exp(:iT_range),T(:iT_range),iT_range)/Efield(i_Probe)%amplitude
     !
   enddo
   !
 endif
 !
end subroutine RT_Fourier_Coefficients

