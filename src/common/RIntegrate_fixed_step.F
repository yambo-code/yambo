!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
function RIntegrate_fixed_step(F,N,dW)
 !
 use pars,    ONLY:SP
 implicit none
 integer,     intent(in)  :: N
 real(SP),    intent(in)  :: dW,F(N)
 real(SP)                 :: RIntegrate_fixed_step
 !
 ! Work Space
 !
 integer      :: i_w
 real(SP)     :: M,Q,W,Wp
 !
 RIntegrate_fixed_step=0._SP
 !
 do i_w=1,N-1
   W =i_w*dW
   Wp=(i_w+1)*dW
   M=(F(i_w)-F(i_w+1))/(W-Wp)
   Q=F(i_w)-M*W
   RIntegrate_fixed_step=RIntegrate_fixed_step+1./2.*M*(Wp**2.-W**2.)+Q*(Wp-W)
 enddo
 !
end function
