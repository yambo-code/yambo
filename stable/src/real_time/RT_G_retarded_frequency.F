!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
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
real(SP) function RT_G_retarded_frequency(Approx,E,ib,ik,W,WIDTH)
 !
 use pars,       ONLY:SP,cI,pi
 use electrons,  ONLY:levels
 use ELPH,       ONLY:W_debye
 implicit none
 !
 type(levels),intent(in):: E
 integer                :: ib,ik
 real(SP)               :: W
 !
 ! Work Space
 !
 real(SP)    :: E_vb_max,E_cb_min,WIDTH,W_v_diff,W_c_diff
 character(*):: Approx
 complex(SP) :: Sigma,SF
 !
 E_vb_max=maxval( E%E(:E%nbf,:,:) )-E%E(ib,ik,1)
 E_cb_min=minval( E%E(E%nbf+1:,:,:) )-E%E(ib,ik,1)
 !
 Sigma=cI*WIDTH
 !
 RT_G_retarded_frequency=0.
 !
 if (Approx=="QP-cut".or.Approx=="BOX") then
   !
   if (abs(W)>WIDTH) return
   !
   ! Zero in the gap
   !
   if (W>E_vb_max.and.W<E_cb_min) return
   !
   ! Zero above/below CBM/VBM
   !
   if (ib<=E%nbf.and.W>E_vb_max) return
   if (ib> E%nbf.and.W<E_cb_min) return
   !
 endif
 !
 if (Approx=="BOX") RT_G_retarded_frequency=1./2./WIDTH
 !
 ! Terminators
 !
 if (W_debye>0.) then
   !
   if (ib<=E%nbf.and.W>=E_vb_max-W_debye) then
     !
     if (Approx=="QP-cut") Sigma=cI*EP_terminator(W,E_vb_max-W_debye,E_vb_max,WIDTH)
     if (Approx=="BOX")    RT_G_retarded_frequency=EP_terminator(W,E_vb_max-W_debye,E_vb_max,1./2./WIDTH)
     !
   else if (W<=WIDTH.and.W> WIDTH-W_debye) then
     !
     if (Approx=="QP-cut") Sigma=cI*EP_terminator(W,WIDTH-W_debye,WIDTH,WIDTH)
     if (Approx=="BOX")    RT_G_retarded_frequency=EP_terminator(W,WIDTH-W_debye,WIDTH,1./2./WIDTH)
     !
   endif
   !
   if (ib> E%nbf.and.W<=E_cb_min+W_debye) then
     !
     if (Approx=="QP-cut") Sigma=cI*EP_terminator(W,E_cb_min,E_cb_min+W_debye,WIDTH)
     if (Approx=="BOX")    RT_G_retarded_frequency=EP_terminator(W,E_cb_min,E_cb_min+W_debye,1./2./WIDTH)
     !
   else if (W>=-WIDTH.and.W<-WIDTH+W_debye) then
     !
     if (Approx=="QP-cut") Sigma=cI*EP_terminator(W,-WIDTH+W_debye,-WIDTH,WIDTH)
     if (Approx=="BOX")    RT_G_retarded_frequency=EP_terminator(W,-WIDTH+W_debye,-WIDTH,1./2./WIDTH)
     !
   endif
   !
 endif
 !
 if (Approx=="BOX") return
 !
 if (abs(aimag(Sigma))<1.E-5_SP) return
 !
 RT_G_retarded_frequency=1./pi*aimag(Sigma)/( ( W-real(Sigma))**2 + ( aimag(Sigma) )**2)
 !
 contains
   !
   real(SP) function EP_terminator(W,W_1,W_2,Y)
     !
     real(SP) :: W,W_1,W_2,Y,alpha,dW
     !
     EP_terminator=Y
     !
     if (W_1<W_2.and.(W<W_1.or.W>W_2)) return
     if (W_1>W_2.and.(W<W_2.or.W>W_1)) return
     !
     dW=abs(W_2-W_1)
     alpha=4.*Y/dW**3
     !
     if (W_1<W_2) then
       if (W<W_1+dW/2) then
         EP_terminator=Y-alpha*(W-W_1)**3
       else
         EP_terminator=alpha*(W_2-W)**3
       endif
     else
       if (W>W_1-dW/2) then
         EP_terminator=Y-alpha*(W_1-W)**3
       else
         EP_terminator=alpha*(W-W_2)**3
       endif
     endif
   end function
   !
end function RT_G_retarded_frequency
