!
!        Copyright (C) 2000-2017 the YAMBO team
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
complex(SP) function RT_G_retarded_time(Approx,E,ib,ik,Time,delta_T)
 !
 ! The expression coded here well describes the short and long time dependence of
 ! G_retarded. Note that here G is assumed to be diagonal in the band and dependent 
 ! only on the time difference (Time).
 !
 ! When delta_T >0 the function returns the prefactor P(delta_T) such that
 !
 !  G(t+delta_T)\approx P(delta_T)*G(t)
 !
 ! Note that there is no spin prefactor here
 !
 use pars,       ONLY:SP
 use electrons,  ONLY:levels
 implicit none
 !
 type(levels),intent(in):: E
 character(*)           :: Approx
 integer                :: ib,ik
 real(SP)               :: Time,delta_T
 !
 ! Work Space
 !
 real(SP) :: Gamma_factor,C_factor,eps,exponent_,power,exp_up,exp_dn,P_delta
 ! 
 eps=E%E(ib,ik,1)
 Gamma_factor=abs(E%W(ib,ik,1))! The long-time lifetime
 C_factor=2.*Gamma_factor**2.  ! The short-time lifetime
 !
 power    =Gamma_factor**2./C_factor
 exponent_=-C_factor/Gamma_factor*Time
 !
 ! See Haug, H. & Jauho, A.-P. Springer (ed.) "Quantum Kinetics in Transport and Optics of Semiconductors" 2008.
 ! Page 277.
 !
 ! Hyperbolic cosine
 !
 if (Approx=="HC") then
   exp_up  = 2.**power
   exp_dn  = (exp(exponent_)+exp(-exponent_))**power
 endif
 !
 ! Pure Lorentzian dynamics
 !
 if (Approx=="Lorentzian".or.Approx=="QP") then
   exp_up  = exp(-Gamma_factor*Time)
   exp_dn  = 1._SP
   P_delta = 1._SP
 endif
 !
 if (delta_T>0.) then
   !
   !DEBUG>
   !RT_G_retarded_time=exp(-cI*eps*delta_T)*exp(-Gamma_factor*delta_T)/P_delta
   !DEBUG<
   !
 else
   !
   !DEBUG>
   !RT_G_retarded_time=-cI*exp(-cI*eps*Time)*exp_up/exp_dn
   !DEBUG<
   !
   RT_G_retarded_time=exp_up/exp_dn
   !
 endif
 !
end function RT_G_retarded_time
