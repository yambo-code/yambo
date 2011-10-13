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
complex(SP) function RT_G_retarded(E,ib,ik,Time)
 !
 ! The expression coded here well describes the short and long time dependence of
 ! G_retarded. Note that here G is assumed to be diagonal in the band and dependent 
 ! only on the time difference (Time).
 !
 use pars,       ONLY:SP,cI
 use electrons,  ONLY:levels
 implicit none
 !
 type(levels),intent(in):: E
 integer                :: ib,ik
 real(SP)               :: Time
 !
 ! Work Space
 !
 real(SP) :: Gamma_factor,C_factor,eps,exponent_,exp_up,exp_dn
 ! 
 eps=E%E(ib,ik,1)
 Gamma_factor=E%W(ib,ik,1)     ! The long-time lifetime
 C_factor=2.*Gamma_factor**2.  ! The short-time lifetime
 !
 exponent_=Gamma_factor**2./C_factor
 !
 ! See Haug, H. & Jauho, A.-P. Springer (ed.) "Quantum Kinetics in Transport and Optics of Semiconductors" 2008.
 ! Page 277.
 !
 exp_up=2.**exponent_*exp(-Gamma_factor*Time)
 exp_dn=(1.+exp(-2.*C_factor/Gamma_factor*Time))**exponent_
 !
 RT_G_retarded=-cI*exp(-cI*eps*Time)*exp_up/exp_dn
 !
end function RT_G_retarded
