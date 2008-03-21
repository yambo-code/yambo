!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
subroutine f_sum_rule(W)
 ! 
 use pars,          ONLY:SP,HARTREE,pi
 use D_lattice,     ONLY:DL_vol
 use X_m,           ONLY:X_t,X_epsilon
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:nel
 use com,           ONLY:msg
 implicit none
 !
 type(w_samp) :: W
 !
 ! Work Space
 !
 integer  :: iw
 real(SP) :: integral_,y(2),delta_x,N_el_per_cell
 !
 integral_=0.
 !
 ! \int dw w e_2(w)  = \pi/2 w_p^2 =  2 pi^2 Na/Vol Ne
 !
 ! = > Ne = (\int dw w e_2(w)) * Vol/Na /2 /pi^2
 !     Ne*Na = (\int dw w e_2(w)) * Vol /2 /pi^2
 !
 do iw=1,W%n(1)-1
   !
   delta_x=real(W%p(iw+1)-W%p(iw))
   y(1)= real(W%p(iw))   * aimag(X_epsilon(2,iw))
   y(2)= real(W%p(iw+1)) * aimag(X_epsilon(2,iw+1))
   !
   integral_=integral_+delta_x*(y(1)+y(2))*.5
   !
 enddo
 !
 N_el_per_cell=integral_*DL_vol/2./pi**2.
 !
 call msg('snr','[SUM RULE] Electrons per unit cell (found expected):',(/N_el_per_cell,nel/))
 !
end subroutine
