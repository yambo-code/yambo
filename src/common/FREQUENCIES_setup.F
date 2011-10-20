!
! Copyright (C) 2000-2011 A. Marini and the YAMBO team 
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
subroutine FREQUENCIES_setup(W)
 !
 ! %er %dr %n -> %p
 !
 use pars,          ONLY:SP
 use frequency,     ONLY:w_samp
 use memory_m,      ONLY:mem_est
 implicit none
 type(w_samp):: W 
 ! 
 ! Work Space
 !
 integer          :: i1
 real(SP),external:: FREQUENCIES_damping
 !
 !If %p is associated and consitent with %er/%dr return
 !
 if (associated(W%p)) then
   if (size(W%p)==W%n(1).and.&
&      abs(  real(W%p(1))-W%er(1) )<1.E-5.and.&
&      abs( aimag(W%p(1))-W%dr(1) )<1.E-5.and.&
&      abs(  real(W%p(W%n(1)))-W%er(2) )<1.E-5.and.&
&      abs( aimag(W%p(W%n(1)))-W%dr(2) )<1.E-5) return
 endif
 !
 if (associated(W%p)) then
   deallocate(W%p)
   call mem_est("W-p")
 endif
 !
 ! First the total/mem steps
 !
 if (W%er(2)==W%er(1)) W%n=1
 W%n(2)=nint(W%per_memstps*real(W%n(1))/100.)
 if (W%n(1)<W%n(2)) W%n(2)=W%n(1)
 !
 ! Redefine n(1) to the closest multiplier of n(2)
 !
 W%n(1)=W%n(1)-mod(W%n(1),W%n(2))
 !
 allocate(W%p(W%n(1)))
 call mem_est("W-p",(/W%n(1)/))
 !
 W%p(1)=cmplx(W%er(1),W%dr(1),SP)
 if (W%n(1)==1) then
   W%er(2)=W%er(1)
   W%dr(2)=W%dr(1)
   return
 endif
 forall(i1=1:W%n(1)-1) W%p(i1+1)=W%er(1)+(W%er(2)-W%er(1))/(W%n(1)-1)*i1
 do i1=1,W%n(1)
   W%p(i1)=real(W%p(i1))+FREQUENCIES_damping(W,real(W%p(i1)))*cmplx(0.,1.,SP)
 enddo
 !
end subroutine
