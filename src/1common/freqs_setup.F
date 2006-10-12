!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine freqs_setup(wv)
!==========================
!
! %er %dr %n -> %p
!
 use pars,          ONLY:SP
 use frequency,     ONLY:w_samp
 use memory_m,      ONLY:mem_est
 implicit none
 type(w_samp):: wv 
!ws
 integer          :: i1
 real(SP),external:: damping
!
!If %p is associated and consitent with %er/%dr return
!
 if (associated(wv%p)) then
   if (size(wv%p)==wv%n(1).and.&
&      abs(  real(wv%p(1))-wv%er(1) )<1.E-5.and.&
&      abs( aimag(wv%p(1))-wv%dr(1) )<1.E-5.and.&
&      abs(  real(wv%p(wv%n(1)))-wv%er(2) )<1.E-5.and.&
&      abs( aimag(wv%p(wv%n(1)))-wv%dr(2) )<1.E-5) return
 endif
!
 if (associated(wv%p)) then
   deallocate(wv%p)
   call mem_est("W-p")
 endif
!
! First the total/mem steps
!
 if (wv%er(2)==wv%er(1)) wv%n=1
 wv%n(2)=nint(wv%per_memstps*real(wv%n(1))/100.)
 if (wv%n(1)<wv%n(2)) wv%n(2)=wv%n(1)
!
! Redefine n(1) to the closest multiplier of n(2)
!
 wv%n(1)=wv%n(1)-mod(wv%n(1),wv%n(2))
!DEBUG<
! if (wv%n(1)-wv%n(2)*int(wv%n(1)/wv%n(2)) > 
! stop
!DEBUG>

 allocate(wv%p(wv%n(1)))
 call mem_est("W-p",(/wv%n(1)/))

 wv%p(1)=cmplx(wv%er(1),wv%dr(1),SP)
 if (wv%n(1)==1) then
   wv%er(2)=wv%er(1)
   wv%dr(2)=wv%dr(1)
   return
 endif
 forall(i1=1:wv%n(1)-1) wv%p(i1+1)=wv%er(1)+(wv%er(2)-wv%er(1))/(wv%n(1)-1)*i1
 do i1=1,wv%n(1)
   wv%p(i1)=real(wv%p(i1))+damping(wv%er,wv%dr,real(wv%p(i1)))*cmplx(0.,1.,SP)
 enddo

 end subroutine
