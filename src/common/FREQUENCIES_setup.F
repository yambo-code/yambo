!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
 ! %er %dr %n_freqs -> %p
 !
 use pars,          ONLY:SP,cI
 use frequency,     ONLY:w_samp
 use memory_m,      ONLY:mem_est
 use parser_m,      ONLY:parser
 !
 implicit none
 !
 type(w_samp):: W 
 ! 
 ! Work Space
 !
 integer          :: i1
 logical          :: metallic_damping
 real(SP)         :: delta_w,w_n
 real(SP),external:: FREQUENCIES_damping
 !
 !If %p is associated and consitent with %er/%dr return
 !
 if (associated(W%p)) then
   if (size(W%p)==W%n_freqs.and.&
&      abs(  real(W%p(1))-W%er(1) )<1.E-5.and.&
&      abs( aimag(W%p(1))-W%dr(1) )<1.E-5.and.&
&      abs(  real(W%p(W%n_freqs))-W%er(2) )<1.E-5.and.&
&      abs( aimag(W%p(W%n_freqs))-W%dr(2) )<1.E-5) return
 endif
 !
 if (associated(W%p)) then
   deallocate(W%p)
   call mem_est("W-p")
 endif
 !
 ! First the total/mem steps
 !
 if (W%er(2)==W%er(1)) W%n_freqs=1
 W%n_freqs=nint(W%per_memstps*real(W%n_freqs)/100._SP)
 !
 allocate(W%p(W%n_freqs))
 call mem_est("W-p",(/W%n_freqs/))
 !
 W%p(1)=cmplx(W%er(1),W%dr(1),SP)
 if (W%n_freqs==1) then
   W%er(2)=W%er(1)
   W%dr(2)=W%dr(1)
   return
 endif
 !
 delta_w=(W%er(2)-W%er(1))/(W%n_freqs-1)
 !
 forall(i1=1:W%n_freqs-1) W%p(i1+1)=cmplx(W%er(1)+delta_w*real(i1,SP),0.,SP)
 !
 do i1=1,W%n_freqs
   W%p(i1)=real(W%p(i1))+FREQUENCIES_damping(W,real(W%p(i1)))*cI
 enddo
 !
 !
end subroutine
