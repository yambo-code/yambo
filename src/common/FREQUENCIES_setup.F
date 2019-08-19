!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use parser_m,      ONLY:parser
#include<memory.h>
 !
 type(w_samp):: W 
 ! 
 ! Work Space
 !
 integer          :: i1
 logical          :: metallic_damping
 real(SP)         :: delta_w,w_n
 complex(SP)      :: w_tmp
 real(SP),external:: FREQUENCIES_damping
 !
 !If %p is associated and consitent with %er/%dr return
 !
 if (allocated(W%p)) then
   if (size(W%p)==W%n_freqs.and.&
&      abs(  real(W%p(1))-W%er(1) )<1.E-5.and.&
&      abs( aimag(W%p(1))-W%dr(1) )<1.E-5.and.&
&      abs(  real(W%p(W%n_freqs))-W%er(2) )<1.E-5.and.&
&      abs( aimag(W%p(W%n_freqs))-W%dr(2) )<1.E-5) return
 endif
 !
 YAMBO_FREE(W%p)
 !
 ! First the total/mem steps
 !
 if (W%er(2)==W%er(1)) W%n_freqs=1
 W%n_freqs=nint(W%per_memstps*real(W%n_freqs)/100._SP)
 !
 YAMBO_ALLOC(W%p,(W%n_freqs))
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
   if(abs(W%p(i1))==0._SP) W%p(i1)=W%p(i1)+1.E-10_SP
 enddo
 !
 call parser('MetDamp',metallic_damping)
 !
 if(.not.metallic_damping) return
 !
 ! Davide
 ! Implementation of eq. 10, PRB 82, 035104 (2010): w=sqrt(Re[w]*(Re[w]+2*i*Im[w])),
 ! notice that this is sqrt(w^2+Im[w]^2)
 ! 1) This expression should guarantee the correct behaviour of Re[eps^m1] nearby w=0
 !    in metals. Sistems with a gap are not affected.
 ! 2) Thanks to this expression the length and the velocity gauge are finally identical
 !    and the divergency for w->0 in the velocity is removed (there is just some numerical noise)
 !
 do i1=1,W%n_freqs
   w_n=W%er(1)+delta_w*real(i1-1,SP)
   if(w_n>0._SP) W%p(i1)=cmplx( w_n**2/sqrt(w_n**2+aimag(W%p(i1))**2),aimag(W%p(i1)),SP)
   if(w_n<0._SP) W%p(i1)=cmplx(-w_n**2/sqrt(w_n**2+aimag(W%p(i1))**2),aimag(W%p(i1)),SP)
 enddo
 !
 do i1=1,W%n_freqs
   w_tmp=W%p(i1)
   if ( real(w_tmp)>=0._SP ) W%p(i1)= sqrt(real(w_tmp)*(real(w_tmp)+2._SP*aimag(w_tmp)*cI))
   if ( real(w_tmp)< 0._SP ) W%p(i1)=-sqrt(real(w_tmp)*(real(w_tmp)+2._SP*aimag(w_tmp)*cI))
   if (abs(W%p(i1))==0._SP ) W%p(i1)= W%p(i1)+1.E-10_SP
 enddo
 !
end subroutine
