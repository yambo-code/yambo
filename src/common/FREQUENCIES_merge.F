!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine FREQUENCIES_merge(W1,W2,table)
 !
 ! Merge W1 into W2 and create a table of the W1 points in the new merged type
 !
 ! W2 is overwritten
 !
 use pars,           ONLY:SP,cZERO
 use frequency,      ONLY:w_samp,W_reset
#include<memory.h>
 !
 type(w_samp), intent(in)   :: W1
 type(w_samp), intent(inout):: W2
 integer     , intent(inout):: table(W1%n_freqs+W2%n_freqs)
 ! 
 ! Work Space
 !
 integer     :: iw1,iw2,i1
 type(w_samp):: W_new
 real(SP)    :: r(3)
 !
 call W_reset(W_new)
 YAMBO_ALLOC(W_new%p,(W1%n_freqs+W2%n_freqs))
 W_new%p            =cZERO
 W_new%n_freqs      =W2%n_freqs
 do iw2=1,W2%n_freqs
   W_new%p(iw2) = W2%p(iw2)
   table(iw2)   = -iw2
 enddo
 !
 W1_loop: do iw1=1,W1%n_freqs
   !
   ! N(ew)
   !
   ! N < first
   !
   if (real(W1%p(iw1))<minval(real(W_new%p(:W_new%n_freqs)))) then
     call ADD_it(W1%p(iw1),1,iw1)
     cycle W1_loop
   endif
   !
   ! N > latest
   !
   if (real(W1%p(iw1))>maxval(real(W_new%p(:W_new%n_freqs)))) then
     call ADD_it(W1%p(iw1),W_new%n_freqs+1,iw1)
     cycle W1_loop
   endif
   !
   W2_loop: do iw2=1,W_new%n_freqs-1
     !
     r=(/real(W_new%p(iw2)),real(W1%p(iw1)),real(W_new%p(iw2+1))/)
     !
     ! N=o  
     !
     if (abs(r(1)-r(2))<1.E-5) then
       table(iw2)=iw1
       exit W2_loop 
     endif
     !
     ! o=N  
     !
     if (abs(r(3)-r(2))<1.E-5) then
       table(iw2+1)=iw1
       exit W2_loop 
     endif
     !
     ! o N o
     !
     if (r(1)<r(2).and.r(2)<r(3)) then
       call ADD_it(W1%p(iw1),iw2+1,iw1)
       cycle W2_loop
     endif
     !
   enddo W2_loop
   !
 enddo W1_loop
 !
 call W_reset(W2)
 W2%n_freqs=W_new%n_freqs
 W2%damp_reference=W_new%damp_reference
 W2%per_memstps=W_new%per_memstps
 YAMBO_ALLOC(W2%p,(W2%n_freqs))
 W2%p=W_new%p(:W_new%n_freqs)
 W2%er=(/real(W2%p(1)) ,real(W2%p(W2%n_freqs))/)
 W2%dr=(/aimag(W2%p(1)),aimag(W2%p(W2%n_freqs))/)
 !
 call W_reset(W_new)
 !
 contains
   !
   subroutine ADD_it(E,i_POS,i_REF)
     complex(SP) :: E 
     integer     :: i_POS,i_REF
     do i1=W1%n_freqs+W2%n_freqs-1,i_POS,-1
       table(i1+1)  =table(i1)
       W_new%p(i1+1)=W_new%p(i1)
     enddo
     W_new%p(i_POS)=E
     table(i_POS)=i_REF
     W_new%n_freqs=W_new%n_freqs+1
   end subroutine
   !
end subroutine FREQUENCIES_merge
