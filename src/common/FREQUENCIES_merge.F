!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
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
