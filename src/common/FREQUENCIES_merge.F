!
!        Copyright (C) 2000-2016 the YAMBO team
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
 ! Merge W1 and W2 in a new W2
 !
 use pars,           ONLY:SP
 use frequency,      ONLY:w_samp,W_reset,W_duplicate
 use memory_m,       ONLY:mem_est
 !
 implicit none
 !
 type(w_samp):: W1,W2
 integer     :: table(W1%n_freqs+W2%n_freqs)
 ! 
 ! Work Space
 !
 integer     :: nw,iw1,iw2,i1
 type(w_samp):: Wbase
 real(SP)    :: r(3)
 !
 call W_reset(Wbase)
 call W_duplicate(W2,Wbase)
 call W_reset(W2)
 !
 nw=Wbase%n_freqs
 table=0
 do i1=1,Wbase%n_freqs
   table(i1)=-i1
 enddo
 !
 W1_loop: do iw1=W1%n_freqs,1,-1
   !
   do iw2=1,Wbase%n_freqs
     !
     r=(/real(Wbase%p(iw2)),real(W1%p(iw1)),0._SP/)
     if (iw2<Wbase%n_freqs) r(3)=real(Wbase%p(iw2+1))
     !
     if (abs(r(1)-r(2))<1.E-5) then
       table(iw2)=iw1
       cycle W1_loop 
     endif
     !
     if ((iw2< Wbase%n_freqs.and.r(1)<r(2).and.r(2)<r(3)).or.&
&        (iw2==Wbase%n_freqs.and.r(1)<r(2)) ) then
       do i1=W1%n_freqs+Wbase%n_freqs-1,iw2+1,-1
         table(i1+1)=table(i1)
       enddo
       table(iw2+1)=iw1
       nw=nw+1
       cycle W1_loop
     endif
     !
   enddo
   !
 enddo W1_loop
 !
 W2%n_freqs=nw
 W2%damp_reference=Wbase%damp_reference
 W2%per_memstps=Wbase%per_memstps
 allocate(W2%p(nw))
 call mem_est("W-p",(/W2%n_freqs/))
 do iw1=1,nw
   if (table(iw1)>0) W2%p(iw1)=W1%p(table(iw1))
   if (table(iw1)<0) W2%p(iw1)=Wbase%p(-table(iw1))
 enddo
 W2%er=(/real(W2%p(1)) ,real(W2%p(nw))/)
 W2%dr=(/aimag(W2%p(1)),aimag(W2%p(nw))/)
 !
 call W_reset(Wbase)
 !
end subroutine FREQUENCIES_merge
