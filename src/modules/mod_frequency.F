!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
module frequency
 !
 use pars,  ONLY:SP
 use units, ONLY:HA2EV
 implicit none
 !
 ! Bare 2 Coarse grid
 !
 ! cg_npts      ! Coarse grid points
 ! cg_pt        ! Coarse grid point
 ! cg_index_bg(ibg)  ! Tells the index in the reordered (not coarse) 
 !                   ! grid of the ibg-th element
 !                   ! of the original (not sorted) grid
 ! bg_npts(icg) ! Tells how many poles are linked to the POLE of the
 !              ! non degenerate new grid (icg)
 !
 integer :: cg_npts
 real(SP),allocatable :: cg_pt(:)
 integer, allocatable :: rg_index_bg(:)
 integer, allocatable :: cg_index_bg(:)
 integer, allocatable :: bg_npts(:)
 !
 type w_samp
   integer      :: n(2)
   real(SP)     :: er(2)
   real(SP)     :: damp_reference
   real(SP)     :: dr(2)
   real(SP)     :: per_memstps
   complex(SP),pointer :: p(:) 
 end type w_samp
 !
 contains
   !
   subroutine W_merge(W1,Wo,table)
     use memory_m, ONLY:mem_est
     type(w_samp):: W1,Wo
     integer     :: table(W1%n(1)+Wo%n(1))
     ! 
     ! Work Space
     !
     integer     :: nw,iw1,iw2,i1
     type(w_samp):: Wbase
     real(SP)    :: r(3)
     !
     call W_reset(Wbase)
     call W_duplicate(Wo,Wbase)
     call W_reset(Wo)
     !
     nw=Wbase%n(1)
     table=0
     do i1=1,nw
       table(i1)=-i1
     enddo
     !
     W1_loop: do iw1=W1%n(1),1,-1
       do iw2=1,Wbase%n(1)
         !
         r=(/real(Wbase%p(iw2)),real(W1%p(iw1)),0._SP/)
         if (iw2<Wbase%n(1)) r(3)=real(Wbase%p(iw2+1))
         !
         if (abs(r(1)-r(2))<1.E-5) then
           table(iw2)=iw1
           cycle W1_loop 
         endif
         !
         if ((iw2< Wbase%n(1).and.r(1)<r(2).and.r(2)<r(3)).or.&
&            (iw2==Wbase%n(1).and.r(1)<r(2)) ) then
           do i1=W1%n(1)+Wbase%n(1)-1,iw2+1,-1
             table(i1+1)=table(i1)
           enddo
           table(iw2+1)=iw1
           nw=nw+1
           cycle W1_loop
         endif
         !
       enddo
     enddo W1_loop
     !
     Wo%n=nw
     allocate(Wo%p(nw))
     call mem_est("W-p",(/Wo%n(1)/))
     do iw1=1,nw
       if (table(iw1)>0) Wo%p(iw1)=W1%p(table(iw1))
       if (table(iw1)<0) Wo%p(iw1)=Wbase%p(-table(iw1))
     enddo
     Wo%er=(/ minval(real(Wo%p)), maxval(real(Wo%p)) /)
     Wo%dr=(/ minval(aimag(Wo%p)), maxval(aimag(Wo%p)) /)
     !
     call W_reset(Wbase)
     !
   end subroutine W_merge
   !
   subroutine W_duplicate(Wi,Wo)
     use memory_m, ONLY:mem_est
     type(w_samp):: Wi,Wo
     Wo%n=Wi%n
     Wo%per_memstps=Wi%per_memstps
     Wo%er=Wi%er
     Wo%dr=Wi%dr
     Wo%damp_reference=Wi%damp_reference
     if (associated(Wi%p)) then
       allocate(Wo%p(Wo%n(1)))
       call mem_est("W-p",(/Wo%n(1)/))
       Wo%p=Wi%p
     endif
   end subroutine W_duplicate
   !
   subroutine W_reset(W)
     use memory_m, ONLY:mem_est
     type(w_samp)::W
     W%n=100
     W%per_memstps=100.
     W%er=(/0.,10./)/HA2EV
     W%dr=(/.1,.1/)/HA2EV
     W%damp_reference=0.
     nullify(W%p)
     call mem_est("W-p")
   end subroutine W_reset
   !
end module frequency
