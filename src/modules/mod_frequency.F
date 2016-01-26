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
module frequency
 !
 use pars,  ONLY:SP
 use units, ONLY:HA2EV
 implicit none
 !
 ! Bare 2 Coarse grid
 !
 ! coarse_grid_N          ! Coarse grid points
 ! coarse_grid_Pt         ! Coarse grid point
 ! coarse_grid_index(ibg) ! Tells the index in the reordered (not coarse) 
 !                        ! grid of the ibg-th element
 !                        ! of the original (not sorted) grid
 ! ordered_grid_index(ibg)! Tells the index in the reordered (not coarse) 
 !                        ! grid of the ibg-th element
 !                        ! of the original (not sorted) grid
 ! bare_grid_N(icg)       ! Tells how many poles are linked to the POLE of the
 !                        ! non degenerate new grid (icg)
 !
 integer :: coarse_grid_N
 real(SP),allocatable :: coarse_grid_Pt(:)
 integer, allocatable :: ordered_grid_index(:)
 integer, allocatable :: coarse_grid_index(:)
 integer, allocatable :: bare_grid_N(:)
 !
 type w_samp
   integer      :: n_freqs
   real(SP)     :: er(2)
   real(SP)     :: damp_reference
   real(SP)     :: dr(2)
   real(SP)     :: per_memstps
   complex(SP),pointer :: p(:) => null() 
 end type w_samp
 !
 contains
   !
   subroutine W_merge(W1,Wo,table)
     use memory_m, ONLY:mem_est
     type(w_samp):: W1,Wo
     integer     :: table(W1%n_freqs+Wo%n_freqs)
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
     nw=Wbase%n_freqs
     table=0
     do i1=1,Wbase%n_freqs
       table(i1)=-i1
     enddo
     !
     W1_loop: do iw1=W1%n_freqs,1,-1
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
&            (iw2==Wbase%n_freqs.and.r(1)<r(2)) ) then
           do i1=W1%n_freqs+Wbase%n_freqs-1,iw2+1,-1
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
     Wo%n_freqs=nw
     allocate(Wo%p(nw))
     call mem_est("W-p",(/Wo%n_freqs/))
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
     Wo%n_freqs=Wi%n_freqs
     Wo%per_memstps=Wi%per_memstps
     Wo%er=Wi%er
     Wo%dr=Wi%dr
     Wo%damp_reference=Wi%damp_reference
     if (associated(Wi%p)) then
       allocate(Wo%p(Wo%n_freqs))
       call mem_est("W-p",(/Wo%n_freqs/))
       Wo%p=Wi%p
     endif
   end subroutine W_duplicate
   !
   subroutine W_reset(W)
     use memory_m, ONLY:mem_est
     type(w_samp)::W
     W%n_freqs=100
     W%per_memstps=100._SP
     W%er=(/0._SP,10._SP/)/HA2EV
     W%dr=(/0.1_SP,0.1_SP/)/HA2EV
     W%damp_reference=0._SP
     nullify(W%p)
     call mem_est("W-p")
   end subroutine W_reset
   !
end module frequency
