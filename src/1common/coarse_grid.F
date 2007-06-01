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
subroutine coarse_grid(title,bg_pt,npts,cg_percentual)
 !
 ! Input
 !-------
 ! integer     :: npts
 ! real(SP)    :: bg_pt(npts),cg_percentual
 !
 ! Output
 !--------
 ! integer :: cg_npts
 ! real(SP),allocatable :: cg_pt(:)
 ! integer, allocatable :: cg_index_bg(:)
 ! integer, allocatable :: rg_index_bg(:)
 ! integer, allocatable :: bg_npts(:)
 !
 !  cg_npts      ! Number of Coarse grid points
 !  cg_pt        ! Coarse grid point
 !  rg_index_bg(ibg)  ! Tells the index in the reordered (not coarse) 
 !                    ! grid of the ibg-th element
 !                    ! of the original (not sorted) grid
 !  cg_index_bg(ibg)  ! Tells the index in the coarse 
 !                    ! grid of the ibg-th element
 !                    ! of the original (not sorted) grid
 !  bg_npts(icg) ! Tells how many poles are linked to the POLE of the coarse grid
 !
 use pars,        ONLY:SP,IP,schlen
 use com,         ONLY:msg
 use frequency,   ONLY:bg_npts,cg_npts,cg_pt,rg_index_bg,cg_index_bg
 use vec_operate, ONLY:sort
 use memory_m   , ONLY:mem_est
 implicit none
 !
 character(*):: title
 integer     :: npts
 real(SP)    :: bg_pt(npts),cg_percentual
 !
 ! DEFAULT TRESHOLD
 !
 real(SP), parameter :: default_treshold=1.E-6
 !
 ! Work Space
 ! 
 integer ::i1,i2,i3,dncg,ipos(npts,2),icycle,itresh
 real(SP)::tresh,df
 logical ::lcycle
 character(schlen)   :: ch
 integer ,allocatable:: bg_sorted_x(:),i_vec(:)
 real(SP),allocatable:: bg_diffs(:),bg_sorted(:)
 !
 if (cg_percentual==0.) then
   !
   ! Zero cg_percentual does nothing!  This is used when 
   ! the response function is calculated using an external set of
   ! k-points/bands.
   !
   allocate(rg_index_bg(npts),cg_index_bg(npts),bg_npts(npts),cg_pt(npts))
   call mem_est("RGi CGi BGn CGp",(/npts,npts,npts,npts/),(/IP,IP,IP,SP/))
   cg_npts=npts
   bg_npts=1
   cg_pt=bg_pt
   forall ( i1=1:npts) rg_index_bg(i1)=i1
   forall ( i1=1:npts) cg_index_bg(i1)=i1
   return
 endif
 !
 allocate(bg_diffs(npts-1),bg_sorted(npts),bg_sorted_x(npts))
 bg_sorted=bg_pt
 call sort(bg_sorted,indx=bg_sorted_x)
 do i1=1,npts-1
   bg_diffs(i1)=bg_sorted(i1+1)-bg_sorted(i1)
 enddo
 !
 call sort(bg_diffs)
 !
 tresh=default_treshold
 if (cg_percentual<0.) tresh=minval(bg_diffs)+&
&                      abs(cg_percentual)/100.*(maxval(bg_diffs)-minval(bg_diffs))
 icycle=0
 cg_npts=-1
 lcycle=.true.
 do while(lcycle)
   icycle=icycle+1
   i2=1
   ipos(1,1)=i2
   ipos(bg_sorted_x(1),2)=1
   do i1=2,npts
     df=bg_sorted(i1)-bg_sorted(i1-1)
     if (df>tresh) i2=i2+1
     ipos(i1,1)=i2
     ipos(bg_sorted_x(i1),2)=i1
   enddo
   if (icycle==1) dncg=i2
   if (cg_percentual>0.) cg_npts=max(int(cg_percentual*real(dncg)/100.),1)
   if (i2<=cg_npts.or.cg_percentual==100.) lcycle=.false.
   if (icycle==1) then
     itresh=npts-cg_npts
     tresh=bg_diffs(max(itresh,1))
     cycle
   endif
   itresh=min(itresh+i2-cg_npts,npts-1)
   tresh=bg_diffs(max(itresh,1))
 enddo
 cg_npts=i2
 !
 allocate(rg_index_bg(npts),cg_index_bg(npts),bg_npts(cg_npts),cg_pt(cg_npts))
 call mem_est("RGi CGi BGn CGp",(/npts,npts,cg_npts,cg_npts/),(/IP,IP,IP,SP/))
 !
 rg_index_bg=ipos(:,2)
 !
 i2=1
 cg_pt=0.
 bg_npts=0
 do i1=1,npts
   if (ipos(i1,1)/=i2) then
     cg_pt(i2)=cg_pt(i2)/real(bg_npts(i2))
     i2=i2+1
   endif
   cg_pt(i2)=cg_pt(i2)+bg_sorted(i1)
   bg_npts(i2)=bg_npts(i2)+1
 enddo
 cg_pt(cg_npts)=cg_pt(cg_npts)/real(bg_npts(cg_npts))
 !
 i3=0
 allocate(i_vec(npts))
 do i1=1,cg_npts
   do i2=1,bg_npts(i1)
     i3=i3+1
     i_vec(i3)=i1
   enddo
 enddo
 forall(i1=1:npts) cg_index_bg(i1)=i_vec( rg_index_bg(i1) )
 deallocate(i_vec)
 !
 write (ch,'(3a)') '[',title,'-CG] R(p) Tot o/o(of R)  :'
 !
 call msg('rs',trim(ch),(/cg_npts,npts,int(real(cg_npts)/real(dncg)*100.)/))
 !
 deallocate(bg_diffs,bg_sorted,bg_sorted_x)
 !
 end subroutine
!
