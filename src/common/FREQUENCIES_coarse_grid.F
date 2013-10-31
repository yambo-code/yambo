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
subroutine FREQUENCIES_coarse_grid(title,bg_pt,npts,cg_percentual)
 !
 ! Input
 !-------
 ! integer     :: npts
 ! real(SP)    :: bg_pt(npts),cg_percentual
 !
 ! Output
 !--------
 ! coarse_grid_N          ! Coarse grid points
 ! coarse_grid_Pt         ! Coarse grid point
 ! coarse_grid_index(ibg) ! Tells the index in the coarse
 !                        ! grid of the ibg-th element
 !                        ! of the original (not sorted) grid
 ! ordered_grid_index(ibg)! Tells the index in the reordered (not coarse) 
 !                        ! grid of the ibg-th element
 !                        ! of the original (not sorted) grid
 ! bare_grid_N(icg)       ! Tells how many poles are linked to the POLE of the
 !                        ! non degenerate new grid (icg)
 !
 use pars,        ONLY:SP,IP,schlen
 use com,         ONLY:msg
 use frequency,   ONLY:bare_grid_N,coarse_grid_N,coarse_grid_Pt,ordered_grid_index,coarse_grid_index
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
 if (cg_percentual==0.or.npts==1) then
   !
   ! Zero cg_percentual does nothing!  This is used when 
   ! the response function is calculated using an external set of
   ! k-points/bands.
   !
   allocate(ordered_grid_index(npts),coarse_grid_index(npts),bare_grid_N(npts),coarse_grid_Pt(npts))
   call mem_est("RGi CGi BGn CGp",(/npts,npts,npts,npts/),(/IP,IP,IP,SP/))
   coarse_grid_N=npts
   bare_grid_N=1
   coarse_grid_Pt=bg_pt
   forall ( i1=1:npts) ordered_grid_index(i1)=i1
   forall ( i1=1:npts) coarse_grid_index(i1)=i1
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
 coarse_grid_N=-1
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
   if (cg_percentual>0.) coarse_grid_N=max(int(cg_percentual*real(dncg)/100.),1)
   if (i2<=coarse_grid_N.or.cg_percentual==100.) lcycle=.false.
   if (icycle==1) then
     itresh=npts-coarse_grid_N
     tresh=bg_diffs(max(itresh,1))
     cycle
   endif
   itresh=min(itresh+i2-coarse_grid_N,npts-1)
   tresh=bg_diffs(max(itresh,1))
 enddo
 coarse_grid_N=i2
 !
 allocate(ordered_grid_index(npts),coarse_grid_index(npts),bare_grid_N(coarse_grid_N),coarse_grid_Pt(coarse_grid_N))
 call mem_est("RGi CGi BGn CGp",(/npts,npts,coarse_grid_N,coarse_grid_N/),(/IP,IP,IP,SP/))
 !
 ordered_grid_index=ipos(:,2)
 !
 i2=1
 coarse_grid_Pt=0.
 bare_grid_N=0
 do i1=1,npts
   if (ipos(i1,1)/=i2) then
     coarse_grid_Pt(i2)=coarse_grid_Pt(i2)/real(bare_grid_N(i2))
     i2=i2+1
   endif
   coarse_grid_Pt(i2)=coarse_grid_Pt(i2)+bg_sorted(i1)
   bare_grid_N(i2)=bare_grid_N(i2)+1
 enddo
 coarse_grid_Pt(coarse_grid_N)=coarse_grid_Pt(coarse_grid_N)/real(bare_grid_N(coarse_grid_N))
 !
 i3=0
 allocate(i_vec(npts))
 do i1=1,coarse_grid_N
   do i2=1,bare_grid_N(i1)
     i3=i3+1
     i_vec(i3)=i1
   enddo
 enddo
 forall(i1=1:npts) coarse_grid_index(i1)=i_vec( ordered_grid_index(i1) )
 deallocate(i_vec)
 !
 write (ch,'(3a)') '[',title,'-CG] R(p) Tot o/o(of R)  :'
 !
 call msg('rs',trim(ch),(/coarse_grid_N,npts,int(real(coarse_grid_N)/real(dncg)*100.)/))
 !
 deallocate(bg_diffs,bg_sorted,bg_sorted_x)
 !
end subroutine
