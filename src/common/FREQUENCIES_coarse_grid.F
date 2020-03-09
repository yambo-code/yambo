!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine FREQUENCIES_coarse_grid(title,bg_pt,npts,cg_percentual,in_pt,lterm)
 !
 ! Input
 !-------
 ! integer     :: npts
 ! real(SP)    :: bg_pt(npts),cg_percentual
 ! real(SP)    :: in_pt(npts)   energies of the initial states (if lterm)
 ! logical     :: lterm         whether terminator techniques are used
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
 use pars,        ONLY:SP,schlen
 use com,         ONLY:msg
 use frequency,   ONLY:bare_grid_N,coarse_grid_N,coarse_grid_Pt,ordered_grid_index,coarse_grid_index
 use vec_operate, ONLY:sort
#include<memory.h>
 !
 character(*),intent(in) :: title
 integer,intent(in)      :: npts
 real(SP),intent(in)     :: bg_pt(npts),cg_percentual
 real(SP),intent(in)     :: in_pt(*)
 logical,intent(in)      :: lterm
 !
 ! DEFAULT TRESHOLD
 !
 real(SP), parameter :: default_treshold=1.E-5
 real(SP), parameter :: default_treshold_ini=1.E-5
 integer,  parameter :: MAX_cycles=1000
 !
 ! Work Space
 ! 
 integer ::i_cg,i_bg,i_bg_loc,i_bg_ref,dncg,ipos(npts,2),icycle,itresh
 real(SP)::tresh,tresh_ini,df
 logical ::lcycle
 character(schlen)   :: ch
 integer ,allocatable:: bg_sorted_x(:),i_vec(:)
 real(SP),allocatable:: bg_diffs(:),bg_sorted(:)
 real(SP),allocatable:: in_sorted(:)
 !
 if(npts==0) then
   YAMBO_ALLOC(ordered_grid_index,(npts))
   YAMBO_ALLOC(coarse_grid_index,(npts))
   YAMBO_ALLOC(bare_grid_N,(npts))
   YAMBO_ALLOC(coarse_grid_Pt,(npts))
   coarse_grid_N=0
   return
 endif   
 !
 if (cg_percentual==0._SP .or. npts==1) then
   !
   ! Zero cg_percentual does nothing!  This is used when 
   ! the response function is calculated using an external set of
   ! k-points/bands.
   !
   YAMBO_ALLOC(ordered_grid_index,(npts))
   YAMBO_ALLOC(coarse_grid_index,(npts))
   YAMBO_ALLOC(bare_grid_N,(npts))
   YAMBO_ALLOC(coarse_grid_Pt,(npts))
   coarse_grid_N=npts
   bare_grid_N=1
   coarse_grid_Pt=bg_pt
   forall ( i_bg=1:npts) ordered_grid_index(i_bg)=i_bg
   forall ( i_bg=1:npts) coarse_grid_index(i_bg)=i_bg
   return
 endif
 !
 YAMBO_ALLOC(bg_diffs,(npts-1))
 YAMBO_ALLOC(bg_sorted,(npts))
 YAMBO_ALLOC(bg_sorted_x,(npts))
 bg_sorted=bg_pt
 !
 YAMBO_ALLOC(in_sorted,(npts))
 in_sorted=0.0_SP
 !
 call sort(bg_sorted,indx=bg_sorted_x)
 do i_bg=1,npts-1
   bg_diffs(i_bg)=bg_sorted(i_bg+1)-bg_sorted(i_bg)
 enddo
 !
 if (lterm) then
   do i_bg=1,npts
     in_sorted(i_bg)=in_pt(bg_sorted_x(i_bg))
   enddo
 endif
 !
 if(npts>2) call sort(bg_diffs)
 !
 tresh=default_treshold
 tresh_ini=default_treshold_ini
 !
 if (cg_percentual<0.) tresh=minval(bg_diffs)+&
&                      abs(cg_percentual)/100._SP*(maxval(bg_diffs)-minval(bg_diffs))
 icycle=0
 coarse_grid_N=-1
 lcycle=.true.
 do while(lcycle)
   icycle=icycle+1
   i_cg=1
   i_bg_ref=1
   ipos(1,1)=i_cg
   ipos(bg_sorted_x(1),2)=1
   do i_bg=2,npts
     !
     ! df redefined using abs(), since the v-->v transition energies
     ! can be negative (when the terminator is used)
     !
     df=bg_sorted(i_bg)-bg_sorted(i_bg_ref)
     if (abs(df)>tresh .or. (lterm.and.abs(in_sorted(i_bg)-in_sorted(i_bg_ref))>tresh_ini) ) then
       i_cg=i_cg+1
       i_bg_ref=i_bg
     endif
     ipos(i_bg,1)=i_cg
     ipos(bg_sorted_x(i_bg),2)=i_bg
   enddo
   if (icycle==1) dncg=i_cg
   if (cg_percentual>0._SP) coarse_grid_N=max(int(cg_percentual*real(dncg,SP)/100._SP),1)
   if (i_cg<=coarse_grid_N.or.cg_percentual==100._SP.or.icycle>MAX_cycles) lcycle=.false.
   if (icycle==1) then
     itresh=npts-coarse_grid_N
     tresh=bg_diffs(max(itresh,1))
     cycle
   endif
   itresh=min(itresh+i_cg-coarse_grid_N,npts-1)
   tresh=bg_diffs(max(itresh,1))
 enddo
 coarse_grid_N=i_cg
 !
 YAMBO_ALLOC(ordered_grid_index,(npts))
 YAMBO_ALLOC(coarse_grid_index,(npts))
 YAMBO_ALLOC(bare_grid_N,(coarse_grid_N))
 YAMBO_ALLOC(coarse_grid_Pt,(coarse_grid_N))
 !
 ordered_grid_index=ipos(:,2)
 !
 i_cg=1
 coarse_grid_Pt=0._SP
 bare_grid_N=0
 do i_bg=1,npts
   if (ipos(i_bg,1)/=i_cg) then
     coarse_grid_Pt(i_cg)=coarse_grid_Pt(i_cg)/real(bare_grid_N(i_cg))
     i_cg=i_cg+1
   endif
   coarse_grid_Pt(i_cg)=coarse_grid_Pt(i_cg)+bg_sorted(i_bg)
   bare_grid_N(i_cg)=bare_grid_N(i_cg)+1
 enddo
 coarse_grid_Pt(coarse_grid_N)=coarse_grid_Pt(coarse_grid_N)/real(bare_grid_N(coarse_grid_N))
 !
 i_bg=0
 YAMBO_ALLOC(i_vec,(npts))
 do i_cg=1,coarse_grid_N
   do i_bg_loc=1,bare_grid_N(i_cg)
     i_bg=i_bg+1
     i_vec(i_bg)=i_cg
   enddo
 enddo
 forall(i_bg=1:npts) coarse_grid_index(i_bg)=i_vec( ordered_grid_index(i_bg) )
 YAMBO_FREE(i_vec)
 !
 if(.not.trim(title)=='COLL') then
   write (ch,'(3a)') '[',title,'-CG] R(p) Tot o/o(of R)  '
   call msg('rs',trim(ch),(/coarse_grid_N,npts,int(real(coarse_grid_N)/real(dncg)*100._SP)/))
 endif
 !
 YAMBO_FREE(bg_diffs)
 YAMBO_FREE(bg_sorted)
 YAMBO_FREE(bg_sorted_x)
 YAMBO_FREE(in_sorted)
 !
end subroutine
