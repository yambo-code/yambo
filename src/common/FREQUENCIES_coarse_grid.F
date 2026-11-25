!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
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
 !
#include<y_memory.h>
 !
 character(*),intent(in) :: title
 integer,intent(in)      :: npts
 real(SP),intent(in)     :: bg_pt(npts),cg_percentual
 real(SP),intent(in)     :: in_pt(*)
 logical,intent(in)      :: lterm
 !
 ! DEFAULT TRESHOLD
 !
 ! Work Space
 ! 
 integer ::i_cg,i_bg,i_bg_loc,dncg,ipos(npts,2)
 character(schlen)   :: ch
 integer ,allocatable:: bg_sorted_x(:),i_vec(:)
 real(SP),allocatable:: bg_sorted(:)
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
 YAMBO_ALLOC(bg_sorted,(npts))
 YAMBO_ALLOC(bg_sorted_x,(npts))
 call FREQUENCIES_group_engine(bg_pt,npts,cg_percentual,in_pt,lterm,coarse_grid_N,dncg,&
&                             bg_sorted,bg_sorted_x,ipos(:,1))
 do i_bg=1,npts
   ipos(bg_sorted_x(i_bg),2)=i_bg
 enddo
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
 YAMBO_FREE(bg_sorted)
 YAMBO_FREE(bg_sorted_x)
 !
end subroutine
!
subroutine FREQUENCIES_group_engine(bg_pt,npts,cg_percentual,in_pt,lterm,n_groups,n_groups_initial,&
&                                   bg_sorted,bg_sorted_x,group_sorted)
 ! Side-effect-free X-CG grouping engine shared by the production wrapper and
 ! the weighted-band prepass.
 use pars,        ONLY:SP
 use vec_operate, ONLY:sort
 !
 integer,intent(in)  :: npts
 real(SP),intent(in) :: bg_pt(npts),cg_percentual,in_pt(*)
 logical,intent(in)  :: lterm
 integer,intent(out) :: n_groups,n_groups_initial,bg_sorted_x(npts),group_sorted(npts)
 real(SP),intent(out):: bg_sorted(npts)
 !
 real(SP),parameter :: default_treshold=1.E-5_SP,default_treshold_ini=1.E-5_SP
 integer,parameter  :: MAX_cycles=1000
 integer :: i_bg,i_bg_ref,icycle,itresh
 real(SP) :: tresh,tresh_ini,df
 logical :: lcycle
 real(SP),allocatable :: bg_diffs(:),in_sorted(:)
 !
 if (npts==0) then
   n_groups=0
   n_groups_initial=0
   return
 endif
 bg_sorted=bg_pt
 call sort(bg_sorted,indx=bg_sorted_x)
 if (cg_percentual==0._SP.or.npts==1) then
   n_groups=npts
   n_groups_initial=npts
   do i_bg=1,npts
     group_sorted(i_bg)=i_bg
   enddo
   return
 endif
 allocate(bg_diffs(npts-1),in_sorted(npts))
 in_sorted=0._SP
 do i_bg=1,npts-1
   bg_diffs(i_bg)=bg_sorted(i_bg+1)-bg_sorted(i_bg)
 enddo
 if (lterm) then
   do i_bg=1,npts
     in_sorted(i_bg)=in_pt(bg_sorted_x(i_bg))
   enddo
 endif
 if (npts>2) call sort(bg_diffs)
 tresh=default_treshold
 tresh_ini=default_treshold_ini
 if (cg_percentual<0._SP) tresh=minval(bg_diffs)+&
& abs(cg_percentual)/100._SP*(maxval(bg_diffs)-minval(bg_diffs))
 icycle=0
 n_groups=-1
 lcycle=.true.
 do while(lcycle)
   icycle=icycle+1
   n_groups=1
   i_bg_ref=1
   group_sorted(1)=n_groups
   do i_bg=2,npts
     df=bg_sorted(i_bg)-bg_sorted(i_bg_ref)
     if (abs(df)>tresh.or.(lterm.and.abs(in_sorted(i_bg)-in_sorted(i_bg_ref))>tresh_ini)) then
       n_groups=n_groups+1
       i_bg_ref=i_bg
     endif
     group_sorted(i_bg)=n_groups
   enddo
   if (icycle==1) n_groups_initial=n_groups
   if (cg_percentual>0._SP) n_groups=max(int(cg_percentual*real(n_groups_initial,SP)/100._SP),1)
   if (group_sorted(npts)<=n_groups.or.cg_percentual==100._SP.or.icycle>MAX_cycles) lcycle=.false.
   if (icycle==1) then
     itresh=npts-n_groups
     tresh=bg_diffs(max(itresh,1))
     cycle
   endif
   itresh=min(itresh+group_sorted(npts)-n_groups,npts-1)
   tresh=bg_diffs(max(itresh,1))
 enddo
 n_groups=group_sorted(npts)
 deallocate(bg_diffs,in_sorted)
 !
end subroutine
