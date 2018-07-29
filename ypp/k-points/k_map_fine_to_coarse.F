!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine k_map_fine_to_coarse(Xk,Xen)
 !
 use pars,                ONLY:SP
 use units,               ONLY:HA2EV
 use zeros,               ONLY:k_iku_zero,k_rlu_zero,define_zeros
 use stderr,              ONLY:intc
 use R_lattice,           ONLY:bz_samp,k_the_nearest,bz_samp_reset,rl_sop
 use parser_m,            ONLY:parser
 use D_lattice,           ONLY:alat,nsym,input_GS_Tel,a,dl_sop,i_time_rev
 use com,                 ONLY:msg,error,warning
 use YPP,                 ONLY:BZ_DbGd_nkpts_to_use,DbGd_DB1_paths,MAX_DbGd_DBs,n_DbGd_DBs
 use electrons,           ONLY:levels,default_nel,n_bands,BZ_ASSOCIATED_nkpts,&
&                              BZ_DbGd_nbands,BZ_DbGd_nkpt,BZ_DbGd_table,BZ_DbGd_kpts,&
&                              BZ_DbGd_tot_nk_levels,BZ_DbGd_tot_nk,n_sp_pol,BZ_DbGd_max_filling,&
&                              BZ_DbGd_weight,E_reset,DbGd_is_symmetry_expanded
 use parallel_m,          ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,          ONLY:PARALLEL_index,OCCUPATIONS_Fermi,io_DB1_selective_scan
 use LIVE_t,              ONLY:live_timing
 use IO_m,                ONLY:io_control,LOG,OP_WR_CL
 !
#include<memory.h>
 !
 type(bz_samp) :: Xk
 type(levels)  :: Xen
 !
 ! Work Space
 !
 real(SP)     ::a_save(3,3),alat_save(3),gap(2),tmp_zero(3)
 integer      ::ik,ik_near,ik_ibz,is,ik_bz,ik_DbGd,i_c,NK_to_process,ik_grid,iE_grid,NK_found
 logical      ::l_skip_check,l_flag
 integer,  allocatable :: BZ_SYM_table(:,:)
 real(SP), allocatable :: rand_E(:,:,:),rand_k(:,:)
 !
 ! BZ DB1 I/O
 !
 type(levels)         :: DbGd_E(MAX_DbGd_DBs)
 type(bz_samp)        :: DbGd_k(MAX_DbGd_DBs)
 integer              :: ID,i_err(MAX_DbGd_DBs),nsym_save,nsym_DbGd_DB,i_time_rev_save,&
&                        i_time_rev_DB,i_db,i_db_merge,i_dummy
 real(SP)             :: dl_sop_save(3,3,48)
 integer, allocatable :: TMP_map2(:,:),TMP_map3(:,:,:),IK_ref(:)
 type(PP_indexes)     :: px
 !
 ! Functions
 !
 integer, external :: io_Double_Grid
 !
 BZ_DbGd_nbands=0
 !
 if (n_DbGd_DBs==0) then
   call msg("s",'DbGd: no database secified. Nothing to do.')
   return
 endif
 !
 if (allocated(BZ_DbGd_nkpt)) then
   call msg("s",'DbGd: database components already allocated. Nothing to do.')
   return
 endif
 !
 ! Saves 2 check
 !
 a_save=a
 alat_save=alat
 nsym_save=nsym
 dl_sop_save(:,:,:nsym)=dl_sop
 i_time_rev_save=i_time_rev
 !
 ! DB1 reloading
 !
 i_err=0
 if (n_DbGd_DBs>0) then
   do i_db=1,n_DbGd_DBs
     i_err(i_db)=io_DB1_selective_scan('all',trim(DbGd_DB1_paths(i_db,1)),DbGd_E(i_db),DbGd_k(i_db))
     if (i_err(i_db)/=0) call msg("s",'DbGd database NOT FOUND @'//trim(DbGd_DB1_paths(i_db,1)))
     if (i_err(i_db)==0) call msg("s",'DbGd database FOUND @'//trim(DbGd_DB1_paths(i_db,1)))
     !
     ! DB1 checks
     !
     if (i_err(i_db)==0.and.any(a/=a_save))  then
       call msg("s",':: DbGd DB1 DL structure does not fit')
       i_err(i_db)=-1
     endif
     if (i_err(i_db)==0.and.any(alat/=alat_save))  then
       call msg("s",':: DbGd DB1 alat does not fit.')
       i_err(i_db)=-1
     endif
     !
   enddo
   !
   if (any((/i_err(:n_DbGd_DBs)/=0/))) call error(" One or more DbGd databases have not been found")
   !
 endif
 !
 ! Now that I have the finer grid of Kpts I redefine the zeros for the kpts
 !
 do i_db=1,n_DbGd_DBs
   if(.not. trim(DbGd_DB1_paths(i_db,1))=="." ) then
     call define_zeros(vector_=DbGd_k(i_db)%pt, zero_=tmp_zero)
     k_iku_zero=min(k_iku_zero,tmp_zero)
     call define_zeros(vector_=DbGd_k(i_db)%pt, zero_=tmp_zero,RLU=.TRUE.)
     k_rlu_zero=min(k_rlu_zero,tmp_zero)
   endif
 enddo
 !
 ! Symmetry restoring
 !
 if (allocated(dl_sop)) then
   YAMBO_FREE(dl_sop)
   nsym_DbGd_DB=nsym
   nsym       =nsym_save
   i_time_rev_DB=i_time_rev
   i_time_rev   =i_time_rev_save
   YAMBO_ALLOC(dl_sop,(3,3,nsym))
   dl_sop=dl_sop_SAVE(:,:,:nsym)
 endif
 !
 call parser('SkipCheck',l_skip_check)
 !GPL_EXCLUDE_START
 call parser('noBZExpand',l_flag)
 !
 DbGd_is_symmetry_expanded=.not.l_flag
 !GPL_EXCLUDE_END
 !
 !GPL_INCLUDE_START
 !DbGd_is_symmetry_expanded=.TRUE.
 !GPL_INCLUDE_END
 !
 call section('*','BZ Double Grid DB(s)')
 !
 call msg('s',':: Electrons             :',default_nel)
 call msg('s',':: Temperature       [ev]:',input_GS_Tel)
 call msg('s',':: Lattice factors [a.u.]:',alat)
 do i_db=1,n_DbGd_DBs
   if (i_db==1) call msg('s',':: K points        [DB #'//trim(intc(i_db))//']:',DbGd_E(i_db)%nk)
   if (i_db> 1) call msg('s','::                 [DB #'//trim(intc(i_db))//']:',DbGd_E(i_db)%nk)
 enddo
 !
 ! Mergering of ALL DbGd_E(:) => DbGd_E(n_DbGd_DBs+1)
 !
 i_db_merge=1
 !
 if (n_DbGd_DBs>1) then
   i_db_merge=n_DbGd_DBs+1
   call bz_samp_reset(DbGd_k(i_db_merge))
   call E_reset(DbGd_E(i_db_merge))
   DbGd_E(i_db_merge)%nk=sum(DbGd_E(:n_DbGd_DBs)%nk)
   DbGd_E(i_db_merge)%nb=DbGd_E(1)%nb
   DbGd_k(i_db_merge)%nibz=sum(DbGd_E(:n_DbGd_DBs)%nk)
   YAMBO_ALLOC(DbGd_E(i_db_merge)%E,(DbGd_E(i_db_merge)%nb,DbGd_E(i_db_merge)%nk,n_sp_pol))
   YAMBO_ALLOC(DbGd_k(i_db_merge)%pt,(DbGd_E(i_db_merge)%nk,3))
   do i_db=1,n_DbGd_DBs
     if (i_db==1) i_dummy=0
     if (i_db> 1) i_dummy=sum(DbGd_E(1:i_db-1)%nk)
     do ik=1,DbGd_E(i_db)%nk
       DbGd_E(i_db_merge)%E(:,ik+i_dummy,:)= DbGd_E(i_db)%E(:,ik,:)
       DbGd_k(i_db_merge)%pt(ik+i_dummy,:)= DbGd_k(i_db)%pt(ik,:)
     enddo
   enddo
 endif
 !
 ! Use of less (only BZ_DbGd_nkpts, read from input) k-points
 !
 if (BZ_DbGd_nkpts_to_use>0) then
   DbGd_E(i_db_merge)%nk=BZ_DbGd_nkpts_to_use
   DbGd_k(i_db_merge)%nibz=BZ_DbGd_nkpts_to_use
   YAMBO_ALLOC(rand_E,(DbGd_E(i_db_merge)%nb,BZ_DbGd_nkpts_to_use,n_sp_pol))
   YAMBO_ALLOC(rand_k,(BZ_DbGd_nkpts_to_use,3))
   rand_E(:,:,:)=DbGd_E(i_db_merge)%E(:,:BZ_DbGd_nkpts_to_use,:)
   rand_k(:BZ_DbGd_nkpts_to_use,:)=DbGd_k(i_db_merge)%pt(:BZ_DbGd_nkpts_to_use,:)
   YAMBO_FREE(DbGd_E(i_db_merge)%E)
   YAMBO_FREE(DbGd_k(i_db_merge)%pt)
   YAMBO_ALLOC(DbGd_E(i_db_merge)%E,(DbGd_E(i_db_merge)%nb,BZ_DbGd_nkpts_to_use,n_sp_pol))
   YAMBO_ALLOC(DbGd_k(i_db_merge)%pt,(BZ_DbGd_nkpts_to_use,3))
   DbGd_E(i_db_merge)%E(:,:,:)=rand_E(:,:,:)
   DbGd_k(i_db_merge)%pt(:,:)=rand_k(:,:)
   YAMBO_FREE(rand_E)
   YAMBO_FREE(rand_k)
 endif
 !
 gap(1)=minval(DbGd_E(i_db_merge)%E(Xen%nbf+1,:,1))-maxval(DbGd_E(i_db_merge)%E(Xen%nbf,:,1)) 
 gap(2)=10000._SP
 do ik=1,DbGd_k(i_db_merge)%nibz
   if (DbGd_E(i_db_merge)%E(Xen%nbf+1,ik,1)-DbGd_E(i_db_merge)%E(Xen%nbf,ik,1)<gap(2)) then
     gap(2)=DbGd_E(i_db_merge)%E(Xen%nbf+1,ik,1)-DbGd_E(i_db_merge)%E(Xen%nbf,ik,1)
   endif
 enddo
 !
 call msg('s','::              [imposed]:',BZ_DbGd_nkpts_to_use)
 call msg('s',':: Bands                 :',n_bands)
 call msg('s',':: Symmetries            :',nsym_DbGd_DB)
 if(i_time_rev_DB==1)&
&   call msg('s',':: Time reversal included ')
 if(i_time_rev_DB==0)&
&   call msg('s',':: Time reversal not included')
 if (Xen%nbf==Xen%nbm) then
   call msg('s',':: Gap indirect      [ev]:',gap(1)*HA2EV)
   call msg('s','::     direct        [ev]:',gap(2)*HA2EV)
 endif
 !
 ! See if there is correspondance between the random and the built-in k-points
 !
 YAMBO_ALLOC(IK_ref,(DbGd_k(i_db_merge)%nibz))
 !
 if (.not.l_skip_check) then
   !
   call section('+','Internal consistency check ...')
   !
   call k_DbGd_grid_consistency_check(IK_ref,Xk,Xen,DbGd_k(i_db_merge),DbGd_E(i_db_merge))
   !
   NK_found=count(IK_ref>0)
   !
   if (NK_found>0) then
     !
     call msg('s','Found '//trim(intc(NK_found))//" Double-Grid k-point(s) already in the built-in grid")
     !
     if (NK_found==Xk%nibz) then
       call warning("Empty group of k-points to be written in the DbGd DB")
       l_skip_check=.TRUE.
     else
       !
       call section('=','Removing the reference point(s) from the Double Grid ...')
       YAMBO_ALLOC(rand_k,(DbGd_k(i_db_merge)%nibz-NK_found,3))
       YAMBO_ALLOC(rand_E,(DbGd_E(i_db_merge)%nb,DbGd_k(i_db_merge)%nibz-NK_found,n_sp_pol))
       i_c=1
       do ik=1,DbGd_k(i_db_merge)%nibz
         if (IK_ref(ik)>0) cycle
         rand_k(i_c,:)  =DbGd_k(i_db_merge)%pt(ik,:)
         rand_E(:,i_c,:)=DbGd_E(i_db_merge)%E(:,ik,:)
         i_c=i_c+1
       enddo
       YAMBO_FREE(DbGd_E(i_db_merge)%E)
       YAMBO_FREE(DbGd_k(i_db_merge)%pt)
       DbGd_k(i_db_merge)%nibz=DbGd_k(i_db_merge)%nibz-NK_found
       YAMBO_ALLOC(DbGd_E(i_db_merge)%E,(DbGd_E(i_db_merge)%nb,DbGd_k(i_db_merge)%nibz,n_sp_pol))
       YAMBO_ALLOC(DbGd_k(i_db_merge)%pt,(DbGd_k(i_db_merge)%nibz,3))
       DbGd_E(i_db_merge)%E(:,:,:)=rand_E(:,:,:)
       DbGd_k(i_db_merge)%pt(:,:)=rand_k(:,:)
       YAMBO_FREE(rand_E)
       YAMBO_FREE(rand_k)
       DbGd_E(i_db_merge)%nk=DbGd_k(i_db_merge)%nibz
       !
     endif
   else 
     call msg('s','failed') 
     call error(" Internal check failed. A consistent X grid k-point was not found among the Double Grid list")
   endif
 endif
 !
 if (l_skip_check) then
   !
   call warning(" Internal check failed. A consistent X grid k-point was not found among the Double Grid list")
   YAMBO_ALLOC(DbGd_k(i_db_merge)%weights,( DbGd_E(i_db_merge)%nk ))
   DbGd_k(i_db_merge)%weights=1./real(DbGd_E(i_db_merge)%nk )
   call OCCUPATIONS_Fermi(DbGd_E(i_db_merge),DbGd_k(i_db_merge),mode="FERMI")
   YAMBO_FREE(DbGd_k(i_db_merge)%weights)
   !
   ! I want that E_dbgd%E'-Xe%E_fermi=E_dbgd%E-E_dbgd%E_fermi=> E_dbgd%E'= E_dbgd%E+Xe%E_fermi-E_dbgd%E_fermi
   !
   DbGd_E(i_db_merge)%E(:,:,:)=DbGd_E(i_db_merge)%E(:,:,:)+Xen%E_Fermi-DbGd_E(i_db_merge)%E_Fermi
   !
 endif
 !
 YAMBO_FREE(IK_ref)
 !
 BZ_DbGd_nbands=DbGd_E(i_db_merge)%nb
 !
 ! Xk (IBZ->BZ)
 !
 BZ_ASSOCIATED_nkpts=Xk%nbz
 call k_build_up_BZ_tables(Xk)
 call k_ibz2bz(Xk,'i',.true.)
 !
 if (DbGd_is_symmetry_expanded) then
   !
   call section('=','BZ Double Grid K-points expansion ...')
   !
   ! DbGd_k (IBZ->BZ)
   !
   call k_ibz2bz(DbGd_k(i_db_merge),'i',.true.)
   !
   NK_to_process=DbGd_k(i_db_merge)%nbz
   !
   call msg('s','found '//trim(intc(DbGd_k(i_db_merge)%nbz))//' ('//trim(intc(DbGd_k(i_db_merge)%nibz))//') points in the BZ (IBZ)')
   !
 endif
 !
 !GPL_EXCLUDE_START
 if (.not.DbGd_is_symmetry_expanded) then
   !
   call section('=','BZ Double Grid K-points summary ...')
   !
   NK_to_process=DbGd_k(i_db_merge)%nibz
   !
   call msg('s','using '//trim(intc(DbGd_k(i_db_merge)%nibz))//' k-points')
   !
 endif
 !GPL_EXCLUDE_END
 !
 ! Maps allocation
 !
 YAMBO_ALLOC(TMP_map2,(NK_to_process,2))
 TMP_map2=0
 !
 ! Par Proc
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/NK_to_process/))
 call PP_redux_wait
 !
 call live_timing('BZ Double Grid Map',px%n_of_elements(myid+1))
 !                ============
 do ik=1,NK_to_process
   !
   if (.not.px%element_1D(ik)) cycle
   !
   if (DbGd_is_symmetry_expanded) then
     ik_near=k_the_nearest(DbGd_k(i_db_merge)%ptbz(ik,:),Xk%ptbz(:,:),Xk%nbz)
     TMP_map2(ik,1)=ik_near ! bz@CG (coars grid)
   endif
   !
   !GPL_EXCLUDE_START
   if (.not.DbGd_is_symmetry_expanded) then
     ik_ibz = k_the_nearest(DbGd_k(i_db_merge)%pt(ik,:),Xk%pt(:,:),Xk%nibz,ROTATE_k_ref=.TRUE.,k_symm=is)
     TMP_map2(ik,1)=ik_ibz ! ibz@CG
     TMP_map2(ik,2)=is     ! is@CG
   endif
   !GPL_EXCLUDE_END
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call PP_redux_wait(TMP_map2)
 !
 ! Number of points in each zone & Tables
 !
 YAMBO_ALLOC(BZ_DbGd_nkpt,(Xk%nbz))
 !
 do i_c=1,2
   !
   if (i_c==2)  then
     YAMBO_ALLOC(TMP_map3,(Xk%nbz,maxval(BZ_DbGd_nkpt),2))
     TMP_map3=0
     if (.not.DbGd_is_symmetry_expanded) then
       YAMBO_ALLOC(BZ_SYM_table,(Xk%nbz,maxval(BZ_DbGd_nkpt)))
       BZ_SYM_table=0
       BZ_SYM_table(1:Xk%nbz,1)=1
     endif
   endif
   !
   BZ_DbGd_nkpt=0
   !
   do ik=1,NK_to_process
     ik_near=TMP_map2(ik,1)
     if (.not.DbGd_is_symmetry_expanded) then
       ik_ibz = TMP_map2(ik,1)
       ik_near     = Xk%k_table(ik_ibz,1)
     endif
     BZ_DbGd_nkpt(ik_near)=BZ_DbGd_nkpt(ik_near)+1
     !
     if (i_c==1) cycle
     !
     if (DbGd_is_symmetry_expanded) then
       TMP_map3(ik_near,BZ_DbGd_nkpt(ik_near),1)=DbGd_k(i_db_merge)%sstar(ik,1) ! ibz@FG
       TMP_map3(ik_near,BZ_DbGd_nkpt(ik_near),2)=ik ! bz@FG
     endif
     !GPL_EXCLUDE_START
     if (.not.DbGd_is_symmetry_expanded) then
       TMP_map3(ik_near,BZ_DbGd_nkpt(ik_near),1)=ik ! ibz@FG 
       BZ_SYM_table(ik_near,BZ_DbGd_nkpt(ik_near))=TMP_map2(ik,2) ! symm@CG
     endif
     !GPL_EXCLUDE_END
   enddo
   !
 enddo
 !
 YAMBO_FREE(TMP_map2)
 !
 ! Adding the built-in grid and filling/reordering
 !=================================================
 !
 ! +1 is for the build-it grid
 !
 BZ_DbGd_nkpt=BZ_DbGd_nkpt+1
 !
 BZ_DbGd_max_filling=maxval(BZ_DbGd_nkpt)
 call msg('s',':: Blocks filling range  :',(/minval(BZ_DbGd_nkpt),BZ_DbGd_max_filling/))
 !
 ! Dimensions & allocations
 !
 if (DbGd_is_symmetry_expanded) then
   !
   BZ_DbGd_tot_nk       =DbGd_k(i_db_merge)%nbz+Xk%nbz
   BZ_DbGd_tot_nk_levels=DbGd_k(i_db_merge)%nibz+Xk%nibz
   !
 else
   !
   BZ_DbGd_tot_nk       =DbGd_k(i_db_merge)%nibz+Xk%nbz
   BZ_DbGd_tot_nk_levels=DbGd_k(i_db_merge)%nibz+Xk%nbz
   !
 endif
 !
 YAMBO_ALLOC(Xen%E_DbGd,(BZ_DbGd_nbands,BZ_DbGd_tot_nk_levels,n_sp_pol))
 YAMBO_ALLOC(BZ_DbGd_table,(Xk%nbz,BZ_DbGd_max_filling,2))
 YAMBO_ALLOC(BZ_DbGd_kpts,(BZ_DbGd_tot_nk,3))
 YAMBO_ALLOC(BZ_DbGd_weight,(BZ_DbGd_max_filling))
 BZ_DbGd_weight = 1.0_SP
 BZ_DbGd_table  = 0
 !
 !GPL_EXCLUDE_START
 if (.not.DbGd_is_symmetry_expanded) then
   !
   call live_timing('Built-in grid + re-ordering',Xk%nbz)
   !                ============================
   ik=0
   do ik_bz=1,Xk%nbz
     do ik_DbGd=1,BZ_DbGd_nkpt(ik_bz)
       ik=ik+1
       if (ik_DbGd==1) then
         Xen%E_DbGd(:,ik,:)=Xen%E(:BZ_DbGd_nbands,Xk%sstar(ik_bz,1),:)+Xen%E_fermi
         BZ_DbGd_kpts(ik,:)=Xk%ptbz(ik_bz,:)
       else
         is     =BZ_SYM_table(ik_bz,ik_DbGd-1)
         ik_grid=TMP_map3(ik_bz,ik_DbGd-1,1)
         Xen%E_DbGd(:,ik,:)=DbGd_E(i_db_merge)%E(:,ik_grid,:)
         BZ_DbGd_kpts(ik,:)=matmul(rl_sop(:,:, is),DbGd_k(i_db_merge)%pt(ik_grid,:) )
       endif
       BZ_DbGd_table(ik_bz,ik_DbGd,1)=ik_DbGd+sum(BZ_DbGd_nkpt(:ik_bz-1))
     enddo
     call live_timing(steps=1)
   enddo
   !
   call live_timing()
   !
 endif
 !GPL_EXCLUDE_END
 !
 if (DbGd_is_symmetry_expanded) then
   !
   call live_timing('Built-in grid',Xk%nbz)
   !
   Xen%E_DbGd(:,:DbGd_k(i_db_merge)%nibz,:)=DbGd_E(i_db_merge)%E(:,:,:)
   BZ_DbGd_kpts(:DbGd_k(i_db_merge)%nbz,:) =DbGd_k(i_db_merge)%ptbz(:,:)
   !
   ik_grid=DbGd_k(i_db_merge)%nbz
   iE_grid=DbGd_k(i_db_merge)%nibz
   do ik_bz=1,Xk%nbz
      !
      ik_grid=ik_grid+1
      if (Xk%sstar(ik_bz,2)==1) iE_grid=iE_grid+1
      !
      BZ_DbGd_table(ik_bz,:BZ_DbGd_nkpt(ik_bz)-1,:)=TMP_map3(ik_bz,:BZ_DbGd_nkpt(ik_bz)-1,:)
      BZ_DbGd_table(ik_bz, BZ_DbGd_nkpt(ik_bz)  ,:)=(/ iE_grid, ik_grid/)
      !
      BZ_DbGd_kpts(ik_grid,:)=Xk%ptbz(ik_bz,:)
      Xen%E_DbGd(:,iE_grid,:)=Xen%E(:BZ_DbGd_nbands,Xk%sstar(ik_bz,1),:)+Xen%E_fermi
      !
      call live_timing(steps=1)
      !
   enddo
   !
   call live_timing( )
   !
 endif
 !
 ! Final I/O
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1/),COM=LOG,ID=ID)
 i_err=io_Double_Grid(Xen,Xk,ID)
 !
 ! CLEAN
 !
 YAMBO_FREE(TMP_map3)
 YAMBO_FREE(BZ_DbGd_nkpt)
 YAMBO_FREE(BZ_DbGd_table)
 YAMBO_FREE(Xen%E_DbGd)
 YAMBO_FREE(BZ_DbGd_weight)
 YAMBO_FREE(BZ_SYM_table)
 YAMBO_FREE(BZ_DbGd_kpts)
 call k_ibz2bz(Xk,'d',.false.) 
 call k_ibz2bz(DbGd_k(i_db_merge),'d',.false.) 
 !
end subroutine
