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
 use R_lattice,           ONLY:bz_samp,nkibz,k_the_nearest,bz_samp_reset,rl_sop
 use parser_m,            ONLY:parser
 use D_lattice,           ONLY:alat,nsym,input_GS_Tel,a,dl_sop,i_time_rev
 use com,                 ONLY:msg,error,warning
 use YPP,                 ONLY:BZ_DbGd_nkpts_to_use,DbGd_DB1_paths,MAX_DbGd_DBs,n_DbGd_DBs
 use electrons,           ONLY:levels,default_nel,n_bands,BZ_ASSOCIATED_nkpts,&
&                              BZ_DbGd_nbands,BZ_DbGd_nkpt,BZ_DbGd_table,BZ_DbGd_kpts,&
&                              BZ_DbGd_tot_nkpts,BZ_DbGd_nkpts,n_sp_pol,BZ_DbGd_max_filling,&
&                              BZ_DbGd_weight,E_reset,DbGd_is_symmetry_expanded
 use parallel_m,          ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,          ONLY:PARALLEL_index
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
 real(SP)     ::rand_k(BZ_DbGd_nkpts_to_use+1,3),a_save(3,3),alat_save(3),&
&               gap(2),tmp_zero(3)
 integer      ::i1,ik_near,ik_near_ibz,is_near,ik_bz,ik_DbGd
 logical      ::l_skip_check,l_flag
 integer,  allocatable :: BZ_SYM_table(:,:)
 real(SP), allocatable :: rand_E(:,:,:)
 !
 ! BZ DB1 I/O
 !
 type(levels)         :: DbGd_E(MAX_DbGd_DBs)
 type(bz_samp)        :: DbGd_k(MAX_DbGd_DBs)
 integer              :: ID,i_err(MAX_DbGd_DBs),nsym_save,nsym_DbGd_DB,i_time_rev_save,&
&                        i_time_rev_DB,i_db,i_db_merge,ik_ref(2)
 real(SP)             :: dl_sop_save(3,3,48)
 integer, allocatable :: TMP_map(:,:)
 type(PP_indexes)     :: px
 !
 ! Functions
 !
 integer, external :: k_DbGd_grid_consistency_check
 integer, external :: io_DB1_selective_scan,io_Double_Grid
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
     i_err(i_db)=io_DB1_selective_scan(DbGd_E(i_db),DbGd_k(i_db),trim(DbGd_DB1_paths(i_db,1)),'all')
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
 !
 DbGd_is_symmetry_expanded=.TRUE.
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
     if (i_db==1) ik_ref(1)=0
     if (i_db> 1) ik_ref(1)=sum(DbGd_E(1:i_db-1)%nk)
     do i1=1,DbGd_E(i_db)%nk
       DbGd_E(i_db_merge)%E(:,i1+ik_ref(1),:)= DbGd_E(i_db)%E(:,i1,:)
       DbGd_k(i_db_merge)%pt(i1+ik_ref(1),:)= DbGd_k(i_db)%pt(i1,:)
     enddo
   enddo
 endif
 !
 ! Use of less (only BZ_DbGd_nkpts, read from input) k-points
 !
 if (BZ_DbGd_nkpts_to_use<nkibz.and.BZ_DbGd_nkpts_to_use>0) then
   nkibz   =BZ_DbGd_nkpts_to_use
   DbGd_E(i_db_merge)%nk=nkibz
   DbGd_k(i_db_merge)%nibz=nkibz
   YAMBO_ALLOC(rand_E,(DbGd_E(i_db_merge)%nb,nkibz,n_sp_pol))
   rand_E(:,:,:)=DbGd_E(i_db_merge)%E(:,:nkibz,:)
   rand_k(:nkibz,:)=DbGd_k(i_db_merge)%pt(:nkibz,:)
   YAMBO_FREE(DbGd_E(i_db_merge)%E)
   YAMBO_FREE(DbGd_k(i_db_merge)%pt)
   YAMBO_ALLOC(DbGd_E(i_db_merge)%E,(DbGd_E(i_db_merge)%nb,nkibz,n_sp_pol))
   YAMBO_ALLOC(DbGd_k(i_db_merge)%pt,(nkibz,3))
   DbGd_E(i_db_merge)%E(:,:,:)=rand_E(:,:,:)
   DbGd_k(i_db_merge)%pt(:,:)=rand_k(:,:)
   YAMBO_FREE(rand_E)
 endif
 !
 gap(1)=minval(DbGd_E(i_db_merge)%E(Xen%nbf+1,:,1))-maxval(DbGd_E(i_db_merge)%E(Xen%nbf,:,1)) 
 gap(2)=10000._SP
 do i1=1,nkibz
   if (DbGd_E(i_db_merge)%E(Xen%nbf+1,i1,1)-DbGd_E(i_db_merge)%E(Xen%nbf,i1,1)<gap(2)) then
     gap(2)=DbGd_E(i_db_merge)%E(Xen%nbf+1,i1,1)-DbGd_E(i_db_merge)%E(Xen%nbf,i1,1)
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
 ! See if there is correspondance between the random and the built-in
 ! k-points
 !
 call section('+','Internal consistency check ...')
 ik_ref(2)=k_DbGd_grid_consistency_check(Xk,Xen,DbGd_k(i_db_merge),DbGd_E(i_db_merge))
 !
 if (ik_ref(2)>0) then
   call msg('l','passed @ Double Grid k-point #'//trim(intc(ik_ref(2)))) 
 else 
   call msg('l','failed') 
   if (l_skip_check) then
     call warning(" Internal check failed. A consistent X grid k-point was not found among the Double Grid list")
   else
     call error(" Internal check failed. A consistent X grid k-point was not found among the Double Grid list")
   endif
 endif
 !
 BZ_DbGd_nbands=DbGd_E(i_db_merge)%nb
 !
 BZ_ASSOCIATED_nkpts=Xk%nbz
 !
 if (DbGd_is_symmetry_expanded) then
   !
   call section('+','BZ Double Grid K-points expansion ...')
   !
   ! DbGd_k (IBZ->BZ)
   !
   call k_ibz2bz(DbGd_k(i_db_merge),'i',.true.)
   !
   call msg('l','found '//trim(intc(DbGd_k(i_db_merge)%nbz))//' ('//trim(intc(DbGd_k(i_db_merge)%nibz))//') points in the BZ (IBZ)')
   !
   BZ_DbGd_nkpts    =DbGd_k(i_db_merge)%nbz
   BZ_DbGd_tot_nkpts=DbGd_k(i_db_merge)%nibz
   !
 endif
 !
 !
 ! Xk (IBZ->BZ)
 !
 call k_build_up_BZ_tables(Xk)
 call k_ibz2bz(Xk,'i',.true.)
 !
 !
 YAMBO_ALLOC(BZ_DbGd_nkpt,(Xk%nbz))
 YAMBO_ALLOC(TMP_map,(BZ_DbGd_nkpts,2))
 BZ_DbGd_nkpt=1
 TMP_map=0
 !
 ! Par Proc
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/BZ_DbGd_nkpts/))
 call PP_redux_wait
 !
 call live_timing('BZ Double Grid Map',px%n_of_elements(myid+1))
 !                ============
 do i1=1,BZ_DbGd_nkpts
   !
   if (.not.px%element_1D(i1)) cycle
   !
   if (i1==ik_ref(2)) then
     call live_timing(steps=1)
     cycle
   endif
   !
   if (DbGd_is_symmetry_expanded) then
     ik_near=k_the_nearest(DbGd_k(i_db_merge)%ptbz(i1,:),Xk%ptbz(:,:),Xk%nbz)
     TMP_map(i1,1)=ik_near
   endif
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call PP_redux_wait(TMP_map)
 !
 ! Number of points in each zone
 !
 BZ_DbGd_nkpt=1
 do i1=1,BZ_DbGd_nkpts
   if (i1==ik_ref(2)) cycle
   ik_near=TMP_map(i1,1)
   if (.not.DbGd_is_symmetry_expanded) then
     ik_near_ibz = TMP_map(i1,1)
     ik_near     = Xk%k_table(ik_near_ibz,1)
   endif
   BZ_DbGd_nkpt(ik_near)=BZ_DbGd_nkpt(ik_near)+1
 enddo
 !
 ! Tables
 !
 YAMBO_ALLOC(BZ_DbGd_table,(Xk%nbz,maxval(BZ_DbGd_nkpt),2))
 BZ_DbGd_table=0
 do i1=1,Xk%nbz
   BZ_DbGd_table(i1,1,1)=Xk%sstar(i1,1)
   BZ_DbGd_table(i1,1,2)=i1
 enddo
 if (.not.DbGd_is_symmetry_expanded) then
   YAMBO_ALLOC(BZ_SYM_table,(Xk%nbz,maxval(BZ_DbGd_nkpt)))
   BZ_SYM_table=0
   BZ_SYM_table(1:Xk%nbz,1)=1
 endif
 !
 BZ_DbGd_nkpt=1
 do i1=1,BZ_DbGd_nkpts
   if (i1==ik_ref(2)) cycle
   ik_near=TMP_map(i1,1)
   if (.not.DbGd_is_symmetry_expanded) then
     ik_near_ibz = TMP_map(i1,1)
     ik_near     = Xk%k_table(ik_near_ibz,1)
   endif
   BZ_DbGd_nkpt(ik_near)=BZ_DbGd_nkpt(ik_near)+1
   !
   if (DbGd_is_symmetry_expanded) then
     BZ_DbGd_table(ik_near,BZ_DbGd_nkpt(ik_near),1)=DbGd_k(i_db_merge)%sstar(i1,1)
     BZ_DbGd_table(ik_near,BZ_DbGd_nkpt(ik_near),2)=i1
   endif
   !
 enddo
 !
 YAMBO_FREE(TMP_map)
 !
 BZ_DbGd_max_filling=maxval(BZ_DbGd_nkpt)
 call msg('s',':: Blocks filling range  :',(/minval(BZ_DbGd_nkpt),BZ_DbGd_max_filling/))
 !
 ! DbGd double grid interpolation weights, not used here, but must be defined.
 !
 YAMBO_ALLOC(BZ_DbGd_weight,(BZ_DbGd_max_filling))
 BZ_DbGd_weight = 1.0_SP
 !
 YAMBO_ALLOC(Xen%E_DbGd,(BZ_DbGd_nbands,BZ_DbGd_tot_nkpts,n_sp_pol))
 !
 if (DbGd_is_symmetry_expanded) then
   !
   YAMBO_ALLOC(BZ_DbGd_kpts,(BZ_DbGd_nkpts,3))
   BZ_DbGd_kpts=0._SP
   !
   Xen%E_DbGd(:,:,:)=DbGd_E(i_db_merge)%E(:,:,:)
   BZ_DbGd_kpts(:,:)=DbGd_k(i_db_merge)%ptbz(:,:)
   !
 endif 
 !
 !
 ! Final I/O
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1/),COM=LOG,ID=ID)
 i_err=io_Double_Grid(Xen,Xk,ID)
 !
 ! CLEAN
 !
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
