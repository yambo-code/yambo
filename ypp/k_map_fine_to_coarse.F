!
!        Copyright (C) 2000-2014 the YAMBO team
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
 use pars,                ONLY:SP,DP,lchlen,schlen
 use units,               ONLY:HA2EV
 use zeros,               ONLY:k_iku_zero,k_rlu_zero,define_zeros
 use stderr,              ONLY:set_real_printed_length,intc
 use R_lattice,           ONLY:bz_samp,nkibz,k_the_nearest,bz_samp_reset,rl_sop
 use parser_m,            ONLY:parser
 use D_lattice,           ONLY:alat,nsym,input_GS_Tel,a,dl_sop,i_time_rev
 use com,                 ONLY:msg,of_open_close,error,warning
 use vec_operate,         ONLY:v_norm,c2a,k2bz,v_is_zero
 use YPP,                 ONLY:BZ_RIM_nkpts,RIM_DB1_paths,K_transform,MAX_RIM_DBs,n_RIM_DBs
 use electrons,           ONLY:levels,default_nel,n_bands,&
&                              BZ_RIM_nbands,BZ_RIM_nkpt,BZ_RIM_table,&
&                              BZ_RIM_tot_nkpts,n_sp_pol,BZ_RIM_max_filling,&
&                              BZ_RIM_ipol_weight,E_reset,RIM_is_symmetry_expanded
 use LIVE_t,              ONLY:live_timing
 use IO_m,                ONLY:io_control,LOG,OP_WR_CL
 !
 implicit none
 !
 type(bz_samp) :: Xk
 type(levels)  :: Xen
 !
 ! Work Space
 !
 real(SP)     ::rand_k(BZ_RIM_nkpts+1,3),a_save(3,3),alat_save(3),&
&               gap(2),tmp_zero(3)
 integer      ::i1,ik_near,is_near,i2,RIM_nk,ik_bz,ik_RIM
 logical           :: No_Weight,l_skip_check
 integer,  allocatable :: BZ_SYM_table(:,:)
 real(SP), allocatable :: rand_E(:,:,:)
 !
 ! BZ DB1 I/O
 !
 type(levels)      :: RIM_E(MAX_RIM_DBs)
 type(bz_samp)     :: RIM_k(MAX_RIM_DBs)
 integer           :: ID
 integer           :: i_err(MAX_RIM_DBs),nsym_save,nsym_RIM_DB
 integer           :: i_time_rev_save,i_time_rev_DB
 integer           :: i_db,i_db_merge,ik_ref(2)
 integer, external :: io_DB1_selective_scan,io_RIM_E_and_k
 real(SP)          :: dl_sop_save(3,3,48)
 !
 !
 !
 if (n_RIM_DBs==0) then
   call msg("s",'RIM: no database secified. Nothing to do.')
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
 if (n_RIM_DBs>0) then
   do i_db=1,n_RIM_DBs
     i_err(i_db)=io_DB1_selective_scan(RIM_E(i_db),RIM_k(i_db),trim(RIM_DB1_paths(i_db,1)),'all')
     if (i_err(i_db)/=0) call msg("s",'RIM database in '//trim(RIM_DB1_paths(i_db,1))//' NOT FOUND')
     if (i_err(i_db)==0) call msg("s",'RIM database in'//trim(RIM_DB1_paths(i_db,1))//' FOUND')
     !
     ! DB1 checks
     !
     if (i_err(i_db)==0.and.any(a/=a_save))  then
       call msg("s",':: RIM DB1 DL structure does not fit')
       i_err(i_db)=-1
     endif
     if (i_err(i_db)==0.and.any(alat/=alat_save))  then
       call msg("s",':: RIM DB1 alat does not fit.')
       i_err(i_db)=-1
     endif
     !
   enddo
   !
   if (any((/i_err(:n_RIM_DBs)/=0/))) call error(" One or more RIM databases have not been found")
   !
 endif
 !
 ! Now that I have the finer grid of Kpts I redefine the zeros for the kpts
 !
 do i_db=1,n_RIM_DBs
   if(.not. trim(RIM_DB1_paths(i_db,1))=="." ) then
     call define_zeros(vector_=RIM_k(i_db)%pt, zero_=tmp_zero)
     k_iku_zero=min(k_iku_zero,tmp_zero)
     call define_zeros(vector_=RIM_k(i_db)%pt, zero_=tmp_zero,RLU=.TRUE.)
     k_rlu_zero=min(k_rlu_zero,tmp_zero)
   endif
 enddo
 !
 ! Symmetry restoring
 !
 if (allocated(dl_sop)) then
   deallocate(dl_sop)
   nsym_RIM_DB=nsym
   nsym       =nsym_save
   i_time_rev_DB=i_time_rev
   i_time_rev   =i_time_rev_save
   allocate(dl_sop(3,3,nsym))
   dl_sop=dl_sop_SAVE(:,:,:nsym)
 endif
 !
 call parser('NoWeights',No_Weight)
 call parser('SkipCheck',l_skip_check)
 call parser('BZExpand',RIM_is_symmetry_expanded)
 !
 call section('*','BZ RIM DB(s)')
 !
 call msg('s',':: Electrons             :',default_nel)
 call msg('s',':: Temperature       [ev]:',input_GS_Tel)
 call msg('s',':: Lattice factors [a.u.]:',alat)
 do i_db=1,n_RIM_DBs
   if (i_db==1) call msg('s',':: K points        [DB #'//trim(intc(i_db))//']:',RIM_E(i_db)%nk)
   if (i_db> 1) call msg('s','::                 [DB #'//trim(intc(i_db))//']:',RIM_E(i_db)%nk)
 enddo
 !
 ! Mergering of ALL RIM_E(:) => RIM_E(n_RIM_DBs+1)
 !
 i_db_merge=1
 !
 if (n_RIM_DBs>1) then
   i_db_merge=n_RIM_DBs+1
   call bz_samp_reset(RIM_k(i_db_merge))
   call E_reset(RIM_E(i_db_merge))
   RIM_E(i_db_merge)%nk=sum(RIM_E(:n_RIM_DBs)%nk)
   RIM_E(i_db_merge)%nb=RIM_E(1)%nb
   RIM_k(i_db_merge)%nibz=sum(RIM_E(:n_RIM_DBs)%nk)
   allocate(RIM_E(i_db_merge)%E(RIM_E(i_db_merge)%nb,RIM_E(i_db_merge)%nk,n_sp_pol))
   allocate(RIM_k(i_db_merge)%pt(RIM_E(i_db_merge)%nk,3))
   do i_db=1,n_RIM_DBs
     if (i_db==1) ik_ref(1)=0
     if (i_db> 1) ik_ref(1)=sum(RIM_E(1:i_db-1)%nk)
     do i1=1,RIM_E(i_db)%nk
       RIM_E(i_db_merge)%E(:,i1+ik_ref(1),:)= RIM_E(i_db)%E(:,i1,:)
       RIM_k(i_db_merge)%pt(i1+ik_ref(1),:)= RIM_k(i_db)%pt(i1,:)
     enddo
   enddo
 endif
 !
 ! Use of less (only BZ_RIM_nkpts, read from input) k-points
 !
 if (BZ_RIM_nkpts<nkibz.and.BZ_RIM_nkpts>0) then
   nkibz   =BZ_RIM_nkpts
   RIM_E(i_db_merge)%nk=nkibz
   RIM_k(i_db_merge)%nibz=nkibz
   allocate(rand_E(RIM_E(i_db_merge)%nb,nkibz,n_sp_pol))
   rand_E(:,:,:)=RIM_E(i_db_merge)%E(:,:nkibz,:)
   rand_k(:nkibz,:)=RIM_k(i_db_merge)%pt(:nkibz,:)
   deallocate(RIM_E(i_db_merge)%E,RIM_k(i_db_merge)%pt)
   allocate(RIM_E(i_db_merge)%E(RIM_E(i_db_merge)%nb,nkibz,n_sp_pol),RIM_k(i_db_merge)%pt(nkibz,3))
   RIM_E(i_db_merge)%E(:,:,:)=rand_E(:,:,:)
   RIM_k(i_db_merge)%pt(:,:)=rand_k(:,:)
   deallocate(rand_E)
 endif
 !
 gap(1)=minval(RIM_E(i_db_merge)%E(Xen%nbf+1,:,1))-maxval(RIM_E(i_db_merge)%E(Xen%nbf,:,1)) 
 gap(2)=10000.
 do i1=1,nkibz
   if (RIM_E(i_db_merge)%E(Xen%nbf+1,i1,1)-RIM_E(i_db_merge)%E(Xen%nbf,i1,1)<gap(2)) then
     gap(2)=RIM_E(i_db_merge)%E(Xen%nbf+1,i1,1)-RIM_E(i_db_merge)%E(Xen%nbf,i1,1)
   endif
 enddo
 !
 call msg('s','::              [imposed]:',BZ_RIM_nkpts)
 call msg('s',':: Bands                 :',n_bands)
 call msg('s',':: Symmetries            :',nsym_RIM_DB)
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
 ik_ref=0
 i1_loop: do i1=1,Xen%nk
   do i2=1,RIM_E(i_db_merge)%nk
     if (v_is_zero(RIM_k(i_db_merge)%pt(i2,:)-Xk%pt(i1,:))) then
       ik_ref=(/i1,i2/)
       exit  i1_loop
     endif
   enddo
 enddo i1_loop
 !
 if (ik_ref(1)> 0) then
   call msg('l','passed') 
   RIM_E(i_db_merge)%E(:,:,:)=RIM_E(i_db_merge)%E(:,:,:)+Xen%E(Xen%nbm,ik_ref(1),1)-RIM_E(i_db_merge)%E(Xen%nbm,ik_ref(2),1)
 else 
   call msg('l','failed') 
   if (     l_skip_check) call warning("Internal check failed. A X grid k-point was not found among the RIM list")
   if (.not.l_skip_check) call error("Internal check failed. A X grid k-point was not found among the RIM list")
 endif
 !
 BZ_RIM_nbands=RIM_E(i_db_merge)%nb
 !
 RIM_nk=RIM_k(i_db_merge)%nibz
 BZ_RIM_tot_nkpts=RIM_k(i_db_merge)%nibz+Xk%nbz-1
 !
 if (RIM_is_symmetry_expanded) then
   !
   call section('+','BZ RIM K-points expansion ...')
   !
   ! RIM_k (IBZ->BZ)
   !
   call k_ibz2bz(RIM_k(i_db_merge),'i',.true.)
   !
   call msg('l','found '//trim(intc(RIM_k(i_db_merge)%nbz))//' ('//trim(intc(RIM_k(i_db_merge)%nibz))//') points in the BZ (IBZ)')
   !
   RIM_nk=RIM_k(i_db_merge)%nbz
   !
   BZ_RIM_tot_nkpts=RIM_k(i_db_merge)%nibz
   !
 endif
 !
 ! Xk (IBZ->BZ)
 !
 call k_build_up_BZ_tables(Xk)
 call k_ibz2bz(Xk,'i',.true.)
 !
 ! The -1 is for the reference k that should not be added
 !
 allocate(BZ_RIM_nkpt(Xk%nbz))
 BZ_RIM_nkpt=1
 !
 call live_timing('BZ RIM Tables',RIM_nk*2,SERIAL=.true.)
 !                ===============
 do i2=1,2
   if (i2==2) then
     !
     allocate(BZ_RIM_table(Xk%nbz,maxval(BZ_RIM_nkpt)))
     BZ_RIM_table=0
     do i1=1,Xk%nbz
       BZ_RIM_table(i1,1)=Xk%sstar(i1,1)
     enddo
     !
     if (.not.RIM_is_symmetry_expanded) then
       allocate(BZ_SYM_table(Xk%nbz,maxval(BZ_RIM_nkpt)))
       BZ_SYM_table=0
       BZ_SYM_table(1:Xk%nbz,1)=1
     endif
     !
   endif
   !
   BZ_RIM_nkpt=1
   !
   do i1=1,RIM_nk
     !
     if (i1==ik_ref(2)) then
       call live_timing(steps=1)
       cycle
     endif
     !
     if (RIM_is_symmetry_expanded) then
       ik_near=k_the_nearest(RIM_k(i_db_merge)%ptbz(i1,:),Xk%ptbz(:,:),Xk%nbz)
       BZ_RIM_nkpt(ik_near)=BZ_RIM_nkpt(ik_near)+1
       if (allocated(BZ_RIM_table)) BZ_RIM_table(ik_near,BZ_RIM_nkpt(ik_near))=RIM_k(i_db_merge)%sstar(i1,1)
     else
       ik_near =Xk%s_table( k_the_nearest(RIM_k(i_db_merge)%pt(i1,:),Xk%pt(:,:),Xk%nibz,ROTATE_k_ref=.TRUE.,k_symm=is_near) , 1)
       BZ_RIM_nkpt(ik_near)=BZ_RIM_nkpt(ik_near)+1
       if (allocated(BZ_SYM_table)) BZ_SYM_table(ik_near,BZ_RIM_nkpt(ik_near))=is_near
       if (allocated(BZ_RIM_table)) BZ_RIM_table(ik_near,BZ_RIM_nkpt(ik_near))=i1+1
     endif
     !
     call live_timing(steps=1)
     !
   enddo
 enddo
 call live_timing()
 !
 BZ_RIM_max_filling=maxval(BZ_RIM_nkpt)
 call msg('s',':: Blocks filling range  :',(/minval(BZ_RIM_nkpt),BZ_RIM_max_filling/))
 !
 ! RIM double grid interpolation weights, not used here, but must be defined.
 !
 allocate(BZ_RIM_ipol_weight(BZ_RIM_max_filling))
 BZ_RIM_ipol_weight = 1.0_SP
 !
 allocate(Xen%E_RIM(BZ_RIM_nbands,BZ_RIM_tot_nkpts,n_sp_pol))
 !
 if (RIM_is_symmetry_expanded) then
   !
   allocate(Xk%pt_RIM(BZ_RIM_tot_nkpts,3))
   !
   Xen%E_RIM(:,:,:)=RIM_E(i_db_merge)%E(:,:,:)
   Xk%pt_RIM(:,:)  =RIM_k(i_db_merge)%pt(:,:)
   !
 else
   !
   ! Presently all the points in the RIM DB are associated to points in
   ! the IBZ of the small grid looking for the nearest of the rotated.
   ! Thus the kpts in Xk%ptbz_RIM contains all the Xk%ptbz plus
   ! the associated points in the RIM. Thus if Xk%ptbz is in the IBZ
   ! it has a cloud of points (Xk%ptbz_RIM) otherwise it has only
   ! no points associated
   ! One should rotate the cloud from the ibz and associate it to the
   ! rotated point out of the ibz. I started the implementation in the
   ! k_random.F_project which is now inside my repository
   ! 
   allocate(Xk%ptbz_RIM(BZ_RIM_tot_nkpts,3))
   !
   call live_timing('RIM re-ordering and distribution',Xk%nbz,SERIAL=.true.)
   !                ===============
   i1=0
   do ik_bz=1,Xk%nbz
     do ik_RIM=1,BZ_RIM_nkpt(ik_bz)
       i1=i1+1
       if (ik_RIM==1) then
         Xen%E_RIM(:,i1,:)=Xen%E(:BZ_RIM_nbands,BZ_RIM_table(ik_bz,1),:)
         Xk%ptbz_RIM(i1,:)=Xk%ptbz(ik_bz,:)
       else
         Xen%E_RIM(:,i1,:)=RIM_E(i_db_merge)%E(:,BZ_RIM_table(ik_bz,ik_RIM)-1,:)
         Xk%ptbz_RIM(i1,:)=matmul(rl_sop(:,:, BZ_SYM_table(ik_bz,ik_RIM) ),RIM_k(i_db_merge)%pt(BZ_RIM_table(ik_bz,ik_RIM)-1,:))
       endif
       BZ_RIM_table(ik_bz,ik_RIM)=ik_RIM+sum(BZ_RIM_nkpt(:ik_bz-1))
     enddo
     call live_timing(steps=1)
   enddo
   !
   call live_timing()
   !
 endif
 !
 ! Final I/O
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1/),COM=LOG,ID=ID)
 i_err=io_RIM_E_and_k(Xen,Xk,ID)
 !
 ! CLEAN
 !
 deallocate(BZ_RIM_nkpt,BZ_RIM_table,Xen%E_RIM,BZ_RIM_ipol_weight)
 if(allocated(BZ_SYM_table)) deallocate(BZ_SYM_table)
 if(associated(Xk%ptbz_RIM)) deallocate(Xk%ptbz_RIM)
 if(associated(Xk%pt_RIM))   deallocate(Xk%pt_RIM)
 call k_ibz2bz(Xk,'d',.false.) 
 call k_ibz2bz(RIM_k(i_db_merge),'d',.false.) 
 !
 !
end subroutine
