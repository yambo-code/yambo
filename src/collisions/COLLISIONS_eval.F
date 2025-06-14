!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine COLLISIONS_eval(E,X,k,xk,q,Xw)
 !
 ! The EOM looks like:
 !
 ! i\partial_T G^<+[h+\Sigma_s,G^<]=I^<
 !
 ! here I evaluate the components of both \Sigma_s and I^<.
 !
 ! While for \Sigma_s the POTENTIAL can be specified, for I^< only the KIND is possible to define.
 !
 ! So we have, at the command line,
 !
 ! yambo_rt -e -s e/p/h/a  -> I^<
 ! yambo_rt -e -v (hf),(c)ohsex  -> \Sigma_s
 !
 ! In the \Sigma_s case also yambo_sc can evaluate
 !
 use drivers,        ONLY:l_elel_scatt,l_use_collisions,l_use_Hxc_collisions,&
&                         l_use_COH_collisions,l_elphoton_scatt,l_real_time,l_sc_run
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:i_time_rev
 use wave_func,      ONLY:WF
 use hamiltonian,    ONLY:H_potential,H_kind
 use global_XC,      ONLY:QP_SE_GW_NEQ,QP_SE_GW_QED
 use collision_ext,  ONLY:COLLISIONS_parallel_SC_structure,COLLISIONS_group,COLLISIONS_from_BSE,&
&                        COH_collisions,HXC_collisions,P_collisions,GW_NEQ_collisions,COLLISIONS_naming,&
&                        COLLISIONS_FEW_only,COLL_hole_bands,COLL_elec_bands
 use X_m,            ONLY:X_t
 use plasma,         ONLY:EH_gas
 use frequency,      ONLY:w_samp
 use BS,             ONLY:BS_kpt_bz,BS_kpt_ibz
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:OP_RD_CL,OP_RD,RD_CL,RD_CL_IF_END,VERIFY,REP,NONE,DUMP,&
&                         io_COLLs,manage_action,netcdf_dim_size
 use parallel_m,     ONLY:PAR_IND_G_k,PAR_IND_Bp_mat,PAR_IND_Plasma
 use parallel_int,   ONLY:PP_wait,PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use interfaces,     ONLY:WF_free
 use timing_m,       ONLY:timing
 !
 implicit none
 !
 type(levels)  :: E
 type(w_samp)  :: Xw(2) 
 type(bz_samp) :: k,Xk,q
 type(X_t)     :: X(4)
 !
 ! WorkSpace
 !
 logical       :: l_collisions_from_bse,l_tmp
 integer       :: ID,IO_err,IO_err_BS,IO_ACT,ID_head,ID_mat,ID_cmpr,&
 &                BS_nk_bz,BS_nk_ibz
 character(20) :: PAR_ch,FREE_ch
 !
 integer, external :: io_BS_header,io_BS_PAR_init
 !
 call timing('COLLISIONS_eval',OPR='start')
 !
 call section('*','Extended collisions')
 !=======================================
 !
 ! If I am calculating the collisions then I can use them
 !
 l_use_collisions=.TRUE.
 !
 COLLISIONS_FEW_only=all((/COLL_hole_bands/=-1,COLL_elec_bands/=-1/))
 !
 ! XC Potentials 
 !===============
 !
 call XC_potentials(H_potential)
 !
 ! Check for the Time Reversal Symmetry
 !
 if ( (l_use_Hxc_collisions.or.l_elel_scatt) .and. (i_time_rev==1) ) then
   call error(" COLLISIONS not compatible with Time Reversal Symmetry! ")
 endif
 !
 ! Naming 
 !========
 call COLLISIONS_naming( H_kind , "HXC" )
 !
 ! Initial Setup
 !===============
 if (l_use_COH_collisions) call COLLISIONS_basic_operations( "alloc",    COH_collisions )
 if (l_use_Hxc_collisions) call COLLISIONS_basic_operations( "alloc",    HXC_collisions )
 !
 if (l_elphoton_scatt)     call COLLISIONS_basic_operations( "alloc",      P_collisions )
 if (l_elel_scatt)         call COLLISIONS_basic_operations( "alloc", GW_NEQ_collisions )
 !
 ! PARALLEL Setup
 !================
 if (l_use_Hxc_collisions.or.l_use_COH_collisions.or.l_elphoton_scatt) then
   !
   COLLISIONS_parallel_SC_structure=.TRUE.
   PAR_ch="Self_Energy"
#if defined _RT
   if (.not.l_elphoton_scatt) then
     COLLISIONS_parallel_SC_structure=.FALSE.
     PAR_ch="Real_Time"
   endif
#endif
   !
   ! PARALLEL indexes ...
   call PARALLEL_global_indexes(E,k,q,trim(PAR_ch))
   !
   ! PARALLEL WF distribution ...
   ! ... k-q + final band index...
   call PARALLEL_WF_distribute(K_index=PAR_IND_G_k,Bm_index=PAR_IND_Bp_mat,CLEAN_UP=.TRUE.)
   ! ... COLLISION initial state...
   if (l_use_COH_collisions) then
     call COLLISIONS_basic_operations( "WF_distribute",    COH_collisions )
   else if (l_use_Hxc_collisions) then
     call COLLISIONS_basic_operations( "WF_distribute",    HXC_collisions )
   endif
   call PARALLEL_WF_index( )
   !
 endif
 !
 if (l_use_Hxc_collisions.or.l_use_COH_collisions) then
   !
   call section('+',trim(H_potential)//' (e-e correlation)')
   !=========================================================
   !
   if(l_use_Hxc_collisions) call CHECK_and_ALLOC(HXC_collisions)
   if(l_use_COH_collisions) call CHECK_and_ALLOC(COH_collisions)
   !
   if (.not.COLLISIONS_from_BSE) IO_err_BS=-1
   !
   if (l_use_Hxc_collisions.and.COLLISIONS_from_BSE) then
     !
     ! Check if I can use the BSE kernel for the collisions
     !
     call io_control(ACTION=OP_RD,COM=NONE,MODE=DUMP,ID=ID_head)
     IO_err_BS=io_BS_header(1,X(2),ID_head,"connect")
     BS_nk_bz =netcdf_dim_size(ID_head,"n_kpt_bz")
     BS_nk_ibz=netcdf_dim_size(ID_head,"n_kpt_ibz")
     allocate(BS_kpt_bz(3,BS_nk_bz))
     allocate(BS_kpt_ibz(3,BS_nk_ibz))
     call io_control(ACTION=RD_CL,COM=NONE,MODE=DUMP,ID=ID_head)
     IO_err_BS=io_BS_header(1,X(2),ID_head,"full")
     !
     if (IO_err_BS==0) then 
       call set_BS_blks(Xk)
       call io_control(ACTION=OP_RD,COM=REP,MODE=DUMP,ID=ID_mat)
       IO_err_BS=io_BS_PAR_init(1,ID_mat,"full")
       call build_inverse_BS_eh_table(Xk,ID_mat)!,BS_nk_bz)
       call check_BS_parameters_consistency(Xk,BS_nk_bz,BS_nk_ibz)
       call build_BS_kpt_map(Xk)
     endif
     deallocate(BS_kpt_bz)
     deallocate(BS_kpt_ibz)
     !
   endif
   !
   if (IO_err/=0) then
     !
     call COLLISIONS_HXC(X(2),Xk,E,k,q,Xw(2),IO_err_BS,ID_mat)
     !
     ! COHSEX clean
     !--------------
     !
     if (io_COLLs.and.l_use_Hxc_collisions) call COLLISIONS_alloc_and_free("HXC","v3 table","clean",1)
     if (io_COLLs.and.l_use_COH_collisions) call COLLISIONS_alloc_and_free("COH","v3 table","clean",1)
     !
   endif
   !
   if (l_use_Hxc_collisions.and.IO_err_BS==0) then 
     call io_control(ACTION=RD_CL,COM=REP,ID=ID_mat)
     call io_BS_PAR_free(ID_head,ID_mat,"full",.true.)
   endif
   !
 endif
 !
#if defined _QED
 !
 if (l_elphoton_scatt) then
   !
   if (     l_use_Hxc_collisions) call section('=','Electron-Photon (P)') 
   if (.not.l_use_Hxc_collisions) call section('+','Electron-Photon (P)')
   !=====================================================================
   !
   call CHECK_and_ALLOC(P_collisions)
   !
   if (IO_err/=0) then
     !
     call COLLISIONS_momentum(E,k,q)
     !
     ! Clean
     !-------
     if (io_COLLs) call COLLISIONS_alloc_and_free("Momentum","v3 table","clean",1)
     !
   endif
   !
 endif
 !
#endif
 !
 if (l_elel_scatt) then
   !
   if (     l_use_Hxc_collisions) call section('=','NEQ GW (e-e scattering)')
   if (.not.l_use_Hxc_collisions) call section('+','NEQ GW (e-e scattering)')
   !=========================================================================
   !
   call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.)
   call PARALLEL_global_indexes(E,k,q,"Real_Time")
   !
   call PLASMA_build_up(E,k,q)
   call PLASMA_parallel_setup(EH_gas)
   !
   call PARALLEL_WF_distribute(PLASMA_index=PAR_IND_Plasma,CLEAN_UP=.TRUE.)
   call COLLISIONS_basic_operations( "WF_distribute", GW_NEQ_collisions )
   call PARALLEL_WF_index( )
   !
   call CHECK_and_ALLOC(GW_NEQ_collisions)
   !
   if (IO_err/=0) then 
     !
     call COLLISIONS_NEQ_GW_static(X(2),Xk,E,k,q,Xw(2)) 
     !
     ! GW_NEQ clean
     !--------------
     if (io_COLLs) call COLLISIONS_alloc_and_free("GW_NEQ","v3 table","clean",1)
     !
   endif
   !
 endif
 !
 ! CLEAN
 !=======
 !
 call WF_free(WF)
 !
 FREE_ch="v3"
 !
 if (io_COLLs) then
   !
   if (.not.l_real_time.and..not.l_sc_run) then
     !
     ! %state and %N are needed in RT_driver and SC_driver to distribute the collisions.
     ! This distribution is done BEFORE the call to COLLLISIONS_load. Therefore %state and %N
     ! must not be cleared here.
     !
     call COLLISIONS_basic_operations( "clean",    HXC_collisions )
     call COLLISIONS_basic_operations( "clean",    COH_collisions )
     !
     call COLLISIONS_basic_operations( "clean",      P_collisions )
     call COLLISIONS_basic_operations( "clean", GW_NEQ_collisions )
     !
   endif
   !
   if (IO_err==0) return
   !
   FREE_ch="all"
   !
 endif
 !
 if (l_use_COH_collisions) call COLLISIONS_alloc_and_free("COH"      ,trim(FREE_ch),"clean",1)
 if (l_use_Hxc_collisions) call COLLISIONS_alloc_and_free("HXC"      ,trim(FREE_ch),"clean",1)
 !
 if (l_elel_scatt)         call COLLISIONS_alloc_and_free("GW_NEQ"   ,trim(FREE_ch),"clean",1)
 if (l_elphoton_scatt)     call COLLISIONS_alloc_and_free("Momentum" ,trim(FREE_ch),"clean",1)
 !
 call timing('COLLISIONS_eval',OPR='stop')
 !
 contains
   !
   subroutine CHECK_and_ALLOC(COLL_grp)
     !
#include<collisions.h>
     !
     ! Define io_COLLs_header to handle either serial or parallel I/O
     !
     use COLL_interfaces,   ONLY:io_COLLISIONS
#if defined _PAR_IO
     integer, external :: io_COLLs_header
#endif
     !
     integer :: i_coll
     !
     type(COLLISIONS_group) :: COLL_grp
     !
     IO_err=-1
     !
     if (io_COLLs) then
       COLL_grp%IO_status=-1
       call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
       IO_err=io_COLLs_header(ID,COLL_grp)
#if defined _PAR_IO
       call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),ID=ID)
       io_err=io_COLLISIONS(ID,COLL_grp)
       if(io_err==0) then
         call warning(" Restarting from previous database")
         do i_coll=1,COLL_grp%N
           IO_ACT=manage_action(RD_CL_IF_END,i_coll,1,COLL_grp%N)
           call io_control(ACTION=IO_ACT,COM=REP,SEC=(/4/),ID=ID)
           io_err=io_COLLISIONS(ID,COLL_grp,i_coll=i_coll)
         enddo
       endif
#endif
       if (any(COLL_grp%IO_status/=1)) IO_err=-1
     endif
     !
     ! In case io_Colls is /=0 all CPU have to sincronize before starting
     ! to compute the collisions and write a new header
     !
     call PP_wait()
     !
     if (IO_err==0) return
     !
     if (io_COLLs) then
       call COLLISIONS_alloc_and_free(trim(COLL_grp%name),"collision","alloc",1)
       call COLLISIONS_alloc_and_free(trim(COLL_grp%name),"v3 table","alloc",1)
     else
       call COLLISIONS_alloc_and_free(trim(COLL_grp%name),"collision","alloc",0)
       if (COLL_grp%kind==QP_SE_GW_NEQ.or.COLL_grp%kind==QP_SE_GW_QED) then
         call COLLISIONS_alloc_and_free(trim(COLL_grp%name),"v3","alloc distribute",0)
       else
         call COLLISIONS_alloc_and_free(trim(COLL_grp%name),"v3","alloc",1)
         call COLLISIONS_alloc_and_free(trim(COLL_grp%name),"table","alloc",0)
       endif
     endif
     !
   end subroutine CHECK_and_ALLOC
   !
end subroutine COLLISIONS_eval
!
!
subroutine set_BS_blks(Xk)
 !
 use pars,         ONLY:cZERO
 use BS,           ONLY:BS_K_dim,n_BS_blks,n_BS_blks_min,BS_blk,BS_res_ares_n_mat, &
&                       BSK_IO_mode,l_BSE_kernel_full
 use R_lattice,    ONLY:bz_samp
 !
#include<y_memory.h>
 !
 type(bz_samp), intent(in) :: Xk
 !
 integer   :: iB
 !
 BS_res_ares_n_mat=1
 n_BS_blks=BS_K_dim(1)*2
 n_BS_blks_min=n_BS_blks
 !
 allocate(BS_blk(n_BS_blks))
 BS_blk(:)%size(1)=1
 ! dual part
 BS_blk(:)%zise(2)=1
 !
 do iB=1,BS_K_dim(1)
   BS_blk(iB            )%coordinate(1)=iB
   BS_blk(iB+BS_K_dim(1))%coordinate(1)=iB
   ! dual part
   BS_blk(iB            )%poordinate(1)=1
   BS_blk(iB+BS_K_dim(1))%poordinate(1)=1
   ! mode
   BS_blk(iB            )%mode="R"
   BS_blk(iB+BS_K_dim(1))%mode="C"
 enddo
 !
 if (l_BSE_kernel_full) then
   BS_blk(:)%size(2)=BS_K_dim(1)
   BS_blk(:)%coordinate(2)=1
 else
   do iB=1,BS_K_dim(1)
     BS_blk(iB            )%size(2)=BS_K_dim(1)-iB+1
     BS_blk(iB+BS_K_dim(1))%size(2)=BS_K_dim(1)-iB+1
     BS_blk(iB            )%coordinate(2)=iB
     BS_blk(iB+BS_K_dim(1))%coordinate(2)=iB
     ! dual part
     BS_blk(iB            )%zise(1)=BS_K_dim(1)-BS_blk(iB)%size(2)
     BS_blk(iB+BS_K_dim(1))%zise(1)=BS_K_dim(1)-BS_blk(iB)%size(2)
     BS_blk(iB            )%poordinate(2)=iB
     BS_blk(iB+BS_K_dim(1))%poordinate(2)=iB
   enddo
 endif
 !
end subroutine set_BS_blks
!
!
subroutine check_BS_parameters_consistency(Xk,BS_nk_bz,BS_nk_ibz)
 !
 use R_lattice,       ONLY:bz_samp
 use D_lattice,       ONLY:nsym
 use QP_m,            ONLY:QP_ng_SH,QP_ng_Sx,QP_ng_Sc
 use BS,              ONLY:BS_blk,BS_n_g_W,BS_n_g_exch,BS_K_dim,BS_bands
 use collision_ext,   ONLY:COLL_bands
 !
 implicit none
 !
 type(bz_samp), intent(in) :: Xk
 integer,       intent(in) :: BS_nk_bz,BS_nk_ibz
 !
 integer :: i_H
 !
 if ( Xk%nbz/=BS_nk_bz ) &
 & call error(" Different number of k-points between bse kernel and SAVE folder")
 !
 if ( Xk%nibz/=BS_nk_ibz .and. BS_nk_ibz/=0 ) then
  ! This will lead to wrong results due to phases issues. Indeed the k-pts
  ! outside the new IBZ maybe reached by a new sym, if the original one
  ! is not available anymore
  if ( nsym>1  ) call error("nsym>1 and BSE kernel generated with more syms then SAVE")
  ! This works only if the SAVE without symmetries has been generated by
  ! directly expanding the SAVE used of the BSE run
  if ( nsym==1 ) call warning("nsym=1 and BSE kernel generated with syms")
 endif
 !
 if (BS_n_g_exch/=QP_ng_SH) &
 & call error(" Different cutoff between coll(hartree) and bse(exch)")
 !
 if (BS_n_g_W   /=QP_ng_Sx) &
 & call error(" Different cutoff between coll(fock) and bse(direct)")
 !
 if (BS_n_g_W   /=QP_ng_Sc) &
 & call error(" Different cutoff between coll(corr) and bse(direct)")
 !
 if ( any(BS_bands/=COLL_bands) ) then
   if ( BS_bands(1)>COLL_bands(1) .or. BS_bands(2)<COLL_bands(2) ) then
     call error(" Too few bands in BSE kernel")
   else
     call warning(" More bands than needed in BSE kernel")
   endif
 endif
 !
end subroutine check_BS_parameters_consistency
!
!
subroutine build_inverse_BS_eh_table(Xk,ID_mat)!,BS_nk)
 !
 use electrons,       ONLY:n_sp_pol
 use R_lattice,       ONLY:bz_samp
 use BS,              ONLY:BS_bands,BS_K_dim,BS_blk
 use BS_solvers,      ONLY:BSS_eh_table_m1
 !
 implicit none
 !
 type(bz_samp), intent(in) :: Xk
 integer, intent(in)       :: ID_mat
 !integer, intent(out)      :: BS_nk
 !
 integer :: i_res_ares,i_H,ik_bz,iv,ic,i_sp_c,i_sp_v
 !
 allocate(BS_blk(1)%table(5,BS_K_dim(1)))
 allocate(BS_blk(2)%table(5,BS_K_dim(1)))
 !
 call read_BS_table(1,ID_mat)
 !
 !BS_nk=-1
 !do i_res_ares=1,2
 !do i_H=1,BS_K_dim(1)
 !  BS_nk = max(BS_blk(i_res_ares)%table(1,i_H),BS_nk)
 !enddo
 !enddo
 !
 allocate(BSS_eh_table_m1(Xk%nbz,BS_bands(1):BS_bands(2),BS_bands(1):BS_bands(2),n_sp_pol,n_sp_pol))
 BSS_eh_table_m1=0
 !call live_timing('BSE inverse eh-table',BS_H_dim)
 !
 do i_res_ares=1,2
 do i_H=1,BS_K_dim(1)
   !
   ik_bz    = BS_blk(i_res_ares)%table(1,i_H)
   iv       = BS_blk(i_res_ares)%table(2,i_H)
   ic       = BS_blk(i_res_ares)%table(3,i_H)
   i_sp_c   = BS_blk(i_res_ares)%table(4,i_H)
   i_sp_v   = BS_blk(i_res_ares)%table(5,i_H)
   !
   BSS_eh_table_m1(ik_bz,iv,ic,i_sp_c,i_sp_v)=i_H+(i_res_ares-1)*BS_K_dim(1)
   !
   !call live_timing(steps=1)
   !
 enddo
 enddo
 !call live_timing( )
 !
 deallocate(BS_blk(1)%table)
 deallocate(BS_blk(2)%table)
 !
end subroutine build_inverse_BS_eh_table
!
!
subroutine build_BS_kpt_map(Xk)
 !
 use pars,           ONLY:SP
 use BS,             ONLY:BS_K_io_map,BS_kpt_bz
 use R_lattice,      ONLY:bz_samp
 use vec_operate,    ONLY:v_is_zero,rlu_v_is_zero,c2a
 use zeros,          ONLY:k_rlu_zero
 !
 implicit none
 !
 type(bz_samp), intent(in) :: Xk
 !
 integer  :: ik1,ik2
 real(SP) :: tmp_kpt(3)
 !
 allocate(BS_K_io_map(Xk%nbz))
 BS_K_io_map=0
 !
 call k_ibz2bz(Xk,'i',.false.)
 do ik1=1,Xk%nbz
   do ik2=1,Xk%nbz
     call c2a(v_in=Xk%ptbz(ik1,:)-BS_kpt_bz(:,ik2),v_out=tmp_kpt,mode="ki2a")
     if (.not.rlu_v_is_zero(tmp_kpt,zero_=k_rlu_zero)) cycle
     BS_K_io_map(ik1)=ik2
     exit
   enddo
 enddo
 call k_ibz2bz(Xk,'d',.false.)
 !
end subroutine build_BS_kpt_map
