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
 use collision_ext,  ONLY:COLLISIONS_parallel_SC_structure,COLLISIONS_group,&
&                        COH_collisions,HXC_collisions,P_collisions,GW_NEQ_collisions,COLLISIONS_naming
 use X_m,            ONLY:X_t
 use plasma,         ONLY:EH_gas
 use frequency,      ONLY:w_samp
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:OP_RD_CL,OP_RD,RD_CL_IF_END,VERIFY,REP,io_COLLs,manage_action
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
 integer       :: ID,IO_err,IO_ACT
 character(20) :: PAR_ch,FREE_ch
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
   if (IO_err/=0) then
     !
     call COLLISIONS_HXC(X(2),Xk,E,k,q,Xw(2))
     !
     ! COHSEX clean
     !--------------
     !
     if (io_COLLs.and.l_use_Hxc_collisions) call COLLISIONS_alloc_and_free("HXC","v3 table","clean",1)
     if (io_COLLs.and.l_use_COH_collisions) call COLLISIONS_alloc_and_free("COH","v3 table","clean",1)
     !
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
   subroutine CHECK_and_ALLOC(COLL)
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
     type(COLLISIONS_group) :: COLL
     !
     IO_err=-1
     !
     if (io_COLLs) then
       COLL%IO_status=-1
       call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
       IO_err=io_COLLs_header(ID,COLL)
#if defined _PAR_IO
       call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),ID=ID)
       io_err=io_COLLISIONS(ID,COLL)
       if(io_err==0) then
         call warning(" Restarting from previous database")
         do i_coll=1,COLL%N
           IO_ACT=manage_action(RD_CL_IF_END,i_coll,1,COLL%N)
           call io_control(ACTION=IO_ACT,COM=REP,SEC=(/4/),ID=ID)
           io_err=io_COLLISIONS(ID,COLL,i_coll=i_coll)
         enddo
       endif
#endif
       if (any(COLL%IO_status/=1)) IO_err=-1
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
       call COLLISIONS_alloc_and_free(trim(COLL%name),"collision","alloc",1)
       call COLLISIONS_alloc_and_free(trim(COLL%name),"v3 table","alloc",1)
     else
       call COLLISIONS_alloc_and_free(trim(COLL%name),"collision","alloc",0)
       if (COLL%kind==QP_SE_GW_NEQ.or.COLL%kind==QP_SE_GW_QED) then
         call COLLISIONS_alloc_and_free(trim(COLL%name),"v3","alloc distribute",0)
       else
         call COLLISIONS_alloc_and_free(trim(COLL%name),"v3","alloc",1)
         call COLLISIONS_alloc_and_free(trim(COLL%name),"table","alloc",0)
       endif
     endif
     !
   end subroutine
   !
end subroutine
