!
! License-Identifier: GPL
!
! Copyright (C) 2007 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
#include<dev_defs.h>
!
subroutine XCo_Hartree_Fock(E,k,Xk,q,mode)
 !
 ! Hartree-Fock
 !
 use iso_c_binding, ONLY:c_loc,c_f_pointer
 use pars,          ONLY:SP,schlen,DP,pi
 use drivers,       ONLY:l_sc_run,l_gw0,l_cohsex,l_real_time
 use com,           ONLY:msg
 use LIVE_t,        ONLY:live_timing
 use stderr,        ONLY:intc
 use electrons,     ONLY:levels,spin_occ,spin
 use parallel_int,  ONLY:PP_redux_wait,PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index,&
&                        PARALLEL_index
 use parallel_m,    ONLY:PAR_IND_Xk_ibz,PAR_IND_QP,PP_indexes,PP_indexes_reset,PAR_COM_RL_INDEX,&
&                        PAR_IND_QP_ID,PAR_IND_Q_ibz,PAR_IND_G_b,&
&                        PAR_IND_Q_ibz_ID,n_WF_bands_to_load
 use collision_el,  ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc
 use QP_m,          ONLY:QP_Vnl_xc,QP_ng_Sx,QP_ng_Vxc,QP_table,QP_nb,QP_n_states
 use R_lattice,     ONLY:qindx_S,bz_samp
#if defined _SC
 use pseudo,        ONLY:l_use_p2_Vloc_becp
 use drivers,       ONLY:l_sc_exx
 use SC,            ONLY:it_now
#endif
 use timing_m,      ONLY:timing
 use openmp,        ONLY:OPENMP_update,master_thread,n_threads_SE
 use wave_func,     ONLY:WF,WF_buffer,WF_buffered_IO
 use interfaces,    ONLY:WF_load,QP_state_print
 use wrapper,       ONLY:Vstar_dot_VV_gpu
 use global_XC,     ONLY:WF_exx_screening
 use pseudo,        ONLY:becp, pp_is_uspp
 use qe_pseudo_m,   ONLY:beccopy
 use devxlib,       ONLY:devxlib_memcpy_d2d
 !
#include<y_memory.h>
 !
 type(levels) ::E       
 type(bz_samp)::k,Xk,q
 character(*),optional ::mode
 !
 !Work Space
 !
 type(elemental_collision), target :: isc,iscp
 complex(SP), pointer DEV_ATTR     :: gamp_p(:)
 complex(SP), pointer DEV_ATTR     :: isc_rhotw_p(:), iscp_rhotw_p(:)
 !
 integer                  :: iq,iq_ibz,is_ibz,ib,ibp,jb,i_qp
 integer                  :: ik,ikp,iGp,i_sp_pol,WF_g_max  ! QP list
 integer                  :: Sx_lower_band,Sx_upper_band(2),n_lt_bands,n_lt_steps
 integer                  :: ig_first,ig_last
 complex(DP)              :: DP_Sx, DP_Sx_l
 complex(SP), allocatable :: local_rhotw(:)
 character(schlen)        :: ch,mode_
 type(PP_indexes)         :: px

 !
 ! Resets & basis setup
 !
 call elemental_collision_free(isc)
 call elemental_collision_free(iscp) 
 !
 Sx_lower_band=1
 Sx_upper_band=E%nbm
 !
 mode_=""
 if (present(mode)) mode_=trim(mode)
 !
 call msg('r', '[VXC] Plane waves ',QP_ng_Vxc)
 call msg('r', '[EXS] Plane waves ',QP_ng_Sx)
 !
 ! QP_table -> report
 !
 call QP_state_print( )
 !
 if(.not.l_sc_run .and. .not.l_real_time) then
   !
   ! WF distributed & load
   !=======================
   !
   call PARALLEL_global_indexes(E,k,q,"Self_Energy")
   !
   ! Final states (k-q,np)...
   call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,B_index=PAR_IND_G_b,CLEAN_UP=.TRUE.)
   ! Initial states (k,n \in QP)...
   call PARALLEL_WF_distribute(QP_index=PAR_IND_QP)
   call PARALLEL_WF_index()
   !
   if ((l_gw0.or.l_cohsex).and.WF_buffered_IO) then
     !
     call WF_load(WF_buffer,0,1,(/1,n_WF_bands_to_load/),(/1,k%nibz/),space='B',title='-BUFFER')
     !
   endif
   !
   WF_g_max=max(QP_ng_Sx,QP_ng_Vxc)
   ! In case Vxc is computed from Hzero, only cutoff on Sx is needed here
#if defined _SC
   if (l_use_p2_Vloc_becp) WF_g_max=QP_ng_Sx
#endif
   call WF_load(WF,WF_g_max,maxval(qindx_S(:,:,2)),(/1,max(E%nbm,QP_nb)/),(/1,k%nibz/),title='-HF/Rho')
   !
 endif
 !
 ! G-parallelism
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/QP_ng_Sx/),COMM=PAR_COM_RL_INDEX,CONSECUTIVE=.true.)
 ig_first=px%first_of_1D(PAR_COM_RL_INDEX%cpu_id+1)
 ig_last=px%last_of_1D(PAR_COM_RL_INDEX%cpu_id+1)
 !
 call timing('HF',OPR='start')
 !
 ch='EXS'
#if defined _SC
 if (l_sc_run)          ch='HF@it'//trim(intc(it_now))
#endif
 !
! if (l_sc_exx) then
!   call live_timing(trim(ch),px%n_of_elements(myid+1)*E%nbf*&
!        &(maxval(QP_table(:,2))-E%nbf)*maxval(QP_table(:,3)))
! else  
!   call live_timing(trim(ch),px%n_of_elements(myid+1)*QP_n_states)
! endif
!
 n_lt_bands=count( PAR_IND_G_b%element_1D(:maxval(Sx_upper_band)) )
 n_lt_steps=PAR_IND_QP%n_of_elements(PAR_IND_QP_ID+1)*PAR_IND_Q_ibz%n_of_elements(PAR_IND_Q_ibz_ID+1)*n_lt_bands/n_threads_SE
 if (n_lt_steps>0) call live_timing(trim(ch),n_lt_steps)
 !
 do i_qp=1,QP_n_states
   !
   if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
   !
   ib =QP_table(i_qp,1)
   ibp=QP_table(i_qp,2)
   ik =QP_table(i_qp,3)
   i_sp_pol=spin(QP_table(i_qp,:))
   !
#if defined _SC  
   !
   ! In Exact-Exchange only vc matrix elements so ... no cc' no vv' 
   ! (cv is not permitted by the order in QP_table)
   !                   (c)         (v)
   if (l_sc_exx.and.(ib>E%nbf(i_sp_pol).or.ibp<=E%nbf(i_sp_pol))) cycle
#endif
   !    
   !==============
   !
   DP_Sx = 0.0d0
   !
   !  !!! num_threads(n_outthr)
   !DEV_OMP parallel default(shared), &
   !DEV_OMP          private(isc,iscp,gamp_p,isc_rhotw_p,iscp_rhotw_p,&
   !DEV_OMP &                iq,iq_ibz,is_ibz,ikp,iGp,jb,DP_Sx_l), reduction(+:DP_sx)
   !
   call OPENMP_update(master_thread)
   !
   ! memory estimate and local alloc
   !=================================
   !
   call elemental_collision_free(isc)
   call elemental_collision_free(iscp)
   call elemental_collision_alloc(isc,NG=QP_ng_Sx,GAMP_NG=(/QP_ng_Sx,1/),TITLE="HF")
   call elemental_collision_alloc(iscp,NG=QP_ng_Sx,TITLE="HF")
   !
   isc%qs    =1
   !
   call scatter_Gamp_gpu(isc,'x')
   !
   gamp_p => DEV_VAR(isc%gamp)(:,1)
   isc_rhotw_p => DEV_VAR(isc%rhotw) 
   iscp_rhotw_p => DEV_VAR(iscp%rhotw) 
   !
   ! AF: Alternative coding to be removed if not needed
   !call c_f_pointer(c_loc(isc%gamp), gamp_p, (/size(isc%gamp)/) )
   !
   if (trim(mode_)=='hyb') call scatter_ModScr(isc,WF_exx_screening)
   !
   !DEV_OMP do schedule(dynamic)
   !
   do iq=1,q%nbz
     iq_ibz=q%sstar(iq,1)
     is_ibz=q%sstar(iq,2)
     !
     if (.not.PAR_IND_Q_ibz%element_1D(iq_ibz)) cycle
     !
     isc%qs(2:)=(/iq_ibz,is_ibz/)
     !
     if (isc%qs(2)/=isc%iqref) then 
       call scatter_Gamp_gpu(isc,'x')
       if (trim(mode_)=='hyb') call scatter_ModScr(isc,WF_exx_screening)
     endif
     !
     !  (n,k,sp_n).     
     !              |
     !              | (m,p,r,sp_m)
     !              |
     !              |
     !  (m,k,sp_m).     
     !
     ! n   =QP_table(i_qp,1)
     ! m   =QP_table(i_qp,2)
     ! k   =QP_table(i_qp,3)
     !
     ikp=k%sstar(qindx_S(ik,iq,1),1)
     iGp=k%sstar(qindx_S(ik,iq,1),2)
     !
     isc%is =(/ib, ik , 1 ,i_sp_pol/)
     isc%os(2:)=(/ ikp,iGp,i_sp_pol/)
     iscp%is=(/ibp,ik , 1 ,i_sp_pol/)
     !
     isc%qs(1)=qindx_S(ik,iq,2)
     iscp%qs=isc%qs
     !
     if (pp_is_uspp) then
        !
        call beccopy( becp(ik,i_sp_pol),   isc%ibec )
        !
        call PP_rotate_becp(isc%os, becp(ikp,i_sp_pol), isc%obec)
        !
        call PP_scatterBamp_init(isc)
        !
        if (ib/=ibp) then
           !
           call beccopy( becp(ik,i_sp_pol), iscp%ibec )
           call beccopy( isc%obec, iscp%obec)
           call PP_scatterBamp_init(iscp)
           !
        endif
        !
     endif
     !
     do jb=Sx_lower_band,Sx_upper_band(i_sp_pol)
       !
       if (.not.PAR_IND_G_b%element_1D(jb)) cycle
       !      
       isc%os(1)=jb
       iscp%os=isc%os
       !
       call scatter_Bamp_gpu(isc)
       !
       ! Normal case, the density matrix is diagonal
       !
       if (isc%is(1)/=iscp%is(1)) then
         call scatter_Bamp_gpu(iscp)
       else
         ! dev2dev, iscp%rhotw = isc%rhotw
         call devxlib_memcpy_d2d(iscp_rhotw_p,isc_rhotw_p)
       endif
       !
       ! when GPUs are not available, revert on CPUs
       ! here isc%ngrho == QP_ng_Sx
       !
       DP_Sx_l=Vstar_dot_VV_gpu(isc%ngrho,iscp_rhotw_p,isc_rhotw_p,gamp_p,&
&                               BOUNDS=(/ig_first,ig_last/))
       !
       DP_Sx=DP_Sx + DP_Sx_l * ( -4._SP/spin_occ*pi*E%f(jb,ikp,i_sp_pol) )
       !
       if (master_thread.and.is_ibz==1.and.n_lt_steps>0) call live_timing(steps=1)
       !
     enddo
     !
   enddo
   !DEV_OMP end do
   !
   call elemental_collision_free(isc)
   call elemental_collision_free(iscp)
   !
   !DEV_OMP end parallel
   !
   QP_Vnl_xc(i_qp)=QP_Vnl_xc(i_qp)+ cmplx(DP_Sx,kind=SP)
   !
 enddo
 !
 if(n_lt_steps>0) call live_timing()
 !
 call elemental_collision_free(isc)
 call elemental_collision_free(iscp) 
 call PP_indexes_reset(px)
 !
 call timing('HF',OPR='stop')
 call timing('HF(REDUX)',OPR='start')
 !
 call PP_redux_wait(QP_Vnl_xc)
 !
 call timing('HF(REDUX)',OPR='stop')
 !
end subroutine
