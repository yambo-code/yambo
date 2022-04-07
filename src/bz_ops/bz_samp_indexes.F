!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine bz_samp_indexes(en,k,Xk,q)
 !
 ! ikbz=(ik,is) --<--:--<-- okbz=(ok,os) = (IK-Q)
 !                   :
 !                  /:\ iqbz=(iq,is)
 !                   :
 !
 ! iq_is = ik_is-ok_os-Go
 !
 ! qindx_X(iq,ikbz,1)=okbz
 ! qindx_X(iq,ikbz,2)=iGo
 !
 ! qindx_B(okbz,ikbz,1)=iqbz
 ! qindx_B(okbz,ikbz,2)=iGo
 !
 ! qindx_S(ik,iqbz,1)=okbz
 ! qindx_S(ik,iqbz,2)=iGo
 !
 ! qindx_C(ikbz,iqbz,1)=okbz
 ! qindx_C(ikbz,iqbz,2)=iGo
 !
 use pars,           ONLY:SP,lchlen,zero_dfl
 use drivers,        ONLY:l_bse,l_elel_scatt,l_setup,l_EXCPH_gkkp
 use com,            ONLY:msg,fat_log
 use parallel_m,     ONLY:PAR_K_scheme,master_cpu,PP_indexes,myid,PP_indexes_reset
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
 use LIVE_t,         ONLY:live_timing
 use matrix_operate, ONLY:m3inv,m3det
 use electrons,      ONLY:n_sp_pol,levels
 use vec_operate,    ONLY:v_is_zero,c2a
 use BS,             ONLY:BS_res_K_corr,BS_cpl_K_corr
 use R_lattice,      ONLY:d3q_factor,RL_vol,nqibz,nqbz,QP_states_k,&
&                         qindx_X,qindx_B,qindx_S,qindx_C,qindx_free,&
&                         bse_scattering,qindx_alloc,Xk_grid_is_uniform,&
&                         k_grid,k_grid_b,q_pt,k_pt,bz_samp,q_norm,q0_def_norm,&
&                         q_sstar,coll_scattering,q_source,smallest_q_norm,&
&                         Xk_grid_new_mapping,k_GRIDS_string,X_scattering,sigma_scattering
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:OP_RD_CL,OP_WR_CL,OP_APP_CL,VERIFY,REP
 use parser_m,       ONLY:parser
 use timing_m,       ONLY:timing
 use parallel_int,   ONLY:PARALLEL_global_indexes
 use openmp,         ONLY:n_threads,master_thread,OPENMP_update
#if defined _ELPH
 use ELPH,           ONLY:elph_use_q_grid
#endif
#include<memory.h>
 !
 type(levels) ::en
 type(bz_samp)::k,Xk,q
 !
 ! Work Space
 !
 integer :: io_db,io_err
 integer :: i1,i2,i3,i4,iv1(3),iqibz,iqbz,ikibz,ikbz,ikbz_p,ikbz_mem,iqs,iks,id
 real(SP):: v1(3),k_b_m1(3,3),local_zero(3),q_step(3)
 real(SP):: E_empty(1,q%nibz,n_sp_pol)
 logical :: connected_grids,user_defined_qpts,user_grids,do_qindx_B,do_qindx_X,do_qindx_X_ik
#if defined _ELPH
 logical :: bse_setup_scatt
#endif
 integer, external    :: G_index
 character(lchlen)    :: string(2)
 integer, allocatable :: q_map(:,:,:),q_iptbz(:,:)
 type(PP_indexes)     :: PAR_IND_Xk,PAR_IND_k
 integer, external    :: io_QINDX 
 !
 call section('*','Transferred momenta grid and indexing')
 !========================================================
 ! .... which GRIDS?
 X_scattering    =index(k_GRIDS_string,"X")>0
 sigma_scattering=index(k_GRIDS_string,"S")>0
 bse_scattering  =(l_bse .and. (BS_res_K_corr.or.BS_cpl_K_corr))
 coll_scattering =(l_elel_scatt.or.l_EXCPH_gkkp)
#if defined _ELPH
 call parser('BSEscatt',bse_setup_scatt)
 if (.not.bse_scattering ) bse_scattering =elph_use_q_grid.or.bse_setup_scatt.or.trim(q_source)=="Electron-Phonon databases"
#endif
 !
 ! ... User defined
 call parser('K_grids', user_grids)
 if (user_grids) then
   bse_scattering  = index(k_GRIDS_string,"B")/=0
   coll_scattering = index(k_GRIDS_string,"C")/=0
   X_scattering    = index(k_GRIDS_string,"X")/=0
   sigma_scattering= index(k_GRIDS_string,"S")/=0
   if (len_trim(k_GRIDS_string)==0) k_GRIDS_string="none"
   call k_GRIDS_update( )
 endif
 !
 if (bse_scattering)  X_scattering    =.TRUE.
 if (coll_scattering) sigma_scattering=.TRUE.
 !
 q%description='q'
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2,3,4,6/),MODE=VERIFY,ID=io_db)
 io_err=io_QINDX(Xk,q,io_db)
 if (io_err==0) then
   call k_expand(q)
   d3q_factor=RL_vol/real(q%nbz)
   if(l_setup.and.fat_log) call print_q_indirect( )
   call REPORT_Energies(E_empty,q,q,1,(/1,q%nibz/),'Q',.TRUE.,.FALSE.)
   call q_shadows(.false.)
   return
 endif
 !
 call timing('bz_samp_indexes',OPR='start')
 !
 ! First I map the Xk grid in a simple cubic Net
 !
 call k_ibz2bz(Xk,'a',.false.)  
 !
 ! Parallelization without Memory distribution
 !=============================================
 !
 call PP_indexes_reset(PAR_IND_Xk)
 call PARALLEL_index(PAR_IND_Xk,(/Xk%nbz/))
 call PP_indexes_reset(PAR_IND_k)
 call PARALLEL_index(PAR_IND_k,(/k%nbz/))
 !
 YAMBO_ALLOC(q_map,(k_grid(1),k_grid(2),k_grid(3)))
 YAMBO_ALLOC(q_iptbz,(Xk%nbz,3))
 !
 ! k_b_m1=transpose(k_grid_b) in rlu
 !
 ! k_grid_b is in cc !
 !
 call c2a(v_in=k_grid_b(1,:),v_out=k_b_m1(:,1),mode='kc2a')
 call c2a(v_in=k_grid_b(2,:),v_out=k_b_m1(:,2),mode='kc2a')
 call c2a(v_in=k_grid_b(3,:),v_out=k_b_m1(:,3),mode='kc2a')
 !
 ! q_iptbz(i,:) = Xk%ptbz(1,:) - Vo  in units of k_grid_b
 ! q_map gives the index of q_iptbz given the components along k_grid_b
 !
 ! As first step I define the map with respect to Q defined as difference
 ! of K-pts
 !
 if (abs(m3det(k_b_m1))>1.E-7) then
   !
   call m3inv(M=k_b_m1)
   !
   if(     Xk_grid_new_mapping) call define_q_map_new( Xk%ptbz(:,:), Xk%ptbz(1,:))
   if(.not.Xk_grid_new_mapping) call define_q_map    ( Xk%ptbz(:,:), Xk%ptbz(1,:))
   !
   ! THE GRID IS UNIFORM IF ALL THE Q_MAP IS FILLED
   !
   Xk_grid_is_uniform=all(q_map/=0)
   !
 else
   Xk_grid_is_uniform=.false.
 endif
 !
 user_defined_qpts=.false.
 !
 if (Xk_grid_is_uniform) then
   !
   q%nbz=Xk%nbz
   !
   ! q_ptbz in iku for k_reduce
   !
   YAMBO_ALLOC(q%ptbz,(q%nbz,3))
   do i1=1,q%nbz
     !
     v1=Xk%ptbz(i1,:)-Xk%ptbz(1,:)
     !
     call c2a(v_in= v1,v_out=q%ptbz(i1,:),mode='ka2i')
     !
   enddo
   if (.not.allocated(q_pt)) then
     call k_reduce(q,.TRUE.)
     YAMBO_FREE(q%ptbz)
   else
     q%nibz=nqibz
     YAMBO_ALLOC(q%pt,(q%nibz,3))
     q%pt=q_pt(:q%nibz,:)
     call msg('rsn','[BZ sampling] Q-grid '//trim(q_source))
     user_defined_qpts=.true.
   endif
   !
   ! q_ptbz in rlu for qindx_*.
   ! At difference with the Q list used in evaluating the map before
   ! here I need to recalculate the map so that it correctly points
   ! to the q%ptbz obtaine trought q_expand
   !
   call k_expand(q)
   call q_shadows(.false.)
   call k_ibz2bz(q,'a',.false.)
   nqbz =q%nbz
   nqibz=q%nibz
   call REPORT_Energies(E_empty,q,q,1,(/1,q%nibz/),'Q',.TRUE.,.FALSE.)
   !
   ! When using USER defined Q-points q%nbz may be different from Xk%nbz
   ! if the given list is not correct.
   ! In this case I switch to the Gamma only support
   !
   if (q%nbz/=Xk%nbz) then
     call bz_X_only_exit('[RL indx] Q BZ pts are /= from X grid BS pts. Gamma point only.')
   else
     if(     Xk_grid_new_mapping) call define_q_map_new(q%ptbz(:,:),(/0._SP,0._SP,0._SP/))
     if(.not.Xk_grid_new_mapping) call define_q_map    (q%ptbz(:,:),(/0._SP,0._SP,0._SP/))
   endif
   !
 else
   !
   call bz_X_only_exit('[RL indx] X grid is not uniform. Gamma point only.')
   !
 endif
 !
 d3q_factor=RL_vol/real(q%nbz)
 !
 ! X indexes
 !
 ! qindx_X(iq,ikbz,1)=okbz
 ! qindx_X(iq,ikbz,2)=iGo
 !
 ! qindx_B(okbz,ikbz,1)=iqbz
 ! qindx_B(okbz,ikbz,2)=iGo
 !
 if (X_scattering)     string(1)='X'
 if (sigma_scattering) string(2)='Sigma'
 if ( bse_scattering)  string(1)=trim(string(1))//' + BSE'
 if (coll_scattering)  string(2)=trim(string(2))//' + COLL'
 !
#if defined _PAR_IO
 !
 ! Parallelization with Memory distribution
 !==========================================
 !
 if(X_scattering.and.Xk_grid_is_uniform.and.bse_scattering) then
   call PARALLEL_global_indexes(en,Xk,q,"BZ_Indexes")
   call PARALLEL_SETUP_K_scheme("BZINDX")
 endif
 !
#endif
 !
 if (X_scattering.and.Xk_grid_is_uniform) then
   !
   ! Allocate
   !
   call qindx_alloc('XB')
   !
   call section('+',trim(string(1))//' indexes')
   !============================================
   !
   ! X/BSE
   !------
   call live_timing(trim(string(1))//' [eval]',max(1,PAR_IND_Xk%n_of_elements(myid+1)/n_threads))
   !
   if (Xk_grid_new_mapping) call k_find_smallest_q(Xk%ptbz,q_step,Xk%nbz)
   !
   !$omp parallel default(shared), private(ikbz,ikbz_mem,ikbz_p,v1,iv1,iqbz,iqibz,iqs,do_qindx_B,do_qindx_X,do_qindx_X_ik)
   call OPENMP_update(master_thread)
   !
   !$omp do
   do ikbz=1,Xk%nbz
     !
     do_qindx_X_ik=PAR_IND_Xk%element_1D(ikbz)
     do_qindx_B   =PAR_IND_Xk%element_1D(ikbz).and.bse_scattering
     ikbz_mem=ikbz
     !
#if defined _PAR_IO
     if(bse_scattering) then
       do_qindx_B=PAR_K_scheme%IND_bz%element_1D(ikbz)
       ikbz_mem=PAR_K_scheme%bz_index(ikbz)
     endif
#endif
     !
     if ((.not.do_qindx_X_ik).and.(.not.do_qindx_B)) cycle
     !
     do ikbz_p=1,Xk%nbz
       !
       if(Xk_grid_new_mapping) then
         !  
         v1=Xk%ptbz(ikbz,:)-Xk%ptbz(ikbz_p,:)
         v1=v1-nint(v1)       ! Bring in the BZ, remove the G0
         where(v1<-zero_dfl)  ! Bring between [0,1)
           v1(:)=1._SP+v1(:)
         end where
         do id=1,3
           if(q_step(id)==0) then
             iv1(id)=1 ! Non-periodic directions or no kpoints in this direction
           else
             iv1(id)=nint(v1(id)/q_step(id))+1
           endif
         enddo
         !
       else
         !  
         v1=matmul(k_b_m1,Xk%ptbz(ikbz,:)-Xk%ptbz(ikbz_p,:)) ! K-K_p= Q + Go
         iv1=nint(v1)
         call k_grid_shift(iv1)
         !
       endif
       !
       iqbz =q_map(iv1(1),iv1(2),iv1(3))
       iqibz=q%sstar(iqbz,1)
       iqs  =q%sstar(iqbz,2)
       !
       do_qindx_X=do_qindx_X_ik .and. iqs==1
       !
       if ((.not.do_qindx_X).and.(.not.do_qindx_B)) cycle
       !
       if (do_qindx_X) qindx_X(iqibz,ikbz,1)=ikbz_p
       if (do_qindx_B) qindx_B(ikbz_p,ikbz_mem,1)=iqbz
       !
       v1=Xk%ptbz(ikbz,:)-Xk%ptbz(ikbz_p,:)-q%ptbz(iqbz,:)
       call c2a(v_in=v1,mode='ka2i')
       !
       if (do_qindx_X) qindx_X(iqibz, ikbz    ,2)=G_index(v1,.true.)
       if (do_qindx_B) qindx_B(ikbz_p,ikbz_mem,2)=G_index(v1,.false.)
       !
       if ( do_qindx_X .and. qindx_X(iqibz,ikbz,2)==0 ) exit
       !
     enddo
     !
     if (master_thread) call live_timing(steps=1)
     !
   enddo
   !$omp end do
   !$omp end parallel
   !
   call live_timing()
   !
   ! ... PARALLEL redux
   call live_timing(trim(string(1))//'[REDUX]',Xk%nbz)
   do ikbz=1,Xk%nbz
     call PP_redux_wait(qindx_X(:,ikbz,1))
     call PP_redux_wait(qindx_X(:,ikbz,2))
#if !defined _PAR_IO
     if (bse_scattering) then
       call PP_redux_wait(qindx_B(:,ikbz,1))
       call PP_redux_wait(qindx_B(:,ikbz,2))
     endif
#endif
     call live_timing(steps=1)
   enddo
   call live_timing()
   !
   if (any(qindx_X==0)) then
     call bz_X_only_exit('Null transition detected [qindx_X]. Gamma point only.')
   endif
   !
 endif
 !
 ! SIGMA/COLL
 !------------
 if (sigma_scattering) then
   !
   call section('+',trim(string(2))//' indexes')
   !============================================
   !
   if(     coll_scattering) i1=Xk%nbz
   if(.not.coll_scattering) i1=QP_states_k(2)-QP_states_k(1)+1
   !
   call live_timing(trim(string(2))//' [eval]',max(1,i1/n_threads))
   !
   call k_ibz2bz(k,'a',.false.)
   !
   connected_grids=.true.
   local_zero=1.E-4_SP
   !
   call qindx_alloc('SC')
   !
   ! qindx_S(ik,iqbz,1)=okbz
   ! qindx_S(ik,iqbz,2)=iGo
   !
   ! qindx_C(ikbz,iqbz,1)=okbz
   ! qindx_C(ikbz,iqbz,2)=iGo
   !
   !$omp parallel default(shared), private(ikbz,ikibz,iks,ikbz_p,v1,iv1,iqbz)
   call OPENMP_update(master_thread)
   !
   !$omp do
   do ikbz=1,k%nbz
     !
     ikibz=k%sstar(ikbz,1)
     iks  =k%sstar(ikbz,2)
     !
     if (iks/=1.and..not.coll_scattering) cycle
     !
     do ikbz_p=1,k%nbz
       !
       if (.not.PAR_IND_k%element_1D(ikbz_p)) cycle 
       !
       if(Xk_grid_new_mapping) then
         !
         v1=Xk%ptbz(ikbz,:)-Xk%ptbz(ikbz_p,:)
         v1=v1-nint(v1)       ! Bring in the BZ, remove the G0
         where(v1<-zero_dfl)  ! Bring between [0,1)
           v1(:)=1._SP+v1(:)
         end where
         do id=1,3
           if(q_step(id)==0) then
             iv1(id)=1 ! Non-periodic directions or no kpoints in this direction
           else
             iv1(id)=nint(v1(id)/q_step(id))+1
           endif
         enddo
         !
       else
         !   
         v1=matmul(k_b_m1,k%ptbz(ikbz,:)-k%ptbz(ikbz_p,:))
         iv1=nint(v1)
         !
         if (.not.v_is_zero(v1-real(iv1,SP),zero_=local_zero)) then
            !$omp critical
            connected_grids=.false.
            !$omp end critical
            cycle
         endif
         !
         call k_grid_shift(iv1)
         !
       endif
       !
       iqbz=q_map(iv1(1),iv1(2),iv1(3))
       !
       if (iks==1) qindx_S(ikibz,iqbz,1)=ikbz_p
       if (coll_scattering) qindx_C(ikbz,iqbz,1)=ikbz_p
       v1=k%ptbz(ikbz,:)-k%ptbz(ikbz_p,:)-q%ptbz(iqbz,:)
       call c2a(v_in=v1,mode='ka2i')
       if (iks==1) qindx_S(ikibz,iqbz,2)=G_index(v1,.false.)
       if (coll_scattering) qindx_C(ikbz,iqbz,2)=G_index(v1,.false.)
     enddo
     !
     if (master_thread) call live_timing(steps=1)
     !
   enddo
   !$omp end do
   !$omp end parallel
   !
   call live_timing()
   !
   ! ... PARALLEL redux
   !
   call live_timing(trim(string(2))//'[REDUX]',nqbz)
   do iqbz=1,nqbz
     call PP_redux_wait(qindx_S(:,iqbz,1))
     call PP_redux_wait(qindx_S(:,iqbz,2))
     if (coll_scattering) then
       call PP_redux_wait(qindx_C(:,iqbz,1))
       call PP_redux_wait(qindx_C(:,iqbz,2))
     endif
     call live_timing(steps=1)
   enddo
   call live_timing( )
   !
   if (.not.connected_grids) call msg('rsn','[RL indx] X & Total k-grids are not connected')
   !
   K_LOOP: do ikbz=1,k%nbz
     ikibz=k%sstar(ikbz,1)
     if (any(qindx_S(ikibz,:,:)==0)) then 
       call bz_X_only_exit('Null transition detected [qindx_S]. Gamma point only.')
       exit K_LOOP
     endif
   enddo K_LOOP
 endif
 !
 ! Grids string update
 !
 call k_GRIDS_update( )
 !
 ! Before writing the new DB I need to wait that all cores are done
 ! with checking DB which was present before
 !
 call PP_redux_wait()
 !
 if(l_setup.and.fat_log) call print_q_indirect( )
 !
 if(master_cpu) then
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2,3,4,6/),ID=io_db)
   io_err=io_QINDX(Xk,q,io_db)
 endif
 !
 ! Before writing section 5 (qindx_B) I need to wait that the master is done
 ! with writing all other sections
 !
 call PP_redux_wait()
 !
 do_qindx_B=master_cpu.and.X_scattering.and.Xk_grid_is_uniform.and.bse_scattering
#if defined _PAR_IO
 do_qindx_B=PAR_K_scheme%COM_ibz_A2A%CPU_id==0.and.X_scattering.and.Xk_grid_is_uniform.and.bse_scattering
#endif
 !
 if (do_qindx_B) then
   call io_control(ACTION=OP_APP_CL,COM=REP,SEC=(/5/),ID=io_db)
   io_err=io_QINDX(Xk,q,io_db)
 endif
 !
 !CLEAN
 !
 call k_ibz2bz(Xk,'d',.false.)
 YAMBO_FREE(q_map)
 YAMBO_FREE(q_iptbz)
 if (Xk_grid_is_uniform) then
   YAMBO_FREE(q%ptbz)
 endif
 call qindx_free('B')
 call PP_redux_wait()
 call PP_indexes_reset(PAR_IND_Xk)
 call PP_indexes_reset(PAR_IND_k)
 !
#if defined _PAR_IO
 if(X_scattering.and.Xk_grid_is_uniform.and.bse_scattering) call PARALLEL_global_indexes(en,Xk,q," ",RESET=.TRUE.)
#endif
 !
 call timing('bz_samp_indexes',OPR='stop')
 !
 contains 
   !
   subroutine k_GRIDS_update()
     !
     ! Grids string update
     !
     k_GRIDS_string="X"
     if (bse_scattering  ) k_GRIDS_string=trim(k_GRIDS_string)//" B"
     if (coll_scattering ) k_GRIDS_string=trim(k_GRIDS_string)//" C"
     if (sigma_scattering) k_GRIDS_string=trim(k_GRIDS_string)//" S"
     !
   end subroutine 
   !
   subroutine k_grid_shift(v)
     implicit none
     integer  :: v(3),u(3),i1
     do i1=1,3
       if (v(i1)>=0) u(i1)=mod(v(i1)+1,k_grid(i1))
       if (v(i1)>=0.and.u(i1)==0) u(i1)=k_grid(i1)
       if (v(i1)<0) u(i1)=mod(v(i1),k_grid(i1))
       if (v(i1)<0.and.u(i1)/=0) u(i1)=u(i1)+k_grid(i1)+1
       if (v(i1)<0.and.u(i1)==0) u(i1)=1
     enddo
     v=u 
   end subroutine
   !
   subroutine define_q_map(qpt_map,q_ref)
     !
     implicit none
     !
     real(SP)::qpt_map(Xk%nbz,3),q_ref(3)
     !
     q_map=0
     do i1=1,Xk%nbz
       v1=matmul(k_b_m1,qpt_map(i1,:)-q_ref)
       q_iptbz(i1,:)=nint(v1)
       call k_grid_shift(q_iptbz(i1,:))
       if (q_map(q_iptbz(i1,1),q_iptbz(i1,2),q_iptbz(i1,3))/=0) then
         call warning('[RL indx] 2 equivalent points in the rlu grid found')
         q_map=0
         return
       endif
       q_map(q_iptbz(i1,1),q_iptbz(i1,2),q_iptbz(i1,3))=i1
     enddo
     !
     ! Now I fill the holes in the map shifting the whole grid
     !
     do i1=1,Xk%nbz
       do i2=-2,2
         do i3=-2,2
           do i4=-2,2
             v1=matmul(k_b_m1,qpt_map(i1,:)-q_ref+real((/i2,i3,i4/)))
             iv1=nint(v1)
             call k_grid_shift(iv1)
             if (q_map(iv1(1),iv1(2),iv1(3))==0) q_map(iv1(1),iv1(2),iv1(3))=i1
           enddo
         enddo
       enddo
     enddo
   end subroutine
   !
   subroutine define_q_map_new(qpt_map,q_ref)
     !
     use pars,      ONLY:zero_dfl
     !
     implicit none
     !
     real(SP)  :: qpt_map(Xk%nbz,3)
     real(SP), intent(in)     :: q_ref(3)
     !
     ! Work space
     !
     real(SP) :: qpt_save(Xk%nbz,3),q_step(3)
     integer  :: id
     !
     qpt_save=qpt_map
     !
     ! Remove shift if present 
     do id=1,3
       qpt_map(:,id)=qpt_map(:,id)-q_ref(id)
     enddo
     !
     qpt_map(:,:)=qpt_map(:,:)-nint(qpt_map(:,:)) ! Bring in the BZ
     !  
     ! Bring k-points between [0,1)
     !
     where(qpt_map(:,:)<-zero_dfl)
       qpt_map(:,:)=1._SP+qpt_map(:,:)     
     end where
     !
     ! Find the smallest q in the 3 direction 
     !  
     call k_find_smallest_q(qpt_map,q_step,Xk%nbz)
     !
     q_map=0
     do i1=1,Xk%nbz
       !
       do id=1,3
         !
         if(q_step(id)==0._SP) then ! Non periodic directions
           q_iptbz(i1,id)=1
           cycle
         endif
         !
         q_iptbz(i1,id)=nint(qpt_map(i1,id)/q_step(id))+1
         !
       enddo
       !
       q_map(q_iptbz(i1,1),q_iptbz(i1,2),q_iptbz(i1,3))=i1
       !
     enddo
     !
     qpt_map=qpt_save
     !
     if(any(q_map==0)) then
       call error('[RL indx] error in mapping')
       q_map=0
       return
     endif
     !
   end subroutine
   !
   subroutine q_shadows(force_gamma_only)
     use vec_operate,    ONLY:iku_v_norm
     logical                ::force_gamma_only
     !
     ! Gamma only, deallocate and reallocate 
     ! using Gamma point only definition
     !
     if (force_gamma_only) then
       YAMBO_FREE(k_pt)
       YAMBO_FREE(q_pt)
       YAMBO_FREE(q_norm)
       YAMBO_FREE(q_sstar)
       YAMBO_FREE(q%pt)
       nqbz=1
       nqibz=1
       q%nibz=1
       q%nbz=1
     endif
     !
     if (.not.allocated(q_pt)) then
       YAMBO_ALLOC(q_pt,(q%nibz,3))
     endif
     if (.not.allocated(k_pt)) then
       YAMBO_ALLOC(k_pt,(k%nibz,3))
     endif
     if (.not.allocated(q_norm)) then
       YAMBO_ALLOC(q_norm,(q%nibz))
     endif
     if (allocated(q%sstar).and..not.allocated(q_sstar)) then
       YAMBO_ALLOC(q_sstar,(q%nbz,2))
     endif
     !
     if (force_gamma_only) then
       YAMBO_ALLOC(q%pt,(q%nibz,3))
       if (.not.allocated(q_sstar)) then
         YAMBO_ALLOC(q_sstar,(q%nbz,2))
       endif
       q%pt=0._SP
       q_pt=0._SP
       k_pt=k%pt
       if(allocated(q_sstar)) q_sstar=1
       q_norm=q0_def_norm
       smallest_q_norm=q0_def_norm
     else
       q_pt=q%pt
       k_pt=k%pt
       q_sstar=q%sstar
       q_norm(1)=q0_def_norm
       do i1=2,q%nibz
         q_norm(i1)=iku_v_norm(q_pt(i1,:))
       enddo
       smallest_q_norm=minval(q_norm(2:))
     endif
     !
   end subroutine q_shadows
   !
   subroutine bz_X_only_exit(message)
     character(*), intent(in) :: message
     call qindx_free('ALL')
     nqibz=1
     if (X_scattering) then
       call qindx_alloc('X')
       do i1=1,Xk%nbz
         qindx_X(1,i1,1)=i1
         qindx_X(1,i1,2)=1
       enddo
     endif
     call msg('nsrn',trim(message))
     call q_shadows(.true.)
     Xk_grid_is_uniform=.false.
     bse_scattering=.false.
     sigma_scattering=.false.
     coll_scattering=.false.
   end subroutine bz_X_only_exit
   !
   subroutine print_q_indirect( )
     !
     use stderr,        ONLY:intc
     use interfaces,    ONLY:OCCUPATIONS_Gaps
     use com,           ONLY:COMPOSE_spin_msg
     !
     integer :: E_k_ind(n_sp_pol,2)
     integer :: okbz,ik,iqbz,i_sp_pol
     logical :: q_found
     !
     call OCCUPATIONS_Gaps(en,E_k_ind=E_k_ind)
     !
     if(all(E_k_ind(:,1)==E_k_ind(:,2))) return  ! Direct gap case
     if(.not.allocated(qindx_S))         return
     !
     do i_sp_pol=1,n_sp_pol
       q_found=.false.
       do iqbz=1,q%nbz
         okbz=qindx_S(E_k_ind(i_sp_pol,1),iqbz,1)
         if(k%sstar(okbz,1)==E_k_ind(i_sp_pol,2)) then
           q_found=.true.
           exit
         endif
       enddo
       if(q_found) call msg('r',COMPOSE_spin_msg('',0,'Q-point that connects indirect gap',i_sp_pol),q%sstar(iqbz,1))
     enddo
     !
   end subroutine print_q_indirect
   !
end subroutine
