!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM,DS,AF,FA
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
!> @callgraph
!> @callergraph
subroutine X_irredux(iq,X_par,Xen,Xk,Xw,X,Dip)
 !
 ! Non interacting Xo
 !
 ! OPENMP parallelism  (AF & FA)
 !   The current implementation is based on mutexes (locks).
 !   At the price of some replicated memory (Xo_res) it provides a much
 !   better omp scaling.
 !
 !   _NESTING introduces the nesting of scatterbamp parallel regions inside the
 !   outer region opened here (not working yet)
 !
 ! X terminator (BG kind) implemented (IM,AF)
 ! 
 use drivers,       ONLY:l_life
 use IO_m,          ONLY:io_RESPONSE,io_DIP
 use pars,          ONLY:SP,cZERO,schlen
 use wrapper,       ONLY:V_by_V_plus_V
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:msg
 use matrix,        ONLY:PAR_matrix
 use stderr,        ONLY:intc
 use wave_func,     ONLY:WF
 use parallel_m,    ONLY:PP_redux_wait,PAR_COM_X_WORLD,PAR_COM_RL_INDEX,myid,PAR_COM_X_WORLD_RL_resolved
 use openmp,        ONLY:OPENMP_update,n_threads_X,master_thread,OPENMP_set_threads,n_threads_X,&
&                        n_out_threads,n_inn_threads,OPENMP_locks_reset,n_threads_FFT
#if defined _OPENMP
 use openmp,        ONLY:OPENMP_compute_mutex,omp_locks
#endif
 use frequency,     ONLY:w_samp,bare_grid_N,coarse_grid_N,coarse_grid_Pt
 use interfaces,    ONLY:WF_load
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:qindx_X,bz_samp,G_m_G
 use D_lattice,     ONLY:i_space_inv
 use collision_el,  ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc
 use DIPOLES,       ONLY:DIPOLE_t,DIP_alloc
 use X_m,           ONLY:X_t,X_poles,X_Ein_poles,current_iq,X_poles_tab,X_lower_triangle_matrix_in_use,&
&                        self_detect_E_range,X_FILL_UP_matrix_only,use_X_DbGd,&
&                        l_X_terminator,X_terminator_E,X_term_E,X_par_lower_triangle
 use X_m,           ONLY:rhotw_save_d
 use cuda_m,        ONLY:have_cuda
#if defined _SC 
 use drivers,       ONLY:l_sc_run,l_eval_collisions
 use SC,            ONLY:it_now
#endif
 use timing_m,      ONLY:timing
 !
#include<dev_defs.h>
#include<memory.h>
 !
 type(PAR_matrix), target :: X_par
 type(levels)         :: Xen
 type(bz_samp)        :: Xk
 type(X_t)            :: X
 type(DIPOLE_t)       :: Dip
 type(w_samp)         :: Xw
 integer              :: iq
 !
 ! Work Space
 !
 character(schlen)        :: live_t_string
 integer                  :: ig1,ig_row,ig_col,ig_row_transp,ig_col_transp,iw,n_poles,i_cg,i_bg,mutexid,ngrho,&
&                            X_cols_transp(2),X_rows1,X_rows2,X_cols1,X_cols2,X_rows1_l,X_rows2_l,X_nrows,io_err
 logical                  :: force_bare_X_G,Drude_pole,skip_WF_load
 real(SP)                 :: minmax_ehe(2,PAR_COM_X_WORLD%n_CPU),cutoff
 complex(SP)              :: GreenF(Xw%n_freqs),drude_GreenF(Xw%n_freqs),ctmp
 complex(SP),allocatable  :: Xo_res(:,:)
 !
 complex(SP),allocatable DEV_ATTRIBUTE :: Xo_res_d(:,:)
 complex(SP),pointer     DEV_ATTRIBUTE :: X_par_blc_d(:,:)
 complex(SP),pointer     DEV_ATTRIBUTE :: X_par_lowtri_blc_d(:,:)
 complex(SP)              :: GreenF_iw
 integer                  :: PAR_COM_X_WORLD_n_CPU
 !
 integer,    external     :: X_eh_setup
 type(elemental_collision), target :: Xo_scatt
 !
 ! Defaults & Setups
 !===================
 GreenF                           = cZERO
 drude_GreenF                     = cZERO
 !
 X_rows1=X_par%rows(1)
 X_rows2=X_par%rows(2)
 X_cols1=X_par%cols(1)
 X_cols2=X_par%cols(2)
 PAR_COM_X_WORLD_n_CPU=PAR_COM_RL_INDEX%n_CPU
 !
 !
 ! Logicals to use bare or Double Grid GF (no poles accumulation)
 !=======================================================
 force_bare_X_G=use_X_DbGd.or.allocated(Xen%W).or.allocated(Xen%GreenF)
 !
 skip_WF_load= (iq==1.and.X%ng==1)
 !
 ! Drude term
 !============
 if (iq==1) call X_drude(iq,Xen,Xk,Xw,X%Wd,drude_GreenF)
 !
 ! Dipoles
 !=========
 if (iq==1) then
   call DIPOLE_dimensions(Xen,Dip,X%ib,X%q0)
   call DIPOLE_IO(Xk,Xen,Dip,'read ',io_err,'X')
 endif
 !
 ! WF load
 !=========
 ngrho=X%ng
 if (l_X_terminator) ngrho=maxval(G_m_G)
 !
 if(.not.skip_WF_load) call WF_load(WF,ngrho,maxval(qindx_X(:,:,2)),X%ib,(/1,Xk%nibz/),title='-X')
 !
 call timing('Xo (procedure)',OPR='start')
 !
 ! Poles tabulation
 !==================
 if(l_X_terminator) then
   cutoff=minval(Xen%E(X%ib(2),:,:))
   X_term_E=cutoff+X_terminator_E
 endif
 !
 if (iq/=current_iq) then
   !
   n_poles=X_eh_setup(-iq,X,Xen,Xk,minmax_ehe)
   !
   if (n_poles==0) call warning(' [CPU '//trim(intc(myid))//'] has no poles')
   !
   YAMBO_ALLOC(X_poles_tab,(n_poles,4))
   !
   if (.not.force_bare_X_G) call FREQUENCIES_coarse_grid('X',X_poles,n_poles,X%cg_percentual,X_Ein_poles,l_X_terminator)
   if (     force_bare_X_G) call FREQUENCIES_coarse_grid('X',X_poles,n_poles,0._SP,(/0.0_SP/),.FALSE.)
   !
   minmax_ehe=0._SP
   !
   n_poles=X_eh_setup(iq,X,Xen,Xk,minmax_ehe(:,PAR_COM_X_WORLD%CPU_id+1))
   !
   YAMBO_FREE(X_poles)
   YAMBO_FREE(X_Ein_poles)
   !
   if (self_detect_E_range) then
     call PP_redux_wait(minmax_ehe,COMM=PAR_COM_X_WORLD%COMM)
     Xw%er(1)=minval(minmax_ehe(1,:))
     Xw%er(2)=maxval(minmax_ehe(2,:))
   endif
   !
   ! This call is needed as Xw%p is deallocated inside
   ! the q-loop of X_em1. But only when the EM1D is written or when it is not but we are not doing
   ! lifetimes calculations
   !
   if (io_RESPONSE.or.(.not.io_RESPONSE.and..not.l_life)) call FREQUENCIES_setup(Xw)
   !
 endif
 !
 ! GPL_EXCLUDE_START
 !
#if defined _ELPH
 !
 ! Green Functions must be all mapped to the Xw range so to be easily convoluted
 !
 if (allocated(Xen%GreenF).and.current_iq==0) call X_GreenF_remap(X%ib,Xen,Xw)
 !
#endif
 !
 ! GPL_EXCLUDE_END
 !
 ! Time-Rev is Spatial Inv => only half X is eval
 !                            ===================
 if (X_FILL_UP_matrix_only.and.current_iq==0) call msg('s','[X] Upper matrix triangle filled')
 !
 ! omp settings and workspace
 !=================================
 !
#if defined _CUDA
 n_threads_FFT=1
 n_threads_X=1
 call OPENMP_set_threads(n_threads_in=n_threads_X, use_nested=.false.)
#else
#  if defined _OPENMP
#    if defined _NESTING
 call OPENMP_set_threads(n_threads_in=n_threads_X, use_nested=.true.)
 n_threads_FFT=n_inn_threads
 call msg('s','[X] NESTED openmp parallelism on: n_out_threads/n_inn_threads = ',(/n_out_threads,n_inn_threads/))
#    else
 call OPENMP_set_threads(n_threads_in=n_threads_X, use_nested=.false.)
 n_threads_FFT=1
#    endif
 if(n_threads_X>1) call OPENMP_locks_reset(INIT=.true.,nlocks=16)
#  endif
#endif
 !
 ! Timing
 !========
 live_t_string='Xo@q['//trim(intc(iq))//'] '
 !
#if defined _SC 
 if (l_sc_run.and..not.l_eval_collisions) live_t_string='Xo @it['//trim(intc(it_now))//'] '
#endif
 !
 if (coarse_grid_N>=n_out_threads) call live_timing(trim(live_t_string),coarse_grid_N/n_out_threads)
 !
 ! OpenMP setup
 !==============
 !
#if !defined _CUDA
 !$omp parallel num_threads(n_out_threads) default(shared), &
 !$omp &        private(i_cg,Drude_pole,GreenF,i_bg,Xo_res,Xo_scatt,ig_col,&
 !$omp &        ig1,ig_row_transp,iw,mutexid,X_rows1,X_rows2,X_cols1,X_cols2,&
 !$omp &        X_rows1_l,X_rows2_l,X_cols_transp,X_nrows)
#endif
 !
 call OPENMP_update(master_thread)
 !
 ! memory estimate and local alloc
 !=================================
 YAMBO_ALLOC(Xo_res,(X_par%rows(1):X_par%rows(2),X_par%cols(1):X_par%cols(2)))
 if (have_cuda) then
   YAMBO_ALLOC(Xo_res_d,(X_par%rows(1):X_par%rows(2),X_par%cols(1):X_par%cols(2)))
   YAMBO_ALLOC(rhotw_save_d,(ngrho))
 endif
 call elemental_collision_free(Xo_scatt)
 call elemental_collision_alloc(Xo_scatt,NG=ngrho,TITLE="Xo") 
 !
 ! Drude term
 !
 if (iq==1.and.master_thread) then
   do i_cg = 1,coarse_grid_N
     Drude_pole= (iq==1) .and. abs(coarse_grid_Pt(i_cg))<1.E-5
     if(.not.Drude_pole) cycle
     if(.not.(X_par%rows(1)==1.and.X_par%cols(1)==1)) cycle
     !
     call X_irredux_residuals(Xen,Xk,X,Dip,i_cg,iq,DEV_VARNAME(Xo_res),Xo_scatt)
     ctmp = DEV_VARNAME(Xo_res)(1,1)
     !
     X_par%blc(1,1,:)=X_par%blc(1,1,:)+ctmp*drude_GreenF(:)/real(bare_grid_N(i_cg),SP)
     !
     if (have_cuda) then
       do iw=1,Xw%n_freqs
         X_par%blc_d(1,1,iw)=X_par%blc(1,1,iw)
       enddo
     endif
     !
     exit
   enddo
 endif
 !
 ! MAIN LOOP
 !===========
 !
#if !defined _CUDA
 !$omp do
#endif
 do i_cg = 1,coarse_grid_N
   !
   i_bg=sum(bare_grid_N(1:i_cg-1))+1
   !
   ! Drude term already accounted for
   !
   Drude_pole= (iq==1) .and. abs(coarse_grid_Pt(i_cg))<1.E-5
   if(Drude_pole) then
     if (master_thread) call live_timing(steps=1)
     cycle
   endif
   !
   ! 1) First compute the residuals
   !================================
   call X_irredux_residuals(Xen,Xk,X,Dip,i_cg,iq,DEV_VARNAME(Xo_res),Xo_scatt)

   !
   ! 2) Then the frequency dependent term
   !=======================================
   !
   ! GPL_EXCLUDE_START
   !
#if defined _ELPH
   if(     allocated(Xen%GreenF)) call X_GreenF_convoluted(iq,X_poles_tab(i_bg,:),Xw,Xen,Xk,GreenF,X%ordering)
   if(.not.allocated(Xen%GreenF)) call X_GreenF_analytical(iq,X_poles_tab(i_bg,:),Xw,Xen,Xk,GreenF,X%ordering,'G',.FALSE.)
#else
   !
   ! GPL_EXCLUDE_END
   !
   call X_GreenF_analytical(iq,X_poles_tab(i_bg,:),Xw,Xen,Xk,GreenF,X%ordering,'G',.FALSE.)
   !
   ! GPL_EXCLUDE_START
   !
#endif
   !
   ! GPL_EXCLUDE_END
   !
   ! 3) Finally multiply residual and frequency dependent term
   !===========================================================
   freq_loop:&
   do iw=1,Xw%n_freqs
     !
#ifdef _CUDA
     !
     GreenF_iw=GreenF(iw)
     X_par_blc_d => X_par%blc_d(:,:,iw)
     if (X_lower_triangle_matrix_in_use)  X_par_lowtri_blc_d => X_par_lower_triangle%blc_d(:,:,iw)
     !
     !$cuf kernel do(2) <<<*,*>>>
     do ig_col=X_cols1,X_cols2
       do ig1=X_rows1,X_rows2
         if (ig1 <= ig_col) X_par_blc_d(ig1,ig_col)=X_par_blc_d(ig1,ig_col)+GreenF_iw*Xo_res_d(ig1,ig_col)
       enddo
     enddo
     !
#else
     !
     do ig_col=X_cols1,X_cols2
       !
#  if defined _OPENMP
       if(n_threads_X>1) then
         call OPENMP_compute_mutex(ig_col,mutexid)
         call omp_set_lock(omp_locks(mutexid))
       endif
#  endif
       !
       ! ----    ----
       ! -xxx    -xxx
       ! ---- => ----
       ! ----    ----
       !
       !
       X_rows1_l = X_rows1
       X_rows2_l = min(ig_col,X_rows2)
       X_nrows = X_rows2_l-X_rows1_l+1 
       !
       call V_by_V_plus_V(X_nrows,GreenF(iw),Xo_res(X_rows1_l:X_rows2_l,ig_col),X_par%blc(X_rows1_l:X_rows2_l,ig_col,iw))
       !
#  if defined _OPENMP
       if(n_threads_X>1) call omp_unset_lock(omp_locks(mutexid))
#  endif
     end do
     !
#endif
     !
     if (.not.X_FILL_UP_matrix_only) then
       !
       !$cuf kernel do(2)
       do ig_col=X_cols1,X_cols2
         !
#if defined _OPENMP && ! defined _CUDA
         if(n_threads_X>1) then
           call OPENMP_compute_mutex(ig_col,mutexid)
           call omp_set_lock(omp_locks(mutexid))
         endif
#endif
         !
#ifdef _CUDA
         !
         do ig1=X_rows1,X_rows2
           if (ig1 < ig_col) then
             ig_row_transp = ig_col
             if (PAR_COM_X_WORLD_n_CPU>1) then
               X_par_lowtri_blc_d(ig_row_transp,ig1)=X_par_lowtri_blc_d(ig_row_transp,ig1) + &
&                   GreenF_iw*conjg(Xo_res_d(ig1,ig_col))
             else
               X_par_blc_d(ig_row_transp,ig1)=X_par_blc_d(ig_row_transp,ig1)+GreenF_iw*conjg(Xo_res_d(ig1,ig_col))
             end if
           end if
         enddo
         !
#else
         !
         X_rows1_l = X_rows1
         X_rows2_l = min(ig_col-1,X_rows2)
         X_nrows = X_rows2_l-X_rows1_l+1
         !
         ig_row_transp = ig_col
         X_cols_transp = (/X_rows1_l,X_rows2_l/)
         !
         ! ----    ----
         ! -xxx    ----
         ! ---- => -x--
         ! ----    -x--
         !
         if (PAR_COM_RL_INDEX%n_CPU>1) then
           call V_by_V_plus_V(X_nrows,GreenF(iw),&
&                             conjg(Xo_res(X_rows1_l:X_rows2_l,ig_col)),&
&                             X_par_lower_triangle%blc(ig_row_transp,X_cols_transp(1):X_cols_transp(2),iw))
         else
           call V_by_V_plus_V(X_nrows,GreenF(iw),&
&                             conjg(Xo_res(X_rows1_l:X_rows2_l,ig_col)),&
&                             X_par%blc(ig_row_transp,X_cols_transp(1):X_cols_transp(2),iw))
         endif
         !
#  if defined _OPENMP
         if(n_threads_X>1) call omp_unset_lock(omp_locks(mutexid))
#  endif
         !
#endif
         !
       enddo
       !
     endif
     !
     if (have_cuda) then
       nullify(X_par_blc_d)
       if (X_lower_triangle_matrix_in_use)  nullify(X_par_lowtri_blc_d)
     endif
     !
   enddo freq_loop
   !
   if (master_thread) call live_timing(steps=1)
   !
 enddo 
 !
#if !defined _CUDA
 !$omp end do
#endif
 !
 ! CLEAN
 !=======
 YAMBO_FREE(Xo_res)
 YAMBO_FREE(Xo_res_d)
 YAMBO_FREE(rhotw_save_d)
 call elemental_collision_free(Xo_scatt)
 !
#if !defined _CUDA
!$omp end parallel
#endif
 !
 if (iq==1.and.io_DIP) then
   call DIP_alloc('DIP_iR')
   call DIP_alloc('DIP_P')
   call DIP_alloc('DIP_v')
   call DIP_alloc('DIP_spin')
   call DIP_alloc('DIP_orbital')
   call DIP_alloc('P_square') 
   call DIP_alloc('DIP_P_spinor')
 endif
 !
 if (coarse_grid_N>=n_out_threads) call live_timing( )
 !
 call OPENMP_update(master_thread) 
 call OPENMP_locks_reset()
 !
 current_iq=iq
 n_threads_FFT=0
 !
 call timing('Xo (procedure)',OPR='stop')
 call timing('Xo (REDUX)',OPR='start')
 !
 if (have_cuda) then
   X_par%blc=X_par%blc_d
   if (X_lower_triangle_matrix_in_use) X_par_lower_triangle%blc=X_par_lower_triangle%blc_d
   YAMBO_FREE(X_par%blc_d)
   YAMBO_FREE(X_par_lower_triangle%blc_d)
 endif
 !
 do iw=1,Xw%n_freqs
   call PP_redux_wait(X_par%blc(:,:,iw),COMM=PAR_COM_X_WORLD_RL_resolved%COMM)
   if (X_lower_triangle_matrix_in_use) call PP_redux_wait(X_par_lower_triangle%blc(:,:,iw),COMM=PAR_COM_X_WORLD_RL_resolved%COMM)
 enddo
 !
 call timing('Xo (REDUX)',OPR='stop')
 !
 ! Populate the lower triangle/piece of Xo 
 !=========================================
 !
 if (.not.X_FILL_UP_matrix_only.and.PAR_COM_RL_INDEX%n_CPU==1) return
 !
 ! X_FILL_UP_matrix_only=.TRUE.
 !
 ! oooooo
 ! xooooo
 ! xxoooo
 ! ------
 ! ------
 ! ------
 !
 ! X_FILL_UP_matrix_only=.FALSE.
 !
 ! PAR_COM_RL_INDEX%n_CPU=1
 !
 ! oooooo
 ! oooooo
 ! oooooo
 ! oooooo
 ! oooooo
 ! oooooo
 !
 ! PAR_COM_RL_INDEX%n_CPU > 1
 !
 ! oooooo    xxx---
 ! xooooo    oxx---
 ! xxoooo    oox---
 ! ------ +  ooo---
 ! ------    ooo---
 ! ------    ooo---
 !
 ! "o" = calculated
 ! "x" = to be filled now
 !
 if (X_FILL_UP_matrix_only) then
   !
   !$omp parallel do default(shared), private(iw,ig_col,ig_row,ig_row_transp,ig_col_transp), collapse(2)
   do iw=1,Xw%n_freqs
   do ig_col=X_par%cols(1),X_par%cols(2)
     ig_row_transp=ig_col
     do ig_row=max(ig_col+1,X_par%rows(1)),X_par%rows(2)
       ig_col_transp=ig_row
       if (i_space_inv==1) X_par%blc(ig_row,ig_col,iw)=      X_par%blc(ig_row_transp,ig_col_transp,iw)
       if (i_space_inv==0) X_par%blc(ig_row,ig_col,iw)=conjg(X_par%blc(ig_row_transp,ig_col_transp,iw))
     enddo
   enddo
   enddo
   !$omp end parallel do
   !
 else if (PAR_COM_RL_INDEX%n_CPU>1) then
   !
   !$omp parallel do default(shared), private(iw,ig_col,ig_row,ig_row_transp,ig_col_transp), collapse(2)
   do iw=1,Xw%n_freqs
   do ig_col=X_par%cols(1),X_par%cols(2)
     ig_row_transp=ig_col
     do ig_row=max(ig_col,X_par%rows(1)),X_par%rows(2)
       ig_col_transp=ig_row
       ! FILL THE UPPER PART OF X_par_lower
       X_par_lower_triangle%blc(ig_row_transp,ig_col_transp,iw)=X_par%blc(ig_row_transp,ig_col_transp,iw)
       ! FILL THE LOWER PART OF X_par
       X_par%blc(ig_row,ig_col,iw)                             =X_par_lower_triangle%blc(ig_row,ig_col,iw)
     enddo
   enddo
   enddo
   !$omp end parallel do
   !
 endif
 !
end subroutine
