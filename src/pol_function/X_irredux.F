!
!        Copyright (C) 2000-2017 the YAMBO team
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
 use com,           ONLY:msg,warning
 use matrix,        ONLY:PAR_matrix
 use stderr,        ONLY:intc
 use wave_func,     ONLY:WF
 use parallel_m,    ONLY:PP_redux_wait,PAR_COM_X_WORLD,PAR_COM_RL_INDEX,myid,PAR_COM_X_WORLD_RL_resolved
 use openmp,        ONLY:OPENMP_update,n_threads_X,master_thread,OPENMP_set_threads,n_threads_X,&
&                        n_threads_DIP,n_outthr,n_innthr,OPENMP_locks_reset,n_threads_FFT
#if defined _OPENMP
 use openmp,        ONLY:OPENMP_compute_mutex,omp_locks
#endif
 use frequency,     ONLY:w_samp,bare_grid_N,coarse_grid_N,coarse_grid_Pt
 use interfaces,    ONLY:WF_load,WF_free
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:qindx_X,bz_samp,G_m_G
 use D_lattice,     ONLY:i_space_inv
 use collision_el,  ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc
 use DIPOLES,       ONLY:DIPOLE_t,DIP_alloc
 use X_m,           ONLY:X_t,X_poles,X_Ein_poles,current_iq,X_poles_tab,X_lower_triangle_matrix_in_use,&
&                        self_detect_E_range,X_FILL_UP_matrix_only,use_X_DbGd,&
&                        l_X_terminator,X_terminator_E,X_term_E,X_par_lower_triangle
#if defined _SC 
 use drivers,       ONLY:l_sc_run,l_eval_collisions
 use SC,            ONLY:it_now
#endif
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 !
#include<memory.h>
 !
 type(PAR_matrix)     :: X_par
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
 integer                  :: ig1,ig2,iw,n_poles,i_cg,i_bg,mutexid,ngrho,io_err,X_cols(2)
 logical                  :: force_bare_X_G,Drude_pole,skip_WF_load
 real(SP)                 :: minmax_ehe(2,PAR_COM_X_WORLD%n_CPU),cutoff
 complex(SP)              :: GreenF(Xw%n_freqs),drude_GreenF(Xw%n_freqs)
 complex(SP),allocatable  :: Xo_res(:,:)
 integer,    external     :: X_eh_setup
 type(elemental_collision):: Xo_scatt
 !
 ! Defaults & Setups
 !===================
 X_par%blc(:,:,:)                 = cZERO
 GreenF                           = cZERO
 drude_GreenF                     = cZERO
 if (X_lower_triangle_matrix_in_use) X_par_lower_triangle%blc(:,:,:) = cZERO
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
   call DIPOLE_IO(Xk,Xen,Dip,'read ',io_err)
   call DIPOLE_expand_and_project(Xk,Dip)   
 endif
 !
 ! WF load
 !=========
 ngrho=X%ng
 if (l_X_terminator) ngrho=maxval(G_m_G)
 !
 if(.not.skip_WF_load) call WF_load(WF,ngrho,maxval(qindx_X(:,:,2)),X%ib,(/1,Xk%nibz/),title='-X')
 !
#if defined _TIMING
 call timing('Xo (procedure)',OPR='start')
#endif
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
   if (     force_bare_X_G) call FREQUENCIES_coarse_grid('X',X_poles,n_poles,0._SP,0.0_SP,.FALSE.)
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
 ! Green Functions must be all mapped to the Xw range so
 ! to be easily convoluted
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
#if defined _OPENMP
#  if defined _NESTING
 call OPENMP_set_threads(n_threads_in=n_threads_X, use_nested=.true.)
 n_threads_FFT=n_innthr
 call msg('s','[X] NESTED openmp parallelism on: n_outthr/n_innthr = ',(/n_outthr,n_innthr/))
#  else
 call OPENMP_set_threads(n_threads_in=n_threads_X, use_nested=.false.)
 n_threads_FFT=1
 call msg('s','[X] NESTED openmp parallelism off')
#  endif
#endif
 call OPENMP_locks_reset(INIT=.true.,nlocks=16)
 !
 ! Timing
 !========
 live_t_string='Xo@q['//trim(intc(iq))//'] '
 !
#if defined _SC 
 if (l_sc_run.and..not.l_eval_collisions) live_t_string='Xo @it['//trim(intc(it_now))//'] '
#endif
 !
 if(coarse_grid_N>0) call live_timing(trim(live_t_string),coarse_grid_N/n_outthr)
 !
 ! OpenMP setup
 !==============
 !
#if defined _OPENMP
!$omp parallel num_threads(n_outthr) default(shared), &
!$omp &        private(i_cg, Drude_pole,GreenF, i_bg, Xo_res, Xo_scatt, ig1, iw, mutexid,X_cols)
#endif
 !
 call OPENMP_update(master_thread)
 !
 ! memory estimate and local alloc
 !=================================
 YAMBO_ALLOC(Xo_res,(X_par%rows(1):X_par%rows(2),X_par%cols(1):X_par%cols(2)))
 call elemental_collision_free(Xo_scatt,INIT_ONLY=.true.)
 call elemental_collision_alloc(Xo_scatt,NG=ngrho,TITLE="Xo") 
 !
 ! MAIN LOOP
 !===========
 !
#if defined _OPENMP
!$omp do
#endif
 do i_cg = 1,coarse_grid_N
   !
   i_bg=sum(bare_grid_N(1:i_cg-1))+1
   !
   ! 1) First compute the residuals
   !================================
   call X_irredux_residuals(Xen,Xk,X,Dip,i_cg,iq,Xo_res,Xo_scatt)
   !
   ! 2) Then the frequency dependent term
   !=======================================
   Drude_pole= (iq==1) .and. abs(coarse_grid_Pt(i_cg))<1.E-5
   !
   if(Drude_pole.and.X_par%rows(1)==1.and.X_par%cols(1)==1) then
#if defined _OPENMP
     !$omp critical
#endif
     X_par%blc(1,1,:)=X_par%blc(1,1,:)+Xo_res(1,1)*drude_GreenF(:)/real(bare_grid_N(i_cg))
#if defined _OPENMP
     !$omp end critical
#endif
     call live_timing(steps=1)
     cycle
   endif
   !
   ! GPL_EXCLUDE_START
   !
#if defined _ELPH
   if( allocated(Xen%GreenF) ) then
     call X_GreenF_convoluted(iq,X_poles_tab(i_bg,:),Xw,Xen,Xk,GreenF,X%ordering)
   else
     call X_GreenF_analytical(iq,X_poles_tab(i_bg,:),Xw,Xen,Xk,GreenF,X%ordering,'G',.FALSE.)
   endif
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
   do iw=1,Xw%n_freqs
     !
     do ig1=X_par%rows(1),X_par%rows(2)
#if defined _OPENMP
       call OPENMP_compute_mutex(ig1,mutexid)
       call omp_set_lock(omp_locks(mutexid))
#endif
       !
       X_cols=(/ig1,X_par%cols(2)/)
       !
       ! ----    ----
       ! -xxx    -xxx
       ! ---- => ----
       ! ----    ----
       !
       call V_by_V_plus_V(X_cols(2)-X_cols(1)+1,GreenF(iw),Xo_res(ig1,X_cols(1):X_cols(2)),&
&                         X_par%blc(ig1,X_cols(1):X_cols(2),iw))
       !
       if (.not.X_FILL_UP_matrix_only) then
         !
         X_cols(1)=ig1+1
         !
         ! ----    ----
         ! -xxx    ----
         ! ---- => -x--
         ! ----    -x--
         !
         if (PAR_COM_RL_INDEX%n_CPU>1) then
           call V_by_V_plus_V(X_cols(2)-X_cols(1)+1,GreenF(iw),&
&                             conjg(Xo_res(ig1,X_cols(1):X_cols(2))),&
&                             X_par_lower_triangle%blc(X_cols(1):X_cols(2),ig1,iw))
         else
           call V_by_V_plus_V(X_cols(2)-X_cols(1)+1,GreenF(iw),&
&                             conjg(Xo_res(ig1,X_cols(1):X_cols(2))),&
&                             X_par%blc(X_cols(1):X_cols(2),ig1,iw))
         endif
       endif
#if defined _OPENMP
       call omp_unset_lock(omp_locks(mutexid))
#endif
     enddo
     !
   enddo
   !
   if (master_thread) call live_timing(steps=1)
   !
 enddo 
 !
#if defined _OPENMP
!$omp end do
#endif
 !
 ! CLEAN
 !=======
 YAMBO_FREE(Xo_res)
 call elemental_collision_free(Xo_scatt)
 !
 if (iq==1.and.io_DIP) then
   call DIP_alloc('DIP_iR')
   call DIP_alloc('DIP_P')
   call DIP_alloc('DIP_spin')
   call DIP_alloc('DIP_orbital')
   call DIP_alloc('P_square') 
   call DIP_alloc('DIP_projected')
   call DIP_alloc('DIP_P_spinor')
 endif
 !
#if defined _OPENMP
!$omp end parallel
#endif
 !
 !
 if(coarse_grid_N>0) call live_timing()
 !
 call OPENMP_update(master_thread) 
 call OPENMP_locks_reset()
 !
 current_iq=iq
 n_threads_FFT=0
 !
#if defined _TIMING
 call timing('Xo (procedure)',OPR='stop')
 call timing('Xo (REDUX)',OPR='start')
#endif
 !
 do iw=1,Xw%n_freqs
   call PP_redux_wait(X_par%blc(:,:,iw),COMM=PAR_COM_X_WORLD_RL_resolved%COMM)
   if (X_lower_triangle_matrix_in_use) then
     call PP_redux_wait(X_par_lower_triangle%blc(:,:,iw),COMM=PAR_COM_X_WORLD_RL_resolved%COMM)
   endif
 enddo
 !
#if defined _TIMING
 call timing('Xo (REDUX)',OPR='stop')
#endif
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
#if defined _OPENMP
 !$omp parallel default(shared), private(ig2,ig1)
 !$omp do
#endif
 do ig1=X_par%rows(1),X_par%rows(2)
   if (X_FILL_UP_matrix_only) then
     do ig2=X_par%cols(1),ig1-1
       if (i_space_inv==1) X_par%blc(ig1,ig2,:)=X_par%blc(ig2,ig1,:)
       if (i_space_inv==0) X_par%blc(ig1,ig2,:)=conjg(X_par%blc(ig2,ig1,:))
     enddo
   else if (PAR_COM_RL_INDEX%n_CPU>1) then
     do ig2=X_par%cols(1),ig1
       X_par_lower_triangle%blc(ig2,ig1,:)=X_par%blc(ig2,ig1,:)
       X_par%blc(ig1,ig2,:)               =X_par_lower_triangle%blc(ig1,ig2,:)
     enddo
   endif
 enddo
#if defined _OPENMP
 !$omp end do
 !$omp end parallel
#endif
 !
end subroutine
