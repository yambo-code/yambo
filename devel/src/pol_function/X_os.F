!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM,AF,DS
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
subroutine X_os(Xo,iq,Xen,Xk,Xw,X)
 !
 ! Non interacting Xo
 !
 use drivers,       ONLY:l_life,l_em1s
 use IO_m,          ONLY:io_RESPONSE
 use pars,          ONLY:SP,cZERO,schlen
 use wrapper,       ONLY:V_by_V_plus_V
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:msg,warning
 use stderr,        ONLY:intc
 use wave_func,     ONLY:WF
 use parallel_m,    ONLY:PP_redux_wait,PAR_COM_q_for_Xo,PAR_COM_Q_INDEX,myid
 use openmp,        ONLY:OPENMP_update,n_threads_X,master_thread,OPENMP_set_threads,n_threads_X,n_threads_DIP
 use frequency,     ONLY:w_samp,bare_grid_N,coarse_grid_N,coarse_grid_Pt
 use interfaces,    ONLY:WF_load
 use D_lattice,     ONLY:i_space_inv
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:qindx_X,bz_samp
 use memory_m,      ONLY:mem_est
 use X_m,           ONLY:X_t,X_poles,current_iq,X_poles_tab,&
&                        self_detect_E_range,half_X_mat_only,use_X_DbGd,   &
&                        X_terminator,global_gauge
 use IO_m,          ONLY:IO_and_Messaging_switch
#if defined _SC 
 use drivers,       ONLY:l_sc_run,l_eval_collisions
 use SC,            ONLY:it_now
#endif
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 implicit none
 type(levels)         :: Xen
 type(bz_samp)        :: Xk
 type(X_t)            :: X
 type(w_samp)         :: Xw
 integer              :: iq
 complex(SP)          :: Xo(X%ng,X%ng,Xw%n_freqs)
 !
 ! Work Space
 !
 character(schlen)       :: live_t_string
 integer                 :: ig1,ig2,iw,n_poles,i_cg
 logical                 :: force_bare_X_G,Drude_pole,skip_WF_load
 real(SP)                :: minmax_ehe(2,PAR_COM_q_for_Xo%n_CPU)
 complex(SP)             :: GreenF(Xw%n_freqs),drude_GreenF(Xw%n_freqs),pole
 complex(SP),allocatable :: Xo_res(:,:)
 integer,    external    :: X_eh_setup
 complex(SP),external    :: X_simple_GreenF
 !
 ! Defaults & Setups
 !===================
 Xo           = cZERO
 GreenF       = cZERO
 drude_GreenF = cZERO
 !
 ! Logicals to use bare or Double Grid GF (no poles accumulation)
 !=======================================================
 force_bare_X_G=associated(Xen%W).or.associated(Xen%Z).or.use_X_DbGd.or.associated(Xen%GreenF)
 !
 skip_WF_load= (iq==1.and.X%ng==1)
 !
 !  Optical strengths
 !=====================
 if (iq==1) then
   !
   ! When the screened interaction is computed only the CPU assigned to q=1 enter here.
   ! By default, in PARALLEL_global_indexes, only the head of these cpu's is allowed to do I/O.
   ! But this means that only a fraction of the dipoles is actually written on disk.
   ! To make all dipoles I/O possible I temporarely swicth on the IO for all q=1 cpu's.
   !
   if (l_em1s) call IO_and_Messaging_switch("+io_out",CONDITION=.TRUE.)
   !
   call OPENMP_set_threads(n_threads_in=n_threads_DIP)
   !
   call DIPOLE_driver(Xen, Xk, X, X%q0)
   !
   call X_Drude(iq,Xen,Xk,Xw,X%Wd,drude_GreenF,X%ordering)
   !
   call OPENMP_set_threads(n_threads_in=n_threads_X)
   !
   if (l_em1s) call IO_and_Messaging_switch("+io_out",PAR_COM_Q_INDEX%my_CHAIN==1)
   !
 endif
 !
 ! WF load
 !=========
 if(.not.skip_WF_load) call WF_load(WF,X%ng,maxval(qindx_X(:,:,2)),X%ib,(/1,Xk%nibz/),title='-X')
 !
#if defined _TIMING
 call timing('Xo (procedure)',OPR='start')
#endif
 !
 ! Poles tabulation
 !==================
 if (iq/=current_iq) then
   !
   n_poles=X_eh_setup(-iq,X,Xen,Xk,minmax_ehe)
   !
   if (n_poles==0) call warning(' [CPU '//trim(intc(myid))//'] has no poles')
   !
   allocate(X_poles_tab(n_poles,4)) 
   call mem_est("X_poles_tab",(/size(X_poles_tab)/))
   !
   if (.not.force_bare_X_G) call FREQUENCIES_coarse_grid('X',X_poles,n_poles,X%cg_percentual)
   if (     force_bare_X_G) call FREQUENCIES_coarse_grid('X',X_poles,n_poles,0._SP)
   !
   minmax_ehe=0._SP
   !
   n_poles=X_eh_setup(iq,X,Xen,Xk,minmax_ehe(:,PAR_COM_q_for_Xo%CPU_id+1))
   deallocate(X_poles)
   !
   if (self_detect_E_range) then
     call PP_redux_wait(minmax_ehe,COMM=PAR_COM_q_for_Xo%COMM)
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
 if (associated(Xen%GreenF).and.current_iq==0) call X_GreenF_remap(X%ib,Xen,Xw)
 !
#endif
 !
 ! GPL_EXCLUDE_END
 !
 ! Time-Rev is Spatial Inv => only half X is eval
 !                            ===================
 call WF_spatial_invertion(Xen,Xk)
 !
 half_X_mat_only=i_space_inv==1
 if (.not.half_X_mat_only) then
   half_X_mat_only= all( aimag(Xw%p(:))<1.E-4 ).and. all( real(Xw%p(:))<1.E-4 )
 endif
 if (half_X_mat_only.and.current_iq==0) call msg('s','[X] Upper matrix triangle filled')
 !
 ! Xo_residual allocation 
 !------------------------
 allocate(Xo_res(X%ng,X%ng))
 call mem_est("Xo_WS",(/X%ng**2/))
 !
 ! Timing
 !========
 ! 
 live_t_string='Xo@q['//trim(intc(iq))//'] '
 !
#if defined _SC 
 if (l_sc_run.and..not.l_eval_collisions) live_t_string='Xo @it['//trim(intc(it_now))//'] '
#endif
 !
 if(coarse_grid_N>0) call live_timing(trim(live_t_string),coarse_grid_N)
 !
 call OPENMP_update(master_thread)
 !
 ! MAIN LOOP
 !===========
 !
 do i_cg = 1,coarse_grid_N
   !
   ! 1) First compute the residuals
   !================================
   call X_os_residuals(Xen,Xk,X,i_cg,iq,Xo_res)
   !
   ! 2) Then the frequency dependent term
   !=======================================
   Drude_pole= (iq==1) .and. abs(coarse_grid_Pt(i_cg))<1.E-5
   !
   if(Drude_pole) then
     Xo(1,1,:)=Xo(1,1,:)+Xo_res(1,1)*drude_GreenF(:)/real(bare_grid_N(i_cg))
     call live_timing(steps=1)
     cycle
   endif
   !
   if(force_bare_X_G) then
     call X_bare_Double_Grid_GreenF(iq,X_poles_tab(i_cg,:),Xw,Xen,Xk,GreenF,X%ordering,'G')
   else
     pole=cmplx(coarse_grid_Pt(i_cg),0._SP)
     do iw=1,Xw%n_freqs
       GreenF(iw) = X_simple_GreenF(Xw%p(iw),pole,X%ordering)
     enddo
   endif
   !
   ! 3) Finally multiply residual and frequency dependent term
   !===========================================================
   do iw=1,Xw%n_freqs
     !
     !$omp parallel default(shared)
     !$omp do private(ig2)
     do ig2=1,X%ng
       call V_by_V_plus_V(ig2,GreenF(iw),Xo_res(:ig2,ig2),Xo(:ig2,ig2,iw))
     enddo
     !$omp end do
     if (.not.half_X_mat_only ) then 
       !$omp do private(ig2)
       do ig2=1,X%ng
         call V_by_V_plus_V(X%ng-ig2,GreenF(iw),conjg(Xo_res(ig2,ig2+1:)),Xo(ig2+1:,ig2,iw))
       enddo
       !$omp end do
     endif
     !$omp end parallel
     !
   enddo
   !
   call live_timing(steps=1)
   !
 enddo 
 !
 if(coarse_grid_N>0)  call live_timing()
 !
 call OPENMP_update(master_thread) 
 !
#if defined _TIMING
 call timing('Xo (procedure)',OPR='stop')
 call timing('Xo (REDUX)',OPR='start')
#endif
 !
 do iw=1,Xw%n_freqs
   call PP_redux_wait(Xo(:,:,iw),COMM=PAR_COM_q_for_Xo%COMM)
 enddo
 !
#if defined _TIMING
 call timing('Xo (REDUX)',OPR='stop')
#endif
 !
 ! Symmetrize Xo when only half has been avaluated
 !=================================================
 !
 if (half_X_mat_only) then
   !
   !$omp parallel default(shared), private(ig2,ig1)
   if (i_space_inv==0) then
     !$omp do
     do ig2=1,X%ng
       do ig1=ig2+1,X%ng
         Xo(ig1,ig2,:)=conjg(Xo(ig2,ig1,:))
       enddo
     enddo
     !$omp end do
   endif
   !
   if (i_space_inv==1) then
     !$omp do
     do ig2=1,X%ng
       do ig1=ig2+1,X%ng
         Xo(ig1,ig2,:)=Xo(ig2,ig1,:)
       enddo
     enddo
     !$omp end do
   endif
   !$omp end parallel
   !
 endif
 !
 ! CLEAN
 !=======
 deallocate(Xo_res)
 call mem_est("Xo_WS")
 !
 current_iq=iq
 !
end subroutine
