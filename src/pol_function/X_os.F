!
!        Copyright (C) 2000-2014 the YAMBO team
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
subroutine X_os(Xo,iq,Xen,Xk,Xw,X)
 !
 ! Non interacting Xo
 !
 use drivers,       ONLY:l_life
 use IO_m,          ONLY:io_RESPONSE
 use pars,          ONLY:SP
 use wrapper,       ONLY:V_by_V_plus_V
 use collision,     ONLY:ggwinfo,collision_reset
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:msg
 use stderr,        ONLY:intc
 use wave_func,     ONLY:WF
 use parallel_m,    ONLY:PP_redux_wait,PAR_Xk_bz_index,PAR_COM_q_for_Xo
 use openmp,        ONLY:OPENMP_update,n_threads_X,master_thread,OPENMP_set_threads,n_threads_X,n_threads_DIP
 use frequency,     ONLY:w_samp,bare_grid_N,coarse_grid_N,coarse_grid_Pt
 use interfaces,    ONLY:WF_load
 use D_lattice,     ONLY:nsym,DL_vol,i_time_rev,sop_inv,i_space_inv
 use electrons,     ONLY:levels,spin_occ
 use R_lattice,     ONLY:g_rot,qindx_X,bz_samp
 use memory_m,      ONLY:mem_est
 use X_m,           ONLY:X_t,X_poles,DIP_q_dot_iR,current_iq,X_poles_tab,&
&                        self_detect_E_range,half_X_mat_only,use_X_RIM
#if defined _SC 
 use drivers,       ONLY:l_sc_run,l_eval_collisions
 use SC,            ONLY:it_now
 use fields,        ONLY:global_gauge
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
 integer                 :: ig1,ig2,ik,is,ikp,ikbz,ikpbz,i_spin,ik_bz_mem,&
&                           isp,iv,ic,iw,isave(4),n_poles,i_cg,i_bg
 logical                 :: force_bare_X_G,Drude_pole
 real(SP)                :: minmax_ehe(2),eh_occ
 type(ggwinfo)           :: isc
 complex(SP)             :: GreenF(Xw%n_freqs),drude_GreenF(Xw%n_freqs)
 complex(SP),allocatable :: rhotw_save(:),Xo_residual(:,:)
 integer,    external    :: X_eh_setup
 complex(SP),external    :: X_simple_GreenF
 !
 ! Defaults & Setups
 !===================
 Xo  = cmplx(0.,0.,SP)
 GreenF  = cmplx(0.,0.,SP)
 drude_GreenF = cmplx(0.,0.,SP)
 !
 ! Logicals to use bare or RIM GF (no poles accumulation)
 !=======================================================
 force_bare_X_G=associated(Xen%W).or.associated(Xen%Z).or.use_X_RIM.or.associated(Xen%GreenF)
 !
 !  Optical strengths
 !=====================
 if (iq==1) then
   call OPENMP_set_threads(n_threads_in=n_threads_DIP)
   call Dipole_driver(Xen, Xk, X, X%q0)
   call X_Drude(iq,Xen,Xk,Xw,X,drude_GreenF,X%ordering)
   call OPENMP_set_threads(n_threads_in=n_threads_X)
 endif
 !
 ! WF load
 !=========
 call WF_load(WF,X%ng,maxval(qindx_X(:,:,2)),X%ib,(/1,Xk%nibz/),title='-X')
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
   allocate(X_poles_tab(n_poles,4)) 
   call mem_est("X_poles_tab",(/size(X_poles_tab)/))
   !
   if (.not.force_bare_X_G) call FREQUENCIES_coarse_grid('X',X_poles,n_poles,X%cg_percentual)
   if (force_bare_X_G)      call FREQUENCIES_coarse_grid('X',X_poles,n_poles,0._SP)
   !
   n_poles=X_eh_setup(iq,X,Xen,Xk,minmax_ehe)
   deallocate(X_poles)
   !
   if (self_detect_E_range) Xw%er=minmax_ehe
   !
   ! This call is needed as Xw%p is deallocated inside
   ! the q-loop of X_em1. But only when the EM1D is written or when it is not but we are not  doing
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
 ! Mem est of Xo_residual (to be done here and not in the omp parallel)
 !------------------------
 call mem_est("Xo_WS",(/X%ng**2 *n_threads_X/))
 !
 ! OpenMP setup
 !==============
 !
!$omp parallel default(shared), &
!$omp &        private( i_cg, Drude_pole,GreenF,n_poles, i_bg, Xo_residual, &
!$omp &                 ikbz,iv,ic,i_spin,ik,is,ikpbz,ikp,isp, eh_occ, ik_bz_mem, &
!$omp &                 rhotw_save, isave, ig1,ig2, iw, isc )
 !
 call OPENMP_update(master_thread) 

 !
 ! prepare the scattering module
 !
 call collision_reset(isc)

 !
 ! Timing
 !========
 ! 
#if defined _SC 
 if (l_sc_run.and..not.l_eval_collisions) then
   call live_timing('Xo @it'//trim(intc(it_now)),coarse_grid_N/n_threads_X)
 else
   call live_timing('Xo@q['//trim(intc(iq))//'] ',coarse_grid_N/n_threads_X)
 endif
#else
 call live_timing('Xo@q['//trim(intc(iq))//'] ',coarse_grid_N/n_threads_X)
#endif
 !
 ! MAIN LOOP
 !===========
 !
 ! Main allocation
 !-----------------
 isc%ngrho=X%ng
 if (iq==1) allocate(rhotw_save(X%ng))
 allocate(isc%rhotw(X%ng))
 allocate(Xo_residual(X%ng,X%ng))
 !
 isave=0
 !
!$omp do
 do i_cg = 1,coarse_grid_N
   !
   Drude_pole=iq==1.and.abs(coarse_grid_Pt(i_cg))<1.E-5
   !
   if (Drude_pole) then
     GreenF=drude_GreenF/real(bare_grid_N(i_cg))
   else
     if (force_bare_X_G) call X_bare_RIM_GreenF(iq,X_poles_tab(i_cg,:),Xw,Xen,Xk,GreenF,X%ordering)
   endif
   !
   n_poles = sum(bare_grid_N(1:i_cg-1))
   !
   Xo_residual(:,:) = 0.0_SP
   !
   loop_bare_grid: do i_bg = 1,bare_grid_N(i_cg)
     !
     n_poles=n_poles+1
     !
     ! Scattering geometry
     !---------------------
     !
     ikbz   = X_poles_tab(n_poles,1)
     iv     = X_poles_tab(n_poles,2)
     ic     = X_poles_tab(n_poles,3)
     i_spin = X_poles_tab(n_poles,4)
     ik = Xk%sstar(ikbz,1)
     is = Xk%sstar(ikbz,2)
     ikpbz=qindx_X(iq,ikbz,1)
     ikp=Xk%sstar(ikpbz,1)
     isp=Xk%sstar(ikpbz,2)
     !
     ! Note the renormalization of the eh_occ=f(1-f) factor
     !
     !     n_spin n_sp_pol n_spinor  spin_occ eh_occ
     !       1        1        1         2      2
     !       2        1        2         1      1
     !       2        2        1         1      1
     !
     eh_occ = Xen%f(iv,ikp,i_spin)*(spin_occ-Xen%f(ic,ik,i_spin))/spin_occ/real(Xk%nbz)/DL_vol
     !
     ! Scattering CALL
     !-----------------
     !
     if (iq==1) then
       isc%is = (/ic,ik,1,i_spin/)
       isc%os = (/iv,ik,1,i_spin/)
       isc%qs = (/1,1,1/)
       ik_bz_mem=PAR_Xk_bz_index(ikbz)
       if (any((/isave(1)/=iv,isave(2)/=ic,isave(3)/=ik,isave(4)/=i_spin/))) then
         call scatterBamp(isc)
         rhotw_save=isc%rhotw
         isave=(/iv,ic,ik,i_spin/)
       endif
       do ig1=1,X%ng
         ig2=g_rot(sop_inv(is),ig1)
         isc%rhotw(ig1)=rhotw_save(ig2)
       enddo
       if (is>nsym/(i_time_rev+1)) then
         do ig1=1,X%ng
           isc%rhotw(ig1)=conjg(isc%rhotw(ig1))
         enddo
       endif
       isc%rhotw(1)=-conjg(DIP_q_dot_iR(ic,iv,ik_bz_mem,i_spin))
     else
       isc%is=(/ic,ik,is,i_spin/)
       isc%os=(/iv,ikp,isp,i_spin/)
       isc%qs=(/qindx_X(iq,ikbz,2),iq,1/)
       call scatterBamp(isc)
     endif
     !
     ! Filling the upper triangular part of the residual here ! 
     !-------------^^^^^---------------------------------------
     do ig2=1,X%ng
       call V_by_V_plus_V(ig2,eh_occ*isc%rhotw(ig2),conjg(isc%rhotw(:ig2)),Xo_residual(:ig2,ig2))
     enddo
     !
   enddo loop_bare_grid
   !
   do iw=1,Xw%n_freqs
     !
     if (.not.force_bare_X_G.and..not.Drude_pole) then
       GreenF(iw) = X_simple_GreenF(Xw%p(iw),coarse_grid_Pt(i_cg),X%ordering)
     endif
     !
!$omp critical
     do ig2=1,X%ng
       call V_by_V_plus_V(ig2,GreenF(iw),Xo_residual(:ig2,ig2),Xo(:ig2,ig2,iw))
     enddo
     if (.not.half_X_mat_only ) then 
       do ig2=1,X%ng
         call V_by_V_plus_V(X%ng-ig2,GreenF(iw),conjg(Xo_residual(ig2,ig2+1:)),Xo(ig2+1:,ig2,iw))
       enddo
     endif
!$omp end critical
     !
   enddo
   !
   call live_timing(steps=1)
   !
 enddo 
!$omp end do
 !
 ! Cleaning the omp region
 !-------------------------
 !
 if (iq==1) deallocate(rhotw_save)
 deallocate(isc%rhotw)
 deallocate(Xo_residual)
 !
 call live_timing()
 !
!$omp end parallel
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
#if defined _SC
 !
 ! Divide the head by w^2 in case a velocity gauge is used
 !=========================================================
 !
 if (trim(global_gauge)=='velocity') then
   do iw=1,Xw%n_freqs
     Xo(1,1,iw)=Xo(1,1,iw)/real(Xw%p(iw))**2
   enddo
 endif
 !
#endif
 !
 ! CLEAN
 !=======
 !
 call collision_reset(isc)
 call mem_est("Xo_WS")
 !
 current_iq=iq
 !
end subroutine
