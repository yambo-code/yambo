!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine X_os(Xo,iq,fr,Xen,Xk,Xw,X)
 !
 ! Non interacting Xo
 !
 use pars,          ONLY:SP
 use wrapper,       ONLY:V_by_V_plus_V,V_copy
 use collision,     ONLY:ggwinfo,collision_reset
 use timing,        ONLY:live_timing
 use com,           ONLY:msg
 use stderr,        ONLY:intc
 use parallel_m,    ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use frequency,     ONLY:w_samp,bg_npts,cg_pt,cg_npts
 use interfaces,    ONLY:PARALLEL_index
 use D_lattice,     ONLY:nsym,DL_vol,i_time_rev,sop_inv,i_space_inv
 use electrons,     ONLY:levels,spin_occ
 use R_lattice,     ONLY:g_rot,qindx_X,bz_samp,q_norm 
 use wave_func,     ONLY:WF_load
 use memory_m,      ONLY:mem_est
 use X_m,           ONLY:X_t,X_poles,DIP_q_dot_iR,current_iq,X_poles_tab,&
&                        self_detect_E_range,half_X_mat_only,use_X_RIM
#if defined _SC 
 use drivers,       ONLY:l_sc_run,l_collisions_IO
 use SC,            ONLY:it_now
 use fields,        ONLY:global_gauge
#endif
#if defined _DISTRIBUTED
 use WF_distribute, ONLY: Distributed_Memory,WF_states_setup
#endif
 implicit none
 type(levels)         :: Xen
 type(bz_samp)        :: Xk
 type(X_t)            :: X
 type(w_samp)         :: Xw
 integer              :: iq,fr(2)
 complex(SP)          :: Xo(X%ng,X%ng,Xw%n(2))
 !
 ! Work Space
 !
 integer                 :: i1,i2,ik,is,ikp,ikbz,ikpbz,i_spin,&
&                           isp,iv,ic,g1,g2,g2_max,iw,iwmax,isave(4),n_poles 
 logical                 :: force_bare_X_G,skip_WF_load
 real(SP)                :: minmax_ehe(2),eh_occ
 type(PP_indexes)        :: px
 type(ggwinfo)           :: isc
 complex(SP)             :: GreenF(Xw%n(2)),drude_GreenF(Xw%n(2))
 complex(SP),allocatable :: rhotw_save(:),XoR(:,:)
 integer,    external    :: X_eh_setup
 complex(SP),external    :: X_simple_GreenF
 !
 ! Defaults & Setups
 !===================
 call PP_indexes_reset(px) 
 allocate(XoR(X%ng,X%ng))
 if (iq==1) allocate(rhotw_save(X%ng))
 Xo  = cmplx(0.,0.,SP)
 XoR = cmplx(0.,0.,SP)
 GreenF  = cmplx(0.,0.,SP)
 !
 ! Logicals  to skip wf_load and/or to use bare or RIM GF (no poles accumulation)
 !==========
 force_bare_X_G=associated(Xen%W).or.associated(Xen%Z).or.use_X_RIM.or.associated(Xen%GreenF)
 skip_WF_load=.false.
 !
#if defined _DISTRIBUTED
 if(Distributed_Memory) force_bare_X_G=.true.
#endif
 call mem_est("Xo_WS",(/size(XoR)+2*X%ng/))
 !
 !  Optical strengths
 !=====================
 if (iq==1) call Dipole_driver(Xen, Xk, X, X%q0)
 !
 call Drude(iq,fr,Xen,Xk,Xw,X,drude_GreenF,X%ordering)
 !
#if !defined _DISTRIBUTED
 ! WF load
 !=========
 if (.not.skip_WF_load) call WF_load(X%ng,maxval(qindx_X(:,:,2)),X%ib,(/1,Xk%nibz/),title='-X')
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
   !  This call is needed as Xw%p is deallocated inside
   !  the q-loop of X_em1
   !
   call FREQUENCIES_setup(Xw)
   !
#if defined _DISTRIBUTED
   if(Distributed_Memory) call X_poles_sort(cg_npts,bg_npts,n_poles,Xen,X,Xw)
#endif
   !
 endif 
 !
#if defined _DISTRIBUTED
 ! WF load
 !=========
 if (.not.skip_WF_load) then
    !
    call WF_states_setup(cg_npts=cg_npts,bg_npts=bg_npts,k=Xk,qpoint=iq,Xw=Xw)
    !
    call WF_states_setup(b_to_load=X%ib,report=.TRUE.)
    !
    call WF_load(X%ng,maxval(qindx_X(:,:,2)),X%ib,(/1,Xk%nibz/),title='-X')
    !
 endif
 !
#endif
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
 !
 g2_max=X%ng
 iwmax=1
 if (half_X_mat_only) g2_max=-1
 if (Xw%n(2)==1.and.half_X_mat_only) iwmax=2
 if (current_iq==0.and.half_X_mat_only ) call msg('s','[X] Upper matrix triangle filled')
 !
 ! Parallel pointers
 !===================
 allocate(px%weight_1D(cg_npts))
 px%weight_1D=bg_npts+Xw%n(2)
 call PARALLEL_index(px,(/cg_npts/))
 !
 ! LiveTiming Title
 !==================
#if defined _SC 
 if (l_sc_run.and..not.l_collisions_IO) then
   call live_timing('Xo @it'//trim(intc(it_now)),px%n_of_elements(myid+1))
 else
   call live_timing('Xo@q['//trim(intc(iq))//'] '//&
&                    trim(intc(fr(1)))//'-'//trim(intc(fr(2))),px%n_of_elements(myid+1))
 endif
#else
 call live_timing('Xo@q['//trim(intc(iq))//'] '//&
&                  trim(intc(fr(1)))//'-'//trim(intc(fr(2))),px%n_of_elements(myid+1))
#endif
 !
 n_poles=0
 !
 ! Here I prepare the scattering module.
 !                    ==================
 ! Note the collision_reset destrois the bw_plan
 !
 call collision_reset(isc)
 isc%ngrho=X%ng
 allocate(isc%rhotw(X%ng))
 !
 ! MAIN LOOP
 !===========
 select case(iq)
   !
   ! Gamma Point
   !
   case(1)
   !
   isave=0
   do i1 = 1,cg_npts
     n_poles = sum(bg_npts(1:i1-1))
     if (.not.px%element_1D(i1)) cycle
     if (iwmax==1) XoR=(0.,0.)
     if (cg_pt(i1)==0.) then
       GreenF=drude_GreenF/real(bg_npts(i1))
     else
       if (.not.force_bare_X_G) GreenF(1) = X_simple_GreenF(Xw%p(fr(1)),cg_pt(i1),X%ordering)
       if (force_bare_X_G) call X_bare_RIM_GreenF(iq,X_poles_tab(i1,:),fr,Xw,Xen,Xk,GreenF,X%ordering)
     endif
loop_bg: do i2 = 1,bg_npts(i1)
       n_poles = n_poles+1
       ikbz   = X_poles_tab(n_poles,1)
       iv     = X_poles_tab(n_poles,2)
       ic     = X_poles_tab(n_poles,3)
       i_spin = X_poles_tab(n_poles,4)
       ik = Xk%sstar(ikbz,1)
       is = Xk%sstar(ikbz,2)
       !
       isc%is = (/ic,ik,1,i_spin/)
       isc%os = (/iv,ik,1,i_spin/)
       isc%qs = (/1,1,1/)
       !
       ! Note the renormalization of the eh_occ=f(1-f) factor
       !
       !     n_spin n_sp_pol n_spinor  spin_occ eh_occ
       !       1        1        1         2      2
       !       2        1        2         1      1
       !       2        2        1         1      1
       !
       eh_occ = Xen%f(iv,ik,i_spin)*(spin_occ-Xen%f(ic,ik,i_spin))/&
&               spin_occ/real(Xk%nbz)/DL_vol
       if (any((/isave(1)/=iv,isave(2)/=ic,isave(3)/=ik,isave(4)/=i_spin/))) then
         call scatterBamp(isc)
         rhotw_save=isc%rhotw
         isave=(/iv,ic,ik,i_spin/)
       endif
       do g1=1,X%ng
         g2=g_rot(sop_inv(is),g1)
         isc%rhotw(g1)=rhotw_save(g2)
       enddo
       if (is>nsym/(i_time_rev+1)) isc%rhotw=conjg(isc%rhotw)
       isc%rhotw(1)=-conjg(DIP_q_dot_iR(ic,iv,ikbz,i_spin))
       !
       ! Filling the upper triangular part of the residual here ! 
       !             ^^^^^
       do g2=1,X%ng
         call V_by_V_plus_V(g2,GreenF(1)*eh_occ*isc%rhotw(g2),conjg(isc%rhotw(:g2)),XoR(:g2,g2))
       enddo
       !
     enddo loop_bg
     !
     do i2=iwmax,Xw%n(2)
       iw=i2+fr(1)-1
       if (.not.force_bare_X_G.and.cg_pt(i1)/=0.) GreenF(i2)=X_simple_GreenF(Xw%p(iw),cg_pt(i1),X%ordering)
       do g2=1,X%ng
         call V_by_V_plus_V(g2,GreenF(i2)/GreenF(1),XoR(:g2,g2),Xo(:g2,g2,i2))
         if (g2<g2_max) call V_by_V_plus_V(X%ng-g2,GreenF(i2)/conjg(GreenF(1)),conjg(XoR(g2,g2+1:)),Xo(g2+1:,g2,i2))
         !
       enddo
     enddo
     !
     call live_timing(steps=Xw%n(2)+bg_npts(i1))
     !
   enddo 
   !
   ! Any other Point
   !
   case(2:)
   !
   do i1=1,cg_npts
     n_poles=sum(bg_npts(1:i1-1))
     if (.not.px%element_1D(i1)) cycle
     if (iwmax==1) XoR=(0.,0.)
     if (.not.force_bare_X_G) GreenF(1)=X_simple_GreenF(Xw%p(fr(1)),cg_pt(i1),X%ordering)
     if (force_bare_X_G) call X_bare_RIM_GreenF(iq,X_poles_tab(i1,:),fr,Xw,Xen,Xk,GreenF,X%ordering)
     do i2=1,bg_npts(i1)
       n_poles=n_poles+1
       ikbz   = X_poles_tab(n_poles,1)
       iv     = X_poles_tab(n_poles,2)
       ic     = X_poles_tab(n_poles,3)
       i_spin = X_poles_tab(n_poles,4)
       ik=Xk%sstar(ikbz,1)
       is=Xk%sstar(ikbz,2)
       ikpbz=qindx_X(iq,ikbz,1)
       ikp=Xk%sstar(ikpbz,1)
       isp=Xk%sstar(ikpbz,2)
       isc%is=(/ic,ik,is,i_spin/)
       isc%os=(/iv,ikp,isp,i_spin/)
       isc%qs=(/qindx_X(iq,ikbz,2),iq,1/)
       eh_occ=Xen%f(iv,ikp,i_spin)*(spin_occ-Xen%f(ic,ik,i_spin))/&
&             spin_occ/real(Xk%nbz)/DL_vol
       call scatterBamp(isc)
       do g2=1,X%ng
         call V_by_V_plus_V(g2,GreenF(1)*eh_occ*isc%rhotw(g2),conjg(isc%rhotw(:g2)),XoR(:g2,g2))
       enddo
     enddo
     do i2=iwmax,Xw%n(2)
       iw=i2+fr(1)-1
       if (.not.force_bare_X_G) GreenF(i2)=X_simple_GreenF(Xw%p(iw),cg_pt(i1),X%ordering)
       do g2=1,X%ng
         call V_by_V_plus_V(g2,GreenF(i2)/GreenF(1),XoR(:g2,g2),Xo(:g2,g2,i2))
         if (g2<g2_max) call V_by_V_plus_V(X%ng-g2,GreenF(i2)/conjg(GreenF(1)),conjg(XoR(g2,g2+1:)),Xo(g2+1:,g2,i2))
       enddo
     enddo
     call live_timing(steps=Xw%n(2)+bg_npts(i1))
   enddo 
   !
 end select 
 !
 if (iwmax==2) then
   do g2=1,X%ng
     call V_copy(g2,XoR(:,g2),Xo(:,g2,1))
   enddo
 endif
 do i1=1,Xw%n(2)
   call PP_redux_wait(Xo(:,:,i1))
 enddo
 call live_timing
 !
 ! Symmetrize Xo when only half has been avaluated
 !=================================================
 !
 if (half_X_mat_only) then
   do i2=1,X%ng
     do i1=i2+1,X%ng
        if (i_space_inv==0) Xo(i1,i2,:)=conjg(Xo(i2,i1,:))
        if (i_space_inv==1) Xo(i1,i2,:)=Xo(i2,i1,:)
     enddo
   enddo
 endif
 !
#if defined _SC
 !
 ! Divide the head by w^2 in case a velocity gauge is used
 !=========================================================
 !
 if (trim(global_gauge)=='velocity') then
   do iw=fr(1),fr(2)
     Xo(1,1,iw)=Xo(1,1,iw)/real(Xw%p(iw))**2
   enddo
 endif
 !
#endif
 !
 ! CLEAN
 !=======
 !
 call PP_indexes_reset(px) 
 if (iq==1) deallocate(rhotw_save)
 deallocate(isc%rhotw,XoR)
 call mem_est("Xo_WS")
 call collision_reset(isc)
 !
 current_iq=iq
 !
end subroutine
