!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
 ! Non interacting X with SPIN support.
 !
 use pars,          ONLY:SP
 use units,         ONLY:HARTREE
 use collision,     ONLY:ggwinfo,collision_reset
 use timing,        ONLY:live_timing
 use com,           ONLY:msg
 use stderr,        ONLY:intc
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use frequency,     ONLY:w_samp,bg_npts,cg_pt,cg_npts
 use par_indexes_m, ONLY:par_indexes
 use D_lattice,     ONLY:nsym,DL_vol,i_time_rev,sop_inv,i_space_inv
 use electrons,     ONLY:levels,spin_occ
 use R_lattice,     ONLY:g_rot,qindx_X,bz_samp,q_norm 
 use wave_func,     ONLY:wf_load
 use memory_m,      ONLY:mem_est
 use X_m,           ONLY:X_t,X_poles,DIP_iq_dot_r,iq_now,X_poles_tab,&
&                        self_detect_E_range,half_X_mat_only,use_X_RIM,Eval_P_and_P2_only
#if defined _SC
 use drivers,       ONLY:l_sc_run
 use SC,            ONLY:it_now
#endif
#if defined _RAS
 use optcut,                ONLY : loptcut, Xcut_rhoq0
 use analyse_peaks,         ONLY : lAnalysePeaks
 use ras_module,            ONLY : lras, lreels 
#endif
#if defined _DIPOLE_RIM
 use parser_m,              ONLY :parser
 use electrons,             ONLY : l_dipole_RIM
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
 integer              :: i1,i2,ik,is,ikp,ikbz,ikpbz,i_spin,&
&                        isp,iv,ic,g1,g2,g2_max,g2_min,iw,iwmax,isave(4),n_poles 
 logical              :: force_bare_X_G,skip_WF_load
 real(SP)             :: minmax_ehe(2),drude_factor,eh_occ
 type(pp_indexes)     :: px
 type(ggwinfo)        :: isc
 complex(SP)             :: Gf(Xw%n(2)),drude_Gf(Xw%n(2))
 complex(SP),allocatable :: rhotw_save(:),XoR(:,:)
 integer,    external :: X_eh_setup
 complex(SP),external :: X_Gf,X_drude
#if defined _RAS
 complex(SP)          :: iscut
#endif
#if defined _DIPOLE_RIM
 real(SP)                :: Ec_m_Ev
 complex(SP),allocatable :: Gf_rhoq0(:)
#endif
 !
 ! Defaults & Setups
 !
 call pp_indexes_reset(px) 
 allocate(XoR(X%ng,X%ng))
 if (iq==1) allocate(rhotw_save(X%ng))
 Xo  = cmplx(0.,0.,SP)
 XoR = cmplx(0.,0.,SP)
 Gf  = cmplx(0.,0.,SP)
 drude_Gf = cmplx(0.,0.,SP)
 !
 force_bare_X_G=associated(Xen%W).or.associated(Xen%Z).or.use_X_RIM
 skip_WF_load=.false.
 g2_min=1
 !
 ! Project's dependent presets
 !
#if defined _MANYK
 if(iq.ne.1) force_bare_X_G=.true.
#endif
#if defined _RAS || _MANYK
 skip_WF_load = iq==1.and.X%ng==1 
#endif
#if defined _RAS
 g2_min=2
#endif
 !
 call mem_est("Xo_WS",(/size(XoR)+2*X%ng/))
 !
 !  Optical strengths
 !
 if (iq==1) call Dipole_driver(Xen, Xk, X, X%q0)
 !
#if defined _RAS
 if (iq == 1.and.&
&    (lras.or.lreels).and.loptcut) call Dipole_driver_cut( Xen, Xk, X, X%q0)
#endif
 !
 ! Drude Contrib.
 !
 if (all((/real(X%Wd)>0.,aimag(X%Wd)>0.,&   ! Non zero Drude Freq.
&          Xen%kf>0,Xen%nbf/=Xen%nbm,&      ! Metallic system 
&          iq==1/))) then                   ! Optical response
   !
   call msg('nrs','[X] Drude contribution @[ev]:',(/real(X%Wd),aimag(X%Wd)/)*HARTREE)
   !
   ! drude_factor is the eh_occ factor calculated in the Xo loop
   ! evaluated at the fermi state
   !
   drude_factor=Xen%f(Xen%bf,Xen%kf,Xen%sf)*&
&               (spin_occ-Xen%f(Xen%bf,Xen%kf,Xen%sf))/&
&               spin_occ/real(Xk%nbz)/DL_vol
   do iw=fr(1),fr(2)
     drude_Gf(iw-fr(1)+1)=X_drude(real(Xw%p(iw)),X%Wd,q_norm(1))/drude_factor
   enddo
 endif
 !
 ! WF load
 !
 if (.not.skip_WF_load) call wf_load(X%ng,maxval(qindx_X(:,:,2)),X%ib,(/1,Xk%nibz/),title='-X')
 !
 if (iq/=iq_now) then
   !
   n_poles=X_eh_setup(-iq,X,Xen,Xk,minmax_ehe)
   !
   allocate(X_poles_tab(n_poles,4)) 
   call mem_est("X_poles_tab",(/size(X_poles_tab)/))
   !
   if (.not.force_bare_X_G) call coarse_grid('X',X_poles,n_poles,X%cg_percentual)
   if (force_bare_X_G)      call coarse_grid('X',X_poles,n_poles,0.)
   !
   n_poles=X_eh_setup(iq,X,Xen,Xk,minmax_ehe)
   deallocate(X_poles)
   !
   if (self_detect_E_range) Xw%er=minmax_ehe
   !
#if !defined _REELS
   !
   !  This call is needed as Xw%p is deallocated inside
   !  the q-loop of X_em1
   !
   call freqs_setup(Xw)
#  endif
   !
 endif 
 !
 ! Time-Rev is Spatial Inv => only half X is eval
 !                            ===================
 call space_inv_by_wf(Xen,Xk)
 !
 half_X_mat_only=i_space_inv==1
 if (.not.half_X_mat_only) then
   half_X_mat_only= all( aimag(Xw%p(:))<1.E-4 ).and. all( real(Xw%p(:))<1.E-4 )
 endif
 g2_max=X%ng
 iwmax=1
 if (half_X_mat_only) g2_max=-1
 if (Xw%n(2)==1.and.half_X_mat_only) iwmax=2
 if (iq_now==0.and.half_X_mat_only ) call msg('s','[X] Upper matrix triangle filled')
#if defined _DIPOLE_RIM
 call parser('DoDipole',l_dipole_RIM)
 if(.not.use_X_RIM.or.X%long_gauge) l_dipole_RIM=.false.
 if(l_dipole_RIM) then
   allocate(Gf_rhoq0(Xw%n(2)))
   call msg('nrs','[X] Semi-Interpolation of rho_q0 elements')
 endif
#endif
 !
 ! Parallel pointers
 !
 allocate(px%i1wts(cg_npts))
 px%i1wts=bg_npts+Xw%n(2)
 call par_indexes(px,(/cg_npts/))
 !
#if defined _SC
 if (l_sc_run) then
   call live_timing('Xo @it'//trim(intc(it_now)),px%stps(myid+1))
 else
   call live_timing('Xo@q['//trim(intc(iq))//'] '//&
&                    trim(intc(fr(1)))//'-'//trim(intc(fr(2))),px%stps(myid+1))
 endif
#else
 call live_timing('Xo@q['//trim(intc(iq))//'] '//&
&                  trim(intc(fr(1)))//'-'//trim(intc(fr(2))),px%stps(myid+1))
#endif
 !
 n_poles=0
 !
 ! Here I prepare the scattering module.
 ! Note the collision_reset destrois the bw_plan
 !
 call collision_reset(isc)
 isc%ngrho=X%ng
 allocate(isc%rhotw(X%ng))
 !
 select case(iq)
   !
   ! Gamma Point
   !
   case(1)
   !
   isave=0
   do i1 = 1,cg_npts
     n_poles = sum(bg_npts(1:i1-1))
     if (.not.px%i1p(i1)) cycle
     if (iwmax==1) XoR=(0.,0.)
     if (cg_pt(i1)==0.) then
       Gf=drude_Gf/real(bg_npts(i1))
     else
       if (.not.force_bare_X_G) Gf(1) = X_Gf(Xw%p(fr(1)),cg_pt(i1),X%ordering)
#if defined _DIPOLE_RIM
       if (l_dipole_RIM) call X_dipole_RIM(iq,X_poles_tab(i1,:),fr,Xw,Xen,Xk,Gf_rhoq0,X%ordering)
#endif
       if (force_bare_X_G) call X_bare_RIM_Gf(iq,X_poles_tab(i1,:),fr,Xw,Xen,Xk,Gf,X%ordering)
     endif
     do i2 = 1,bg_npts(i1)
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
#if defined _DIPOLE_RIM
       !
       Ec_m_Ev=Xen%E(ic,ik,i_spin)-Xen%E(iv,ik,i_spin)
       !
#endif
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
#if defined _RAS
       if(iq==1 .and. X%ng==1 ) isave=(/iv,ic,ik,i_spin/)
#endif
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
       isc%rhotw(1)=-conjg(DIP_iq_dot_r(ic,iv,ikbz,i_spin))
       !
#if defined _RAS 
       !-----------------------------------------------------------------------------
       if((lras.or.lreels).and.loptcut) then
         iscut= -conjg( Xcut_rhoq0(ic,iv,ikbz,i_spin) )
       else
         iscut= -conjg( DIP_iq_dot_r(ic,iv,ikbz,i_spin) )
       endif
       !
       XoR(1,1)=XoR(1,1)+Gf(1)*eh_occ*iscut*conjg(isc%rhotw(1))
       !-----------------------------------------------------------------------------
#endif 
       !
       ! Filling the upper triangular part of the residual here ! 
       !             ^^^^^
       do g2=g2_min,X%ng
#if defined _DIPOLE_RIM
        if(g2.eq.1.and.l_dipole_RIM) then
           !      
           XoR(1,1)=XoR(1,1)+Gf_rhoq0(1)*eh_occ*isc%rhotw(1)*conjg(isc%rhotw(1))*Ec_m_Ev**2
           cycle
        endif
#endif
#if defined _DOUBLE
         call zaxpy(g2,Gf(1)*eh_occ*isc%rhotw(g2),conjg(isc%rhotw(:g2)),1,XoR(:g2,g2),1)
#else
         call caxpy(g2,Gf(1)*eh_occ*isc%rhotw(g2),conjg(isc%rhotw(:g2)),1,XoR(:g2,g2),1)
#endif
       enddo
     enddo
     !
     do i2=iwmax,Xw%n(2)
       iw=i2+fr(1)-1
       if (.not.force_bare_X_G.and.cg_pt(i1)/=0.) Gf(i2)=X_Gf(Xw%p(iw),cg_pt(i1),X%ordering)
       do g2=1,X%ng
#if defined _DIPOLE_RIM
          if(g2.eq.1.and.l_dipole_RIM) then
            Xo(1,1,i2)=Xo(1,1,i2)+XoR(1,1)*Gf_rhoq0(i2)/Gf_rhoq0(1)        
            cycle
          endif
#endif
#if defined _DOUBLE
         call zaxpy(g2,Gf(i2)/Gf(1),XoR(:g2,g2),1,Xo(:g2,g2,i2),1)
         if (g2<g2_max) call zaxpy(X%ng-g2,Gf(i2)/conjg(Gf(1)),conjg(XoR(g2,g2+1:)),&
&                                  1,Xo(g2+1:,g2,i2),1)
#else
         call caxpy(g2,Gf(i2)/Gf(1),XoR(:g2,g2),1,Xo(:g2,g2,i2),1)
         if (g2<g2_max) call caxpy(X%ng-g2,Gf(i2)/conjg(Gf(1)),conjg(XoR(g2,g2+1:)),&
&                                  1,Xo(g2+1:,g2,i2),1)
#endif
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
     if (.not.px%i1p(i1)) cycle
     if (iwmax==1) XoR=(0.,0.)
     if (.not.force_bare_X_G) Gf(1)=X_Gf(Xw%p(fr(1)),cg_pt(i1),X%ordering)
     if (force_bare_X_G) call X_bare_RIM_Gf(iq,X_poles_tab(i1,:),fr,Xw,Xen,Xk,Gf,X%ordering)
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
&           spin_occ/real(Xk%nbz)/DL_vol
       call scatterBamp(isc)
       do g2=1,X%ng
#if defined _DOUBLE
         call zaxpy(g2,Gf(1)*eh_occ*isc%rhotw(g2),conjg(isc%rhotw(:g2)),1,XoR(:g2,g2),1)
#else
         call caxpy(g2,Gf(1)*eh_occ*isc%rhotw(g2),conjg(isc%rhotw(:g2)),1,XoR(:g2,g2),1)
#endif
       enddo
     enddo
     do i2=iwmax,Xw%n(2)
       iw=i2+fr(1)-1
       if (.not.force_bare_X_G) Gf(i2)=X_Gf(Xw%p(iw),cg_pt(i1),X%ordering)
       do g2=1,X%ng
#if defined _DOUBLE
         call zaxpy(g2,Gf(i2)/Gf(1),XoR(:g2,g2),1,Xo(:g2,g2,i2),1)
         if (g2<g2_max) call zaxpy(X%ng-g2,Gf(i2)/conjg(Gf(1)),conjg(XoR(g2,g2+1:)),&
&                                  1,Xo(g2+1:,g2,i2),1)
#else
         call caxpy(g2,Gf(i2)/Gf(1),XoR(:g2,g2),1,Xo(:g2,g2,i2),1)
         if (g2<g2_max) call caxpy(X%ng-g2,Gf(i2)/conjg(Gf(1)),conjg(XoR(g2,g2+1:)),&
&                                  1,Xo(g2+1:,g2,i2),1)
#endif
       enddo
     enddo
     call live_timing(steps=Xw%n(2)+bg_npts(i1))
   enddo 
   !
 end select 
 !
 if (iwmax==2) then
   do g2=1,X%ng
#if defined _DOUBLE
     call zcopy(g2,XoR(1,g2),1,Xo(1,g2,1),1)
#else
     call ccopy(g2,XoR(1,g2),1,Xo(1,g2,1),1)
#endif
   enddo
 endif
 do i1=1,Xw%n(2)
   call pp_redux_wait(Xo(:,:,i1))
 enddo
 call live_timing
 !
 ! Symmetrize Xo when only half has been avaluated
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
 ! CLEAN
 !
 call pp_indexes_reset(px) 
 if (iq==1) deallocate(rhotw_save)
 deallocate(isc%rhotw,XoR)
 call mem_est("Xo_WS")
 call collision_reset(isc)
 !
 iq_now=iq
 !
end subroutine
