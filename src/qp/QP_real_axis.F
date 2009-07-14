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
subroutine QP_real_axis(X,Xen,Xk,en,k,q,qp,Xw,GW_iter)
 !
 ! This routine calculates 
 ! 
 ! [1] Real axis GW S.E.
 ! [2] Electronic Lifetimes
 !
 use pars,          ONLY:SP,schlen,pi,IP
 use units,         ONLY:HARTREE
 use drivers,       ONLY:l_life
 use electrons,     ONLY:levels,spin
 use frequency,     ONLY:w_samp,cg_index_bg,cg_npts,W_reset
 use timing,        ONLY:live_timing
 use com,           ONLY:msg
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use collision,     ONLY:ggwinfo,collision_reset
 use IO_m,          ONLY:io_control,REP,VERIFY,NONE,RD,RD_CL,OP_RD,&
&                        OP_WR_CL,OP_APP_WR_CL,RD_CL_IF_END,OP_RD_CL
 use QP_m,          ONLY:QP_t,QP_n_G_bands,QP_nb,QP_dSc_steps,&
&                        QP_Sc,QP_n_states,QP_G_damp,QP_table,QP_dSc_delta,&
&                        QP_W,QP_solver,QP_W_er,QP_W_dr,QP_n_W_freqs,&
&                        GF_energy_range,QP_G_dr,QP_Sc_steps
 use X_m,           ONLY:X_alloc,X_mat,X_t
 use functions,     ONLY:e2et,h2ht,bose_decay
 use memory_m,      ONLY:mem_est
 use R_lattice,     ONLY:qindx_S,bz_samp
 use D_lattice,     ONLY:nsym,i_space_inv,i_time_rev
 use wave_func,     ONLY:wf_load,wf_free
 use stderr,        ONLY:intc
 !
 implicit none
 type(levels) ::en,Xen
 type(bz_samp)::Xk,k,q
 type(X_t)    ::X
 type(QP_t)   ::qp
 type(w_samp) ::Xw
 integer      ::GW_iter
 !
 ! WorkSpace
 !
 type(ggwinfo)    ::isc,iscp
 type(pp_indexes) ::px
 type(w_samp)     ::Sc_W(qp%n_states),X_life_W(q%nibz)
 integer :: i1,i2,i3,i_or,iqbz,iqibz,ib,i_err,iq_to_start,iqs,&
&           iv4(4),io_err,XID,WID,QP_n_W_freqs_redux
 complex(SP) :: lrhotw(X%ng),X_mat_ws(X%ng,X%ng)
#if defined _DOUBLE
 complex(SP) :: zdotc,zdotu
#else
 complex(SP) :: cdotc,cdotu
#endif
 real(SP)            ::life_Fe,life_Fh
 integer, allocatable::life_W_table(:,:)
 integer, external   ::ioX,QP_state_extract,ioQP,X_em1
 !
 ! GPL_EXCLUDE_START
 !
 integer, external   ::QP_life_transitions
 !
 ! GPL_EXCLUDE_END
 !
 character(schlen)   ::ch,Sc_name
 logical             ::X_is_TR_rotated
 !
 ! Presets
 !
 call collision_reset(isc)
 call collision_reset(iscp)
 call pp_indexes_reset(px)
 if (l_life) then
   !
   ! GPL_EXCLUDE_START
   !
   do i1=1,q%nibz
     call W_reset(X_life_W(i1))
     X_life_W(i1)%dr=Xw%dr
   enddo
   !
   ! GPL_EXCLUDE_END
   !
 else
   do i1=1,qp%n_states
     call W_reset(Sc_W(i1))
   enddo
 endif
 !
 call k_expand(k)
 !
 ! ALLOCATION (W/Sc)
 !
 if (.not.l_life) then
   QP_n_W_freqs=Xw%n(1)
   QP_n_W_freqs_redux=Xw%n(1)
   allocate(QP_W(QP_n_states,QP_n_G_bands(2),QP_n_W_freqs),stat=i_err)
   call mem_est("QP_W",(/size(QP_W)/),errors=(/i_err/))
 endif
 !
 !Sc Energy points (1 type each QP state !)
 !
 if (trim(QP_solver)=='n') then
   do i1=1,qp%n_states
     Sc_W(i1)%n=QP_dSc_steps
     allocate(Sc_W(i1)%p(Sc_W(i1)%n(1)))
     forall (i2=1:QP_dSc_steps) Sc_W(i1)%p(i2)=qp%E_bare(i1)+(i2-1)*QP_dSc_delta
   enddo
 else if (trim(QP_solver)=='g') then
   !
   ! I need to put to 0. the G damping embodied in QP_W2Sc
   !
   QP_G_damp=0.
   !
   do i1=1,qp%n_states
     Sc_W(i1)%n =QP_Sc_steps
     Sc_W(i1)%dr=QP_G_dr
     call GF_energy_range(i1,Sc_W(i1),en%E)
     QP_Sc_steps=Sc_W(i1)%n(1)
   enddo
 endif
 !
 Sc_name="G"//trim(intc(GW_iter))
 !
 if (GW_iter==0) call section('+',trim(Sc_name)//"W0 on the real axis")
 if (GW_iter> 0) call section('=',trim(Sc_name)//"W0 on the real axis")
 !
 if (.not.l_life) call msg('r', '[GW] Bands range     :',QP_n_G_bands)
 call msg('r', '[GW] G damping   [ev]:',QP_G_damp*HARTREE)
 call msg('r','')
 iv4=(/1,1,0,0/)
 do while(QP_state_extract(iv4)>0)
   write (ch,'(4(a,i3.3))') 'QP @ K ',iv4(1),' - ',iv4(2),' : b ',iv4(3),' - ',iv4(4)
   call msg('r',trim(ch))
 enddo
 call msg('r','')
 !
 ! W DB 
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=WID)
 io_err=ioQP('W',qp,WID)
 !
 iq_to_start=1
 if (io_err>0) iq_to_start=io_err
 !
 if (io_err==0) then
   if (QP_solver=="n".or.QP_solver=="g") then
     QP_Sc=cmplx(0.,0.,SP)
     call live_timing(trim(Sc_name)//'W0',q%nbz)
     do iqbz=1,q%nbz
       if (iqbz< q%nbz) call io_control(ACTION=RD,COM=NONE,SEC=(/2+iqbz/),ID=WID)
       if (iqbz==q%nbz) call io_control(ACTION=RD_CL,COM=NONE,SEC=(/2+iqbz/),ID=WID)
       io_err=ioQP('W',qp,WID)
       !
       ! Xw%p is stored in ioX, not in ioW !
       !
       Xw%er=QP_W_er
       Xw%dr=QP_W_dr
       Xw%n=QP_n_W_freqs
       call freqs_setup(Xw)
       ! 
       call QP_W2Sc(iqbz,k,en,Xw,Sc_W)
       call live_timing(steps=1)
     enddo
     deallocate(QP_W)
     call mem_est("QP_W")
     call live_timing()
     return
   endif
   !
   if (QP_solver=="s") return
   !
 endif
 !
 ! Lifetimes Transitions 
 !
 ! GPL_EXCLUDE_START
 !
 if (l_life) then
   !
   call section('=','Lifetimes Transitions Selector')
   !
   QP_n_W_freqs=QP_life_transitions(-1,en,k,q,X_life_W(1))
   allocate(life_W_table(q%nibz,QP_n_W_freqs))
   call mem_est("life_W_table",(/size(life_W_table)/),(/IP/))
   life_W_table=0
   QP_n_W_freqs_redux=-1
   do iqibz=1,q%nibz
     !
     X_life_W(iqibz)%dr=Xw%dr
     !
     i1=QP_life_transitions(iqibz,en,k,q,X_life_W(iqibz))
     life_W_table(iqibz,:size(cg_index_bg))=cg_index_bg(:)
     !
     QP_n_W_freqs_redux=max(QP_n_W_freqs_redux,cg_npts)
     deallocate(cg_index_bg)
     call mem_est("CGi")
     !
   enddo
   do iqibz=1,q%nibz
     X%iq=iqibz
     i_err=X_em1(Xen,Xk,q,X,X_life_W(iqibz),iqibz>1)
     if (i_err==0) exit
   enddo
 endif
 !
 ! GPL_EXCLUDE_END
 !
 ! WFs
 !
 call wf_load(X%ng,maxval(qindx_S(:,:,2)),&
&            (/QP_n_G_bands(1),max(QP_n_G_bands(2),QP_nb)/),(/1,k%nibz/),title='-Sc')
 !
 ! Test the spatial Inversion
 !
 call space_inv_by_wf(en,Xk)
 !
 ! ALLOCATION (isc)
 !
 call X_alloc('X',(/X%ng,X%ng,QP_n_W_freqs_redux/))
 !
 allocate(isc%gamp(X%ng,X%ng),isc%rhotw(X%ng),iscp%rhotw(X%ng),stat=i_err)
 call mem_est("ISC-GAMP",(/X%ng**2+2*X%ng/),errors=(/i_err/))
 !
 ! MPI index
 call par_indexes(px,(/QP_n_states,QP_n_G_bands(2)/),low_range=(/1,QP_n_G_bands(1)/))
 !
 QP_Sc=cmplx(0.,0.,SP)
 isc%ngrho=X%ng
 isc%iqref=0
 !
 ! Main Loop
 !===========
 !
 call pp_redux_wait
 call live_timing(trim(Sc_name)//'W0',px%stps(myid+1)*(q%nbz-iq_to_start+1))
 !
 do iqbz=iq_to_start,q%nbz
   !
   if (.not.l_life) QP_W=(0.,0.)
   !
   isc%qs(2:)=(/q%sstar(iqbz,1),q%sstar(iqbz,2)/)
   iqibz=isc%qs(2)
   iqs  =isc%qs(3)
   !
   if (iqibz/=isc%iqref) then
     call scatterGamp(isc,'c')
     !
     ! X I/O
     !
     if (iqbz==iq_to_start) call io_control(ACTION=OP_RD,COM=NONE,&
&                                SEC=(/1,2,2*iqibz+1/),ID=XID)
     if (iqbz/=iq_to_start) call io_control(ACTION=RD_CL_IF_END,COM=NONE,&
&                                SEC=(/2*iqibz,2*iqibz+1/),ID=XID)
     if (q%nbz==1) call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2,3/),ID=XID)
     !
     ! GPL_EXCLUDE_START
     !
     if (l_life) then
       io_err=ioX(X,X_life_W(iqibz),XID)
       call X_delta_part(X,X_life_W(iqibz),isc%gamp)
     else
       !
       ! GPL_EXCLUDE_END
       !
       io_err=ioX(X,Xw,XID)
       !
       ! Xw%er/dr/n are not known here and are not read from ioX so
       ! I need to redefine them in terms of Xw%p (read from ioX)
       !
       Xw%er=(/real(Xw%p(1)),real(Xw%p(Xw%n(1)))/)
       Xw%dr=(/aimag(Xw%p(1)),aimag(Xw%p(Xw%n(1)))/)
       call X_delta_part(X,Xw,isc%gamp)
       !
       ! GPL_EXCLUDE_START
       !
     endif
     !
     if (l_life) QP_n_W_freqs=0
     !
     ! GPL_EXCLUDE_END
     !
     X_is_TR_rotated=.false.
     !
   endif
   !
   if (iqs>nsym/(i_time_rev+1) .and. i_space_inv == 0 .and..not.X_is_TR_rotated) then
     X_is_TR_rotated=.true.
     do i3=1,Xw%n(1)
       forall(i1=1:X%ng,i2=1:X%ng) X_mat_ws(i2,i1)=X_mat(i1,i2,i3)
       X_mat(:,:,i3)=X_mat_ws(:,:)
     enddo
   endif
   !
   do i1=1,QP_n_states
     !
     isc%is=(/QP_table(i1,1),QP_table(i1,3),1,spin(QP_table(i1,:))/)
     isc%os(2:)=(/k%sstar(qindx_S(isc%is(2),iqbz,1),:),spin(QP_table(i1,:))/)
     iscp%is=(/QP_table(i1,2),QP_table(i1,3),1,spin(QP_table(i1,:))/)
     !
     isc%qs(1)=qindx_S(QP_table(i1,3),iqbz,2)
     iscp%qs=isc%qs
     !
     do ib=QP_n_G_bands(1),QP_n_G_bands(2)
       !
       isc%os(1)=ib
       i_or=IOR(e2et(isc%is(:2),isc%os(:2),en,life_Fe),&
&               h2ht(isc%is(:2),isc%os(:2),en,life_Fh))
       if (l_life) QP_n_W_freqs=QP_n_W_freqs+i_or
       !
       if (.not.px%i2p(i1,ib)) cycle
       !
       iscp%os=isc%os
       call live_timing(steps=1)
       !
       if (l_life.and.i_or==0) cycle
       !
       call scatterBamp(isc)
       iscp%rhotw=isc%rhotw
       !
       if (any(isc%is/=iscp%is)) call scatterBamp(iscp)
       !
       ! GPL_EXCLUDE_START
       !
       if (l_life) then
         !
         i2=life_W_table(iqibz,QP_n_W_freqs)
         do i3=1,X%ng
#if defined _DOUBLE
           lrhotw(i3)=zdotu(X%ng,isc%rhotw,1,X_mat(1,i3,i2),1)
#else
           lrhotw(i3)=cdotu(X%ng,isc%rhotw,1,X_mat(1,i3,i2),1)
#endif
         enddo
         !       
         ! To compensate the Tel/w divergence of the Bose function at finite
         ! Tel I multiply the X_mat function by [w/(Tel*Bose_E_cut)]^2
         !
         ! Note that the same procedure is applied in QP_W2Sc when S_c is
         ! calculated
         ! 
#if defined _DOUBLE
         qp%E(i1)=qp%E(i1)-(0.,2.)*pi*(life_Fe+life_Fh)*&
&                 bose_decay( real(X_life_W(iqibz)%p(i2)) )*&
&                 zdotc(X%ng,iscp%rhotw,1,lrhotw,1)
#else
         qp%E(i1)=qp%E(i1)-(0.,2.)*pi*(life_Fe+life_Fh)*&
&                 bose_decay( real(X_life_W(iqibz)%p(i2)) )*&
&                 cdotc(X%ng,iscp%rhotw,1,lrhotw,1)
#endif
         !
       else ! .not.l_life
         !
         ! GPL_EXCLUDE_END
         !
         do i2=1,QP_n_W_freqs
           !
           do i3=1,X%ng
#if defined _DOUBLE
             lrhotw(i3)=zdotu(X%ng,isc%rhotw,1,X_mat(1,i3,i2),1)
#else
             lrhotw(i3)=cdotu(X%ng,isc%rhotw,1,X_mat(1,i3,i2),1)
#endif
           enddo
           !
#if defined _DOUBLE
           QP_W(i1,ib,i2)=-2.*zdotc(X%ng,iscp%rhotw,1,lrhotw,1)
#else
           QP_W(i1,ib,i2)=-2.*cdotc(X%ng,iscp%rhotw,1,lrhotw,1)
#endif
         enddo
         !
         ! GPL_EXCLUDE_START
         !
       endif
       !
       ! GPL_EXCLUDE_END
       !
     enddo
   enddo
   !
   call pp_redux_wait
   call pp_redux_wait(QP_W)
   !
   ! To be written in W
   !
   if (.not.l_life) then
     !
     QP_W_er=Xw%er
     QP_W_dr=Xw%dr
     QP_n_W_freqs=Xw%n(1)
     !
     ! Only if I am doing secant solver of Dyson I store on disk
     ! the W m.e. 
     !
     if (trim(QP_solver)=='n'.or.trim(QP_solver)=='g') then
       call QP_W2Sc(iqbz,k,en,Xw,Sc_W)
     else
       !
       if (iqbz==1) then
         call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2,3/),ID=WID)
       else
         call io_control(ACTION=OP_APP_WR_CL,COM=REP,SEC=(/2+iqbz/),ID=WID)
       endif
       io_err=ioQP('W',qp,WID)
       !
     endif
     !
   endif
   !
 enddo 
 !
 call live_timing()
 call pp_redux_wait()
 !
 ! CLEAN
 !
 deallocate(isc%gamp,isc%rhotw,iscp%rhotw)
 call mem_est("ISC-GAMP")
 !
 if (l_life) then
   deallocate(life_W_table)
   call mem_est("life_W_table")
   call pp_redux_wait(qp%E)
 else
   if (trim(QP_solver)=='n'.or.trim(QP_solver)=='g') then
     deallocate(QP_W)
     call mem_est("QP_W")
   endif
 endif
 if (l_life) then
   do i1=1,q%nibz
     call W_reset(X_life_W(i1))
   enddo
 else
   do i1=1,qp%n_states
     call W_reset(Sc_W(i1))
   enddo
 endif
 !
 call X_alloc('X')
 call wf_free()
 call collision_reset(isc)
 call collision_reset(iscp)
 call pp_indexes_reset(px)
 call pp_redux_wait()
 !
end subroutine
