!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
#include<dev_defs.h>
!
subroutine K_screened_interaction(X,Xw,q,io_X_err)
 !
 use pars,         ONLY:SP
 use stderr,       ONLY:intc
 use com,          ONLY:exp_user
 use frequency,    ONLY:w_samp
 use drivers,      ONLY:l_td_hf,l_tddft,l_rim_w
 use collision_el, ONLY:elemental_collision,elemental_collision_free
 use R_lattice,    ONLY:bz_samp,RIM_qpg,RIM_anisotropy,DEV_VAR(RIM_W),RIM_W_is_diagonal,&
&                       RIM_W_ng
 use D_lattice,    ONLY:i_time_rev,i_space_inv
 use X_m,          ONLY:X_ALLOC_elemental,X_t,X_mat,DEV_VAR(X_mat)
 use BS,           ONLY:BS_n_g_W,BS_W_is_diagonal,BS_W,O1,O2,O_times_W
 use IO_int,       ONLY:io_control
 use IO_m,         ONLY:manage_action,OP_RD_CL,REP,VERIFY,NONE,&
&                       OP_RD,RD,RD_CL,RD_CL_IF_END
 use devxlib,      ONLY:devxlib_memcpy_d2d,devxlib_memcpy_h2d,devxlib_memcpy_d2h
 use gpu_m,        ONLY:have_gpu
 !
#include<y_memory.h>
 !
 type(bz_samp) ::q
 type(X_t)     ::X
 type(w_samp)  ::Xw
 !
 integer,     intent(out) :: io_X_err
 !
 type(elemental_collision), target ::isc
 complex(SP), pointer DEV_ATTR :: gamp_p(:,:)
 complex(SP), pointer DEV_ATTR :: X_mat_p(:,:,:)
 complex(SP):: ctmp(1)
 integer    :: nibz
 !
 integer    :: BS_W_size2,i_g2,i_g3,i_g2_mat,i_g3_mat,iq_W,NQ
 logical    :: W_is_em1s,W_is_pp,W_is_unscreened,l_RIM_W_g
 !
 real(SP), allocatable DEV_ATTR :: RIM_vpW(:,:)
 !
 ! I/O
 !
 integer           :: XID,IO_ACT
 integer, external :: io_X
 !
 ! 2021/02/11 to be modified for functionals with screening
 !
 W_is_unscreened=l_td_hf.or.l_tddft
 !
 if (W_is_unscreened) then
   !
   io_X_err=0
   !
 else
   !
   if(.not.l_rim_w) call section('+','Screened interaction header I/O')
   if(     l_rim_w) call section('=','Screened interaction header I/O')
   !===================================================
   !
   if (X%ng<BS_n_g_W) then
     if (     exp_user) call warning("Using screening with less G than Weh interaction")
     if (.not.exp_user) call   error("Using screening with less G than Weh interaction")
   endif
   !
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=XID)
   io_X_err=io_X(X,Xw,XID) 
   !
   if(io_X_err<0) return
   !
 endif
 !
 call elemental_collision_free(isc)
 !
 if (.not.W_is_unscreened) then
   !
   ! PP/Epsm1S DBs
   !
   W_is_em1s=X%whoami==2
   W_is_pp  =X%whoami==4
   !
   if (W_is_em1s) call X_ALLOC_elemental('X',(/X%ng,X%ng,1/))
   if (W_is_pp)   call X_ALLOC_elemental('X',(/X%ng,X%ng,2/))
   !
 endif
 !
 BS_W_size2=BS_n_g_W
 if (BS_W_is_diagonal) BS_W_size2=1
 !
 ! When TR is present but not the SI X_mat indexes need to be exchanged 
 ! when the TR is applied
 !
 NQ=q%nibz
 if (i_space_inv==0.and.i_time_rev==1.and..not.BS_W_is_diagonal) NQ=2*NQ
 !
 if (have_gpu) then
   YAMBO_ALLOC_GPU(BS_W,(BS_n_g_W,BS_W_size2,NQ))
 else
   YAMBO_ALLOC(BS_W,(BS_n_g_W,BS_W_size2,NQ))
 endif
 !
 YAMBO_ALLOC_GPU(O1,(BS_n_g_W))
 YAMBO_ALLOC_GPU(O2,(BS_n_g_W))
 YAMBO_ALLOC_GPU(O_times_W,(BS_n_g_W))
 !
 YAMBO_ALLOC(isc%gamp,(BS_n_g_W,BS_W_size2))
 if (have_gpu) then
   YAMBO_ALLOC_GPU(DEV_VAR(isc%gamp),(BS_n_g_W,BS_W_size2))
   YAMBO_ALLOC_GPU(RIM_vpW,(RIM_W_ng,RIM_W_ng))
 else
   YAMBO_ALLOC(RIM_vpW,(RIM_W_ng,RIM_W_ng))
 endif
 !
 gamp_p => DEV_VAR(isc%gamp)
 X_mat_p => DEV_VAR(X_mat)
 nibz=q%nibz
 !
 if (.not.W_is_unscreened) then
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=XID)
   io_X_err=io_X(X,Xw,XID)
   if (io_X_err<0) then
     call warning('Reading of io_X heared failed despite previous check.')
     return
   endif
 endif
 !
 do iq_W=1,q%nibz
   !
   isc%qs(2)=iq_W
   call scatter_Gamp_gpu(isc,'c')
   !
   if (.not.W_is_unscreened) then
     !
     IO_ACT=manage_action(RD_CL_IF_END,iq_W,1,q%nibz)
     call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/2*iq_W,2*iq_W+1/),ID=XID)
     io_X_err=io_X(X,Xw,XID)
     if (io_X_err<0) then
       call warning('Reading of io_X for iq='//trim(intc(iq_W))//' failed despite previous check.')
       return
     endif
     !
     forall(i_g2=1:X%ng) X_mat(i_g2,i_g2,1)=X_mat(i_g2,i_g2,1)+1._SP
     !
     ! device <- host transfer
     if (have_gpu) call devxlib_memcpy_h2d(X_mat_p,X_mat)
     !
     if(l_rim_w) then
       !
       !DEV_ACC_DEBUG data present(RIM_vpW,DEV_VAR(RIM_W),gamp_p)
       !DEV_ACC parallel loop collapse(2)
       !DEV_CUF kernel do(2) <<<*,*>>>
       do i_g3=1,RIM_W_ng
         do i_g2=1,RIM_W_ng
           RIM_vpW(i_g2,i_g3)= real(DEV_VAR(RIM_W)(1,iq_W,i_g2,i_g3),SP)
           if(i_g2==i_g3) RIM_vpW(i_g2,i_g2)=RIM_vpW(i_g2,i_g2)+2._SP*gamp_p(i_g2,i_g2)
         enddo
       enddo
       !DEV_ACC_DEBUG end data
       !
     end if
     !
     if (BS_W_is_diagonal) then
       !
       !DEV_ACC_DEBUG data present(BS_W,X_mat_p,gamp_p)
       !DEV_ACC parallel loop private(i_g2_mat)
       !DEV_CUF kernel do(1) <<<*,*>>>
       do i_g2=1, BS_n_g_W
         i_g2_mat=min(i_g2,X%ng)
         BS_W(i_g2,1,iq_W)=real(X_mat_p(i_g2_mat,i_g2_mat,1),SP)*gamp_p(i_g2,1)
       enddo
       !DEV_ACC_DEBUG end data
       !
     else if (.not.BS_W_is_diagonal) then
       !
       !DEV_ACC_DEBUG data present(BS_W,RIM_vpW,X_mat_p,gamp_p)
       !DEV_ACC parallel loop private(i_g3,i_g3_mat,i_g2,i_g2_mat,l_RIM_W_g)
       !DEV_CUF kernel do(1) <<<*,*>>>
       do i_g3=1,BS_n_g_W
         i_g3_mat=min(i_g3,X%ng)
         do i_g2=1,BS_n_g_W
           i_g2_mat=min(i_g2,X%ng)
           !
           !RIM_W support
           l_RIM_W_g=(l_rim_w.and.i_g2<=RIM_W_ng.and.i_g3<=RIM_W_ng)
           if (RIM_W_is_diagonal.and.l_RIM_W_g) l_RIM_W_g=(i_g2==i_g3)
           !
           if (l_RIM_W_g) then
             !
             BS_W(i_g2,i_g3,iq_W) = cmplx(RIM_vpW(i_g2_mat,i_g3_mat)/2._SP, &
&                             aimag(X_mat_p(i_g2_mat,i_g3_mat,1))*real(gamp_p(i_g2_mat,i_g3_mat)),kind=SP)
             if (i_space_inv==0.and.i_time_rev==1) &
&                             BS_W(i_g2_mat,i_g3_mat,nibz+iq_W)= cmplx(RIM_vpW(i_g3_mat,i_g2_mat)/2._SP, &
&                             aimag(X_mat_p(i_g3_mat,i_g2_mat,1))*real(gamp_p(i_g2_mat,i_g3_mat)),kind=SP)
             !
           else
             !
             BS_W(i_g2,i_g3,iq_W)=X_mat_p(i_g2_mat,i_g3_mat,1)*gamp_p(i_g2,i_g3)
             if (i_space_inv==0.and.i_time_rev==1) &
&                             BS_W(i_g2,i_g3,nibz+iq_W)=X_mat_p(i_g3_mat,i_g2_mat,1)*gamp_p(i_g2,i_g3)
             !
           endif
         enddo
       enddo
       !DEV_ACC_DEBUG end data
       !
     endif
     !
   else
     !
     call devxlib_memcpy_d2d(BS_W(:,1,iq_W),DEV_VAR(isc%gamp)(:,1))
     !
   endif
   ! 
 enddo
 !
 ! Anisotropy correction. Note that isc%gamp(1,1)=RIM_anisotropy while
 ! the \delta(G,G') term must be multiplied by the standard RIM_qpg(1,1)
 !
 if (RIM_anisotropy/=0.) then
 !
#ifdef _GPU
   ! d2h: ctmp=BS_W(1,1,1)
   call devxlib_memcpy_d2h(ctmp,BS_W(:,1,1),range1=(/1,1/))
   ! update
   ctmp(1:1)=ctmp(1:1)+RIM_qpg(1,1,1)/2._SP-RIM_anisotropy/2._SP
   ! h2d: BS_W(1,1,1)=ctmp
   call devxlib_memcpy_h2d(BS_W(:,1,1),ctmp,range1=(/1,1/))
#else
   BS_W(1,1,1)=BS_W(1,1,1)+RIM_qpg(1,1,1)/2._SP-RIM_anisotropy/2._SP
#endif
 endif
 !
 call elemental_collision_free(isc)
 !
 if (have_gpu) then
   YAMBO_FREE_GPU(RIM_vpW)
 endif
 YAMBO_FREE(RIM_vpW)
 !
 if (.not.W_is_unscreened) call X_ALLOC_elemental('X')
 !
end subroutine
