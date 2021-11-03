!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine K_screened_interaction(X,Xw,q,io_X_err)
 !
 use pars,         ONLY:SP
 use stderr,       ONLY:intc
 use frequency,    ONLY:w_samp
 use drivers,      ONLY:l_td_hf,l_tddft,l_RIM_W
 use collision_el, ONLY:elemental_collision,elemental_collision_free
 use R_lattice,    ONLY:bz_samp,RIM_qpg,RIM_anisotropy,RIM_W,RIM_W_d,RIM_W_is_diagonal,&
&                       RIM_W_ng,RIM_vpW,RIM_vpW_d
 use D_lattice,    ONLY:i_time_rev,i_space_inv
 use X_m,          ONLY:X_alloc,X_t,X_mat,X_mat_d
 use BS,           ONLY:BS_n_g_W,BS_W_is_diagonal,BS_W
 use IO_int,       ONLY:io_control
 use IO_m,         ONLY:manage_action,OP_RD_CL,REP,VERIFY,NONE,&
&                       OP_RD,RD,RD_CL,RD_CL_IF_END
 use deviceXlib_m, ONLY:dev_memcpy
 use cuda_m,       ONLY:have_cuda
 !
#include<dev_defs.h>
#include<memory.h>
 !
 type(bz_samp) ::q
 type(X_t)     ::X
 type(w_samp)  ::Xw
 !
 integer,     intent(out) :: io_X_err
 !
 type(elemental_collision), target ::isc
 complex(SP), pointer DEV_ATTR :: gamp_d(:,:)
 real(SP)   :: rtmp
 integer    :: nibz
 !
 integer    :: BS_W_size2,i_g2,i_g3,i_g2_mat,i_g3_mat,iq_W
 logical    :: W_is_em1s,W_is_pp,W_is_unscreened,l_RIM_W_g
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
   call section('+','Screened interaction header I/O')
   !===================================================
   !
   if (X%ng<BS_n_g_W) call warning(" Using screening with less G than Weh interaction")
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
   if (W_is_em1s) call X_alloc('X',(/X%ng,X%ng,1/))
   if (W_is_pp)   call X_alloc('X',(/X%ng,X%ng,2/))
   !
 endif
 !
 BS_W_size2=BS_n_g_W
 if (BS_W_is_diagonal) BS_W_size2=1
 !
 ! When TR is present but not the SI X_mat indexes need to be exchanged 
 ! when the TR is applied
 !
 if (i_space_inv==0.and.i_time_rev==1.and..not.BS_W_is_diagonal) then
   YAMBO_ALLOC(BS_W,(BS_n_g_W,BS_W_size2,2*q%nibz))
 else
   YAMBO_ALLOC(BS_W,(BS_n_g_W,BS_W_size2,q%nibz))
 endif
 !
 YAMBO_ALLOC(isc%gamp,(BS_n_g_W,BS_W_size2))
 if (have_cuda) then
   YAMBO_ALLOC(isc%gamp_d,(BS_n_g_W,BS_W_size2))
   YAMBO_ALLOC(RIM_vpW_d,(RIM_W_ng,RIM_W_ng))
 else
  YAMBO_ALLOC(RIM_vpW,(RIM_W_ng,RIM_W_ng))
 endif
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
   call DEV_SUB(scatter_Gamp)(isc,'c')
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
#if defined _CUDA
     ! device <- host transfer
     X_mat_d=X_mat
     !
     ! CUDA Fortran workarounds
     gamp_d => isc%gamp_d
     nibz=q%nibz
#endif
     !
     if(l_RIM_W) then
       !$cuf kernel do(2) <<<*,*>>>
       do i_g3=1,RIM_W_ng
         do i_g2=1,RIM_W_ng
#if defined _CUDA
           RIM_vpW_d(i_g2,i_g3)= RIM_W_d(iq_W,i_g2,i_g3)
           if(i_g2==i_g3) RIM_vpW_d(i_g2,i_g2)=RIM_vpW_d(i_g2,i_g2)+2._SP*gamp_d(i_g2,i_g2)
#else
           RIM_vpW(i_g2,i_g3) = RIM_W(iq_W,i_g2,i_g3)
           if(i_g2==i_g3) RIM_vpW(i_g2,i_g2)=RIM_vpW(i_g2,i_g2)+2._SP*isc%gamp(i_g2,i_g2)
#endif
         enddo
       enddo
       !
     end if
     !
     if (BS_W_is_diagonal) then
       !$cuf kernel do(1) <<<*,*>>>
       do i_g2=1, BS_n_g_W
          i_g2_mat=min(i_g2,X%ng)
#if defined _CUDA
          BS_W(i_g2,1,iq_W)=real(X_mat_d(i_g2_mat,i_g2_mat,1),SP)*gamp_d(i_g2,1)
#else
          BS_W(i_g2,1,iq_W)=real(X_mat(i_g2_mat,i_g2_mat,1),SP)*isc%gamp(i_g2,1)
#endif
       enddo
     else if (.not.BS_W_is_diagonal) then
       !$cuf kernel do(1) <<<*,*>>>
       do i_g3=1,BS_n_g_W
         i_g3_mat=min(i_g3,X%ng)
         do i_g2=1,BS_n_g_W
           i_g2_mat=min(i_g2,X%ng)
           !
           !RIM_W support
           l_RIM_W_g=(l_RIM_W.and.i_g2<=RIM_W_ng.and.i_g3<=RIM_W_ng)
           if (RIM_W_is_diagonal.and.l_RIM_W_g) l_RIM_W_g=(i_g2==i_g3)
           !
           if (l_RIM_W_g) then
             !
#if defined _CUDA
             BS_W(i_g2,i_g3,iq_W) = cmplx(RIM_vpW_d(i_g2_mat,i_g3_mat)/2._SP, &
&                             aimag(X_mat_d(i_g2_mat,i_g3_mat,1))*real(gamp_d(i_g2_mat,i_g3_mat)),kind=SP)
             if (i_space_inv==0.and.i_time_rev==1) BS_W(i_g2_mat,i_g3_mat,nibz+iq_W)= cmplx(RIM_vpW_d(i_g3_mat,i_g2_mat)/2._SP, &
&                             aimag(X_mat_d(i_g3_mat,i_g2_mat,1))*real(gamp_d(i_g2_mat,i_g3_mat)),kind=SP)
#else
             BS_W(i_g2,i_g3,iq_W) = cmplx(RIM_vpW(i_g2_mat,i_g3_mat)/2._SP, &
&                             aimag(X_mat(i_g2_mat,i_g3_mat,1))*real(isc%gamp(i_g2_mat,i_g3_mat)),kind=SP)
             if (i_space_inv==0.and.i_time_rev==1) BS_W(i_g2_mat,i_g3_mat,q%nibz+iq_W)= cmplx(RIM_vpW(i_g3_mat,i_g2_mat)/2._SP, &
&                             aimag(X_mat(i_g3_mat,i_g2_mat,1))*real(isc%gamp(i_g2_mat,i_g3_mat)),kind=SP)
#endif
           else

#if defined _CUDA
             BS_W(i_g2,i_g3,iq_W)=X_mat_d(i_g2_mat,i_g3_mat,1)*gamp_d(i_g2,i_g3)
             if (i_space_inv==0.and.i_time_rev==1) BS_W(i_g2,i_g3,nibz+iq_W)=X_mat_d(i_g3_mat,i_g2_mat,1)*gamp_d(i_g2,i_g3)
#else
             BS_W(i_g2,i_g3,iq_W)=X_mat(i_g2_mat,i_g3_mat,1)*isc%gamp(i_g2,i_g3)
             if (i_space_inv==0.and.i_time_rev==1) BS_W(i_g2,i_g3,q%nibz+iq_W)=X_mat(i_g3_mat,i_g2_mat,1)*isc%gamp(i_g2,i_g3)
#endif
           end if
         enddo
       enddo
     endif
     !
   else
     !
     call dev_memcpy(BS_W(:,1,iq_W),DEV_VAR(isc%gamp)(:,1))
     !
   endif
   ! 
 enddo
 !
 ! Anisotropy correction. Note that isc%gamp(1,1)=RIM_anisotropy while
 ! the \delta(G,G') term must be multiplied by the standard RIM_qpg(1,1)
 !
 if (RIM_anisotropy/=0.) then
#ifdef _CUDA
     rtmp=BS_W(1,1,1)
     rtmp=rtmp+RIM_qpg(1,1,1)/2._SP-RIM_anisotropy/2._SP
     BS_W(1,1,1)=rtmp
#else
     BS_W(1,1,1)=BS_W(1,1,1)+RIM_qpg(1,1,1)/2._SP-RIM_anisotropy/2._SP
#endif
 endif
 !
 call elemental_collision_free(isc)
 !
 if (.not.W_is_unscreened) call X_alloc('X')
 !
end subroutine
