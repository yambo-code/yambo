!
!        Copyright (C) 2000-2020 the YAMBO team
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
 use drivers,      ONLY:l_td_hf
 use collision_el, ONLY:elemental_collision,elemental_collision_free
 use R_lattice,    ONLY:bz_samp,RIM_qpg,RIM_anisotropy
 use D_lattice,    ONLY:i_time_rev,i_space_inv
 use X_m,          ONLY:X_alloc,X_t,X_mat
#ifdef _CUDA
 use X_m,          ONLY:X_mat_d
#endif
 use BS,           ONLY:BS_n_g_W,BS_W_is_diagonal,BS_W
 use IO_m,         ONLY:io_control,manage_action,OP_RD_CL,REP,VERIFY,NONE,&
&                       OP_RD,RD,RD_CL,RD_CL_IF_END
 use deviceXlib_m, ONLY:dev_memcpy
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
#ifdef _CUDA
 complex(SP), pointer, device :: gamp_d(:,:)
 real(SP)   :: rtmp
 integer    :: nibz
#endif
 !
 integer    :: BS_W_size2,i_g2,i_g3,iq_W
 logical    :: W_is_em1s,W_is_pp
 !
 ! I/O
 !
 integer           :: XID,IO_ACT
 integer, external :: io_X
 ! 
 if (l_td_hf) then
   !
   io_X_err=0
   !
 else
   !
   call section('+','Screened interaction header I/O')
   !===================================================
   !
   ! X%ng are the G's in the X DB while BS_n_g_W the G's I want to read.
   ! Now to read only BS_n_g_W components I need to redefine X%ng
   !
   X%ng=BS_n_g_W
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=XID)
   io_X_err=io_X(X,Xw,XID) 
   !
   if(io_X_err<0) return
   !
   ! X%ng is set to BS_n_g_W to VERIFY if there are enough bands.
   ! From now on it is used to keep track of the actual sixe of W in the DB
   !
   X%ng=X%ng_db
   !
 endif
 !
 call elemental_collision_free(isc)
 !
 X%ng=BS_n_g_W
 if (.not.l_td_hf) then
   !
   ! PP/Epsm1S DBs
   !
   W_is_em1s=X%whoami==2
   W_is_pp  =X%whoami==4
   !
   if (W_is_em1s) call X_alloc('X',(/BS_n_g_W,BS_n_g_W,1/))
   if (W_is_pp)   call X_alloc('X',(/BS_n_g_W,BS_n_g_W,2/))
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
#ifdef _CUDA
   YAMBO_ALLOC(isc%gamp_d,(BS_n_g_W,BS_W_size2))
#endif
 !
 if (.not.l_td_hf) then
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
   call DEV_SUBNAME(scatter_Gamp)(isc,'c')
   !
   if (.not.l_td_hf) then
     !
     IO_ACT=manage_action(RD_CL_IF_END,iq_W,1,q%nibz)
     call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/2*iq_W,2*iq_W+1/),ID=XID)
     io_X_err=io_X(X,Xw,XID)
     if (io_X_err<0) then
       call warning('Reading of io_X for iq='//trim(intc(iq_W))//' failed despite previous check.')
       return
     endif
     !
     forall(i_g2=1:BS_n_g_W) X_mat(i_g2,i_g2,1)=X_mat(i_g2,i_g2,1)+1._SP
     !
#ifdef _CUDA
     ! device <- host transfer
     X_mat_d=X_mat
     !
     ! CUDA Fortran workarounds
     gamp_d => isc%gamp_d
     nibz=q%nibz
     !
     if (BS_W_is_diagonal) then
       !$cuf kernel do(1) <<<*,*>>>
       do i_g2=1, BS_n_g_W
          BS_W(i_g2,1,iq_W)=real(X_mat_d(i_g2,i_g2,1),SP)*gamp_d(i_g2,1)
       enddo
     else if (.not.BS_W_is_diagonal) then
       !$cuf kernel do(2) <<<*,*>>>
       do i_g3=1,BS_n_g_W
         do i_g2=1,BS_n_g_W
           BS_W(i_g2,i_g3,iq_W)=X_mat_d(i_g2,i_g3,1)*gamp_d(i_g2,i_g3)
           if (i_space_inv==0.and.i_time_rev==1) BS_W(i_g2,i_g3,nibz+iq_W)=X_mat_d(i_g3,i_g2,1)*gamp_d(i_g2,i_g3)
         enddo
       enddo
     endif
#else
     if (BS_W_is_diagonal) then
       forall(i_g2=1:BS_n_g_W) BS_W(i_g2,1,iq_W)=real(X_mat(i_g2,i_g2,1),SP)*isc%gamp(i_g2,1)
     else if (.not.BS_W_is_diagonal) then
       do i_g3=1,BS_n_g_W
         do i_g2=1,BS_n_g_W
           BS_W(i_g2,i_g3,iq_W)=X_mat(i_g2,i_g3,1)*isc%gamp(i_g2,i_g3)
           if (i_space_inv==0.and.i_time_rev==1) BS_W(i_g2,i_g3,q%nibz+iq_W)=X_mat(i_g3,i_g2,1)*isc%gamp(i_g2,i_g3)
         enddo
       enddo
     endif
#endif
     !
   else
     !
     call dev_memcpy(BS_W(:,1,iq_W),DEV_VARNAME(isc%gamp)(:,1))
     !
   endif
   ! 
 enddo
 !
 ! Anisotropy correction. Note that isc%gamp(1,1)=RIM_anisotropy while
 ! the \delta(G,G') term must be multiplied by the standard RIM_qpg(1,1)
 !
#ifdef _CUDA
 if (RIM_anisotropy/=0.) then
    rtmp=BS_W(1,1,1)
    rtmp=rtmp+RIM_qpg(1,1,1)/2._SP-RIM_anisotropy/2._SP
    BS_W(1,1,1)=rtmp
 endif
#else
 if (RIM_anisotropy/=0.) BS_W(1,1,1)=BS_W(1,1,1)+RIM_qpg(1,1,1)/2._SP-RIM_anisotropy/2._SP
#endif
 !
 call elemental_collision_free(isc)
 !
 if (.not.l_td_hf) call X_alloc('X')
 X%ng=X%ng_db
 !
end subroutine
