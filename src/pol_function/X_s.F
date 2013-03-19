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
subroutine X_s(iq,X,wv)
 !
 use pars,          ONLY:SP,pi,cZero
 use drivers,       ONLY:l_bs_fxc,l_alda_fxc,l_lrc_fxc
 use timing,        ONLY:live_timing
 use memory_m,      ONLY:mem_est
 use parallel_m,    ONLY:PP_redux_wait,PAR_COM_Q_A2A,PAR_COM_INV_INDEX,PAR_COM_INV,&
&                        PP_indexes,PP_indexes_reset,PAR_IND_INV_ID
 use R_lattice,     ONLY:bare_qpg,q_norm
 use frequency,     ONLY:w_samp
 use interfaces,    ONLY:PARALLEL_index,PARALLEL_live_message
 use stderr,        ONLY:intc
 use X_m,           ONLY:X_t,X_mat,X_fxc
 use matrix_operate,ONLY:mat_dia_inv,INV,USE_SVD
 use TDDFT,         ONLY:FXC_n_g_corr,FXC,FXC_n_mem_freqs,&
&                        FXC_LRC_alpha,FXC_LRC_beta,FXC_SVD_digits,ioBS_Fxc
 use IO_m,          ONLY:io_control,OP_RD_CL,NONE
 !
 implicit none
 type(X_t)    :: X
 type(w_samp) :: wv
 integer      :: iq
 !
 ! Work Space
 !
 type(PP_indexes) ::PAR_IND_freqs
 integer     :: i1,i2,iw,INV_MODE
 complex(SP), allocatable :: Xom1(:,:),tddftk(:,:),Xo(:,:)
 !
 ! Fxc I/O (for the BS based kernel)
 !
 integer           ::ioFxc_err,ID,FXC_w_ref
 !
 ! Setup 
 !
 call PP_indexes_reset(PAR_IND_freqs)
 !
 allocate(tddftk(X%ng,X%ng),Xo(X%ng,X%ng))
 call mem_est("X_WS",(/2*size(Xo)/))
 !
 ! Xo^-1 matrix (BS Fxc)
 !
 if (l_bs_fxc) then
   allocate(Xom1(FXC_n_g_corr,FXC_n_g_corr))
   call mem_est("Xo_m1",(/size(Xom1)/))
 endif
 !
 ! Frequencies distribution
 !
 call PARALLEL_index(PAR_IND_freqs,(/wv%n_freqs/),COMM=PAR_COM_INV_INDEX)
 !
 if (iq==1) call PP_redux_wait()
 if (iq> 1) call PP_redux_wait(COMM=PAR_COM_Q_A2A%COMM)
 !
 call PARALLEL_live_message("Frequencies",LOADED=PAR_IND_freqs%n_of_elements(PAR_IND_INV_ID+1),TOTAL=wv%n_freqs)
 !
 if (PAR_IND_freqs%n_of_elements(PAR_IND_INV_ID+1)>0) then
   call live_timing('X@q['//trim(intc(iq))//'] ',PAR_IND_freqs%n_of_elements(PAR_IND_INV_ID+1))
 endif
 !
 do iw=1,wv%n_freqs
   !
   if (.not.PAR_IND_freqs%element_1D(iw)) then
     X_mat(:,:,iw)=cZero
     cycle
   endif
   !
   Xo=X_mat(:,:,iw)
   !
   tddftk=(0.,0.) 
   !
   ! If the freqs remaining are not anough for all the cpu's or
   ! Fxc needs the SVD procedure use the SLK
   !
   if (FXC_SVD_digits>0) INV_MODE=USE_SVD
   !
   ! TDDFT Kernel. Different procedure depending on the kernel 
   !
   ! Kind: BS,ALDA,LRC.
   !
   if (l_bs_fxc) then
!     !
!     ! GPL_EXCLUDE_START
!     !
!     ! BS Fxc
!     !
!     if ( mod(iw,FXC_n_mem_freqs)==1.or.wv%n_freqs==1) then
!       FXC_w_ref=iw-1
!       if (FXC_w_ref==0) then
!         call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2,3/),ID=ID)
!       else
!         call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1+iw/),ID=ID)
!       endif
!       ioFxc_err=ioBS_Fxc(iq,wv,ID)
!     endif
!     ! 
!     !if (.not.PAR_IND_freqs%element_1D(iw)) cycle
!     ! 
!     Xom1=Xo(:FXC_n_g_corr,:FXC_n_g_corr)
!     !
!     ! Use SVD to invert Xo if FXC_SVD_digits > 0
!     !
!     if (FXC_SVD_digits>0) then
!       call mat_dia_inv(USE_SVD,USE_SLK,Xom1,SVD_digits=FXC_SVD_digits)
!     else
!       call mat_dia_inv(INV,INV_MODE,Xom1)
!     endif
!     !
!     tddftk(:FXC_n_g_corr,:FXC_n_g_corr)=matmul(FXC(:,:,iw-FXC_w_ref),Xom1)
!     FXC(:,:,iw-FXC_w_ref)=matmul(Xom1,tddftk(:FXC_n_g_corr,:FXC_n_g_corr))
!     tddftk(:,:FXC_n_g_corr)=-matmul(Xo(:,:FXC_n_g_corr),FXC(:,:,iw-FXC_w_ref))
!     !
!     ! Fxc to be written to the o file
!     !
!     X_fxc(iw-FXC_w_ref)=FXC(1,1,iw-FXC_w_ref)
!     !
!     ! When all CPU's are doing this frequency point X_fxc must be rescaled
!     ! because of the call pp_redux_wait(X_fxc) in O_driver.
!     !
!     !if (iw>nw_max_par) X_fxc(iw-FXC_w_ref)=X_fxc(iw-FXC_w_ref)/real(ncpu)
!     !
!     ! GPL_EXCLUDE_END
!     !
   else if (l_lrc_fxc) then
     !
     ! LRC Fxc
     !
     tddftk(1,1)=-Xo(1,1)*(FXC_LRC_alpha + FXC_LRC_beta*abs(wv%p(iw))**2)/q_norm(iq)**2
     !
   else if (l_alda_fxc) then
     !
     ! ALDA Fxc
     !
     tddftk(:,:FXC_n_g_corr)=-matmul(Xo(:,:FXC_n_g_corr),FXC(:,:,1))
     !
   endif
   !
   ! I must cycle here to allow the FXC I/O properly
   !
   !if (.not.PAR_IND_freqs%element_1D(iw)) cycle
   !
   do i1=1,X%ng ! no Fxc [delta_(g1,g2)-Xo(g1,g2)*v(g2)]
     tddftk(:,i1)=tddftk(:,i1)-Xo(:,i1)*4.*pi/bare_qpg(iq,i1)**2
     tddftk(i1,i1)=tddftk(i1,i1)+1.
   enddo
   !
   call mat_dia_inv(INV,tddftk)
   !
   ! X(g,gp)=Sum_gpp[tddftk]^-1_(g,gpp)*Xo(gpp,gp)
   !
   X_mat(:,:,iw)=matmul(tddftk,Xo)
   !
   forall(i1=1:X%ng,i2=1:X%ng) X_mat(i1,i2,iw)=X_mat(i1,i2,iw)*4.*pi/bare_qpg(iq,i1)/bare_qpg(iq,i2)
   !
   call live_timing(steps=1)
   !
 enddo
 !
 if (PAR_IND_freqs%n_of_elements(PAR_IND_INV_ID+1)>0) call live_timing
 !
 do iw=1,wv%n_freqs
   call PP_redux_wait(X_mat(:,:,iw),COMM=PAR_COM_INV_index%COMM)
 enddo
 !
 ! CLEAN
 !
 call PP_indexes_reset(PAR_IND_freqs)
 deallocate(tddftk,Xo)
 if (l_bs_fxc) then
   deallocate(Xom1)
   call mem_est("Xo_m1")
 endif
 call mem_est("X_WS")
 !
end subroutine
