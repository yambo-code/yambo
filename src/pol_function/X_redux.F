!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine X_redux(iq,X,Xw,W_range)
 !
 use pars,          ONLY:SP,pi,cZERO,cONE
 use drivers,       ONLY:l_bs_fxc,l_alda_fxc,l_lrc_fxc
 use LIVE_t,        ONLY:live_timing
 use parallel_m,    ONLY:PP_redux_wait,PP_indexes,PP_indexes_reset,&
&                        PAR_COM_DIPOLES,PAR_COM_SLK_INDEX_local
 use R_lattice,     ONLY:bare_qpg,q_norm
 use frequency,     ONLY:w_samp
 use linear_algebra,ONLY:INV,SVD_INV,LIN_SYS,MAT_MUL
 use wrapper,       ONLY:M_by_M
 use interfaces,    ONLY:PARALLEL_index,PARALLEL_live_message,LINEAR_ALGEBRA_driver 
 use stderr,        ONLY:intc
 use X_m,           ONLY:X_t,X_mat,X_use_lin_sys
 use TDDFT,         ONLY:FXC_n_g_corr,FXC,&
&                        FXC_LRC_alpha,FXC_LRC_beta,FXC_SVD_digits,io_BS_Fxc
 use IO_m,          ONLY:io_control,OP_RD_CL,NONE
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 !
#include<memory.h>
 type(X_t)    :: X
 type(w_samp) :: Xw
 integer      :: iq,W_range(2)
 !
 ! Work Space
 !
 type(PP_indexes) ::PAR_IND_freqs
 integer          ::i1,i2,iw,iw_mat,INV_MODE,PAR_IND_FREQ_ID
 complex(SP), allocatable :: Xom1(:,:),tddftk(:,:),Xo(:,:)
 !
 ! Fxc I/O (for the BS based kernel)
 !
 !integer           ::ioFxc_err,ID,FXC_w_ref
 !
 ! Setup 
 !
#if defined _TIMING
 call timing('X (procedure)',OPR='start')
#endif
 !
 call PP_indexes_reset(PAR_IND_freqs)
 !
 YAMBO_ALLOC(tddftk,(X%ng,X%ng))
 YAMBO_ALLOC(Xo,(X%ng,X%ng))
 !
 ! Xo^-1 matrix (BS Fxc)
 !
 if (l_bs_fxc) then
   YAMBO_ALLOC(Xom1,(FXC_n_g_corr,FXC_n_g_corr))
 endif
 !
 ! Stop... 
 !
 call PP_redux_wait(COMM=PAR_COM_DIPOLES%COMM)
 !
 ! ... and frequencies distribution
 !
 call PARALLEL_index(PAR_IND_freqs,(/Xw%n_freqs/),COMM=PAR_COM_SLK_INDEX_local)
 PAR_IND_FREQ_ID=PAR_COM_SLK_INDEX_local%CPU_id+1
 call PARALLEL_live_message("X Frequencies",LOADED=PAR_IND_freqs%n_of_elements(PAR_IND_FREQ_ID),TOTAL=Xw%n_freqs,&
&                           NCPU=PAR_COM_SLK_INDEX_local%n_CPU)
 !
 if (PAR_IND_freqs%n_of_elements(PAR_IND_FREQ_ID)>0) &
&  call live_timing('X@q['//trim(intc(iq))//'] ',PAR_IND_freqs%n_of_elements(PAR_IND_FREQ_ID))
 !
 do iw=1,Xw%n_freqs
   !
   iw_mat=iw+W_range(1)-1
   !
   if (.not.PAR_IND_freqs%element_1D(iw)) then
     X_mat(:,:,iw_mat)=cZERO
     cycle
   endif
   !
   Xo=X_mat(:,:,iw_mat)
   !
   tddftk=cZERO
   !
   ! If the freqs remaining are not enough for all the cpu's or
   ! Fxc needs the SVD procedure use the SLK
   !
   if (FXC_SVD_digits>0) INV_MODE=SVD_INV
   !
   ! TDDFT Kernel. Different procedure depending on the kernel 
   !
   ! Kind: BS,ALDA,LRC.
   !
   if (l_bs_fxc) then
!     !
!     !
   else if (l_lrc_fxc) then
     !
     ! LRC Fxc
     !
     tddftk(:,1)=-Xo(:,1)*(FXC_LRC_alpha + FXC_LRC_beta*abs(Xw%p(iw))**2)/q_norm(iq)**2
     !
   else if (l_alda_fxc) then
     !
     ! ALDA Fxc
     !
     call M_by_M('N','N', X%ng, -Xo(:,:FXC_n_g_corr), FXC(:,:,1), tddftk(:,:FXC_n_g_corr))
     !
   endif
   !
   ! I must cycle here to allow the FXC I/O properly
   !
   !if (.not.PAR_IND_freqs%element_1D(iw)) cycle
   !
   ! no Fxc [delta_(g1,g2)-Xo(g1,g2)*v(g2)]
   !
#if defined _OPENMP
   !$omp parallel do default(shared), private(i1)
#endif
   do i1=1,X%ng
     tddftk(:,i1)=tddftk(:,i1)-Xo(:,i1)*4._SP*pi/bare_qpg(iq,i1)**2
     tddftk(i1,i1)=tddftk(i1,i1)+cONE
   enddo
#if defined _OPENMP
   !$omp end parallel do
#endif
   !
   ! X(g,gp)=Sum_gpp[tddftk]^-1_(g,gpp)*Xo(gpp,gp)
   !
   if (X_use_lin_sys) then
     !
     ! formulation as a linear system
     !
     X_mat(:,:,iw_mat)=Xo
     call LINEAR_ALGEBRA_driver(LIN_SYS,M=tddftk,B=X_mat(:,:,iw_mat))
     !
  else
     !
     ! formulation matrix inversion + matmul
     !
     call LINEAR_ALGEBRA_driver(INV,M=tddftk)
     call LINEAR_ALGEBRA_driver(MAT_MUL,M=tddftk,B=Xo,C=X_mat(:,:,iw_mat))
     !
   endif
   !
#if defined _OPENMP
   !$omp parallel do default(shared), private(i1,i2)
   do i2=1,X%ng
     do i1=1,X%ng
       X_mat(i1,i2,iw_mat)=X_mat(i1,i2,iw_mat)*4._SP*pi/bare_qpg(iq,i1)/bare_qpg(iq,i2)
     enddo
   enddo
   !$omp end parallel do
#else
   forall(i1=1:X%ng,i2=1:X%ng) X_mat(i1,i2,iw_mat)=X_mat(i1,i2,iw_mat)*4._SP*pi/bare_qpg(iq,i1)/bare_qpg(iq,i2)
#endif
   !
   if (PAR_IND_freqs%n_of_elements(PAR_IND_FREQ_ID)>0) call live_timing(steps=1)
   !
 enddo
 !
 if (PAR_IND_freqs%n_of_elements(PAR_IND_FREQ_ID)>0) call live_timing()
 !
#if defined _TIMING
 call timing('X (procedure)',OPR='stop')
 call timing('X (REDUX)',OPR='start')
#endif
 !
 do iw=W_range(1),W_range(2)
   call PP_redux_wait(X_mat(:,:,iw),COMM=PAR_COM_SLK_INDEX_local%COMM)
 enddo
 !
#if defined _TIMING
 call timing('X (REDUX)',OPR='stop')
#endif
 !
 ! CLEAN
 !
 call PP_indexes_reset(PAR_IND_freqs)
 YAMBO_FREE(tddftk)
 YAMBO_FREE(Xo)
 if (l_bs_fxc) then
   YAMBO_FREE(Xom1)
 endif
 !
end subroutine
