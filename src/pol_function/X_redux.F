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
subroutine X_redux(iq,Xw,X)
 !
 use pars,          ONLY:SP,pi,cZERO,cONE
 use drivers,       ONLY:l_bs_fxc,l_alda_fxc,l_lrc_fxc
 use LIVE_t,        ONLY:live_timing
 use parallel_m,    ONLY:PP_redux_wait,PP_indexes,PP_indexes_reset,ncpu,&
&                        PAR_COM_X_WORLD,PAR_COM_SLK_INDEX_local,PAR_COM_SLK,PAR_n_freqs,PAR_FREQS_index
 use R_lattice,     ONLY:bare_qpg,q_norm
 use frequency,     ONLY:w_samp
 use linear_algebra,ONLY:INV,SVD_INV,LIN_SYS,MAT_MUL,PAR_matrix,PAR_matrix_reset
 use wrapper,       ONLY:M_by_M
 use interfaces,    ONLY:LINEAR_ALGEBRA_driver,PAR_matrix_transfer
 use stderr,        ONLY:intc
 use X_m,           ONLY:X_t,X_mat,X_use_lin_sys,iW_offset,X_par,X_par_lower_triangle,X_lower_triangle_matrix_in_use
 use TDDFT,         ONLY:FXC_n_g_corr,FXC,FXC_LRC_alpha,FXC_LRC_beta,FXC_SVD_digits
#if defined _SCALAPACK
 use SLK_m,         ONLY:BLACS_structure_in_use
#endif
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
#include<memory.h>
 !
 type(X_t)    :: X
 type(w_samp) :: Xw
 integer      :: iq
 !
 ! Work Space
 !
 type(PP_indexes) ::PAR_IND_freqs
 integer          ::ig1,ig2,iw_par,iw,iw_X(1),INV_MODE,i_LA_pool,PAR_n_freqs_global(PAR_COM_SLK_INDEX_local%n_CPU)
 type(PAR_matrix) ::X_kernel,Xo
 !
 ! Setup 
 !
#if defined _TIMING
 call timing('X (procedure)',OPR='start')
#endif
 !
 ! Stop... 
 !
 call PP_redux_wait(COMM=PAR_COM_X_WORLD%COMM)
 !
 ! ... and frequencies distribution
 !
 call PARALLEL_FREQS_setup(Xw,PAR_IND_freqs,PAR_COM_SLK_INDEX_local)
 PAR_n_freqs_global                                  =0
 PAR_n_freqs_global(PAR_COM_SLK_INDEX_local%CPU_id+1)=PAR_n_freqs
 call PP_redux_wait(PAR_n_freqs_global,COMM=PAR_COM_X_WORLD%COMM)
 !
 if (PAR_n_freqs>0) call live_timing('X@q['//trim(intc(iq))//'] ',PAR_n_freqs)
 !
 ! I build here a local BLACS structure for the response function components
 !
 call PARALLEL_matrix_distribute( "SLK", X_kernel , X%ng )
 Y_ALLOCATE(X_kernel%blc(X_kernel%nrows,X_kernel%ncols))
 Y_MEM(X_kernel%blc)
 X_kernel%blc=cZERO
 call PARALLEL_matrix_distribute( "SLK", Xo , X%ng )
 Y_ALLOCATE(Xo%blc(Xo%nrows,Xo%ncols))
 Y_MEM(Xo%blc)
 Xo%blc=cZERO
 !
 do iw_par=1,maxval(PAR_n_freqs_global)
   !
   ! In this delicate part I need to transfer the PAR structure of Xo in the BLACS/serial
   ! by filling the appropriate frequency element. This is why I need to loop on the frequency pools
   !
   do i_LA_pool=1,PAR_COM_SLK_INDEX_local%n_CPU
     !
     if (iw_par>PAR_n_freqs_global(i_LA_pool)) cycle
     !
     iw_X    = 0
     if (i_LA_pool==PAR_COM_SLK_INDEX_local%CPU_id+1.and.PAR_COM_SLK%CPU_id==0) then
       iw_X  = PAR_FREQS_index(iw_par)+iW_offset
     endif
     !
     call PP_redux_wait(iw_X,COMM=PAR_COM_X_WORLD%COMM)
     !
     if (ncpu>1.and.i_LA_pool/=PAR_COM_SLK_INDEX_local%CPU_id+1) iw_X(1)=-iw_X(1)
     !
     call LOCAL_X_transfer(iw_X(1))
     !
   enddo
   !
   if (iw_par>PAR_n_freqs) cycle
   !
   iw    = PAR_FREQS_index(iw_par)
   iw_X  = iw+iW_offset
   X_kernel%blc=cZERO
   !
   if (FXC_SVD_digits>0) INV_MODE=SVD_INV
   !
   ! TDDFT Kernel. Different procedure depending on the kernel 
   !
   ! Kind: BS,ALDA,LRC.
   !
   if (l_bs_fxc) then
     !
   else if (l_lrc_fxc) then
     !
     ! LRC Fxc
     !
     if (Xo%cols(1)==1) then
#if defined _OPENMP
       !$omp parallel do default(shared), private(ig1)
#endif
       do ig1=Xo%rows(1),Xo%rows(2)
         X_kernel%blc(ig1,1)=-Xo%blc(ig1,1)*(FXC_LRC_alpha + FXC_LRC_beta*abs(Xw%p(iw))**2)/q_norm(iq)**2
       enddo
#if defined _OPENMP
       !$omp end parallel do
#endif
     endif
     !
   else if (l_alda_fxc) then
     !
     ! ALDA Fxc
     !
     call M_by_M('N','N', X%ng, -Xo%blc(:,:FXC_n_g_corr), FXC(:,:,1), X_kernel%blc(:,:FXC_n_g_corr))
     !
   endif
   !
   ! no Fxc [delta_(g1,g2)-Xo(g1,g2)*v(g2)]
   !
#if defined _OPENMP
   !$omp parallel do default(shared), private(ig1,ig2)
#endif
   do ig1=Xo%rows(1),Xo%rows(2)
     do ig2=Xo%cols(1),Xo%cols(2)
       X_kernel%blc(ig1,ig2) =X_kernel%blc(ig1,ig2)-Xo%blc(ig1,ig2)*4._SP*pi/bare_qpg(iq,ig2)**2
     enddo
     X_kernel%blc(ig1,ig1)=X_kernel%blc(ig1,ig1)+cONE
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
     X_mat(:,:,iw_X(1))=Xo%blc
     call LINEAR_ALGEBRA_driver(LIN_SYS,M=X_kernel%blc,B=X_mat(:,:,iw_X(1)))
     !
  else
     !
     ! formulation matrix inversion + matmul
     !
     call LINEAR_ALGEBRA_driver(INV,M=X_kernel%blc)
     call LINEAR_ALGEBRA_driver(MAT_MUL,M=X_kernel%blc,B=Xo%blc,C=X_mat(:,:,iw_X(1)))
     !
   endif
   !
#if defined _OPENMP
   !$omp parallel do default(shared), private(ig1,ig2)
#endif
   do ig1=Xo%rows(1),Xo%rows(2)
     do ig2=Xo%cols(1),Xo%cols(2)
       X_mat(ig1,ig2,iw_X(1))=X_mat(ig1,ig2,iw_X(1))*4._SP*pi/bare_qpg(iq,ig1)/bare_qpg(iq,ig2)
     enddo
   enddo
#if defined _OPENMP
   !$omp end parallel do
#endif
   !
   if (PAR_n_freqs>0) call live_timing(steps=1)
   !
 enddo
 !
 if (PAR_n_freqs>0) call live_timing()
 !
#if defined _TIMING
 call timing('X (procedure)',OPR='stop')
 call timing('X (REDUX)',OPR='start')
#endif
 !
 do iw=1,Xw%n_freqs
   call PP_redux_wait(X_mat(:,:,iw+iW_offset),COMM=PAR_COM_SLK_INDEX_local%COMM)
 enddo
 !
#if defined _TIMING
 call timing('X (REDUX)',OPR='stop')
#endif
 !
 ! CLEAN
 !
 call PP_indexes_reset(PAR_IND_freqs)
 Y_FREE(X_kernel%blc)
 Y_FREE(Xo%blc)
 call PAR_matrix_reset(X_kernel)
 call PAR_matrix_reset(Xo)
 !
 contains
   !
   subroutine LOCAL_X_transfer(iW_to_be_filled)
     integer :: iW_to_be_filled
     if (X_lower_triangle_matrix_in_use) then
#if defined _SCALAPACK
       if (BLACS_structure_in_use) then
         call PAR_matrix_transfer("X > slk",M_slk=Xo,X_up=X_par,X_dn=X_par_lower_triangle,i_MAT=iW_to_be_filled)
       else
#endif
         call PAR_matrix_transfer("X > M",  X_up=X_par,X_dn=X_par_lower_triangle,M=Xo%blc,i_MAT=iW_to_be_filled)
#if defined _SCALAPACK
       endif
#endif
     else
#if defined _SCALAPACK
       if (BLACS_structure_in_use) then
         call PAR_matrix_transfer("X > slk",M_slk=Xo,X_up=X_par,i_MAT=iW_to_be_filled)
       else
#endif
         call PAR_matrix_transfer("X > M",  X_up=X_par,M=Xo%blc,i_MAT=iW_to_be_filled)
#if defined _SCALAPACK
       endif
#endif
     endif
   end subroutine
   !
end subroutine
