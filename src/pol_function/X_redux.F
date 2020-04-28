!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM (AF)
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
subroutine X_redux(iq,X_par,Xw,X)
 !
 ! Here we solve the Dyson equation using a combination of Serial(SER)/Parallel(PAR)/SLK environments.
 !
 ! More precisely the routine handles:
 !
 !  1. Xo(XUP/XDN/SER): INPUT
 !  2. Dyson(SER/SLK) : SOLVER
 !  3. X(PAR/SER)     : OUTPUT
 !
 ! Therefore the loop is dived in a PAR/SER -> SER/SLK interface, a SER/SLK solver and, finally, a
 ! SER/SLK -> PAR/SER interface
 !
 use pars,          ONLY:SP,pi,cZERO,cONE
 use com,           ONLY:msg
 use drivers,       ONLY:l_bs_fxc,l_alda_fxc,l_lrc_fxc
 use LIVE_t,        ONLY:live_timing
 use parallel_m,    ONLY:PP_redux_wait,PP_indexes,PP_indexes_reset,PAR_COM_X_WORLD_RL_resolved,PP_bcast,&
&                        PAR_COM_X_WORLD,PAR_COM_SLK_INDEX_local,PAR_COM_SLK,PAR_n_freqs,PAR_FREQS_index
 use R_lattice,     ONLY:bare_qpg, bare_qpg_d
 use frequency,     ONLY:w_samp
 use linear_algebra,ONLY:INV,SVD_INV,LIN_SYS,MAT_MUL
 use matrix,        ONLY:PAR_matrix,MATRIX_reset
 use wrapper,       ONLY:M_by_M
 use interfaces,    ONLY:LINEAR_ALGEBRA_driver,MATRIX_transfer
 use stderr,        ONLY:intc
 use X_m,           ONLY:X_t,X_use_lin_sys,X_use_gpu,X_par_lower_triangle
 use TDDFT,         ONLY:FXC_n_g_corr,FXC,FXC_LRC_alpha,FXC_LRC_beta,FXC_SVD_digits
 use timing_m,      ONLY:timing
 use cuda_m
 use deviceXlib_m
 !
#include<dev_defs.h>
#include<memory.h>
 !
 type(PAR_matrix) :: X_par
 type(X_t)        :: X
 type(w_samp)     :: Xw
 integer          :: iq
 !
 ! Work Space
 !
 type(PP_indexes) :: PAR_IND_freqs
 integer          :: ig1,ig2,iw_par,iw,iw_X(1),INV_MODE,i_LA_pool,&
&                    PAR_n_freqs_global(PAR_COM_SLK_INDEX_local%n_CPU)
 integer          :: Xo_rows(2), Xo_cols(2)
 logical          :: compute_on_gpu
 type(PAR_matrix) :: K_FXC
 type(PAR_matrix), target :: BUFFER,KERNEL,Xo
 complex(SP), pointer DEV_ATTRIBUTE :: BUFFER_blc_d(:,:,:)
 !
 ! Setup 
 !
 call timing('X (procedure)',OPR='start')
 !
 ! Barrier... 
 !
 call PP_redux_wait(COMM=PAR_COM_X_WORLD%COMM)
 !
 ! compute X on GPU if compiled with GPU support,
 ! unless GPU-usage is switchoff from input
 !
 compute_on_gpu=have_cuda.and.X_use_gpu
 call msg('s','[LA] X compute_on_gpu ',compute_on_gpu )
 !
 !
 ! ... and frequencies distribution
 !         ========================
 call PARALLEL_FREQS_setup(Xw,PAR_IND_freqs,PAR_COM_SLK_INDEX_local)
 PAR_n_freqs_global =0
 PAR_n_freqs_global(PAR_COM_SLK_INDEX_local%CPU_id+1)=PAR_n_freqs
 call PP_redux_wait(PAR_n_freqs_global,COMM=PAR_COM_SLK_INDEX_local%COMM)
 !
 if (PAR_n_freqs>0) call live_timing('X@q['//trim(intc(iq))//'] ',PAR_n_freqs)
 !
 ! ... and local BLACS structures for the response function components
 !         ======================
 call MATRIX_init( "SLK", KERNEL , X%ng, 1 )
 YAMBO_ALLOC(KERNEL%blc,(KERNEL%BLCrows(1):KERNEL%BLCrows(2),KERNEL%BLCcols(1):KERNEL%BLCcols(2),1))
 KERNEL%blc=cZERO
 call MATRIX_init( "SLK", Xo , X%ng, 1 )
 YAMBO_ALLOC(Xo%blc,(Xo%BLCrows(1):Xo%BLCrows(2),Xo%BLCcols(1):Xo%BLCcols(2),1))
 Xo%blc=cZERO
 call MATRIX_init( "SLK", BUFFER , X%ng, max(PAR_n_freqs,1) )
 YAMBO_ALLOC(BUFFER%blc,(BUFFER%BLCrows(1):BUFFER%BLCrows(2),BUFFER%BLCcols(1):BUFFER%BLCcols(2),max(PAR_n_freqs,1)))
 BUFFER%blc=cZERO
 !
 if (compute_on_gpu) then
   YAMBO_ALLOC(KERNEL%blc_d,(KERNEL%BLCrows(1):KERNEL%BLCrows(2),KERNEL%BLCcols(1):KERNEL%BLCcols(2),1))
   YAMBO_ALLOC(BUFFER%blc_d,(BUFFER%BLCrows(1):BUFFER%BLCrows(2),BUFFER%BLCcols(1):BUFFER%BLCcols(2),max(PAR_n_freqs,1)))
   YAMBO_ALLOC(Xo%blc_d,(Xo%BLCrows(1):Xo%BLCrows(2),Xo%BLCcols(1):Xo%BLCcols(2),1))
   call dev_memset(KERNEL%blc_d,cZERO)
   call dev_memset(BUFFER%blc_d,cZERO)
   call dev_memset(Xo%blc_d,cZERO)   
 endif
 !
 if (l_alda_fxc.and.Xo%kind=="SLK") then
   call MATRIX_init( "SLK", K_FXC , FXC_n_g_corr, 1 )
   YAMBO_ALLOC(K_FXC%blc,(K_FXC%BLCrows(1):K_FXC%BLCrows(2),K_FXC%BLCcols(1):K_FXC%BLCcols(2),1))
   K_FXC%blc=cZERO
   call MATRIX_transfer(M=-FXC(:,:,1),M_out=K_FXC)
 endif
 !
 do iw_par=1,maxval(PAR_n_freqs_global)
   !
   ! [1] Interface PAR/SER(Xo) => SLK/SER(X_redux)
   ! =============================================
   ! In this delicate part I need to transfer the PAR structure of Xo in the BLACS/serial
   ! by filling the appropriate frequency element. This is why I need to loop on the frequency pools.
   !
   do i_LA_pool=1,PAR_COM_SLK_INDEX_local%n_CPU
     !
     ! Empty pool at this frequency?
     !
     if (iw_par>PAR_n_freqs_global(i_LA_pool)) cycle
     !
     ! The PP_redux forces me to define the iw_X only using the first CPU of each freq pool
     !
     iw_X    = 0
     if (i_LA_pool==PAR_COM_SLK_INDEX_local%CPU_id+1.and.PAR_COM_SLK%CPU_id==0) then
       iw_X  = PAR_FREQS_index(iw_par)
     endif
     call PP_redux_wait(iw_X,COMM=PAR_COM_X_WORLD%COMM)
     !
     ! Only the CPU of this POOL has to store the X matrix (RCV logical).
     !
     call MATRIX_transfer(M_in=X_par,               M_out=Xo,INDEX_in=iw_X(1),&
&                                                   SND=X_par%INTRA_comm%CPU_id==0,&
&                                                   RCV=i_LA_pool==PAR_COM_SLK_INDEX_local%CPU_id+1,&
&                                                   COMM=PAR_COM_X_WORLD,COMM_name="X_redux_1_"//trim(intc(i_LA_pool)))
     if(allocated(X_par_lower_triangle%blc)) then
       call MATRIX_transfer(M_in=X_par_lower_triangle,M_out=Xo,INDEX_in=iw_X(1),&
&                                                   SND=X_par%INTRA_comm%CPU_id==0,&
&                                                   RCV=i_LA_pool==PAR_COM_SLK_INDEX_local%CPU_id+1,&
&                                                   COMM=PAR_COM_X_WORLD,COMM_name="X_redux_1_"//trim(intc(i_LA_pool)))
     endif
     !
   enddo
   !
   if (iw_par>PAR_n_freqs) cycle
   !
   ! [2] SER/SLK Solver
   ! ==================
   !
   iw         = PAR_FREQS_index(iw_par)
   BUFFER%I   = iw_par
   KERNEL%blc = cZERO
   if (compute_on_gpu) call dev_memset(KERNEL%blc_d,cZERO)
   !
   Xo_cols(:)=Xo%cols(:)
   Xo_rows(:)=Xo%rows(:)
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
     ! DS : notice that here I've redefined the 1/q^2 kernel for the cases where the cutoff is used
     !
     if (Xo%cols(1)==1) then
       !$omp parallel do default(shared), private(ig1)
       do ig1=Xo%rows(1),Xo%rows(2)
         KERNEL%blc(ig1,1,1)=-Xo%blc(ig1,1,1)*(FXC_LRC_alpha + FXC_LRC_beta*abs(Xw%p(iw))**2)/bare_qpg(iq,1)**2
       enddo
       !$omp end parallel do
     endif
     !
   else if (l_alda_fxc) then
     !
     ! ALDA Fxc
     !
     if (Xo%kind=="SLK") then
#if defined _SCALAPACK
       call PARALLEL_M_by_M(Xo,K_FXC,KERNEL,"N","N",X%ng,FXC_n_g_corr,FXC_n_g_corr)
#else
       call error("[LA] SLK library required but not available")
#endif
     else
       call M_by_M('N','N',X%ng,FXC_n_g_corr,FXC_n_g_corr,cONE,-Xo%blc(:,:FXC_n_g_corr,1),X%ng,&
&                  FXC(:,:,1),FXC_n_g_corr,cZERO,KERNEL%blc(:,:FXC_n_g_corr,1),X%ng)
     endif
     !
   endif
   !
   ! no Fxc [delta_(g1,g2)-Xo(g1,g2)*v(g2)]
   !
   if (compute_on_gpu) then
      if (l_bs_fxc.or.l_lrc_fxc.or.l_alda_fxc) call dev_memcpy(KERNEL%blc_d,KERNEL%blc)
      call dev_memcpy(Xo%blc_d,Xo%blc)
   endif
   !
   call X_redux_build_kernel(KERNEL,Xo,Xo_rows,Xo_cols)
   !
   ! X(g,gp)=Sum_gpp[KERNEL]^-1_(g,gpp)*Xo(gpp,gp)
   !
   if (X_use_lin_sys) then
     !
     ! Linear System
     !
     if (compute_on_gpu) then
       call dev_memcpy(BUFFER%blc_d(:,:,BUFFER%I), Xo%blc_d(:,:,1))
     else
                       BUFFER%blc(:,:,BUFFER%I) =  Xo%blc(:,:,1)
     endif
     !
     call LINEAR_ALGEBRA_driver(LIN_SYS,M_slk=KERNEL,B_slk=BUFFER)
     !
   else
     !
     ! Matrix Inversion + Matmul
     !
     call LINEAR_ALGEBRA_driver(INV,M_slk=KERNEL)
     call LINEAR_ALGEBRA_driver(MAT_MUL,M_slk=KERNEL,B_slk=Xo,C_slk=BUFFER)
     !
   endif

   !
   ! update X by multiplying left and right for v_coul^1/2 (diagonal in reciprocal space)
   !
   if (compute_on_gpu) then
      !
!      call dev_mat_upd_dMd(Xo_rows(2)-Xo_rows(1)+1,Xo_cols(2)-Xo_cols(1)+1,BUFFER%blc_d(:,:,iw_par),&
!&                          bare_qpg_d(Xo_rows(1):,iq),"R",bare_qpg_d(Xo_cols(1):,iq),"R",scal=cmplx(4._SP*pi,kind=SP)) 
      !
      BUFFER_blc_d => BUFFER%blc_d
      !
      !$cuf kernel do(2)
      do ig2=Xo_cols(1),Xo_cols(2)
        do ig1=Xo_rows(1),Xo_rows(2)
          BUFFER_blc_d(ig1,ig2,iw_par)=BUFFER_blc_d(ig1,ig2,iw_par)*4._SP*pi/bare_qpg_d(ig1,iq)/bare_qpg_d(ig2,iq)
        enddo
      enddo
      !
      call dev_stream_sync(stream_default)
      call dev_memcpy_async(BUFFER%blc(:,:,BUFFER%I),BUFFER%blc_d(:,:,BUFFER%I), stream_d2h)
      !
   else
      !
      !$omp parallel do default(shared), private(ig1,ig2), collapse(2)
      do ig2=Xo%cols(1),Xo%cols(2)
        do ig1=Xo%rows(1),Xo%rows(2)
          BUFFER%blc(ig1,ig2,iw_par)=BUFFER%blc(ig1,ig2,iw_par)*4._SP*pi/bare_qpg(iq,ig1)/bare_qpg(iq,ig2)
        enddo
      enddo
      !$omp end parallel do
      !
   endif
   !
   if (PAR_n_freqs>0) call live_timing(steps=1)
   !
 enddo
 !
 call dev_stream_sync(stream_d2h)
 !
 if (PAR_n_freqs>0) call live_timing()
 !
 ! Clean-up of local BLACS structures
 ! ========
 call MATRIX_reset(KERNEL)
 call MATRIX_reset(Xo)
 if (l_alda_fxc.and.Xo%kind=="SLK") call MATRIX_reset(K_FXC)
 !
 call timing('X (procedure)',OPR='stop')
 call timing('X (REDUX)',OPR='start')
 !
 ! [3] Interface SER/SLK(X_redux) => SER/PAR(X)
 ! ============================================
 !
 ! The old X_par structure need to be cleaned and replaced by a non symmetric structure
 !
 if (X_par%INTER_comm%n_CPU>1) then
   !
   call MATRIX_reset(X_par)
   call X_PARALLEL_alloc(X_par,X%ng,Xw%n_freqs,"X")
   !
 endif
 !
 ! Now the interfaces. 
 !
 ! From all CPU of the SLK group to one of the RL group.
 !
 do i_LA_pool=1,PAR_COM_SLK_INDEX_local%n_CPU
   !
   do iw_par=1,PAR_n_freqs_global(i_LA_pool)
     !
     ! The PP_redux forces me to define the iw_X only using the first CPU of each freq pool
     !
     iw_X    = 0
     if (i_LA_pool==PAR_COM_SLK_INDEX_local%CPU_id+1.and.PAR_COM_SLK%CPU_id==0) then
       iw_X  = PAR_FREQS_index(iw_par)
     endif
     call PP_redux_wait(iw_X,COMM=PAR_COM_X_WORLD%COMM)
     !
     ! iw_par is used ONLY by the CPU's that SEND the matrix (i_LA_pool==PAR_COM_SLK_INDEX_local%CPU_id+1)
     ! All other CPU's do not use the BUFFER and can set INDEX_in as a dummy 1. This is needed as 
     ! BUFFER is dimensioned with the PAR #of freqs.
     !
     iw=iw_par
     if (iw_par>PAR_n_freqs) iw=1
     !
     call MATRIX_transfer(M_in=BUFFER,M_out=X_par,INDEX_in=iw,INDEX_out=iw_X(1),&
&                         SND=i_LA_pool==PAR_COM_SLK_INDEX_local%CPU_id+1,&
&                         RCV=PAR_COM_X_WORLD_RL_resolved%CPU_id==0,&
&                         COMM=PAR_COM_X_WORLD,COMM_name="X_redux_2_"//trim(intc(i_LA_pool)))
     !
   enddo
 enddo
 !
 ! complete the transfer with a collective communication
 !
 call PP_bcast(X_par%blc,node=0,COMM=PAR_COM_X_WORLD_RL_resolved%COMM)
 !
 call timing('X (REDUX)',OPR='stop')
 !
 ! Clean-up of FREQ structure
 !==========
 call MATRIX_reset(BUFFER)
 YAMBO_FREE(PAR_FREQS_index)
 call PP_indexes_reset(PAR_IND_freqs)
 !
contains
   !
   subroutine X_redux_build_kernel(KERNEL,Xo,Xo_rows,Xo_cols)
     use R_lattice,     ONLY:bare_qpg, bare_qpg_d
     use pars,          ONLY:pi,cONE
     implicit none
     type(PAR_matrix), target :: KERNEL,Xo
     integer, intent(in)      :: Xo_rows(2),Xo_cols(2)
     integer :: ig1,ig2
     !
     complex(SP), pointer DEV_ATTRIBUTE :: KERNEL_blc_d(:,:,:)
     complex(SP), pointer DEV_ATTRIBUTE :: Xo_blc_d(:,:,:)
     !
     if (compute_on_gpu) then
       !
       KERNEL_blc_d=>KERNEL%blc_d(:,:,:)
       Xo_blc_d    => Xo%blc_d(:,:,:)
       !
       !$cuf kernel do(2) <<<*,*>>>
       do ig2=Xo_cols(1),Xo_cols(2)
       do ig1=Xo_rows(1),Xo_rows(2)
         KERNEL_blc_d(ig1,ig2,1) =KERNEL_blc_d(ig1,ig2,1)-Xo_blc_d(ig1,ig2,1)*4._SP*pi/bare_qpg_d(ig2,iq)**2
         if (ig1==ig2) KERNEL_blc_d(ig1,ig1,1)=KERNEL_blc_d(ig1,ig1,1)+cONE
       enddo
       enddo
       !
     else
       !
       !$omp parallel do default(shared), private(ig1,ig2), collapse(2)
       do ig2=Xo%cols(1),Xo%cols(2)
       do ig1=Xo%rows(1),Xo%rows(2)
         KERNEL%blc(ig1,ig2,1) =KERNEL%blc(ig1,ig2,1)-Xo%blc(ig1,ig2,1)*4._SP*pi/bare_qpg(iq,ig2)**2
         if (ig1==ig2) KERNEL%blc(ig1,ig1,1)=KERNEL%blc(ig1,ig1,1)+cONE
       enddo
       enddo
       !$omp end parallel do
       !
     endif
     !
   end subroutine X_redux_build_kernel
   !
end subroutine
