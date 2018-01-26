!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine K_inversion_driver(iq,W,E,k,q) 
 !
 use pars,           ONLY:SP,pi
 use stderr,         ONLY:real2ch
 use X_m,            ONLY:Epsilon_ii,X_drude_term,l_drude
 use BS_solvers,     ONLY:BSS_inversion_mode,BSS_n_descs,BSS_description,Co_factor,&
&                         K_INV_EPS,BSS_add_antiresonant,K_Inversion_data,&
&                         K_slk,K_inv_slk
 use BS,             ONLY:BS_H_dim,BS_Blocks_symmetrize_K,BS_blk
 use electrons,      ONLY:levels
 use frequency,      ONLY:w_samp
 use R_lattice,      ONLY:q_norm,bz_samp
 use com,            ONLY:isec,msg,warning
 use LIVE_t,         ONLY:live_timing
 use parallel_m,     ONLY:PP_redux_wait,PP_indexes,PP_indexes_reset,PAR_COM_SLK_INDEX_local,&
&                         PAR_COM_SLK_INDEX_local
 use interfaces,     ONLY:PARALLEL_index,PARALLEL_live_message,MATRIX_duplicate
 use IO_m,           ONLY:io_control,REP,NONE,OP_WR_CL
 use matrix,         ONLY:MATRIX_reset
#if defined _SCALAPACK
 use interfaces,     ONLY:MATRIX_transfer
 use parallel_m,     ONLY:linear_algebra_is_parallel
#endif
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
#if defined _RT
 use RT_control,     ONLY:NEQ_Residuals,NEQ_Kernel
#endif
 !
 ! GPL_EXCLUDE_START
 !
#if defined _KERR
 use KERR,           ONLY:BSS_KERR_free
#endif
#if defined _PL
 use BS_solvers,     ONLY:K_INV_PL
 use PHOTOLUM,       ONLY:BSS_PL_free,PL
#endif
 !
 ! GPL_EXCLUDE_END
 !
#include<memory.h>
 !
 type(w_samp) :: W
 integer      :: iq
 type(levels) :: E
 type(bz_samp):: k,q
 !
 ! Work Space...
 !
 ! ... dummies
 !
 type(PP_indexes)  :: PAR_IND_freqs
 integer           :: i1,i2,iw,PAR_IND_FREQ_ID
 logical, external :: stop_now
 real(SP)          :: ratio
 complex(SP), allocatable :: Lo(:,:,:)
 !
 ! ... frequencies
 !
 integer    ::nw,nw_conv
 logical    ::FULL,PERTURBATIVE,completed
 !
 ! I/O
 !
 integer           :: i_err,ID,Lo_dim,Lo_n_loop
 integer, external :: io_BSS_invert
 !
#if defined _TIMING
 call timing('Inversion Solver',OPR='start')
#endif
 !
 ! Sectioning
 !============
 if (isec(2)/=0) then
   call section('=','Inversion solver')
 else if (isec(2)==0) then
   call section('+','Inversion solver')
 endif
 !
#if defined _SCALAPACK
 !
 ! Symmetrize the K blocks (to be eventually sent to the BLACS)
 !=========================
 if (linear_algebra_is_parallel) call BS_Blocks_symmetrize_K()
 !
#endif
 !
 ! Restart database
 !==================
 call K_inversion_restart(iq,W)
 !
 completed=count(K_INV_EPS%err/=0)==0
#if defined _PL
 completed=count(K_INV_EPS%err/=0)==0.and.count(K_INV_PL%err/=0)==0
#endif
 if (completed) then
   call LOCAL_clean()
   return
 endif
 !
 ! Full inversion ?
 !==================
 FULL         = index(BSS_inversion_mode,'f')/=0
 PERTURBATIVE = index(BSS_inversion_mode,'p')/=0
 !
 ! GPL_EXCLUDE_START
 !
#if defined _RT
 if (NEQ_Residuals.neqv.NEQ_Kernel) then
   call warning("[INV] Different occupation configurations for Kernel and Residuals not supported")
 endif
#endif
 !
 ! GPL_EXCLUDE_END
 !
#if defined _SCALAPACK
 ! 
 ! SLK matrix initialization before the Kernel loading
 !
 call MATRIX_init( "SLK", K_slk, BS_H_dim, 1 )
 !
 YAMBO_ALLOC(K_slk%blc,(K_slk%BLCrows(1):K_slk%BLCrows(2),K_slk%BLCcols(1):K_slk%BLCcols(2),1))
 !
#endif
 !
 ! Kernel and/or BSS components loading
 !======================================
 call K_components_folded_in_serial_arrays(iq,"INV")
 !
 if (K_slk%kind=="SLK") then
   !
#if defined _SCALAPACK
   !
   call msg('rs','[LA] BLACS matrix filling')
   !
   ! Prepare the BLACS matrices (reference, K_slk and local, K_inv_slk)
   !============================
   !
   call MATRIX_transfer(M_out=K_slk, M_bse= BS_blk )
   !
   ! Diagonal removal
   !==================
   call K_diagonal(iq,"rm_OCC DIAG")
   !
   if (FULL) call MATRIX_duplicate( "none", M_in=K_slk, M_out=K_inv_slk )
   !
#endif
   !
 else
   !
   ! Diagonal removal
   !==================
   call K_diagonal(iq,"rm_E rm_OCC DIAG")
   !
 endif
 !
 ! IP-part Lo [also recompute Epsilon_xx(:,3) OR PL (needed in case of DbG)]
 !===========================================================================
 Lo_dim=1
 if (BSS_add_antiresonant) Lo_dim=2
 Lo_n_loop=Lo_dim
 !
#if defined _PL
 !
 ! In the PL case we need, in addition to the standard GF, also some additional L_k^(r)(w) A_k(w) L_k^(a)(w), where
 ! L^(a/r) are deduced from the standard components.
 !
 Lo_dim=2
 if (BSS_add_antiresonant) Lo_dim=4
 !
#endif
 !
 YAMBO_ALLOC(Lo,(BS_H_dim,W%n_freqs,Lo_dim))
 !
 call K_inversion_Lo(iq,W,E,k,Lo_dim,Lo_n_loop,Lo)
 !
 ! Initialize the output file 
 !============================
 call K_output_file(iq,"open Inversion")
 !
 ! Perturbative Inversion
 !========================
 call PP_indexes_reset(PAR_IND_freqs)
 PAR_IND_FREQ_ID=PAR_COM_SLK_INDEX_local%CPU_id+1
 !
 if (PERTURBATIVE) then
   !
   call PARALLEL_index(PAR_IND_freqs,(/W%n_freqs/),COMM=PAR_COM_SLK_INDEX_local)
   call PARALLEL_live_message('Perturbative Inversion Frequencies',LOADED=PAR_IND_freqs%n_of_elements(PAR_IND_FREQ_ID),&
&                              TOTAL=W%n_freqs,NCPU=PAR_COM_SLK_INDEX_local%n_CPU)
   call live_timing('Perturbative inversion ',PAR_IND_freqs%n_of_elements(PAR_IND_FREQ_ID))
   !
   call K_inversion_engine("any",1,W%n_freqs,PAR_IND_freqs,W,Lo,Lo_dim,Lo_n_loop,.FALSE.)
   !
   call PP_redux_wait(K_INV_EPS%err,COMM=PAR_COM_SLK_INDEX_local%COMM)
   call PP_redux_wait(Epsilon_ii(:,2),COMM=PAR_COM_SLK_INDEX_local%COMM)
#if defined _PL
   call PP_redux_wait(K_INV_PL%err,COMM=PAR_COM_SLK_INDEX_local%COMM)
   call PP_redux_wait(PL(:,2),COMM=PAR_COM_SLK_INDEX_local%COMM)
#endif
   !
   ! I/O [after pert inversion]
   !
   K_INV_EPS%spectra(:,1)=Epsilon_ii(:,2)
#if defined _PL
   K_INV_PL%spectra(:,1)=PL(:,2)
#endif
   !
#if defined _PL
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2,3,4/),ID=ID)
   i_err=io_BSS_invert(iq,W,ID)
   call msg('rs','[PERTURBATIVE INV@PL ] Converged frequencies :',count(K_INV_PL%err==0))
#else
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2,3/),ID=ID)
   i_err=io_BSS_invert(iq,W,ID)
#endif
   call msg('rs','[PERTURBATIVE INV@EPS] Converged frequencies :',count(K_INV_EPS%err==0))
   !
 endif
 !
 call K_output_file(iq,"Inversion")
 !
 ! Full Inversion
 !=================
 !
 ! First I define the group of frequencies not converged
 !
 if (FULL) then
   !======
   call FULL_INF_freqs(K_INV_EPS)
#if defined _PL
   call FULL_INF_freqs(K_INV_PL)
#endif
   !
 endif
 !
 if (FULL.and.PERTURBATIVE) then
   !==============
   !
   call FULL_INF_pre_setup(K_INV_EPS)
#if defined _PL
   call FULL_INF_pre_setup(K_INV_PL)
#endif
   !
 endif
 !
 ! FULL INVERSION
 !================
 !
 if (K_INV_EPS%n_full_inv_freqs>0.and.FULL) call do_it_FULL(K_INV_EPS)
#if defined _PL
 if (K_INV_PL%n_full_inv_freqs>0.and.FULL) call do_it_FULL(K_INV_PL)
#endif
 !
 ! Try a polinomial interpolation on the null values (only for EPS)
 !==================================================================
 call K_eps_interpolate(W%n_freqs,real(W%p),K_INV_EPS%err)
 !
 call PP_redux_wait()
 !
 ! I/O [after full inversion]
 !=====
 K_INV_EPS%spectra(:,1)=Epsilon_ii(:,2)
#if defined _PL
 K_INV_PL%spectra(:,1)=PL(:,2)
#endif
 !
#if defined _PL
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2,3,4/),ID=ID)
 i_err=io_BSS_invert(iq,W,ID)
#else
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2,3/),ID=ID)
 i_err=io_BSS_invert(iq,W,ID)
#endif
 !
 ! Final Output
 !==============
 !
 Epsilon_ii(:,2)=1._SP+Epsilon_ii(:,2)
 if (l_drude) Epsilon_ii(:,2)=Epsilon_ii(:,2)+X_drude_term(:)*4._SP*pi/q_norm(1)**2
 !
 BSS_n_descs=BSS_n_descs+1
 ratio=real(count(K_INV_EPS%err==0))/real(W%n_freqs)*100._SP
 BSS_description(BSS_n_descs)='    |Converged frequencies [o/o]:'//trim(real2ch(ratio))
 !
 call K_output_file(iq,"Inversion")
 !
 ! CLEAN
 !
 call LOCAL_clean()
 !
 contains 
   !
   subroutine do_it_FULL(TYP)
     !
     type(K_Inversion_data) :: TYP
     !
     ! Then loop on the groups performing full inversion
     !
     call PP_indexes_reset(PAR_IND_freqs)
     call PARALLEL_index(PAR_IND_freqs,(/TYP%n_full_inv_freqs/),COMM=PAR_COM_SLK_INDEX_local)
     call PARALLEL_live_message('['//TYP%what//'] Exact Inversion Frequencies',LOADED=PAR_IND_freqs%n_of_elements(PAR_IND_FREQ_ID),&
&                               TOTAL=TYP%n_full_inv_freqs,NCPU=PAR_COM_SLK_INDEX_local%n_CPU)
     call live_timing('['//TYP%what//'] Exact inversion ',PAR_IND_freqs%n_of_elements(PAR_IND_FREQ_ID))
     !
     do i1=1,TYP%n_full_inv_freqs
       !
       if (.not.PAR_IND_freqs%element_1D(i1)) cycle
       !
       iw=TYP%iw_full(i1)
       !
       call K_inversion_engine(TYP%what,iw,1,PAR_IND_freqs,W,Lo,Lo_dim,Lo_n_loop,.TRUE.)
       !
       call live_timing(steps=1)
       !
       if (stop_now(.FALSE.)) then
         exit
       endif
       !
     enddo
     !
     call live_timing()
     call PP_redux_wait(TYP%err,imode=2)
     if (TYP%what=="EPS") call PP_redux_wait(Epsilon_ii(:,2),COMM=PAR_COM_SLK_INDEX_local%COMM)
#if defined _PL
     if (TYP%what=="PLS") call PP_redux_wait(PL(:,2),COMM=PAR_COM_SLK_INDEX_local%COMM)
#endif
     !
     nw_conv=count(TYP%err==0)
     call msg('rs','[FULL INV@'//TYP%what//'] Frequencies DONE      :',nw_conv)
     call msg('rs','[FULL INV@'//TYP%what//']                  [o/o]:',real(nw_conv)/real(W%n_freqs)*100._SP)
     !
   end subroutine
   !
   subroutine FULL_INF_freqs(TYP)
     type(K_Inversion_data) :: TYP
     TYP%n_full_inv_freqs=0
     do i1=1,W%n_freqs
       if (TYP%err(i1)/=0) then
         TYP%n_full_inv_freqs=TYP%n_full_inv_freqs+1
         TYP%iw_full(TYP%n_full_inv_freqs)=i1
       endif
     enddo
   end subroutine
   !
   subroutine FULL_INF_pre_setup(TYP)
     !
     type(K_Inversion_data) :: TYP
     integer :: min_dist_non_conv_freqs ! Every W%n_freqs/min_dist_non_conv_freqs
     logical :: found
     !
     min_dist_non_conv_freqs=W%n_freqs
     found = .FALSE.
     !
     do while (.not.found) 
       !
       TYP%n_full_inv_freqs=0
       !
       iw=-1
       nw=0
       do i1=1,W%n_freqs
         if (TYP%err(i1)/= 0.and.iw<0) iw=i1
         if (TYP%err(i1)/= 0.and.iw>0) nw=nw+1
         if ((TYP%err(i1)== 0.or.(TYP%err(i1)/=0.and.i1==W%n_freqs)).and.iw>0) then
           !
           if (nw>=min_dist_non_conv_freqs) then
             !
             do i2=iw,iw+nw-1,min_dist_non_conv_freqs
               TYP%n_full_inv_freqs=TYP%n_full_inv_freqs+1
               TYP%iw_full(TYP%n_full_inv_freqs)=i2
             enddo
             !
           endif
           iw=-1
           nw=0
         endif
       enddo
       min_dist_non_conv_freqs=min_dist_non_conv_freqs-1
       if (min_dist_non_conv_freqs==0) found=.TRUE.
       !
       if (TYP%n_full_inv_freqs>=nint(real(W%n_freqs)/TYP%PERT_FULL_ratio)) found=.TRUE.
       !
     enddo
     !
     if (TYP%n_full_inv_freqs>0.and.FULL) then 
       if (TYP%what=="EPS") Epsilon_ii(:,2)=Epsilon_ii(:,2)/real(PAR_COM_SLK_INDEX_local%n_CPU,SP)
#if defined _PL
       if (TYP%what=="PLS") PL(:,2)=PL(:,2)/real(PAR_COM_SLK_INDEX_local%n_CPU,SP)
#endif
     endif
     !
     call msg('rs','[FULL INV@'//TYP%what//'] Frequencies TODO :',TYP%n_full_inv_freqs)
     !
   end subroutine
   !
   subroutine LOCAL_clean
#if defined _PL
     call BSS_PL_free( )
#endif
     !
     ! GPL_EXCLUDE_START
     !
#if defined _KERR
     call BSS_KERR_free( )
#endif
     !
     ! GPL_EXCLUDE_END
     !
     call LOCAL_TYP_clean(K_INV_EPS)
#if defined _PL
     call LOCAL_TYP_clean(K_INV_PL)
#endif
     !
     call PP_indexes_reset(PAR_IND_freqs)
     YAMBO_FREE(Lo)
     !
#if defined _SCALAPACK
     YAMBO_FREE(K_slk%blc)
     call MATRIX_reset(K_slk)
     !
     ! Here I call MATRIX_duplicate instead of a simple YAMBO_FREE to keep the correct naming 
     ! to be used in the memory module
     !
     call MATRIX_duplicate( "free", M_out=K_inv_slk )
#endif
     !
#if defined _TIMING
     call timing('Inversion Solver',OPR='stop')
#endif
   end subroutine
   !
   subroutine LOCAL_TYP_clean(TYP)
     type(K_Inversion_data) :: TYP
     deallocate(TYP%err,TYP%spectra,TYP%iw_full)
   end subroutine
   !
end subroutine
