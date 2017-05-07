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
integer function X_dielectric_matrix(Xen,Xk,q,X,Xw,APPEND_NO_VERIFY)
 ! 
 ! Calculates and stores on file the dielectric matrix.
 !
 use pars,          ONLY:SP
 use drivers,       ONLY:l_alda_fxc,l_gw0,l_life
#if defined _PL
 use drivers,       ONLY:l_photolum
#endif
 use X_m,           ONLY:X_t,X_alloc,X_mat,self_detect_E_range,X_poles,X_is_evaluated_for_all_Qs,&
&                        l_X_terminator,X_terminator_Kind,Chi_linalg_mode,X_use_lin_sys
 use frequency,     ONLY:w_samp
 use R_lattice,     ONLY:bz_samp,nqibz
 use electrons,     ONLY:levels
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_Q,PAR_COM_Q_A2A,PAR_nq,PAR_Q_index, &
&                        PAR_IND_Xk_ibz,PAR_IND_CON_BANDS_X,PAR_IND_VAL_BANDS_X,PAR_COM_Q_INDEX
 use wave_func,     ONLY:WF_buffered_IO,WF,WF_buffer
 use IO_m,          ONLY:io_control,OP_RD_CL,OP_APP_WR_CL,VERIFY,REP,io_RESPONSE,OP_WR_CL
 use com,           ONLY:depth,error
 use TDDFT,         ONLY:FXC
 use interfaces,    ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,&
&                        PARALLEL_WF_index,WF_load
 use QP_m,          ONLY:QP_n_W_freqs_redux
 !
#include<memory.h>
 !
 type(levels) ::Xen 
 type(bz_samp)::Xk,q
 type(X_t)    ::X
 type(w_samp) ::Xw
 logical      ::APPEND_NO_VERIFY
 !
 ! Work Space
 !
 integer              :: iq,Xdb,i_err,iq_mem,X_range(2),X_n_freqs,io_X_done(nqibz)
 real(SP)             :: minmax_ehe(2)
 integer, external    :: io_X
 character(1)         :: sec_mode

 !
 ! logicals and control
 !
 self_detect_E_range=.false.
 !
 X_use_lin_sys=.false.
 if (trim(Chi_linalg_mode)=="lin_sys") X_use_lin_sys=.true.
 !
 l_X_terminator = X_terminator_Kind/='none'
 if (l_X_terminator.and.trim(X_terminator_Kind)/='BG') &
&   call error(' [X] unknown X_terminator_Kind = '//trim(X_terminator_Kind))
 !
 ! TDDFT SETUP (-1 argument)
 !
 ! X%f_xc is setup when using ALDA, so that 
 ! the comparison in ioX can be done safely
 !
 call TDDFT_do_X_W_typs(-1,X,Xw)
 !
 ! Sectioning
 !
 sec_mode='*'
 if (depth>0) sec_mode='='
 !
 X_dielectric_matrix=-1
 !
 if (.not.APPEND_NO_VERIFY) then
   if (X%whoami==2) then
     call section(sec_mode,'Static Dielectric Matrix')
     Xw%n_freqs=1
     X_n_freqs =1
   else if (X%whoami==3) then
     call section(sec_mode,'Dynamical Dielectric Matrix')
     if (l_gw0) self_detect_E_range=.true.
   else if (X%whoami==4) then
     call section(sec_mode,'Dynamic Dielectric Matrix (PPA)')
     Xw%n_freqs=2
     X_n_freqs =2
     Xw%er=(/epsilon(1._SP),0._SP/)
     Xw%dr=(/0._SP,X%ppaE/)
     X%ordering='R'
   endif
 endif
 !
 ! X_n_freqs defines the size of each q-block loaded in memory when io_RESPONSE=.FALSE.
 ! Note that in lifetimes calculations it is QP_n_W_freqs_redux=MAX(all q-dependent frequencies)
 !
 X_n_freqs             = Xw%n_freqs
 if (l_life) X_n_freqs = QP_n_W_freqs_redux
 !
 call X_pre_setup(Xen,X)
 !
 ! Build frequency range only if Xw%p was not already allocated
 ! like in LifeTimes calculations or when self_detect_E_range=.TRUE. (real axis GW)
 !
 call FREQUENCIES_setup(Xw)
 !
 ! Prepare the IO (and check if q has been already done)
 !
 io_X_done=-1
 !
 if (X%iq(1)>1) io_X_done(:X%iq(1)-1)=1
 !
 ! X%ngostnts is defined in DIPOLE_driver. It may happen that this is called only for the group of CPU's that eval q=1. 
 ! To avoid erroneous errors in the VERIFY mode this value must be spread to all CPUs before doing the IO of the header
 !
 if (l_life.and.X%iq(1)>1) then
   call PP_redux_wait((/X%ngostnts/),COMM=PAR_COM_Q_INDEX%COMM)
 endif
 !
 if (io_RESPONSE) then
   !
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=Xdb)
   i_err=io_X(X,Xw,Xdb)
   !
   if (i_err==0) then
     !
     do iq=X%iq(1),X%iq(2)
       !
       ! In the next lines Yambo will VERIFY the em1d database
       ! to check if this iq has been already done. 
       ! When self_detect_E_range=.TRUE. however the Xw%er setup is
       ! done only in X_irredux and the VERIFY fails. This is why the procedure
       ! must be repeated here:
       !
       if (self_detect_E_range) then
         call X_eh_setup(-iq,X,Xen,Xk,minmax_ehe)
         YAMBO_FREE(X_poles)
         Xw%er=minmax_ehe
       endif
       !
       call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/2*iq/),MODE=VERIFY,ID=Xdb)
       io_X_done(iq)=io_X(X,Xw,Xdb)
       !
     enddo
     !
   endif
   !
   ! All q databases done
   !
   if (all(io_X_done(X%iq(1):X%iq(2))>0)) then
     call FREQUENCIES_reset(Xw,"all")
     if (X%iq(1)==X%iq(2)) X_dielectric_matrix=X%iq(1)
     return
   endif
   !
 endif
 !
 ! Parallel distribution
 !=======================
 if (.not.(l_life.and.X%iq(1)>1)) then
   call PARALLEL_global_indexes(Xen,Xk,q," ",RESET=.TRUE.,X=X)
   call PARALLEL_global_indexes(Xen,Xk,q,"Response_G_space",X=X) 
 endif
 if (.not.allocated(X_mat)) then
   if (     io_RESPONSE) call X_alloc('X',(/X%ng,X%ng,Xw%n_freqs/))
   if (.not.io_RESPONSE) call X_alloc('X',(/X%ng,X%ng,X_n_freqs*PAR_nq/))
 endif
 !
 ! WF distribution 
 !=================
 !
 if (.not.(l_life.and.X%iq(1)>1)) then
   call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,&
&                              B_index=PAR_IND_CON_BANDS_X(X%whoami),&
&                              Bp_index=PAR_IND_VAL_BANDS_X(X%whoami),&
&                              CLEAN_UP=.TRUE.)
   !
   if (WF_buffered_IO) then
     !
     call WF_load(WF_buffer,0,1,X%ib,(/1,Xk%nibz/),space='B',title='-BUFFER')
     !
   endif
   !
 endif
 !
 ! As we are cycling over the q-points the screened interaction evaluated here is supposed
 ! to be used as an integrand. Some procedures (like the DbGd) changes accordingly.
 !
 X_is_evaluated_for_all_Qs=.true.
 !
 do iq=X%iq(1),X%iq(2)
   !
   ! TDDFT setup
   !
   call TDDFT_do_X_W_typs(iq,X,Xw)
   !
   ! ALDA (only once) 
   !
   if (l_alda_fxc.and..not.allocated(FXC)) then
     !
     call PARALLEL_WF_index(COMM=PAR_COM_Q_A2A)
     !
     call TDDFT_ALDA_G_space(Xen,Xk)
     !
   endif
   !
   if (     io_X_done(iq) > 0       ) cycle
   if (.not.PAR_IND_Q%element_1D(iq)) cycle
   !
   iq_mem=PAR_Q_index(iq)
   X_range               =(/1,Xw%n_freqs/)
   if (.not.io_RESPONSE) X_range=(/X_n_freqs*(iq_mem-1)+1,X_n_freqs*(iq_mem-1)+Xw%n_freqs/)
   !
   call X_irredux(X_mat(:,:,X_range(1):X_range(2)),iq,Xen,Xk,Xw,X)  
   !
   ! When X%whoami == 1 X is Xo (ACFDT). 
   !
   if (X%whoami/=1) call X_redux(iq,X,Xw,X_range)
   !
   call PP_redux_wait(COMM=PAR_COM_Q_A2A%COMM)
   !
   ! Only the cpus that write the q=1 component knows the value of ng_db. Thus this must 
   ! be re-initialized for all the cpus
   !
   X%ng_db=X%ng
   !
   if (iq==1) call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2*iq,2*iq+1/),ID=Xdb)
   if (iq> 1) call io_control(ACTION=OP_APP_WR_CL,COM=REP,SEC=(/2*iq,2*iq+1/),ID=Xdb)
   i_err=io_X(X,Xw,Xdb)
   !
   ! CLEAN (1)
   ! Frequencies must not be cleaned in the case of lifetimes calculations when the EM1d
   ! database is not written on disk
   !
   if (.not.io_RESPONSE.and.l_life) then
     call FREQUENCIES_reset(Xw,"coarse_grid")
   else
     call FREQUENCIES_reset(Xw,"all")
   endif
   !
 enddo
 !
 X_is_evaluated_for_all_Qs=.false.
 l_X_terminator=.false.
 !
 ! CLEAN (2)
 !
 if (io_RESPONSE) call X_alloc('X')
 !
 call X_alloc('DIP_projected')
#if defined _PL
 if (l_photolum) call X_alloc('DIP_P')
#endif
 if (.not.APPEND_NO_VERIFY.or.X%iq(2)==q%nibz) call WF_free(WF)
 !
end function
