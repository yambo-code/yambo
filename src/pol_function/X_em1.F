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
integer function X_em1(Xen,Xk,q,X,Xw,APPEND_NO_VERIFY)
 ! 
 ! Calculates and stores on file the dielectric matrix.
 !
 use pars,          ONLY:SP
 use drivers,       ONLY:l_alda_fxc,l_gw0
 use X_m,           ONLY:X_t,X_alloc,X_mat,self_detect_E_range
 use frequency,     ONLY:w_samp
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_Q,PAR_COM_Q_A2A,PAR_nq,PAR_Q_index,&
&                        PAR_IND_Xk_ibz,PAR_IND_CON_BANDS_X,PAR_IND_VAL_BANDS_X,PAR_COM_Q_INDEX
 use wave_func,     ONLY:WF_buffered_IO,WF,WF_buffer
 use IO_m,          ONLY:io_control,OP_RD_CL,OP_APP_WR_CL,OP_WR_CL,VERIFY,REP,&
&                        IO_and_Messaging_switch,io_X
 use com,           ONLY:depth
 use TDDFT,         ONLY:FXC
 use interfaces,    ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,&
&                        PARALLEL_WF_index,WF_load
 implicit none
 type(levels) ::Xen 
 type(bz_samp)::Xk,q
 type(X_t)    ::X
 type(w_samp) ::Xw
 logical      ::APPEND_NO_VERIFY
 !
 ! Work Space
 !
 integer           :: iq,Xdb,i_err,iq_to_start,iq_mem,X_range(2)
 real(SP)          :: minmax_ehe(2)
 integer, external :: ioX
 character(1)      :: sec_mode
 !
 self_detect_E_range=.false.
 !
 ! TDDFT SETUP (-1 argument)
 !
 ! X%f_xc is setup when using ALDA, so that 
 ! the comparison in ioX can be done safely
 !
 call tddft_do_X_W_typs(-1,X,Xw)
 ! 
 ! Sectioning
 !
 sec_mode='*'
 if (depth>0) sec_mode='='
 !
 X_em1=-1
 !
 if (.not.APPEND_NO_VERIFY) then
   if (X%whoami==2) then
     call section(sec_mode,'Static Dielectric Matrix')
     Xw%n_freqs=1
   else if (X%whoami==3) then
     call section(sec_mode,'Dynamical Dielectric Matrix')
     if (l_gw0) self_detect_E_range=.true.
   else if (X%whoami==4) then
     call section(sec_mode,'Dynamic Dielectric Matrix (PPA)')
     Xw%n_freqs=2
     Xw%er=(/epsilon(1._SP),0._SP/)
     Xw%dr=(/0._SP,X%ppaE/)
     X%ordering='c'
   endif
 endif
 !
 call X_pre_setup(Xen,X)
 !
 ! In the next lines Yambo will VERIFY the em1d database
 ! to check if this iq has been alrady done. 
 ! When self_detect_E_range=.TRUE. however the Xw%er setup is
 ! done only in X_os and the VERIFY fails. This is why the procedure
 ! must be repeated here:
 !
 if (self_detect_E_range) then
   call X_eh_setup(-X%iq(1),X,Xen,Xk,minmax_ehe)
   Xw%er=minmax_ehe
 endif
 !
 ! Build frequency range only if Xw%p was not already allocated
 ! like in LifeTimes calculations or when self_detect_E_range=.TRUE. (real axis GW)
 !
 call FREQUENCIES_setup(Xw)
 !
 ! Prepare the IO ( and check if q has been already done)
 !
 iq_to_start=X%iq(1)
 if (.not.APPEND_NO_VERIFY) then
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=Xdb)
   X_em1=ioX(X,Xw,Xdb)
   if (X_em1==0) return
   if (X_em1> 0) iq_to_start=X_em1
 endif
 !
 ! Parallel distribution
 !=======================
 !
 call PARALLEL_global_indexes(Xen,Xk,q," ",RESET=.TRUE.,X=X)
 call PARALLEL_global_indexes(Xen,Xk,q,"Response_G_space",X=X) 
 if (io_X)      call X_alloc('X',(/X%ng,X%ng,Xw%n_freqs/))
 if (.not.io_X) call X_alloc('X',(/X%ng,X%ng,Xw%n_freqs*PAR_nq/))
 !
 ! WF distribution 
 !=================
 !
 call PARALLEL_WF_distribute(X%ib(2),Xk%nibz,K_index=PAR_IND_Xk_ibz,&
&                            B_index=PAR_IND_CON_BANDS_X(X%whoami),&
&                            Bp_index=PAR_IND_VAL_BANDS_X(X%whoami),&
&                            CLEAN_UP=.TRUE.)
 !
 if (WF_buffered_IO) then
   !
   call WF_load(WF_buffer,0,1,X%ib,(/1,Xk%nibz/),space='B',title='-BUFFER',force_WFo=.true.)
   !
 endif
 !
 do iq=iq_to_start,X%iq(2)
   !
   ! TDDFT setup
   !
   call tddft_do_X_W_typs(iq,X,Xw)
   !
   ! ALDA (only once) 
   !
   if (l_alda_fxc.and..not.allocated(FXC)) then
     !
     call PARALLEL_WF_index(COMM=PAR_COM_Q_A2A)
     !
     call tddft_alda_g_space(Xen,Xk)
     !
   endif
   !
   if (.not.PAR_IND_Q%element_1D(iq)) cycle
   !
   iq_mem=PAR_Q_index(iq)
   X_range=(/1,Xw%n_freqs/)
   if (.not.io_X) X_range=(/Xw%n_freqs*(iq_mem-1)+1,iq_mem*Xw%n_freqs/)
   !
   call X_os(X_mat(:,:,X_range(1):X_range(2)),iq,Xen,Xk,Xw,X)  
   !
   ! When X%whoami == 1 X is Xo (ACFDT). 
   !
   if (X%whoami/=1) call X_s(iq,X,Xw,X_range)
   !
   call PP_redux_wait(COMM=PAR_COM_Q_A2A%COMM)
   !
   call IO_and_Messaging_switch("+io_out",CONDITION=PAR_COM_Q_A2A%CPU_id==0)
   !
   if (iq==iq_to_start) call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2,3/),ID=Xdb) 
   !
   ! Only the cpus that write the q=1 component knows the value of ng_db this must 
   ! be initialized for all the cpus
   !
   if (iq>iq_to_start) then
     X%ng_db=X%ng
     call io_control(ACTION=OP_APP_WR_CL,COM=REP,SEC=(/2*iq,2*iq+1/),ID=Xdb)
   endif
   !
   i_err=ioX(X,Xw,Xdb)
   !
   ! CLEAN (1)
   !
   call FREQUENCIES_reset(Xw)
   !
 enddo
 !
 ! X%ngostnts is defined only for the group of CPU's that
 ! eval q=1 and must be spread to all CPUs to allow futrher I/O
 !
 call PP_redux_wait((/X%ngostnts/),COMM=PAR_COM_Q_INDEX%COMM)
 !
 ! CLEAN (2)
 !
 if (io_X) call X_alloc('X')
 call X_alloc('DIP_q_dot_iR')
 if (.not.APPEND_NO_VERIFY.or.X%iq(2)==q%nibz) call WF_free(WF)
 !
end function
