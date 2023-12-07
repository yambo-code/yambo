!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
integer function X_dielectric_matrix(Xen,Xk,q,X,Xw,Dip,SILENT_MODE)
 ! 
 ! Calculates and stores on file the dielectric matrix.
 !
 use pars,          ONLY:SP,schlen
 use drivers,       ONLY:l_alda_fxc,l_gw0,l_life,l_rpa_IP 
 use DIPOLES,       ONLY:DIPOLE_t,DIP_alloc
 use X_m,           ONLY:X_t,X_ALLOC_elemental,self_detect_E_range,X_poles,l_recalculate_X,l_write_disk_Xo,&
&                        l_X_terminator,X_terminator_Kind,X_mat,X_par,X_par_lower_triangle,&
&                        X_DbGd_nkpt_at_q,use_X_DbGd,X_DbGd_nkpt_at_q_table,X_Double_Grid_setup,&
&                        n_OPTICAL_dir_to_eval,X_par_average
 use LIVE_t,        ONLY:live_timing
 use stderr,        ONLY:intc
 use frequency,     ONLY:w_samp
 use R_lattice,     ONLY:bz_samp,nqibz
 use electrons,     ONLY:levels
 use parallel_int,  ONLY:PP_wait,PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use parallel_m,    ONLY:PAR_IND_Q_ibz,PAR_Q_ibz_index,PAR_nQ_ibz,PAR_COM_X_WORLD,&
&                        PAR_IND_Xk_ibz,PAR_IND_CON_BANDS_X,PAR_IND_VAL_BANDS_X
 use wave_func,     ONLY:WF_buffered_IO,WF,WF_buffer
 use IO_int,        ONLY:io_control,IO_and_Messaging_switch
 use IO_m,          ONLY:OP_RD_CL,OP_APP_CL,VERIFY,REP,io_RESPONSE
 use com,           ONLY:depth
 use TDDFT,         ONLY:F_xc_gspace
 use interfaces,    ONLY:WF_load,WF_free
 use QP_m,          ONLY:QP_n_W_freqs_redux
 use matrix,        ONLY:MATRIX_reset,MATRIX_copy
 !
#include<memory.h>
 !
 type(levels)  ::Xen 
 type(bz_samp) ::Xk,q
 type(X_t)     ::X
 type(DIPOLE_t)::Dip
 type(w_samp)  ::Xw
 logical       ::SILENT_MODE
 !
 ! External
 !
 integer, external    :: io_X,X_eh_setup
 !
 ! Work Space
 !
 integer              :: iq,ID(2),i_err,X_MEM_n_freqs,io_X_done(nqibz),iq_mem,&
&                        X_MEM_n_tot_freqs,n_poles,iq_dir
 real(SP)             :: minmax_ehe(2)
 character(1)         :: sec_mode
 character(schlen)    :: X_what
 !
 ! logicals and control
 !
 self_detect_E_range=.false.
 !
 ! Check if there is a Efield directions average to be done
 !
 call X_AVERAGE_setup("INIT",-1,X%q0)
 !
 ! Some check
 !
 if(X%ib(2)>Xen%nb) call warning("More bands required in X than available in the current SAVE.")
 !
 l_X_terminator = X_terminator_Kind/='none'
 if (l_X_terminator.and.trim(X_terminator_Kind)/='BG') then
   call error(' [X] unknown X_terminator_Kind = '//trim(X_terminator_Kind))
 endif
 !
 X%ngostnts=Dip%ng
 !
 ! TDDFT KERNEL SETUP (-1 argument)
 !
 call TDDFT_do_X_W_typs(-1,X,Xw)
 !
 ! Sectioning
 !
 sec_mode='*'
 if (depth>0) sec_mode='='
 !
 X_dielectric_matrix=-1
 Xw%grid_type="ra"
 !
 if (X%whoami==1) then
   if (.not.SILENT_MODE) call section(sec_mode,'Dynamical IP Response Function Matrix')
   X_MEM_n_freqs =Xw%n_freqs
 else if (X%whoami==2) then
   if (.not.SILENT_MODE) call section(sec_mode,'Static Dielectric Matrix')
   Xw%n_freqs    =1
   X_MEM_n_freqs =1
   !
 else if (X%whoami==3) then
   !
   if (.not.SILENT_MODE) call section(sec_mode,'Dynamical Dielectric Matrix')
   if (l_gw0) self_detect_E_range=.true.
   !
   ! X_n_freqs defines the size of each q-block loaded in memory when io_RESPONSE=.FALSE.
   ! Note that in lifetimes calculations it is QP_n_W_freqs_redux=MAX(all q-dependent frequencies)
   !
   X_MEM_n_freqs             = Xw%n_freqs
   if (l_life) X_MEM_n_freqs = QP_n_W_freqs_redux
   !
 else if (X%whoami==4) then
   !
   if (.not.SILENT_MODE) call section(sec_mode,'Dynamic Dielectric Matrix (PPA)')
   Xw%n_freqs    =2
   X_MEM_n_freqs =2
   Xw%er=(/epsilon(1._SP),0._SP/)
   Xw%dr=(/0._SP,X%ppaE/)
   X%ordering='R' !DALV: we should use 't'
   !
 else if (X%whoami==5) then
   !
   if (.not.SILENT_MODE) call section(sec_mode,'Dynamic Dielectric Matrix (MPA)')
   !
   if (mod(Xw%n_freqs,2)>0) Xw%n_freqs=Xw%n_freqs+1
   X%mpa_npoles  = Xw%n_freqs/2
   X_MEM_n_freqs =2*X%mpa_npoles
   X%ordering='t'
   Xw%grid_type="mp"
   !
 endif
 !
 call X_pre_setup(Xen,Xk,X)      !DALV: in here the time ordering is changed to 'R' for metals
                                 !      this could be problematic for FF real-axis GW calculations
 ! AF: the following fix breaks Lifetime calcs
 !     and need to be further analyzed
 !
 !if(X%whoami==3) X%ordering='T' !DALV: solved here
 !
 if (l_write_disk_Xo) then
   if (.not.SILENT_MODE) call warning('On-the-fly extraction of IP Response Function Matrix')
 endif
 !
 ! Build frequency range only if Xw%p was not already allocated
 ! like in LifeTimes calculations or when self_detect_E_range=.TRUE. (real axis GW)
 !
 call FREQUENCIES_setup(Xw)
 !
 ! Prepare the IO (and check if q has been already done)
 !
 io_X_done=-1
 i_err=-1
 !
 if (X%iq(1)>1) io_X_done(:X%iq(1)-1)=1
 !
 if (io_RESPONSE.and..not.l_recalculate_X) then
   !
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID(1))
   i_err=io_X(X,Xw,ID(1))
   !
   if (i_err==0) then
     !
     call live_timing("Loading the dielectric function",(X%iq(2)-X%iq(1)+1))
     !
     do iq=X%iq(1),X%iq(2)
       !
       ! In the next lines Yambo will VERIFY the em1d database
       ! to check if this iq has been already done. 
       ! When self_detect_E_range=.TRUE. however the Xw%er setup is
       ! done only in X_os and the VERIFY fails. This is why the procedure
       ! must be repeated here:
       !
       if (self_detect_E_range) then
         n_poles=X_eh_setup(-iq,X,Xen,Xk,minmax_ehe)
         YAMBO_FREE(X_poles)
         Xw%er=minmax_ehe
       endif
       !
       call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/2*iq/),MODE=VERIFY,ID=ID(1))
       io_X_done(iq)=io_X(X,Xw,ID(1))
       !
       call live_timing(steps=1)
       !
     enddo
     !
     call live_timing()
     !
   endif
   !
   ! All q databases done
   !
   if (all(io_X_done(X%iq(1):X%iq(2))>0)) then
     call FREQUENCIES_reset(Xw,"all")
     if (X%iq(1)==X%iq(2)) X_dielectric_matrix=X%iq(1)
     return
   else
     if(X%ib(2)>Xen%nb) call error("Too many bands required in X. Run DFT with more bands!") 
   endif
   !
 endif
 !
 ! Parallel distribution
 !=======================
 if (.not.(l_life.and.X%iq(1)>1)) then
   call PARALLEL_global_indexes(Xen,Xk,q," ",RESET=.TRUE.,X=X)
   call PARALLEL_global_indexes(Xen,Xk,q,"Response_G_space_and_IO",X=X) 
 endif
 call X_half_mat_check(Xen,Xk,Xw)
 !
 if (     io_RESPONSE) X_MEM_n_tot_freqs=Xw%n_freqs
 if (.not.io_RESPONSE) X_MEM_n_tot_freqs=X_MEM_n_freqs*PAR_nQ_ibz
 if (.not.allocated(X_par)) then
   if (     io_RESPONSE) allocate(X_par(1))
#if !defined _PAR_IO
   if (     io_RESPONSE) call X_ALLOC_elemental('X',(/X%ng,X%ng,X_MEM_n_tot_freqs/))
#endif
   if (.not.io_RESPONSE) allocate(X_par(PAR_nQ_ibz))
   if (.not.io_RESPONSE) call X_ALLOC_elemental('X',(/X%ng,X%ng,X_MEM_n_tot_freqs/))
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
 call PP_wait()
 !
 if (X%iq(1)<=1 .and. 1<=X%iq(2) .and. io_X_done(1)<=0 .and. PAR_IND_Q_ibz%element_1D(1) ) then
   !
   ! TDDFT setup
   !
   call TDDFT_do_X_W_typs(1,X,Xw)
   !
   ! DbGd setup
   !
   call X_Double_Grid_setup(1,Xk,Xk%FGbz)
   !
   ! ALDA (only once) 
   !
   if (l_alda_fxc.and..not.allocated(F_xc_gspace)) then
     !
     call PARALLEL_WF_index(COMM=PAR_COM_X_WORLD)
     !
     call TDDFT_ALDA_G_space(Xen,Xk)
     !
   endif
   !
   call                      elemental_IO( 0 , .FALSE. )
   if (l_write_disk_Xo) call elemental_IO( 0 , .TRUE. )
   !
 endif
 !
 call PP_wait()
 !
 do iq=X%iq(1),X%iq(2)
   !
   if (     io_X_done(iq) > 0       ) cycle
   if (.not.PAR_IND_Q_ibz%element_1D(iq)) cycle
   !
   ! Average setup
   !
   call X_AVERAGE_setup("Q-LOOP",iq,X%q0)
   !
   if (iq/=1) then
     !
     ! TDDFT setup
     !
     call TDDFT_do_X_W_typs(iq,X,Xw)
     !
     ! DbGd setup
     !
     call X_Double_Grid_setup(iq,Xk,Xk%FGbz)
     !
     ! ALDA (only once) 
     !
     if (l_alda_fxc.and..not.allocated(F_xc_gspace)) then
       !
       call PARALLEL_WF_index(COMM=PAR_COM_X_WORLD)
       !
       call TDDFT_ALDA_G_space(Xen,Xk)
       !
     endif
     !
   endif
   !
   ! Allocation Index
   iq_mem=PAR_Q_ibz_index(iq)
   if (io_RESPONSE) iq_mem=1
   !
   ! Live timing string
   X_what=" "
   !
   ! n_OPTICAL_dir_to_eval is set in the previous call to X_AVERAGE_setup("Q-LOOP").
   ! There, for iq>1 n_optical_dir_to_eval is set = 1
   !
   do iq_dir=1,n_OPTICAL_dir_to_eval
     !
     call X_ALLOC_parallel(X_par(iq_mem),X%ng,Xw%n_freqs,"XUP")
     !
     if (n_OPTICAL_dir_to_eval>1) call X_AVERAGE_setup("DIR-LOOP",iq_dir,X%q0)
     if (n_OPTICAL_dir_to_eval>1) X_what="("//trim(intc(iq_dir))//" of "//trim(intc(n_OPTICAL_dir_to_eval))//")"
     !
     call X_irredux(iq,"Xo"//trim(X_what),X_par(iq_mem),Xen,Xk,Xw,X,Dip)  
     !
     if (l_write_disk_Xo.and.n_OPTICAL_dir_to_eval==1) then
       call X_mat_filling(iq_mem,X,Xw,X_MEM_n_freqs,X_MEM_n_tot_freqs,X_mat)
       call elemental_IO( iq , .TRUE. )
     endif
     !
     if (.not.l_rpa_IP) call X_redux(iq,"X"//trim(X_what),X_par(iq_mem),Xw,X)
     !
     if (n_OPTICAL_dir_to_eval>1.and.iq_dir==1) call MATRIX_copy(X_par(iq_mem),X_par_average,.TRUE.)
     if (n_OPTICAL_dir_to_eval>1)               call X_AVERAGE_do_it("ACCUMULATE",X_par(iq_mem))
     !
   enddo
   !
   if (n_OPTICAL_dir_to_eval>1) then
     call X_AVERAGE_do_it("DUMP",X_par(iq_mem))
     call X_AVERAGE_setup("CLOSE",iq,X%q0)
   endif
   !
   ! X_par => X_mat
   !
   call X_mat_filling(iq_mem,X,Xw,X_MEM_n_freqs,X_MEM_n_tot_freqs,X_mat)
   !
   call elemental_IO( iq , .FALSE. )
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
   ! CLEAN (2)
   if (io_RESPONSE) call MATRIX_reset(X_par(iq_mem))
   YAMBO_FREE(X_par_lower_triangle%blc)
   call MATRIX_reset(X_par_lower_triangle)
   !
   if (use_X_DbGd) then
     YAMBO_FREE(X_DbGd_nkpt_at_q)
     YAMBO_FREE(X_DbGd_nkpt_at_q_table)
   endif  
   !
 enddo
 !
 l_X_terminator=.false.
 !
 ! CLEAN (3)
 !
 if (io_RESPONSE) then
#if !defined _PAR_IO
   YAMBO_FREE(X_mat)
#endif
   deallocate(X_par)
 endif
 if (allocated(F_xc_gspace)) then
   YAMBO_FREE(F_xc_gspace)
 endif
 call DIP_alloc('DIP_iR')
 call DIP_alloc('DIP_P')
 call DIP_alloc('DIP_v')
 if (.not.SILENT_MODE.or.X%iq(2)==q%nibz) call WF_free(WF)
 !
 contains
   !
   subroutine elemental_IO(iq_,this_is_Xo)
     !
     use IO_m,          ONLY:NONE
     use pars,          ONLY:schlen
     use parallel_m,    ONLY:PAR_COM_RL_A2A,PAR_COM_RL_INDEX,yMPI_comm,COMM_copy,PAR_COM_NULL,master_cpu
     !
     integer :: iq_,whoami_save,idb,COM,SEC(2)
     logical :: this_is_Xo,reset_permissions,condition
     character(schlen) :: KERNEL_save
     type(yMPI_comm)   :: LOCAL_yMPI_COMM
     !
     ! Only the cpus that write the q=1 component knows the value of ng_db. Thus this must 
     ! be re-initialized for all the cpus
     !
     X%ng_db=X%ng
     idb=1
     COM=REP
     !
     if (this_is_Xo) then
       COM=NONE
       whoami_save=X%whoami
       KERNEL_save=X%KERNEL
       X%whoami=1
       idb=2
       X%KERNEL="IP"
     endif
     !
     reset_permissions=.true.
     call COMM_copy(PAR_COM_NULL,LOCAL_yMPI_COMM)
     condition=PAR_COM_X_WORLD%CPU_id==0
     !
     ! Only the master writes the header
     if(iq_==0) condition=master_cpu
     !
#if defined _PAR_IO
     reset_permissions=(iq_==0)
     if(iq_/=0) then
       call COMM_copy(PAR_COM_RL_INDEX,LOCAL_yMPI_COMM)
       condition=PAR_COM_RL_A2A%CPU_id==0
     endif
#endif
     !
     if (reset_permissions) then
       call IO_and_Messaging_switch("SAVE")
       call IO_and_Messaging_switch("+io_out",CONDITION=condition)
     endif
     !
     SEC           =(/1,1/)
     if (iq_>0) SEC=(/2*iq_,2*iq_+1/)
     !
     call io_control(ACTION=OP_APP_CL,COM=COM,SEC=SEC,ID=ID(idb),COMM=local_yMPI_COMM,DO_IT=condition)
     i_err=io_X(X,Xw,ID(idb))
     !
     if (this_is_Xo) then
       X%whoami=whoami_save
       X%KERNEL=KERNEL_save
     endif
     !
     if (reset_permissions) call IO_and_Messaging_switch("RESTORE")
     !
   end subroutine
   !
end function
