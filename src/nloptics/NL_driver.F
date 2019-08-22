!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG CA
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
subroutine NL_driver(E,X,k,q)
 !
 use pars,           ONLY:SP,cZERO,cONE
 use drivers,        ONLY:l_use_Hxc_collisions
 use stderr,         ONLY:set_real_printed_length
 use com,            ONLY:error
 use LIVE_t,         ONLY:live_timing 
 use electrons,      ONLY:levels
 use X_m,            ONLY:X_t,Dipole_approach,X_alloc,DIP_P
 use wave_func,      ONLY:WF,wf_ng
 use interfaces,     ONLY:WF_load,WF_free
 use hamiltonian,    ONLY:WF_G_max,WF_Go_indx,H_QP_table_setup
 use R_lattice,      ONLY:bz_samp,qindx_free
 use fields,         ONLY:Efield
 use QP_CTL_m,       ONLY:QP_apply
 use R_lattice,      ONLY:qindx_S
 use nl_optics,      ONLY:n_frequencies,Correlation,IPA,NL_alloc,NL_free,V_bands,eval_dG,NL_J,NL_initial_J, &
&                         NL_estep,NL_er,Integrator,l_use_DIPOLES,NL_P_t,eval_COLLISIONS,NL_ng, &
&                         E_ext_t,E_tot_t,E_ks_t,E_ks,E_tot,E_ext,freqs_done,n_freqs_todo,l_eval_CURRENT,&
&                         HF,SEX,NL_bands,eval_OVERLAPS,NL_nbands,WF_sample,V_bands_t,NL_alloc_k_distributed
 use real_time,      ONLY:RT_step,NE_steps,NE_i_time,RT_P,RT_step,RT_nbands,RT_bands
 use IO_m,           ONLY:IO_and_Messaging_switch,io_COLLs
 use interfaces,     ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use parallel_m,     ONLY:PAR_IND_WF_k,PAR_IND_freqs,PAR_IND_freqs_ID,PAR_IND_Xk_ibz
 !
#include<memory.h>
 !
 ! Notice that E is never changed!
 !
 type(levels)      :: E
 type(bz_samp)     :: k,q
 type(X_t)         :: X(2)
 !
 logical, external :: stop_now
 integer, external :: eval_G_minus_G
 ! 
 ! Work Space
 !
 integer           :: i_f,i1,NL_bands1_save,N_sample_size,wf_ng_save
 logical           :: l_dynamics_is_over
 real(SP)          :: Time
 !
 ! In Non-Linear NL_bands always equal to SC_bands
 ! ==============================================
 !
 NL_nbands=NL_bands(2)-NL_bands(1)+1
 !
 RT_bands =NL_bands
 RT_nbands=NL_nbands
 !
 call section('+','Initialization')
 !#################################
 !
 call set_real_printed_length(f_length=10,g_length=10)  
 !
 ! Initialize
 !=========
 call NL_initialize(E,k,q)
 !
 ! Switch 
 !========
 call XC_switch()
 !
 ! Dipoles & WF dimensions
 !=========================
 call WF_and_dipole_dimensions(X)
 !
 ! Deallocate qindx_S (used only in previous subroutine)
 ! ========================
 YAMBO_FREE(qindx_S)
 !
 ! QP dimensions
 !================
 call H_QP_table_setup(E)
 !
 ! Parallelization
 ! =======================
 call setup_DIPOLES_parallelization()
 !
 ! Dipoles
 !=========
 !
 ! before loading the WF in R-space I check and/or calculate
 ! the optical collisions that are done in G-space and would
 ! force the reloading of the WFs
 !
 Dipole_approach='Covariant' 
 if(l_use_DIPOLES.or.l_eval_CURRENT) then
    call DIPOLE_driver(E,k,X(1),X(1)%q0)
 else
     if(load_Overlaps()/=0) call DIPOLE_driver(E,k,X(1),X(1)%q0)
 endif
 !
 !
 ! Free memory: qindx_* and unused DIPOLES
 ! =====================================================
 call qindx_free()
 !
 call X_alloc('P_square')
 if(l_use_DIPOLES) then
   call X_alloc('DIP_S')
 else
   call X_alloc('DIP_R')
   if(.not.l_eval_CURRENT) call X_alloc('DIP_P')
 endif
 !
 ! Non_Linear parallization
 ! ===================================
 call setup_NL_parallelization()
 call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,CLEAN_UP=.TRUE.)
 call PARALLEL_WF_index( )
 !
 ! WFs loading
 !=============
 !
 wf_ng_save=wf_ng
 wf_ng     =NL_ng
 WF_Go_indx =1
 !
 if(Correlation/=IPA) then
   call WF_load(WF,wf_ng,WF_Go_indx,(/1,NL_bands(2)/),(/1,k%nibz/),space='R',title='-NL')  
 endif
 !
 ! QP-corrections 
 !================
 call QP_apply(NL_bands,E,k,"G",msg_fmt='s')
 !
 ! Load Collisions if necessary
 ! ============================
 if(io_COLLs) then
   !      
   ! check if they exist      
   !
   call COLLISIONS_load(.true.)
   !
   eval_dG=l_use_Hxc_collisions
   !
   if(.not.l_use_Hxc_collisions .and. (Correlation==SEX.or.Correlation==HF)) &
&    call error(' HF / SEX need Collisions. Cannot be computed on the fly.')
   !
   ! and load ....
   !
   if(l_use_Hxc_collisions)  call COLLISIONS_load(.false.)
   !
 endif
 !
 ! Allocation
 ! ==================
 call NL_alloc(E) 
 call NL_alloc_k_distributed(E)
 !
 ! Initialization
 ! ==================
 call NL_start_and_restart(E,k,X)
 !
 ! Check if calculation was already done
 ! =====================================
 if(n_freqs_todo==0) return
 !
 ! Initialize IO
 ! ==================
 call NL_databases_IO(0,'write')
 !
 ! Start-Up & Timing
 !===================
 call live_timing('[NL] Frequencies :',PAR_IND_freqs%n_of_elements(PAR_IND_freqs_ID+1)*NE_steps)
 !
 do i_f=1,n_frequencies
   !
   if(.not.PAR_IND_freqs%element_1D(i_f)) cycle
   !
   if(freqs_done(i_f)) then
     call live_timing(steps=NE_steps)
     cycle
   endif
   !
   ! Re-initialization
   ! ================
   V_bands=cZERO
   forall(i1=1:E%nbf) V_bands(i1,i1,:,:)=cONE
   if(WF_sample%ison) V_bands_t = cZERO
   !
   ! Efield
   ! ================
   Efield(1)%frequency=NL_er(1)+real(i_f-1,SP)*NL_estep
   !
   l_dynamics_is_over=.false.
   !
   NE_i_time=0
   N_sample_size=0
   !
   do while(.not.l_dynamics_is_over)
     !
     NE_i_time=NE_i_time+1
     !
     Time=real(NE_i_time-1,SP)*RT_step
     !
     call NL_Hamiltonian(E,k,q,X(1),Time,NE_i_time,V_bands)
     !
     ! I evaluate current after the Hamiltonian because I need
     ! time-dependent overlaps Sm1_plus, and Sm1_mins
     !
     if(l_eval_CURRENT) then
         call NL_current(k,E,V_bands)
         NL_J=NL_J-NL_initial_J
     endif
     !
     ! Store Variables
     !
     NL_P_t (NE_i_time,:)=real(RT_P,SP)
     E_ext_t(NE_i_time,:)=E_ext
     E_tot_t(NE_i_time,:)=E_tot
     E_ks_t (NE_i_time,:)=E_ks
     !
     call NL_output(NE_i_time,i_f) 
     !
     call NL_Integrator(E,k,q,X(1),Integrator,NE_i_time,V_bands)
     !
     call live_timing(steps=1)
     !
     l_dynamics_is_over= (NE_i_time>=NE_steps) .or. stop_now(.false.)
     !
   enddo
   !
   call NL_databases_IO(i_f,'write')
   !
   if(stop_now(.false.)) exit
   !
 enddo
 !
 call live_timing()
 !
 ! Deallocation
 ! ==========================
 !
 wf_ng     = wf_ng_save
 !
 call WF_free(WF)
 call NL_free()
 call X_alloc('DIP_S')
 call X_alloc('DIP_R')
 call X_alloc('DIP_P')
 !
 call set_real_printed_length()
 !
 contains
   !
   subroutine setup_DIPOLES_parallelization()
     use parallel_m,       ONLY:CPU_and_ROLE_strings_save,CPU_and_ROLE_strings_restore,CPU_structure, &
&                               PARALLEL_ENV_uses_default
     use nl_optics,        ONLY:eval_OVERLAPS
     !
     implicit none
     !
     ! Working space
     !
     integer :: n_frequencies_save,NL_bands1_save
     integer :: i_s=8 ! Non-linear environment
     !
     ! Set bands between 1 and NL_bands(2)
     !
     NL_bands1_save  =NL_bands(1)
     NL_bands(1)     =1
     X(1)%ib         =(/1,NL_bands(2)/)
     !
     ! Set n_runs = 1 
     ! Parallelization on frequencies turned off
     !
     n_frequencies_save=n_frequencies
     n_frequencies     =1
     eval_OVERLAPS     =.TRUE.
     !
     ! Save user-defined parallel structure
     ! and force parallelization on k-points (for dipoles and overlaps)
     call CPU_and_ROLE_strings_save()
     !
     !
     ! Erase the user-defined string for non-linear
     ! and force parallelization on k-points
     !
     CPU_structure(i_s)%CPU_string=" "
     CPU_structure(i_s)%ROLE_string=" "
     PARALLEL_ENV_uses_default(i_s)=.true.
     !
     call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.,X=X(1))
     call PARALLEL_global_indexes(E,k,q,"Non_Linear",RESET=.FALSE.,X=X(1))
     !
     call CPU_and_ROLE_strings_restore()
     PARALLEL_ENV_uses_default(i_s)=.false.
     !  
     n_frequencies=n_frequencies_save
     NL_bands(1)=NL_bands1_save
     !
   end subroutine setup_DIPOLES_parallelization
   !
   subroutine setup_NL_parallelization
     use nl_optics,        ONLY:eval_OVERLAPS
     implicit none
     !
     ! Restore original number of bands
     !
     X(1)%ib       =(/NL_bands(1),NL_bands(2)/)
     eval_OVERLAPS=.false.
     !
     call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.,X=X(1))
     call PARALLEL_global_indexes(E,k,q,"Non_Linear",X=X(1))
     !
   end subroutine setup_NL_parallelization

   !
   integer function load_Overlaps()
     !
     use pars,           ONLY:SP
     use X_m,            ONLY:DIP_S
     use IO_m,           ONLY:io_control,OP_RD_CL,VERIFY,REP
     use electrons,      ONLY:n_sp_pol
     use parallel_m,     ONLY:PAR_IND_Xk_bz,PAR_COM_DIPOLES,PAR_IND_Xk_bz_ID,PP_redux_wait
     !
     implicit none
     !
     ! Word Space
     !
     integer :: ID_S,io_err
     integer, external :: io_Overlaps
     integer :: ik,i_sp_pol
     !
     call X_alloc('DIP_S',(/X(1)%ib(2),X(1)%ib(2),6,k%nbz/))
     DIP_S=cZERO
     !
     io_err=0
     !
     call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID_S)
     io_err=io_Overlaps(X,ID_S)
     !
     if(io_err==0) then
       call live_timing('[NL] Overlaps loading :',PAR_IND_Xk_bz%n_of_elements(PAR_IND_Xk_bz_ID+1))
       do ik=1,k%nbz
         if (.not.PAR_IND_Xk_bz%element_1D(ik)                   ) cycle
         do i_sp_pol=1,n_sp_pol
           call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1+ik+(i_sp_pol-1)*k%nbz/),ID=ID_S)
           io_err=io_err+io_Overlaps(X(1),ID_S)
         enddo
         call live_timing(steps=1)
       enddo
       !
       call live_timing()
       !
       do i_sp_pol=1,n_sp_pol
         call PP_redux_wait(DIP_S(:,:,:,:,i_sp_pol),COMM=PAR_COM_DIPOLES%COMM)
       enddo
       !
     endif
     !
     load_Overlaps=io_err
     !
   end function load_Overlaps
   !
end subroutine NL_driver
