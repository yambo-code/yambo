!
!        Copyright (C) 2000-2015 the YAMBO team
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
 use X_m,            ONLY:X_t,Dipole_approach,X_alloc
 use wave_func,      ONLY:WF,wf_ng
 use interfaces,     ONLY:WF_load
 use hamiltonian,    ONLY:WF_G_max,WF_Go_indx,H_QP_table_setup
 use R_lattice,      ONLY:bz_samp,qindx_free
 use fields,         ONLY:Efield
 use QP_CTL_m,       ONLY:QP_apply
 use R_lattice,      ONLY:qindx_S
 use nl_optics,      ONLY:n_frequencies,Correlation,IPA,NL_alloc,NL_free,V_bands,eval_dG, &
&                         NL_estep,NL_er,Integrator,l_use_DIPOLES,NL_P_t,eval_COLLISIONS, &
&                         E_ext_t,E_tot_t,E_ks_t,E_ks,E_tot,E_ext,freqs_done,n_freqs_todo,&
&                         HF,SEX,NL_bands
 use real_time,      ONLY:RT_step,NE_steps,NE_i_time,RT_P,RT_step,RT_nbands,RT_bands
 use IO_m,           ONLY:IO_and_Messaging_switch,io_COLLs
 use interfaces,     ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use parallel_m,     ONLY:PAR_IND_WF_k,PAR_IND_WF_b,PAR_IND_freqs,PAR_IND_freqs_ID
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
 integer           :: i_f,i1,NL_bands1_save
 logical           :: l_dynamics_is_over
 real(SP)          :: Time
 !
 ! In Non-Linear NL_bands always equal to SC_bands
 ! ==============================================
 !
 RT_bands =NL_bands
 RT_nbands=RT_bands(2)-RT_bands(1)+1
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
 if(allocated(qindx_S)) then
   YAMBO_FREE(qindx_S)
 endif
 !
 ! QP dimensions
 !================
 call H_QP_table_setup(E)
 !
 ! Parallelization
 ! =======================
 NL_bands1_save=NL_bands(1)
 NL_bands(1)   =1
 X(1)%ib       =(/1,NL_bands(2)/)
 call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.,X=X(1))
 call PARALLEL_global_indexes(E,k,q,"Non_Linear",RESET=.FALSE.,X=X(1))
 call PARALLEL_WF_distribute(CLEAN_UP=.TRUE.)
 call PARALLEL_WF_index( )
 !
 ! Dipoles
 !=========
 !
 ! before loading the WF in R-space I check and/or calculate
 ! the optical collisions that are done in G-space and would
 ! force the reloading of the WFs
 !
 Dipole_approach='Covariant' 
 if(l_use_DIPOLES .or. load_Overlaps()/=0) call DIPOLE_driver(E,k,X(1),X(1)%q0)
 !
 NL_bands(1)=NL_bands1_save
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
   call X_alloc('DIP_P')
 endif
 !
 ! Non_Linear parallization
 ! ===================================
 call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.,X=X(1))
 call PARALLEL_global_indexes(E,k,q,"Non_Linear",RESET=.FALSE.,X=X(1))
 call PARALLEL_WF_distribute(K_index=PAR_IND_WF_k,B_index=PAR_IND_WF_b,CLEAN_UP=.TRUE.)
 call PARALLEL_WF_index( )
 !
 ! WFs loading
 !=============
 if(Correlation/=IPA) &
&  call WF_load(WF,wf_ng,WF_Go_indx,(/1,NL_bands(2)/),(/1,k%nibz/),space='R',title='-NL')  
 !
 ! QP-corrections 
 !================
 call QP_apply(NL_bands,E,k,"G",msg_fmt='s')
 !
 ! Generate DIPOLES in all the BZ if required
 !===================
 if(l_use_DIPOLES) call Build_DIP_bz(k,E,X)
 !
 !
 ! GPL_EXCLUDE_START
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
 ! GPL_EXCLUDE_END
 !
 ! Allocation
 ! ==================
 call NL_alloc(E) 
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
   !
   ! Efield
   ! ================
   Efield(1)%frequency=NL_er(1)+real(i_f-1,SP)*NL_estep
   !
   l_dynamics_is_over=.false.
   !
   NE_i_time=1
   !
   do while(.not.l_dynamics_is_over)
     !
     Time=real(NE_i_time-1,SP)*RT_step
     !
     call NL_Hamiltonian(E,k,q,X,Time,NE_i_time,V_bands)
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
     call NL_Integrator(E,k,q,X,Integrator,NE_i_time,V_bands)
     !
     call live_timing(steps=1)
     !
     NE_i_time=NE_i_time+1
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
 call WF_free(WF)
 call NL_free()
 call X_alloc('DIP_S')
 !
 call set_real_printed_length()
 !
 contains
   !
   integer function load_Overlaps()
     !
     use pars,          ONLY:SP
     use X_m,            ONLY:DIP_S
     use IO_m,           ONLY:io_control,OP_RD_CL,VERIFY,REP
     use electrons,      ONLY:n_sp_pol
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
       call live_timing('[NL] Overlaps loading :',k%nbz)
       do ik=1,k%nbz
         do i_sp_pol=1,n_sp_pol
           call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1+ik+(i_sp_pol-1)*k%nbz/),ID=ID_S)
           io_err=io_err+io_Overlaps(X,ID_S)
         enddo
         call live_timing(steps=1)
       enddo
       call live_timing()
     endif
     !
     load_Overlaps=io_err
     !
   end function load_Overlaps
   !
end subroutine NL_driver
