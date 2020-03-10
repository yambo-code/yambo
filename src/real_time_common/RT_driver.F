!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA DS
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
subroutine RT_driver(E,X,k,q,Dip)
 !
 use pars,           ONLY:SP,schlen
 use drivers,        ONLY:l_use_collisions,l_use_Hxc_collisions,l_elph_scatt,l_elel_scatt,l_elphoton_scatt
 use stderr,         ONLY:real2ch
 use LIVE_t,         ONLY:live_timing 
 use electrons,      ONLY:levels,n_spin,n_spinor
 use X_m,            ONLY:X_t
 use DIPOLES,        ONLY:DIPOLE_t
 use wave_func,      ONLY:WF
 use FFT_m,          ONLY:fft_size
 use interfaces,     ONLY:WF_load,WF_free
 use R_lattice,      ONLY:bz_samp,qindx_free
 use IO_m,           ONLY:OP_WR_CL,NONE,io_control
 use hamiltonian,    ONLY:WF_G_max,WF_Go_indx,rho_n,magn_n,&
&                         H_potential,H_kind,H_xc_functional,H_perturbation
 use RT_control,     ONLY:RT_control_alloc,RT_control_free,ID,&
&                         NE_i_start_time,RT_carriers_object,RT_apply
 use real_time,      ONLY:RT_potential,RT_kind,RT_xc_functional,RT_perturbation,&
&                         NE_steps,RT_step,G_lesser,RT_bands,&
&                         RT_alloc,RT_free,NE_i_time,eval_energy,&
&                         rho_reference,magn_reference, &
&                         NE_time,RT_carriers,RT_dyn_step,l_RT_is_WFfree,&
&                         dG_lesser,l_initial_equilibrium,NE_tot_time,&
&                         i_MEM_prev,i_MEM_now,RT_is_dynamically_dephased,&
&                         l_RT_update_Energies,l_NE_with_fields,l_update_SOC,l_velocity_gauge_corr
 use plasma,          ONLY:PLASMA_free,EH_gas
 use com,             ONLY:msg
 use fields,          ONLY:A_tot,Efield
 use collision_ext,   ONLY:HXC_COLL_element,GW_NEQ_COLL_element,COLLISIONS_have_HARTREE
 use QP_CTL_m,        ONLY:QP_apply
 use interfaces,      ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use parallel_m,      ONLY:PAR_IND_WF_b,PAR_IND_WF_k,PAR_IND_DIPk_ibz,PAR_IND_B_mat,PP_bcast,PP_redux_wait
 use IO_m,            ONLY:IO_and_Messaging_switch,io_COLLs
 use units,           ONLY:FS2AUT
 !
#include<memory.h>
 !
 ! Notice that E is never changed!
 !
 type(levels)      :: E
 type(bz_samp)     :: k,q
 type(X_t)         :: X(2)
 type(DIPOLE_t)    :: Dip
 logical, external :: STOP_now
 ! 
 ! Work Space
 !
 integer           :: io_REF,io_err
 logical           :: l_Dynamics_is_over,LT_pump
 character(schlen) :: sec_title(3)
 real(SP)          :: OBSERVABLES(25)
 !
 ! I/O
 !
 integer, external :: io_RT_components
 !
 ! Timing windows
 !
 integer           :: LT_pump_steps
 real(SP)          :: LT_pump_duration
 !
 sec_title   =' '
 sec_title(1)='none'
 if (l_elph_scatt)                  sec_title(1)='el-ph'
 if (l_elel_scatt)                  sec_title(2)='el-el'
 if (l_elph_scatt.and.l_elel_scatt) sec_title(2)='+el-el'
 if (l_elphoton_scatt)              sec_title(3)='el-photon'
 if (l_elphoton_scatt.and.&
&   (l_elel_scatt.or.l_elph_scatt)) sec_title(3)='+el-photon'
 !
 ! Potential 
 !===========
 call XC_potentials(H_potential)
 call XC_additional_SC_potentials()
 !
 RT_potential    =H_potential
 RT_kind         =H_kind
 RT_xc_functional=H_xc_functional
 RT_perturbation =H_perturbation
 !
 call section('*',trim(RT_potential)//' Real-Time Dynamics')
 !
 ! GPL_EXCLUDE_START
 !
 call msg('s','Scattering(s)'//trim(sec_title(1))//' '//trim(sec_title(2))//' '//trim(sec_title(3)))
 !
 ! GPL_EXCLUDE_END
 !
 call section('+','Initialization')
 !#################################
 !
 ! Parallel Setups
 !=================
 !
 call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.,X=X(1))
 call PARALLEL_global_indexes(E,k,q,"Real_Time",RESET=.FALSE.,X=X(1))
 !
 ! QP & carriers corrections 
 !===========================
 !
 call QP_apply(RT_bands,E,k,     "G",msg_fmt='rs')
 !
 ! Dipoles
 !=========
 !
 call DIPOLE_dimensions(E,Dip,RT_bands,(/0._SP,0._SP,0._SP/))
 call DIPOLE_IO(k,E,Dip,'read ',io_err,'RT')
 !
 ! In dipole_driver the IO is k-dictated. Here I want only the master to write
 !
 call IO_and_Messaging_switch("RESET")
 !
 ! GPL_EXCLUDE_START
 !
 call RT_apply(RT_bands,E,k,what="G",VERBOSE=.true.)
 !
 ! GPL_EXCLUDE_END
 !
 ! Core initialization
 !
 call RT_initialize(E,k,q)
 !
 ! Dipoles & WF dimensions
 !=========================
 !
 call WF_and_dipole_dimensions(X)
 !
 ! WFs loading
 !=============
 if ( .not. l_RT_is_WFfree ) then
   !
   call PARALLEL_WF_distribute(K_index=PAR_IND_WF_k,B_index=PAR_IND_WF_b,CLEAN_UP=.TRUE.)
   call PARALLEL_WF_index( )
   !
   ! GPL_EXCLUDE_START
   if(n_spinor==2.and.l_update_SOC) call WF_derivative((/RT_bands(1),RT_bands(2)/),(/1,k%nibz/),(/.true.,.true.,.true./))
   ! GPL_EXCLUDE_END
   !
   call WF_load(WF,WF_G_max,WF_Go_indx,(/RT_bands(1),RT_bands(2)/),(/1,k%nibz/),space='R',title='-RT',keep_states_to_load=.true.)
   !
   ! As explained in PARALLEL_global_indexes if a QP parallelization is used (k=b=1 cpu)
   ! this means that the WF's are distributed only among few cpu's. But, as a consequence, the fft_size
   ! is not defined for all CPU's. This causes seg. fault. when the density is computed.
   !
   call PP_bcast(fft_size,0)
   !
 endif
 !
 ! Core allocations
 !
 call RT_alloc(E,"all")
 call RT_control_alloc("all")
 !
 ! GPL_EXCLUDE_START
 !
 ! Initial allocation and basic definitions of the RT_carriers object
 !
 call RT_carriers_object(RT_carriers,E,k,WHAT='allocate')
 !
 ! GPL_EXCLUDE_END
 !
 ! Restart from a previous RT run or from a SC calculation
 ! 
 call section('=','Start and Restart')
 !######################################
 !
 call RT_start_and_restart(E,k,q)
 !
 ! If I loaded the WFs just to compute HARTREE0 and XC0 I can now free the WFs 
 !
 if( (l_use_Hxc_collisions.or.trim(RT_potential)=="IP").and. &
&    .not.(eval_energy.or.l_velocity_gauge_corr)       .and. &
&    (COLLISIONS_have_HARTREE.or.index(trim(RT_potential),"HARTREE")==0)        ) then
   !
   call WF_free(WF)
   !
   YAMBO_FREE(rho_n)
   YAMBO_FREE(rho_reference)
   !
   if(n_spin>1) then
     YAMBO_FREE(magn_n)
     YAMBO_FREE(magn_reference)
   endif
   !
 endif
 !
 ! Calculation already done
 !
 if (NE_i_start_time>=NE_steps) then
   !      
   call CLOSE_and_clean( )      
   !      
   return
   !
 endif
 !
 ! Collisions
 !
 if(l_use_collisions .and. io_COLLs)  call COLLISIONS_load(.FALSE.)
 !
 ! GPL_EXCLUDE_START
 !
 ! Reference lifetimes and/or G_lesser
 !=====================================
 !
 if (NE_i_start_time==1) then
   !
   call msg('s','Reference Lifetimes, GF, Self-Energy ...')
   !
   l_initial_equilibrium=.TRUE.
   !
   call RT_relaxation(E,k,q,0._SP)
   !
   call RT_carriers_object(RT_carriers,WHAT='fill_ref')
   !
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID(8))
   io_REF=io_RT_components('REF',ID(8))
   !
   call msg('l','done')
   !
 endif
 !
 l_initial_equilibrium=.FALSE.
 !
 ! GPL_EXCLUDE_END
 !
 ! Qindx_X and Qindx_S not needed in real-time they are used only in dipoles and collisions calculations
 !
 if (.not.l_elph_scatt.and..not.l_elel_scatt) call qindx_free('ALL')
 !
 ! Initialize output files
 !==========================
 !
 call RT_output_INIT( )
 !
 call RT_output_and_IO_driver('open',E,k,OBSERVABLES)
 !
 ! Loop on time
 !==============
 l_Dynamics_is_over=.FALSE.
 !
 ! See [23/9/2011] comments in RT_start_and_restart regarding the value of it_start
 !
 NE_i_time = NE_i_start_time
 !
 ! Start-Up & Timing
 !===================
 !
 call section('=','Real-Time dynamics')
 !######################################
 !
 call RT_timing(0)
 !
 do while (.not.l_Dynamics_is_over)
   !
   ! Dynamics single step is composed by few sections:
   !
   ! * Observables related to G_lesser@NE_time
   ! * Collisions based on G_lesser@NE_time
   ! * Write databases & observables
   ! * Eval RT_dyn_step
   ! * If NE_time is the last point exit
   ! * NE_time->NE_time+RT_dyn_step
   ! * Update NE_time (note NE_time is always the last evoluted)
   !
   ! G_lesser@NE_time
   !###################
   !
   ! Calculate the RT Hamiltonian Ho_plus_Sigma
   !============================================
   call RT_Hamiltonian(dG_lesser(:,:,:,i_MEM_now),A_tot,E,k)
   !
   ! GPL_EXCLUDE_START
   !
   ! Rotation in the Coherent Hamiltonian basis
   !============================================
   call RT_Hamiltonian_diagonalize(E,k,OBSERVABLES(22:25),A_tot)
   !
   ! GPL_EXCLUDE_END
   !
   ! Energy, number of carriers, fit of electronic temperatures
   !=============================================================
   call RT_Observables(E,k,dG_lesser(:,:,:,i_MEM_now),OBSERVABLES)
   !
   ! GPL_EXCLUDE_START
   !
   ! Calculate all relaxation and dissipation terms
   !===============================================
   call RT_relaxation(E,k,q,NE_time)
   !
   ! GPL_EXCLUDE_END
   !
   ! Write DBs and output
   !=====================
   call RT_output_and_IO_driver('write',E,k,OBSERVABLES)
   !
   ! GPL_EXCLUDE_START
   !
   ! Time step update
   !=======================
   call RT_time_step_update( )
   !
   ! GPL_EXCLUDE_END
   !
   ! Memory maps for the step (to be done before the NE_time/NE_i_time update)
   !=========================
   call RT_MEMORY_index( )
   !	
   ! Timing
   !========
   call RT_timing(nint(RT_dyn_step/RT_step))
   !
   ! I (eventually) stop 
   !=======================
   if (l_Dynamics_is_over) cycle
   !
   ! Integration
   !=============
   call RT_Integrator(G_lesser(:,:,:,i_MEM_now),dG_lesser(:,:,:,i_MEM_now),dG_lesser(:,:,:,i_MEM_prev),A_tot,A_tot,E,k,q)
   !
 enddo
 !
 call RT_timing(-1)
 !
 call RT_output_and_IO_driver('close',E,k,OBSERVABLES)
 !
 ! Finalize & Free
 !-----------------
 call CLOSE_and_clean()
 !
 contains
   !
   subroutine CLOSE_and_clean( )
     call RT_free("all")
     call RT_control_free("all")
     call WF_free(WF)
     !
     ! GPL_EXCLUDE_START
     !
     call PLASMA_free(EH_gas)
     call RT_carriers_object(RT_carriers,WHAT='free')
     if (allocated(GW_NEQ_COLL_element)) call COLLISIONS_alloc_and_free("GW_NEQ","v3","clean",0)
     !
     ! GPL_EXCLUDE_END
     !
     if (allocated(HXC_COLL_element))    call COLLISIONS_alloc_and_free("SEX","v1 table","clean",0)
   end subroutine
   !
   subroutine RT_timing(steps)
     !
     integer :: steps
     !
     if (steps==0) then
       !
       LT_pump=l_NE_with_fields
       if (l_NE_with_fields) then
         LT_pump_duration=min(maxval(Efield(:)%width)*8,NE_tot_time)
         LT_pump_steps=LT_pump_duration/RT_step
       endif
       if (NE_i_start_time<LT_pump_steps.and.l_NE_with_fields) then
         call live_timing("[RT] Dynamics (T<"//trim(real2ch(real(LT_pump_duration/FS2AUT,SP)))//" fs) :",&
&                         LT_pump_steps-NE_i_start_time+1,DEPTH=0.01_SP)
       else
         LT_pump=.FALSE.
         call live_timing('[RT] Dynamics ',NE_steps-NE_i_start_time+1,DEPTH=0.01_SP)
       endif
       !
     else if(steps>0) then
       !
       call PP_redux_wait()
       !
       l_Dynamics_is_over=NE_i_time==NE_steps.or.NE_i_time+nint(RT_dyn_step/RT_step)>NE_steps.or.STOP_now(.FALSE.)
       !
       if (NE_i_time>=LT_pump_steps.and.LT_pump) then
         LT_pump=.FALSE.
         call live_timing( )
         call live_timing('[RT] Dynamics (pump-free region) ',NE_steps-LT_pump_steps-NE_i_start_time+1,DEPTH=0.01_SP) 
       endif
       call live_timing(steps=steps)
       !
     else if(steps<0) then
       !
       call live_timing()
       !
     endif
     !
   end subroutine RT_timing
   !
end subroutine RT_driver
