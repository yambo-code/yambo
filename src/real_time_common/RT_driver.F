!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA
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
subroutine RT_driver(E,X,k,q)
 !
 use pars,           ONLY:SP,schlen
 use drivers,        ONLY:l_use_collisions,l_use_Hxc_collisions,l_elph_scatt,l_elel_scatt,l_elphoton_scatt
 use stderr,         ONLY:set_real_printed_length
 use units,          ONLY:AUT2FS
 use LIVE_t,         ONLY:live_timing 
 use electrons,      ONLY:levels,n_spin
 use X_m,            ONLY:X_t
 use wave_func,      ONLY:WF
 use FFT_m,          ONLY:fft_size
 use interfaces,     ONLY:WF_load
 use R_lattice,      ONLY:bz_samp,qindx_free
 use IO_m,           ONLY:OP_WR_CL,NONE,io_control
 use hamiltonian,    ONLY:WF_G_max,WF_Go_indx,rho_n,magn_n,&
&                         H_potential,H_kind,H_xc_functional,H_perturbation
 use RT_control,     ONLY:RT_output,RT_alloc_control,RT_free_control,ID,&
&                         NE_i_start_time,RT_carriers_object,TIME_adjust,RT_apply
 use real_time,      ONLY:RT_potential,RT_kind,RT_xc_functional,RT_perturbation,&
&                         NE_steps,RT_step,G_lesser,RT_bands,l_Update_Time_Step,&
&                         l_NE_with_fields,l_RT_induced_field,RT_alloc,RT_free,&
&                         NE_i_time,l_RT_impose_N_dN,l_RT_impose_E,&
&                         G_lesser_reference,rho_reference,magn_reference, &
&                         NE_time,RT_carriers,RT_dyn_step,l_Dynamics_is_over,&
&                         dG_lesser,l_initial_equilibrium,NE_initial_time_step_update,&
&                         i_MEM_now,i_MEM_next,i_MEM_collision,RT_is_dynamically_dephased
 use plasma,          ONLY:PLASMA_free,EH_gas
 use com,             ONLY:warning,msg,error
 use fields,          ONLY:A_vecpot,A_vecpot_previous,A_ext
 use collision_ext,   ONLY:HXC_COLL_element,GW_NEQ_COLL_element
 use QP_CTL_m,        ONLY:QP_apply
 use interfaces,      ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use parallel_m,      ONLY:PAR_IND_WF_b,PAR_IND_WF_k,PAR_IND_DIPk_ibz,PAR_IND_B_mat,PP_bcast
 use IO_m,            ONLY:IO_and_Messaging_switch,io_COLLs
 !
#include<memory.h>
 !
 ! Notice that E is never changed!
 !
 type(levels)      :: E
 type(bz_samp)     :: k,q
 type(X_t)         :: X(2)
 logical, external :: stop_now
 ! 
 ! Work Space
 !
 integer           :: io_REF
 character(schlen) :: sec_title(3)
 real(SP)          :: on_the_fly_operators(10)
 !
 integer, external :: io_RT_components
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
 call set_real_printed_length(f_length=10,g_length=10)  
 !
 ! Potential 
 !===========
 !
 call XC_switch()
 !
 RT_potential    =H_potential
 RT_kind         =H_kind
 RT_xc_functional=H_xc_functional
 RT_perturbation =H_perturbation
 !
 call section('*',trim(RT_potential)//' Real-Time Dynamics')
 call msg('s','Scattering(s):'//trim(sec_title(1))//' '//trim(sec_title(2))//' '//trim(sec_title(3)))
 !
 call section('+','Initialization')
 !#################################
 !
 ! Dipoles & WF dimensions
 !=========================
 !
 call WF_and_dipole_dimensions(X)
 !
 ! Parallel Setups
 !=================
 !
 call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.,X=X(1))
 call PARALLEL_global_indexes(E,k,q,"Real_Time",RESET=.FALSE.,X=X(1))
 !
 ! Dipoles
 !=========
 !
 call PARALLEL_WF_distribute(K_index=PAR_IND_DIPk_ibz,Bm_index=PAR_IND_B_mat,CLEAN_UP=.TRUE.)
 !
 ! before loading the WF in R-space I check and/or calculatethe optical collisions that 
 ! are done in G-space and would ! force the reloading of the WFs
 !
 call DIPOLE_driver(E,k,X(1),X(1)%q0)
 !
 ! IO 
 !====
 !
 ! In dipole_driver the IO is k-dictated. Here I want only the master to write
 !
 call IO_and_Messaging_switch("RESET")
 !
 ! WFs loading
 !=============
 !
 call PARALLEL_WF_distribute(K_index=PAR_IND_WF_k,B_index=PAR_IND_WF_b,CLEAN_UP=.TRUE.)
 call PARALLEL_WF_index( )
 !
 call WF_load(WF,WF_G_max,WF_Go_indx,(/1,RT_bands(2)/),(/1,k%nibz/),space='R',title='-RT')  
 !
 ! As explained in PARALLEL_global_indexes if a QP parallelization is used (k=b=1 cpu)
 ! this means that the WF's are distributed only among few cpu's. But, as a consequence, the fft_size
 ! is not defined for all CPU's. This causes seg. fault. when the density is computed.
 !
 call PP_bcast(fft_size,0)
 !
 ! QP & carriers corrections 
 !===========================
 !
 call QP_apply(RT_bands,E,k,     "G",msg_fmt='s')
 call RT_apply(RT_bands,E,k,what="G",VERBOSE=.true.)
 !
 ! Core initialization
 !
 call RT_initialize(E,k,q)
 !
 ! Core allocations
 !
 call RT_alloc(E)
 call RT_alloc_control()
 !
 ! Restart from a previous RT run or from a SC calculation
 ! 
 call section('=','Start and Restart')
 !######################################
 !
 call RT_start_and_restart(E,k,q,X)
 !
 ! Collisions => No need of WFs and Density
 !
 if(l_use_Hxc_collisions) then  
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
 ! Reference lifetimes and/or G_lesser
 !=====================================
 !
 if (NE_i_start_time==1) then
   !
   call msg('s','Reference Lifetimes, GF, Self-Energy ...')
   !
   call RT_relaxation(dG_lesser(:,:,:,1),E,k,q,0._SP,.true.)
   !
   call RT_BGR(G_lesser_reference,E,k)
   !
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID(8))
   io_REF=io_RT_components('REF',ID(8))
   !
   call msg('l','done')
   !
   l_initial_equilibrium=.FALSE.
   !
 endif
 !
 ! Qindx_X and Qindx_S not needed in real-time they are used only in dipoles and collisions calculations
 !
 if (.not.l_elph_scatt.and..not.l_elel_scatt) call qindx_free()
 !
 ! Initialize output files
 !==========================
 !
 call RT_output(what="initialize")
 call RT_output(what="open carr therm energy curr pol magn")
 call RT_output(what="open BGR")
 if (l_NE_with_fields) then
   call RT_output(what="open ext")
   if(l_RT_induced_field) call RT_output(what="open tot ind")
 endif
 if (l_elph_scatt)     call RT_output('open EPlife')
 if (l_elel_scatt)     call RT_output('open EElife')
 if (l_elphoton_scatt) call RT_output('open RADlife')
 if ((l_elel_scatt.or.l_elph_scatt).and.(l_RT_impose_N_dN.or.l_RT_impose_E)) call RT_output('open factors')
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
 call live_timing('[RT] Dynamics :',NE_steps-NE_i_start_time+1)
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
   ! Current, Polarization and Magnetization
   !==========================================
   ! (evaluated also in RT_integrator when and induced field is included)
   call RT_current_AND_polarization(k,E,dG_lesser(:,:,:,i_MEM_collision),A_vecpot)
   call RT_output('cur pol magn',TIME=NE_time*AUT2FS)
   !
   ! Energy, number of carriers, fit of electronic temperatures
   !=============================================================
   call RT_Observables(E,k,dG_lesser(:,:,:,i_MEM_collision),on_the_fly_operators)
   call RT_output('ene',real(on_the_fly_operators(1:5 ),SP),TIME=NE_time*AUT2FS)
   call RT_output('dyn',real(on_the_fly_operators(5:10),SP),TIME=NE_time*AUT2FS)
   !
   ! Update Bose Temp and Phonons distribution
   !===========================================
   !if (l_Debye_TBose_update) then
   !  if (it==it_start) E_previous=real(on_the_fly_operators(5))
   !  if (abs(nint(NE_time/FS2AUT)-(NE_time/FS2AUT))< RT_step/FS2AUT .and. &
   !  &       nint(NE_time/FS2AUT)-(NE_time/FS2AUT) > 0. ) then
   !    E_now=real(on_the_fly_operators(5))
   !    call TEMPERATURE_Debye(E_previous-E_now)
   !    E_previous=E_now
   !  endif
   !endif
   !
   ! Update Bose Temp and Phonons distribution
   !===========================================
   !if (l_Debye_TBose_update) then
   !  if (it==it_start) E_previous=real(on_the_fly_operators(5))
   !  if (abs(nint(NE_time/FS2AUT)-(NE_time/FS2AUT))< RT_step/FS2AUT .and. &
   !  &       nint(NE_time/FS2AUT)-(NE_time/FS2AUT) > 0. ) then
   !    E_now=real(on_the_fly_operators(5))
   !    call TEMPERATURE_Debye(E_previous-E_now)
   !    E_previous=E_now
   !  endif
   !endif
   !
   ! BGR
   !=======================================
   call RT_BGR(dG_lesser(:,:,:,i_MEM_collision),E,k)
   !
   ! COLLISIONS@NE_time
   !####################
   !
   ! Calculate the RT Hamiltonian Ho_plus_Sigma
   !============================================
   call RT_Hamiltonian(dG_lesser(:,:,:,i_MEM_collision),A_vecpot,E,k,q,NE_time)
   !
   ! Calculate all relaxation and dissipation terms
   !===============================================
   call RT_relaxation(dG_lesser(:,:,:,i_MEM_collision),E,k,q,NE_time,.false.)
   !
   ! Write DBs
   !===========
   call set_real_printed_length()
   call IO_and_Messaging_switch("-fragments_IO_log")
   call RT_databases_IO( )
   call IO_and_Messaging_switch("+fragments_IO_log")
   call set_real_printed_length(f_length=10,g_length=10)  
   !
   ! OUTPUT(s)
   !###########
   call RT_output('BGR',TIME=NE_time*AUT2FS)
   call RT_output('curr pol magn therm',TIME=NE_time*AUT2FS)
   call RT_output('energy',VALUES=on_the_fly_operators(1:5 ),TIME=NE_time*AUT2FS)
   call RT_output('carr',VALUES=on_the_fly_operators(6:10),TIME=NE_time*AUT2FS)
   !
   ! Mean Carrier Lifetimes
   !========================
   if (RT_is_dynamically_dephased) then
     call RT_mean_carrier_lifetimes(E,k)
     if(l_elph_scatt)                      call RT_output('EPlife',TIME=NE_time*AUT2FS)
     if(l_elel_scatt)                      call RT_output('EElife',TIME=NE_time*AUT2FS)
     if(l_elphoton_scatt)                  call RT_output('RADlife',TIME=NE_time*AUT2FS)
     if(l_RT_impose_N_dN.or.l_RT_impose_E) call RT_output('factors',TIME=NE_time*AUT2FS)
   endif
   !
   ! Field(s)
   !==========
   if (l_NE_with_fields) then
     call RT_output('external_field',TIME=NE_time*AUT2FS) 
     if(l_RT_induced_field) then
       call RT_output('total_field'  ,TIME=NE_time*AUT2FS) 
       call RT_output('induced_field',TIME=NE_time*AUT2FS) 
     endif
   endif
   !
   ! T -> T+dT step 
   !################
   !
   ! New steps & times ...
   !=======================
   !
   ! ... Dynamical Time-Step evaluation
   !
   l_Update_Time_Step=NE_time>=NE_initial_time_step_update.and.NE_initial_time_step_update>0._SP
   !
   if (l_Update_Time_Step) call RT_time_step_update( )
   !
   ! Memory maps for the step (to be done before the NE_time/NE_i_time update)
   !
   call RT_MEMORY_index( )
   !
   ! I (eventually) stop after all actions at the time just calculated in the previous step of the loop
   !=======================
   !
   l_Dynamics_is_over=NE_i_time==NE_steps.or.NE_i_time+nint(RT_dyn_step/RT_step)>NE_steps.or.stop_now(.FALSE.)
   !
   if (l_Dynamics_is_over) cycle
   !
   ! Integration
   !=============
   !
   call RT_Integrator(dG_lesser(:,:,:,i_MEM_next),dG_lesser(:,:,:,i_MEM_now),A_vecpot,A_vecpot_previous,RT_dyn_step,E,k,q,NE_time)
   !
   ! A update
   !==========
   A_vecpot_previous=A_vecpot
   !
   ! G_lesser update
   !=================
   G_lesser(:,:,:,i_MEM_next)=G_lesser_reference+dG_lesser(:,:,:,i_MEM_next)
   !
   ! Time Update
   !=============
   NE_i_time       = NE_i_time+nint(RT_dyn_step/RT_step)
   NE_time         = TIME_adjust((NE_i_time-1)*RT_step)
   i_MEM_collision = i_MEM_next
   !	
   ! Timing
   !========
   call live_timing(steps=nint(RT_dyn_step/RT_step))
   !
 enddo
 !
 call live_timing( )
 !
 ! Finalize & Free
 !-----------------
 call CLOSE_and_clean()
 !
 contains
   !
   subroutine CLOSE_and_clean( )
     call RT_free()
     call RT_free_control()
     call WF_free(WF)
     call PLASMA_free(EH_gas)
     call RT_carriers_object(RT_carriers,WHAT='free')
     if (allocated(HXC_COLL_element))    call COLLISIONS_alloc_and_free("SEX","v1 table","clean",0)
     if (allocated(GW_NEQ_COLL_element)) call COLLISIONS_alloc_and_free("GW_NEQ","v3","clean",0)
     call set_real_printed_length()
   end subroutine
   !
end subroutine RT_driver
