!
!        Copyright (C) 2000-2016 the YAMBO team
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
 use drivers,        ONLY:l_use_collisions,l_sc_ip,l_elph_scatt,l_elel_scatt,l_elphoton_scatt
 use stderr,         ONLY:set_real_printed_length
 use units,          ONLY:FS2AUT
 use LIVE_t,         ONLY:live_timing 
 use electrons,      ONLY:levels,n_spin
 use X_m,            ONLY:X_t,X_alloc
 use wave_func,      ONLY:WF
 use FFT_m,          ONLY:fft_size
 use interfaces,     ONLY:WF_load
 use R_lattice,      ONLY:bz_samp,qindx_free
 use IO_m,           ONLY:OP_WR_CL,NONE,io_control
 use SC,             ONLY:SC_potential,l_sc_pot_is_local,WF_G_max,WF_Go_indx,rho_n,&
&                         SC_QP_setup,magn_n
 use rt_ctl,         ONLY:RT_output,RT_alloc_control,RT_free_control,RT_IO_G_steps,ID,&
&                         it_start,RT_carriers_object,it_dyn
 use real_time,      ONLY:NE_steps,RT_step,G_lesser,RT_bands,&
&                         l_NE_with_fields,l_RT_induced_field,RT_alloc,RT_free,&
&                         NE_i_time,l_RT_impose_N_dN,l_RT_impose_E,&
&                         G_lesser_reference,rho_reference,magn_reference, &
&                         NE_time,RT_carriers,RT_dyn_step,&
&                         l_Dynamics_is_over,MEM_index,G_MEM_steps,&
&                         dG_lesser,l_initial_equilibrium
 use plasma,          ONLY:PLASMA_free,EH_gas
 use memory_m,        ONLY:mem_est
 use com,             ONLY:warning,msg,error
 use fields,          ONLY:A_vecpot,A_vecpot_previous
 use collision,       ONLY:FOCK_COLL_element,SEX_COLL_element,GW_NEQ_COLL_element
 use QP_CTL_m,        ONLY:QP_apply
 use interfaces,      ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use parallel_m,      ONLY:PAR_IND_WF_b,PAR_IND_WF_k,PAR_IND_DIPk_ibz,PAR_IND_B_mat,PP_bcast
 use IO_m,            ONLY:IO_and_Messaging_switch,io_COLLs
 !
 implicit none
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
 integer           :: it_mem,it_mem_p1,io_REF
 character(schlen) :: sec_title(3)
 complex(SP)       :: on_the_fly_operators(10)
 !
 integer, external :: io_RT_components
 !
 sec_title=' '
 if (l_elph_scatt)                  sec_title(1)='el-ph'
 if (l_elel_scatt)                  sec_title(2)='e-e'
 if (l_elph_scatt.and.l_elel_scatt) sec_title(2)='+e-e'
 if (l_elphoton_scatt)              sec_title(3)='el-photon'
 if (l_elphoton_scatt.and.&
&   (l_elel_scatt.or.l_elph_scatt)) sec_title(3)='+el-photon'
 !
 call section('*',trim(SC_potential)//' Real-Time Dynamics')
 call msg('s','Scattering(s):'//trim(sec_title(1))//' '//trim(sec_title(2))//' '//trim(sec_title(3)))
 !
 call section('+','Initialization')
 !#################################
 !
 call set_real_printed_length(f_length=10,g_length=10)  
 !
 ! Switch 
 !========
 !
 call XC_switch()
 !
 ! Dipoles & WF dimensions
 !=========================
 !
 call SC_WF_and_dipole_dimensions(X)
 !
 ! QP dimensions
 !================
 ! This is also called in SC_alloc (from RT_alloc) but I need it also here
 ! for PARALLEL_global_indexes
 !
 call SC_QP_setup(E)
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
 ! QP-corrections 
 !================
 !
 call QP_apply(RT_bands,E,k,"G",msg_fmt='s')
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
 !
 call RT_start_and_restart(E,k,q,X)
 !
 ! Collisions
 !
 if((.not.l_sc_pot_is_local.and..not.l_sc_ip).or.l_elel_scatt) then
   !
   if (io_COLLs) then
     !
     if (l_use_collisions) then
       !
       call COLLISIONS_load(.FALSE.)
       !
     else
       !
       if (l_elel_scatt) then
         call warning('COLLISIONs database missing. E-E scattering switched off.')
         l_elel_scatt=.FALSE.
       endif
       !
       if (.not.l_sc_pot_is_local) call error('COLLISIONs database missing.')
       !
     endif
     !
   endif
   !
 endif
 !
 ! Reference lifetimes and/or G_lesser
 !=====================================
 !
 if (it_start==1) then
   !
   call msg('s','Reference Lifetimes, GF, Self-Energy ...')
   !
   call RT_relaxation(dG_lesser(:,:,:,1),E,k,q,0.,.true.)
   !
   !call RT_BGR(G_lesser_reference,E,k)
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
 ! Calculation already done
 !
 if (it_start>=NE_steps) then
   !      
   call CLOSE_and_clean( )      
   !      
   return
   !
 endif
 !
 ! IP calculation & not BGR => No need of WFs and Density
 !
 if(l_sc_ip) then  
   !
   call WF_free(WF)
   !
   call mem_est("rho_reference rho")
   !
   deallocate(rho_n,rho_reference)
   if(n_spin>1) deallocate(magn_n,magn_reference)
   !
 endif
 !
 ! Initialize output files
 !==========================
 !
 call RT_output(what="initialize")
 call RT_output(what="open dyn ene cur pol")
 !call RT_output('open BGR')
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
 it_dyn  =it_start-1
 !
 ! Start-Up & Timing
 !===================
 call live_timing('[RT] Dynamics :',NE_steps-it_start+1,SERIAL=.true.)
 !
 do while (.not.l_Dynamics_is_over)
   !
   ! Steps & times ...
   !===================
   !
   ! ... NE_time(1)=0 !
   !
   if (it_dyn>0) NE_time    =NE_time+RT_dyn_step
   !
   it_dyn     =it_dyn+RT_dyn_step/RT_step
   NE_i_time  =it_dyn
   it_mem     =MEM_index(NE_i_time  ,G_MEM_steps)
   it_mem_p1  =MEM_index(NE_i_time+1,G_MEM_steps)
   !
   ! Keep a copy of A_vecpot before starting the collisions
   ! ======================================================
   A_vecpot_previous=A_vecpot
   !
   l_Dynamics_is_over=(it_dyn==NE_steps)
   !
   if (stop_now(.FALSE.)) then
     NE_steps=((NE_i_time-1)/RT_IO_G_steps)*RT_IO_G_steps+1
     l_Dynamics_is_over=.TRUE.
   endif
   !
   ! Current and Polarization
   !==========================
   call RT_current_AND_polarization(k,E,dG_lesser(:,:,:,it_mem),A_vecpot_previous)
   !
   ! Now calculate Energetics, current and other observables
   !=========================================================
   call RT_Observables(E,k,dG_lesser(:,:,:,it_mem),on_the_fly_operators)
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
   if (l_NE_with_fields) call RT_propagate_fields(k,E,A_vecpot,A_vecpot_previous,NE_time,RT_step)
   !
   ! Calculate Collisions and fill Ho_plus_Sigma, H_nl_sc
   !============================================================
   call RT_Collision(dG_lesser(:,:,:,it_mem),A_vecpot,E,k,q,NE_time)
   !
   ! BGR
   !=======================================
   !call RT_BGR(dG_lesser(:,:,:,it_mem),E,k)
   !call RT_output('BGR',TIME=NE_time/FS2AUT)
   !
   ! Eval and Print Mean Carrier Lifetimes
   !=======================================
   if (l_elph_scatt.or.l_elel_scatt.or.l_elphoton_scatt) then
     call RT_mean_carrier_lifetimes(E,k)
     if(l_elph_scatt)      call RT_output('EPlife',TIME=NE_time/FS2AUT)
     if(l_elel_scatt)      call RT_output('EElife',TIME=NE_time/FS2AUT)
     if(l_elphoton_scatt)  call RT_output('RADlife',TIME=NE_time/FS2AUT)
     if((l_elel_scatt.or.l_elph_scatt.or.l_elphoton_scatt).and.&
&       (l_RT_impose_N_dN.or.l_RT_impose_E)) call RT_output('factors',TIME=NE_time/FS2AUT)
   endif
   !
   ! Write DBs
   !===========
   call set_real_printed_length()
   call IO_and_Messaging_switch("-fragments_IO_log")
   call RT_databases_IO(E,it_dyn)
   call IO_and_Messaging_switch("+fragments_IO_log")
   call set_real_printed_length(f_length=10,g_length=10)  
   !
   ! Print some simple observables
   !===============================
   if (NE_time>=0._SP) then
     call RT_output('ene',real(on_the_fly_operators(1:5 )),TIME=NE_time/FS2AUT)
     call RT_output('dyn',real(on_the_fly_operators(5:10)),TIME=NE_time/FS2AUT)
   endif
   call RT_output('cur pol',TIME=NE_time/FS2AUT)
   if (l_NE_with_fields) then
     call RT_output('external_field',TIME=NE_time/FS2AUT) 
     if(l_RT_induced_field) then
       call RT_output('total_field'  ,TIME=NE_time/FS2AUT) 
       call RT_output('induced_field',TIME=NE_time/FS2AUT) 
     endif
   endif
   !
   ! Integration
   !=============
   ! T -> T+dT step 
   !
   call RT_Integrator(dG_lesser(:,:,:,it_mem_p1),dG_lesser(:,:,:,it_mem),A_vecpot,A_vecpot_previous,RT_step,E,k,q,NE_time)
   !
   ! Dynamical Time-Step evaluation
   !================================
   !
   !call RT_time_step_update(dG_lesser(:,:,:,it_mem_p1),dG_lesser(:,:,:,it_mem))
   !
   ! G_lesser update
   !=================
   !
   G_lesser(:,:,:,it_mem_p1)=G_lesser_reference+dG_lesser(:,:,:,it_mem_p1)
   !	
   ! Timing
   !========
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
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
     if (allocated(FOCK_COLL_element))   call COLLISIONS_alloc_and_free("Fock","v1 table","clean",0)
     if (allocated(SEX_COLL_element))    call COLLISIONS_alloc_and_free("SEX","v1 table","clean",0)
     if (allocated(GW_NEQ_COLL_element)) call COLLISIONS_alloc_and_free("GW_NEQ","v3","clean",0)
     call set_real_printed_length()
   end subroutine
   !
end subroutine RT_driver
