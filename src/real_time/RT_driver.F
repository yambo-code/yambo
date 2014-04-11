!
! Copyright (C) 2000-2014 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_driver(E,X,k,q)
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_use_collisions,l_elph_scatt,l_elel_scatt
 use stderr,         ONLY:set_real_printed_length
 use units,          ONLY:FS2AUT
 use LIVE_t,         ONLY:live_timing 
 use electrons,      ONLY:levels
 use X_m,            ONLY:X_t,X_alloc
 use wave_func,      ONLY:WF
 use interfaces,     ONLY:WF_load
 use R_lattice,      ONLY:bz_samp,qindx_free
 use SC,             ONLY:SC_potential,SC_bands,it_now,local_V,WF_G_max,WF_Go_indx,SC_R,rho_n,Ho,&
&                         SC_QP_setup
 use real_time,      ONLY:Thermal_steps,NE_steps,RT_step,G_lesser,l_NE_simulate_IP, &
&                         l_NE_with_fields,RT_alloc,RT_free,XC0_and_HARTREE0,&
&                         NE_i_time,l_RT_diagonalize,RT_IO_G_steps,l_RT_skip_impose,&
&                         G_lesser_reference,RT_diagonalization_steps,rho_reference, &
&                         it_start,RT_output,NE_time,l_NE_dynamics_started,RT_update,&
&                         l_Dynamics_is_over,MEM_index,G_MEM_steps,l_Debye_TBose_update,&
&                         dG_lesser
 use plasma,          ONLY:Plasma_free,EH_gas
 use matrix_operate,  ONLY:mat_dia_inv,DIAGO
 use memory_m,        ONLY:mem_est
 use com,             ONLY:error
 use fields,          ONLY:A_vecpot,A_vecpot_previous
 use collision,       ONLY:HF_collisions,COHSEX_collisions,GW_NEQ_collisions
 use QP_CTL_m,        ONLY:QP_apply
 use interfaces,      ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use parallel_m,      ONLY:PAR_IND_WF_b,PAR_IND_WF_k
 use IO_m,            ONLY:IO_and_Messaging_switch,io_COLLs
 !
 implicit none
 !
 ! if l_RT_diagonalize=.true.  "en"  will contains new energies and occupations
 ! otherwise it will not be used
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
 integer           :: it,ik,ib,it_mem,it_mem_p1
 real(SP)          :: E_previous,E_now
 complex(SP)       :: on_the_fly_operators(10)
 !
 if (l_elph_scatt.and.l_elel_scatt) then
   call section('*',trim(SC_potential)//' Real-Time Dynamics including el-ph AND e-e scattering')
 else if (l_elph_scatt) then
   call section('*',trim(SC_potential)//' Real-Time Dynamics including el-ph scattering')
 else if (l_elel_scatt) then
   call section('*',trim(SC_potential)//' Real-Time Dynamics including e-e scattering')
 else
   call section('*',trim(SC_potential)//' Real-Time Dynamics')
 endif
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
 call PARALLEL_WF_distribute(K_index=PAR_IND_WF_k,B_index=PAR_IND_WF_b,CLEAN_UP=.TRUE.)
 call PARALLEL_WF_index( )
 !
 ! Dipoles
 !=========
 !
 ! before loading the WF in R-space I check and/or calculate
 ! the optical collisions that are done in G-space and would
 ! force the reloading of the WFs
 !
 call Dipole_driver(E,k,X(1),X(1)%q0)
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
 call WF_load(WF,WF_G_max,WF_Go_indx,(/1,SC_bands(2)/),(/1,k%nibz/),space='R',title='-RT')  
 !
 ! QP-corrections 
 !================
 !
 call QP_apply(SC_bands,E,k,"G",msg_fmt='s')
 !
 ! Core initialization
 !
 call RT_initialize(E,k,q)
 !
 ! Core allocations
 !
 call RT_alloc(E)
 !
 ! Restart from a previous RT run or from a SC calculation
 ! 
 call section('=','Start and Restart')
 !
 call RT_start_and_restart(E,k,q,X)
 !
 ! Collisions
 !
 if((.not.local_V.and..not.l_NE_simulate_IP).or.l_elel_scatt) then
   !
   if (io_COLLs) then
     !
     if (.not.l_use_collisions) call error('COLLISIONs database missing')
     !
     call COLLISIONS_load(.FALSE.)
     !
   endif
   !
 endif
 !
 ! Reference lifetimes 
 !====================
 if (l_elph_scatt.or.l_elel_scatt) call RT_relaxation(dG_lesser(:,:,:,1),E,k,q,0.,.true.)
 !
 ! qindx_X and qindx_S not needed in real-time 
 ! they are used only in dipoles and collisions calculations
 !
 if (.not.l_elph_scatt.and..not.l_elel_scatt) call qindx_free()
 !
 ! Calculation already done
 !
 if (it_start>=Thermal_steps+NE_steps) then
   !      
   call RT_free()
   !
   call WF_free(WF)
   !
   call X_alloc("P_square")
   !
   return
   !
 endif
 !
 ! IP calculation => No need of WFs and Density
 !
 if(l_NE_simulate_IP) then  
   !
   call WF_free(WF)
   !
   call mem_est("rho_reference rho")
   !
   deallocate(rho_n,rho_reference)
   !
 endif
 !
 ! Initialize output files
 !==========================
 !
 call RT_output(what="initialize")
 call RT_output(what="open dyn ene cur pol")
 if (l_NE_with_fields) call RT_output(what="open pot occ")
 if (l_elph_scatt) call RT_output('open EPlife')
 if (l_elel_scatt) call RT_output('open EElife')
 if ((l_elel_scatt.or.l_elph_scatt).and..not.l_RT_skip_impose) call RT_output('open factors')
 !
 ! Loop on time
 !==============
 l_Dynamics_is_over=.FALSE.
 !
 ! See [23/9/2011] comments in RT_start_and_restart regarding the value of it_start
 !
 it=it_start-1
 !
 do while (.not.l_Dynamics_is_over)
   !
   it    =it+1
   it_now=it   ! used in RESTART
   !
   ! Steps & times ...
   !===================
   ! 
   !   Thermal_steps   NE_steps
   ! --------------- 0 ---------------------------- t
   ! ^               ^                            ^
   ! |               |                            NE_i_time=NE_steps
   ! it=1            |    
   !                 it=Thermal_steps+1          
   ! NE_i_time<0     NE_i_time=1
   !
   ! 1 .... Thermal_steps (Thermal period) => NE_time=-Thermal_steps*RT_step ... -RT_step
   ! it = Thermal_steps+1                  => NE_time =0 (NE_i_time = 1)
   !
   ! at  it = Thermal_steps+1    NE_dynamics=.TRUE.
   !
   NE_i_time  =it-Thermal_steps
   NE_time    =(NE_i_time-1)*RT_step
   it_mem     =MEM_index(NE_i_time,G_MEM_steps)
   it_mem_p1  =MEM_index(NE_i_time+1,G_MEM_steps)
   !
   l_NE_dynamics_started=NE_i_time>=1
   !
   ! Start-Up & Timing
   !===================
   if(it==it_start.or.it==Thermal_steps+1) then
     !
     if (.not.l_NE_dynamics_started) call live_timing('[RT] Thermalization :',Thermal_steps-it_start+1,SERIAL=.true.)
     if (     l_NE_dynamics_started) call live_timing('[RT] Dynamics :',NE_steps-it_start+1,SERIAL=.true.)
     !
   endif
   !
   ! Keep a copy of A_vecpot before starting the collisions
   ! ======================================================
   A_vecpot_previous=A_vecpot
   !
   ! End dynamics ?
   !===============
   if (it==Thermal_steps+NE_steps) l_Dynamics_is_over=.TRUE.
   !
   if (stop_now(.FALSE.)) then
     NE_steps=((NE_i_time-1)/RT_IO_G_steps)*RT_IO_G_steps+1
     l_Dynamics_is_over=.TRUE.
   endif
   !
   ! Current and Polarization
   !==========================
   call RT_current_AND_polarization(k,E,dG_lesser(:,:,:,it_mem),A_vecpot)
   !
   ! Now calculate Energetics, current and other observables
   !=========================================================
   call RT_Observables(E,k,dG_lesser(:,:,:,it_mem),on_the_fly_operators)
   !
#if defined _ELPH
   !
   ! Update Bose Temp and Phonons distribution
   !===========================================
   if (l_Debye_TBose_update) then
     if (it==it_start) E_previous=real(on_the_fly_operators(5))
     if (abs(nint(NE_time/FS2AUT)-(NE_time/FS2AUT))< RT_step/FS2AUT .and. &
&            nint(NE_time/FS2AUT)-(NE_time/FS2AUT) > 0. ) then
       E_now=real(on_the_fly_operators(5))
       call TEMPERATURE_Debye(E_previous-E_now)
       E_previous=E_now
     endif
   endif
   !
#endif
   !
   ! Calculate Collisions and fill SC_R, Ho_plus_Sigma, H_nl_sc
   !============================================================
   call RT_Collision(dG_lesser(:,:,:,it_mem),A_vecpot_previous,E,k,q,NE_time)
   !
   ! Eval and Print Mean Carrier Lifetimes
   !=======================================
   if (l_elph_scatt.or.l_elel_scatt) then
     call RT_mean_carrier_lifetimes(E,k)
     if(l_elph_scatt) call RT_output('EPlife',TIME=NE_time/FS2AUT)
     if(l_elel_scatt) call RT_output('EElife',TIME=NE_time/FS2AUT)
     if((l_elel_scatt.or.l_elph_scatt).and..not.l_RT_skip_impose) call RT_output('factors',TIME=NE_time/FS2AUT)
   endif
   !
   ! Write DBs
   !===========
   call set_real_printed_length()
   call IO_and_Messaging_switch("-fragments_IO_log")
   call RT_databases_IO(E,it)
   call IO_and_Messaging_switch("+fragments_IO_log")
   call set_real_printed_length(f_length=10,g_length=10)  
   !
   ! Print some simple observables
   !===============================
   call RT_output('ene',real(on_the_fly_operators(1:5 )),TIME=NE_time/FS2AUT)
   call RT_output('dyn',real(on_the_fly_operators(5:10)),TIME=NE_time/FS2AUT)
   call RT_output('cur pol',TIME=NE_time/FS2AUT)
   if (l_NE_with_fields) call RT_output('pot',TIME=NE_time/FS2AUT) 
   !
   ! Integration
   !=============
   ! T -> T+dT step 
   !
   call RT_Integrator(dG_lesser(:,:,:,it_mem_p1),dG_lesser(:,:,:,it_mem),A_vecpot,A_vecpot_previous,RT_step,E,k,q,NE_time)
   !
   G_lesser(:,:,:,it_mem_p1)=G_lesser_reference+dG_lesser(:,:,:,it_mem_p1)
   !
   if (it==Thermal_steps) then
     !
     ! I freeze the last self-energy + hartree in XC0_and_HARTREE and I work
     ! only with the differences rho-rho_reference and G_lesser-G_lesser_reference
     ! 
     call RT_Collision(dG_lesser(:,:,:,it_mem),A_vecpot_previous,E,k,q,NE_time)
     XC0_and_HARTREE0=SC_R-Ho
     !
   endif
   !
   ! Diagonalization
   ! ===============
   if (it==Thermal_steps.or. (mod(it,RT_diagonalization_steps)==0.and.RT_diagonalization_steps>0) ) then
     !
     do ik=1,E%nk
       call mat_dia_inv(DIAGO,SC_R(1:SC_bands(2),1:SC_bands(2),ik,1),E_real=E%E(:SC_bands(2),ik,1))
     enddo
     !
     ! Updating
     !===========
     !
     ! At the end of the thermalization update the reference G_lesser and dipoles
     if (it==Thermal_steps) call RT_update(G_lesser_reference,SC_R,it,E,k,X(1),up_Gref=.true.,up_DIP=.true.)
     !
     ! Analize quasi-particles
     if (l_RT_diagonalize) call RT_update(G_lesser(:,:,:,it_mem),SC_R,it,E,k,X(1),an_QP=.true.)
     !
   endif
   !
   ! Timing
   !========
   call live_timing(steps=1)
   !
   if (it==Thermal_steps) call live_timing()
   !
 enddo
 !
 if(NE_steps/=0) call live_timing
 !
 if (l_RT_diagonalize) call SC_History(-1,E)
 !
 ! Finalize
 !-----------
 call RT_free()
 !
 call WF_free(WF)
 call Plasma_free(EH_gas)
 !
 if (allocated(HF_collisions))     call COLLISIONS_alloc_and_free("HF","v1 table","clean",0)
 if (allocated(COHSEX_collisions)) call COLLISIONS_alloc_and_free("COHSEX","v1 table","clean",0)
 if (allocated(GW_NEQ_collisions)) call COLLISIONS_alloc_and_free("GW_NEQ","v3","clean",0)
 !
 call set_real_printed_length()
 !
end subroutine RT_driver
