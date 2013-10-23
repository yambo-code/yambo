!
! Copyright (C) 2000-2013 A. Marini, C. Attaccalite and the YAMBO team 
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
 use pars,           ONLY:SP,cI
 use drivers,        ONLY:l_eval_collisions,l_ph_corr,l_carrier_dynamics 
 use stderr,         ONLY:set_real_printed_length
 use units,          ONLY:FS2AUT
 use LIVE_t,         ONLY:live_timing 
 use electrons,      ONLY:levels
 use X_m,            ONLY:X_t,X_alloc
 use wave_func,      ONLY:WF_load,WF_free
 use R_lattice,      ONLY:bz_samp,qindx_free
 use SC,             ONLY:SC_potential,SC_bands,it_now,local_V,WF_G_max,WF_Go_indx,SC_R,rho_n,Ho,&
&                         SC_QP_setup
 use real_time,      ONLY:Thermal_steps,NE_steps,RT_step,G_lesser,l_NE_simulate_IP, &
&                         l_NE_with_fields,RT_alloc,RT_free,XC0_and_HARTREE0,&
&                         NE_i_time,Integrator_name,l_RT_diagonalize,RT_IO_steps,&
&                         G_lesser_reference,RT_diagonalization_steps,rho_reference, &
&                         it_start,RT_output,NE_time,NE_dynamics,RT_update,&
&                         l_Dynamics_is_over,MEM_index,G_MEM_steps,l_Debye_TBose_update
 use matrix_operate,  ONLY:mat_dia_inv,DIAGO
 use memory_m,        ONLY:mem_est
 use com,             ONLY:error
 use fields,          ONLY:A_vecpot,A_vecpot_previous
 use collisions_IO,   ONLY:collisions_IO_alloc,collisions_IO_free,collisions_RANGES_setup,l_STORE_COLLISIONS
 use collision,       ONLY:free_COLLISIONS_v
 use QP_CTL_m,        ONLY:QP_apply
 use interfaces,      ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute
 use parallel_m,      ONLY:PAR_IND_WF_b,PAR_IND_WF_k
 use IO_m,            ONLY:IO_and_Messaging_switch
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
 logical           :: do_output
 integer           :: it,ik,ib,it_mem,it_mem_p1
 real(SP)          :: E_previous,E_now
 complex(SP)       :: on_the_fly_operators(8)
 !
 if (l_ph_corr.and.l_carrier_dynamics) then
   call section('*',trim(SC_potential)//' Real-Time Dynamics including el-ph AND e-e scattering')
 else if (l_ph_corr) then
   call section('*',trim(SC_potential)//' Real-Time Dynamics including el-ph scattering')
 else if (l_carrier_dynamics) then
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
 ! Parallel Setups (I)
 !=====================
 !
 call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.,X=X(1))
 call PARALLEL_global_indexes(E,k,q,"Response_G_space_Zero_Momentum",RESET=.FALSE.,X=X(1))
 call PARALLEL_global_indexes(E,k,q,"Wave_Functions",RESET=.FALSE.)
 call PARALLEL_global_indexes(E,k,q,"Real_Time",RESET=.FALSE.,X=X(1))
 !
 ! Dipoles
 !========
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
 call PARALLEL_WF_distribute(SC_bands(2),k%nibz,K_index=PAR_IND_WF_k,B_index=PAR_IND_WF_b,CLEAN_UP=.TRUE.)
 !
 call WF_load(WF_G_max,WF_Go_indx,(/1,SC_bands(2)/),(/1,k%nibz/),space='R',title='-RT')  
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
 if (l_STORE_COLLISIONS) then
   !
   call collisions_RANGES_setup()
   !
   call collisions_IO_alloc()
   !
 endif
 !
 ! Restart from a previous RT run or from a SC calculation
 ! 
 call section('=','Start and Restart')
 !
 call RT_start_and_restart(E,k,q,X)
 !
 ! Collisions
 !
 if((.not.local_V.and..not.l_NE_simulate_IP).or.l_carrier_dynamics) then
   !
   if (l_STORE_COLLISIONS) then
     !
     if (.not.l_eval_collisions) call error('COLLISIONs database missing')
     !
     call COLLISIONS_load(.FALSE.)
     !
   endif
   !
 endif
 !
 ! qindx_X and qindx_S not needed in real-time 
 ! they are used only in dipoles and collisions calculations
 !
 if (.not.l_ph_corr.and..not.l_carrier_dynamics) call qindx_free()
 !
 ! Calculation already done
 !
 if (it_start>=Thermal_steps+NE_steps) then
   !      
   call RT_free()
   !
   call collisions_IO_free() 
   !
   call WF_free()
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
   call WF_free()
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
 call RT_output(what="open dyn cur pol")
 if(l_NE_with_fields) call RT_output(what="open pot occ")
 if (l_ph_corr.or.l_carrier_dynamics) call RT_output('open life')
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
   it=it+1
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
   NE_dynamics=NE_i_time>=1
   it_mem     =MEM_index(NE_i_time,G_MEM_steps)
   it_mem_p1  =MEM_index(NE_i_time+1,G_MEM_steps)
   !
   do_output= (NE_i_time==1).or.mod(NE_i_time,RT_IO_steps)==0
   !
   ! Start-Up & Timing
   !===================
   !
   if(it==it_start.or.it==Thermal_steps+1) then
     !
     if (.not.NE_dynamics) call live_timing('[RT] Thermalization :',Thermal_steps-it_start+1,SERIAL=.true.)
     if (     NE_dynamics) call live_timing('[RT] Dynamics :',NE_steps-it_start+1,SERIAL=.true.)
     !
   endif
   !
   ! Keep a copy of A_vecpot before starting the collisions
   ! ===========
   !
   A_vecpot_previous=A_vecpot
   !
   ! Simmetrization
   !
   forall( ib=SC_bands(1):SC_bands(2) ) G_lesser(ib,ib,:,it_mem)=cI*aimag(G_lesser(ib,ib,:,it_mem))
   !
   ! End dynamics ?
   !===============
   if (it==Thermal_steps+NE_steps) l_Dynamics_is_over=.TRUE.
   if (stop_now(.FALSE.)) then
     NE_steps=(int((it-Thermal_steps)/RT_IO_steps)+1)*RT_IO_steps
     l_Dynamics_is_over=.TRUE.
   endif
   !
   ! Write DBs
   !===========
   it_now=it ! used in RESTART
   !
   call set_real_printed_length()
   call IO_and_Messaging_switch("-fragments_IO_log")
   call RT_databases_IO(E,it)
   call IO_and_Messaging_switch("+fragments_IO_log")
   call set_real_printed_length(f_length=10,g_length=10)  
   !
   ! Current and Polarization
   !==========================
   call RT_current_AND_polarization(k,E,G_lesser(:,:,:,it_mem),A_vecpot)
   !
   if ( (l_ph_corr.or.l_carrier_dynamics) .and. do_output) then
     !
     ! Mean Carrier Lifetimes
     !=======================
     call RT_mean_carrier_lifetimes(G_lesser(:,:,:,it_mem),E,k)
     call RT_output('life',TIME=NE_time/FS2AUT)
     !
   endif
   !
   ! Now print Energetics, current and (eventually) potential 
   !=========================================================
   !
   call RT_Observables(E,k,G_lesser(:,:,:,it_mem),on_the_fly_operators)
   !
#if defined _ELPH
   !
   ! Update Bose Temp and Phonons distribution
   !===========================================
   if (l_Debye_TBose_update) then
     if (it==it_start) E_previous=real(on_the_fly_operators(1)+on_the_fly_operators(2))
     if (abs(nint(NE_time/FS2AUT)-(NE_time/FS2AUT))< RT_step/FS2AUT .and. &
&            nint(NE_time/FS2AUT)-(NE_time/FS2AUT) > 0. ) then
       E_now=real(on_the_fly_operators(1)+on_the_fly_operators(2))
       call TEMPERATURE_Debye(E_previous-E_now)
       E_previous=E_now
     endif
   endif
   !
#endif
   !
   if(do_output) then
     call RT_output('dyn',real(on_the_fly_operators),TIME=NE_time/FS2AUT)
     call RT_output('cur pol',TIME=NE_time/FS2AUT)
     if (l_NE_with_fields) call RT_output('pot',TIME=NE_time/FS2AUT) 
   endif
   !
   ! Calculate Collisions and fill SC_R, Ho_plus_Sigma, H_nl_sc
   !============================================================
   !
   call RT_Collision(G_lesser(:,:,:,it_mem),A_vecpot_previous,E,k,q,NE_time)
   !
   ! Integration
   !=============
   ! T -> T+dT step 
   !
   call RT_Integrator(G_lesser(:,:,:,it_mem_p1),G_lesser(:,:,:,it_mem),A_vecpot,A_vecpot_previous,&
&                     RT_step,E,k,q,Integrator_name,NE_time)
   !
   if (it==Thermal_steps) then
     !
     ! I freeze the last self-energy + hartree in XC0_and_HARTREE and I work
     ! only with the differences rho-rho_reference and G_lesser-G_lesser_reference
     ! 
     call RT_Collision(G_lesser(:,:,:,it_mem),A_vecpot_previous,E,k,q,NE_time)
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
   !
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
 call collisions_IO_free() 
 !
 call WF_free()
 !
 if(.not.local_V.and..not.l_NE_simulate_IP) call free_COLLISIONS_v()
 !
 call set_real_printed_length()
 !
end subroutine RT_driver
