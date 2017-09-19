!
!        Copyright (C) 2000-2017 the YAMBO team
!            http://www.yambo-code.org
!
! Authors (see AUTHORS file for details):
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
subroutine RT_initialize(E,k,q)
 ! 
 use drivers,        ONLY:l_use_collisions,l_elph_scatt,l_elel_scatt,l_sc_hartree,&
&                         l_sc_is_libDFT,l_sc_sex,l_sc_fock,l_elphoton_scatt,l_rt_carriers_in_use
 use pars,           ONLY:SP,pi
 use units,          ONLY:HA2EV,HBAR_eVfs,AU2VMm1,AU2KWCMm2,AUT2FS
 use stderr,         ONLY:intc
 use com,            ONLY:msg,warning,error
 use QP_CTL_m,       ONLY:QP_ctl_user
 use vec_operate,    ONLY:normalize_v
 use plasma,         ONLY:EH_gas
 use X_m,            ONLY:global_gauge
 use electrons,      ONLY:levels
 use RT_control,     ONLY:JPC_RT_IO_t,RT_IO_t,GF_RT_IO_t,OUTPUT_RT_IO_t,RT_carriers_object,   &
&                         SETUP_RT_IO_type,IO_JPSM_cache_steps,TIME_adjust,SAVE_G_history
 use real_time,      ONLY:RT_potential,RT_step,l_NE_with_fields,RT_bands,RT_dyn_step,RT_nk,   &
&                         NE_steps,NE_tot_time,l_RT_induced_field,RT_life_extrap_times,       &
&                         RAD_LifeTime,l_RT_CCA_Kernel,Life_MEM_steps,Phase_LifeTime,         &
&                         NE_MEM_treshold,G_MEM_steps,Gr_kind,l_RT_uses_E_DbGd,               &
&                         RT_life_extrap_times_SAVE,l_RT_lifetimes_extrapolation,             &
&                         l_RT_impose_N_dN,Integrator_name,l_RT_EULER,l_RT_RK2,l_RT_RWA,      &
&                         l_RT_EXP,l_RT_impose_E,DbGd_EE_percent,DbGd_EE_Nk_table,            &
&                         l_BGR,l_RT_HEUN,eval_HARTREE,eval_DFT,l_NE_dynamics,                &
&                         NE_time_step_update_jump,NE_initial_time_step_update,RT_carriers,   &
&                         RAD_magnification,NE_step_update_treshold,l_lftm_fit_temperatures,  &
&                         NE_time_step_update_jump_SAVE,NE_initial_time_step_update,          &
&                         RT_is_dynamically_dephased,RT_is_statically_dephased,RT_MAX_step,RT_DbGd_nk
 use IO_m,           ONLY:io_COLLs
 use R_lattice,      ONLY:bz_samp
 use fields,         ONLY:Efield,n_ext_fields,field_is_ok,EtoT,Efield_strength
 use parser_m,       ONLY:parser
 !
 implicit none
 !
 type(levels), intent(in) :: E
 type(bz_samp),intent(in) :: q,k
 !
 ! Work space
 !
 integer                  :: i1,N_T_samplings
 character(6)             :: MSG_ch
 real(SP)                 :: T_memory
 logical                  :: l_Full_RT_T_evolution,l_skip_it
 integer,         external:: RT_k_grid,RT_Lifetimes_evaluation_plan
 !
 ! Logicals
 !==========
 !
 !
 eval_DFT     =  l_sc_is_libDFT
 eval_HARTREE =  l_sc_hartree
 !
 ! Integrator FLAGS
 !==================
 !
 l_RT_EULER=index(Integrator_name,'EULER')/=0.or.index(Integrator_name,'euler')/=0
 !
 l_RT_RK2  =index(Integrator_name,'RK2'  )/=0.or.index(Integrator_name,'rk2'  )/=0
 !
 l_RT_HEUN =index(Integrator_name,'HEUN' )/=0.or.index(Integrator_name,'heun' )/=0
 !
 l_RT_RWA  =index(Integrator_name,'RWA'  )/=0.or.index(Integrator_name,'rwa'  )/=0
 !
 l_RT_EXP  =index(Integrator_name,'EXP'  )/=0.or.index(Integrator_name,'exp'  )/=0
 !
 Integrator_name=""
 if ((     l_RT_EULER).and.(.not.l_RT_RK2).and.(.not.l_RT_HEUN)) Integrator_name = "EULER"
 if ((.not.l_RT_EULER).and.(     l_RT_RK2).and.(.not.l_RT_HEUN)) Integrator_name = "RK2"
 if ((.not.l_RT_EULER).and.(.not.l_RT_RK2).and.(     l_RT_HEUN)) Integrator_name = "HEUN"
 if(trim(Integrator_name)=="") then
   call warning("Bad integrator. Using default RK2")
   l_RT_RK2=.true.
   Integrator_name = "RK2"
 endif
 if (l_RT_RWA)         Integrator_name = trim(Integrator_name)//" + RWA"
 if (l_RT_EXP)         Integrator_name = trim(Integrator_name)//" + EXP"
 !
 if(.not.any((/trim(global_gauge)=="velocity".or.trim(global_gauge)=="length"/)) ) then
   call warning(" Not a valid gauge in input. Switching to length gauge")
   global_gauge="length"
 endif
 !
 ! Use NE_tot_time if provided
 !==============================
 if (NE_tot_time>0._SP) NE_steps=max(NE_steps,nint(NE_tot_time/RT_step)+1)
 !
 ! Initialize RT_dyn_step
 !========================
 RT_dyn_step=RT_step
 !
 ! Set the I/O steps: (J,P,CARRIERs) (GF) (output)
 !=================================================
 call SETUP_RT_IO_type(JPC_RT_IO_t,.TRUE.)
 call SETUP_RT_IO_type(OUTPUT_RT_IO_t,.TRUE.)
 call SETUP_RT_IO_type(GF_RT_IO_t,.TRUE.)
 !
 ! Dephasing Matrix. Including user-defined matrix-like dephasing factors, constant
 !==================
 ! radiative/phase, polaronic based
 !
 call section('=','Dephasing matrix setup')
 !
 call RT_Dephasing_Matrix(E) 
 !
 !
   ! J, P and M (spin) cache Time size (is switched off when an adaptative time step is used)
   !=========================
   IO_JPSM_cache_steps=JPC_RT_IO_t%INTERVAL_steps
   !
 !
 !
 ! External fields checks
 !========================
 !
 l_NE_with_fields=.false.
 !
 do i1=1,n_ext_fields
   !      
   Efield(i1)%ef_name=trim(Efield(i1)%ef_name)      
   !
   if((Efield(i1)%ef_name/='none')) then
     !
     if (.not.field_is_ok( Efield(i1)) ) &
&      call error('Field '//trim(Efield(i1)%ef_name)//' not supported or polarization is wrong')
     !      
     l_NE_with_fields=.true.
     !
     if(Efield(i1)%ef_name/='DELTA'.and.Efield(i1)%frequency(1)==0._SP) &
&      call error("External "//trim(Efield(i1)%ef_name)//" field has zero frequency.")
     !
     if(Efield(i1)%intensity==0._SP) &
&      call error("External "//trim(Efield(i1)%ef_name)//" field has zero intensity.")
     !
     if( all( abs(Efield(i1)%versor(:))<epsilon(1.) ) ) &
&      call error("External "//trim(Efield(i1)%ef_name)//" field has undefined direction.")
     !
     Efield(i1)%versor   =normalize_v(Efield(i1)%versor)
     Efield(i1)%amplitude=Efield_strength(Efield(i1)%intensity,'AU')
     !
     Efield(i1)%t_initial_indx=nint(Efield(i1)%t_initial/RT_step)+1
     if ( mod(Efield(i1)%t_initial_indx-1,JPC_RT_IO_t%INTERVAL_steps)/=0 ) then
       call warning('Field starting time corrected to be a mutiple of the I/O step.')
       Efield(i1)%t_initial_indx=Efield(i1)%t_initial_indx-mod(Efield(i1)%t_initial_indx-1,JPC_RT_IO_t%INTERVAL_steps)
     endif
     if (Efield(i1)%t_initial_indx< JPC_RT_IO_t%INTERVAL_steps+1 ) then
       call warning('Field starting time corrected to respect the I/O times.')
       Efield(i1)%t_initial_indx=JPC_RT_IO_t%INTERVAL_steps+1
     endif
     Efield(i1)%t_initial=real(Efield(i1)%t_initial_indx-1)*RT_step
     !
     call Check_symmetries(Efield(i1)%versor)
     !
     if ( Efield(i1)%ef_name=='DELTA' ) then
       Efield(i1)%t_final_indx=Efield(i1)%t_initial_indx+2
       Efield(i1)%t_final=RT_step*(Efield(i1)%t_final_indx-1)       
     else if ( (Efield(i1)%ef_name=='QSSIN') .or. (Efield(i1)%ef_name=='GAUSS') ) then
       call RT_Fluence(i1)
     else
       Efield(i1)%t_final_indx=    NE_steps
       Efield(i1)%t_final=RT_step*(NE_steps-1)
     endif
     !
   endif
   !
 enddo
 !
 l_NE_dynamics= l_NE_with_fields .or. l_rt_carriers_in_use
 !
 !
 if ( .not. l_NE_dynamics ) call error('No external fields nor manual pumping provided')
 !
 ! Number of K-points
 !====================
 RT_nk=RT_k_grid(E,k,q)
 !
 !
 ! LOG
 ! ===
 !
 call section('=','Messaging')
 !
 call msg( 'r','---General---')
 call msg( 'r', 'Integrator                 :',trim(Integrator_name))
 call msg( 'r', 'Simulation time        [fs]:',NE_tot_time*AUT2FS)
 call msg( 'r', 'Basic time step        [fs]:',RT_step*AUT2FS)
 !
 !
 call msg( 'nr','---Flags (if any)---')
 !
 call msg( 'r','Hxc approximation          :',trim(RT_potential))
 !
 !
 call msg( 'nr','---I/O---')
 call                          msg('r', 'T between I/O of J,P,f [fs]:',JPC_RT_IO_t%INTERVAL_time*AUT2FS)
 call                          msg('r', '          I/O of   GFs [fs]:',GF_RT_IO_t%INTERVAL_time*AUT2FS)
 call                          msg('r', '               outputs [fs]:',OUTPUT_RT_IO_t%INTERVAL_time*AUT2FS)
 if (l_use_collisions)    call msg( 'r','Extended collisions   [I/O]:',io_COLLs)
 !
 !
 if (l_NE_with_fields) then
   call msg( 'nr','---Fields---')
   call msg('r','Gauge                       :',trim(global_gauge))
   !
   !call msg('r','Pump/Probe periodicity  [fs]:',EtoT(E=maxval(Efield(:)%W_step))*AUT2FS)
   !call msg('r','Pump/Probe energy step  [eV]:',maxval(Efield(:)%W_step)*HA2EV)
   !
   do i1=1,n_ext_fields
     MSG_ch="Field"//trim(intc(i1))
     call msg('r', MSG_ch//' field                       :'//trim(Efield(i1)%ef_name))
     call msg('r', MSG_ch//' polarization                :'//trim(Efield(i1)%ef_pol))
     call msg('r', MSG_ch//' energy window/period [eV/fs]:',(/Efield(i1)%frequency*HA2EV,&
&                  2._SP*pi*HBAR_evfs/(Efield(i1)%frequency*HA2EV)/))
     call msg('r', MSG_ch//' elemental oscillation   [fs]:',EtoT(E=Efield(i1)%frequency(2)-Efield(i1)%frequency(2))*AUT2FS)
     call msg('r', MSG_ch//' width                   [fs]:',Efield(i1)%width*AUT2FS)
     call msg('r', MSG_ch//' energy steps                :',Efield(i1)%n_frequencies)
     call msg('r', MSG_ch//' electric field         [V/m]:',Efield(i1)%amplitude*AU2VMm1)
     call msg('r', MSG_ch//' max intensity      [kW/cm^2]:',Efield(i1)%intensity*AU2KWCMm2)
     !
     if( (Efield(i1)%ef_name=='QSSIN') .or. (Efield(i1)%ef_name=='GAUSS') .or. (Efield(i1)%ef_name=='DELTA') ) then
       call msg('r', MSG_ch//' max fluence        [nJ/cm^2]:',Efield(i1)%fluence )
       call msg('r', MSG_ch//' final time              [fs]:',Efield(i1)%t_final*AUT2FS)
       call msg('r', MSG_ch//' half maximum full width [fs]:',Efield(i1)%width*2.*sqrt(2.*log(2.))*AUT2FS)
     endif
     !
   enddo
   !
 endif
 !
end subroutine RT_initialize
