!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use drivers,        ONLY:l_use_collisions,l_elph_scatt,l_elel_scatt,l_sc_hartree,l_use_Hxc_collisions,&
&                         l_sc_is_libDFT,l_sc_sex,l_sc_fock,l_elphoton_scatt,l_rt_carriers_in_use
 use pars,           ONLY:SP,pi
 use units,          ONLY:HA2EV,HBAR_eVfs,AU2VMm1,AU2KWCMm2,AUT2FS
 use stderr,         ONLY:intc
 use com,            ONLY:msg
 use QP_CTL_m,       ONLY:QP_ctl_user
 use vec_operate,    ONLY:normalize_v
 use plasma,         ONLY:EH_gas
 use X_m,            ONLY:global_gauge
 use electrons,      ONLY:levels
 use collision_ext,  ONLY:COLLISIONS_have_HARTREE
 use RT_control,     ONLY:CARR_RT_IO_t,OBS_RT_IO_t,RT_IO_t,GF_RT_IO_t,OUTPUT_RT_IO_t,                      &
&                         SETUP_RT_IO_type,IO_OBS_cache_steps,TIME_adjust,SAVE_G_history
 use real_time,      ONLY:RT_potential,RT_step,l_NE_with_fields,RT_bands,RT_dyn_step,RT_nk,   &
&                         NE_steps,NE_tot_time,l_RT_induced_field,RT_life_extrap_times,       &
&                         RAD_LifeTime,l_RT_CCA_Kernel,Life_MEM_steps,Phase_LifeTime,         &
&                         NE_MEM_treshold,G_MEM_steps,l_RT_uses_E_DbGd,DbGd_EE_Nk_table,      &
&                         RT_life_extrap_times_SAVE,l_RT_lifetimes_extrapolation,Gr_kind,     &
&                         l_RT_impose_N_dN,Integrator_name,l_RT_impose_E,DbGd_EE_percent,     &
&                         l_RT_update_Energies,eval_HARTREE,eval_DFT,l_NE_dynamics,           &
&                         NE_time_step_update_jump,NE_initial_time_step_update,NE_i_time,     &
&                         RAD_magnification,NE_step_update_treshold,l_lftm_fit_temperatures,  &
&                         NE_time_step_update_jump_SAVE,NE_initial_time_step_update,          &
&                         RT_is_dynamically_dephased,RT_is_statically_dephased,RT_MAX_step,   &
&                         eval_energy,eval_entropy,l_RT_is_WFfree,l_update_SOC,RT_DbGd_nk,    &
&                         l_RT_relax_time_approx,l_RT_eq_is_zero_temp,l_RT_include_eq_scatt,  &
&                         l_velocity_gauge_corr,l_velocity_gauge_diam,l_RT_impose_for_metals, &
&                         l_P_integrating_J,l_RT_rotate_DM,RT_deph_cv_only
 use IO_m,           ONLY:io_COLLs
 use R_lattice,      ONLY:bz_samp
 use fields,         ONLY:Efield,n_ext_fields,field_is_ok,EtoT,Efield_strength
 use parser_m,       ONLY:parser
 !
#include<memory.h>
 !
 type(levels), intent(inout) :: E
 type(bz_samp),intent(in)    :: q,k
 !
 ! Work space
 !
 integer                  :: i1,N_T_samplings
 character(6)             :: MSG_ch
 real(SP)                 :: T_memory
 logical                  :: l_Full_RT_T_evolution,l_skip_it,l_velocity_gauge_tmp
 real(SP), parameter      :: thresh=1.E-5_SP
 integer,         external:: RT_Lifetimes_evaluation_plan
 !
 ! Logicals
 !==========
 !
 call parser('InducedField' ,        l_RT_induced_field)
 call parser('VelGaugeNoDiam',       l_velocity_gauge_tmp)
 l_velocity_gauge_diam=.not.l_velocity_gauge_tmp
 !
 ! GPL_EXCLUDE_START
 !
 call parser('DephCVonly',           RT_deph_cv_only)
 call parser('RAD_prefactor',        RAD_magnification)
 call parser('VelGaugeCorr',         l_velocity_gauge_corr)
 call parser('PfromJ',               l_P_integrating_J)
 call parser('RT_T_evol',            l_Full_RT_T_evolution)
 !
 !...updates
 call parser('RTUpdateE',            l_RT_update_Energies)
 call parser('RTUpdateSOC',          l_update_SOC)
 l_RT_rotate_DM                     =l_RT_update_Energies
 !
 !...imposes
 call parser('RTskipImposeN',        l_skip_it)
 if (l_skip_it) l_RT_impose_N_dN=.FALSE.
 call parser('RTskipImposeE',        l_skip_it)
 if (l_skip_it) l_RT_impose_E=.FALSE.
 call parser('RTImpForMet',          l_RT_impose_for_metals)
 !
 !..lifetimes
 call parser('LifeExtrapolation',    l_RT_lifetimes_extrapolation)
 call parser('LifeFitTemp',          l_lftm_fit_temperatures)
 !
 call parser('RelaxTimeApprox',      l_RT_relax_time_approx)
 call parser('RTzeroTempRef',        l_RT_eq_is_zero_temp)
 call parser('RTEqScatt',            l_RT_include_eq_scatt)
 !
 !
 !...evals
 call parser('RTEvalEnergy',         eval_energy)
 call parser('RTEvalEntropy',        eval_entropy)
 if(.not.eval_energy) eval_entropy=.false.
 call parser('SaveGhistory',         SAVE_G_history)
 !
 !...drivers
 !
 l_RT_CCA_Kernel       =.not.l_Full_RT_T_evolution.and.l_elph_scatt
 !
 ! GPL_EXCLUDE_END
 !
 eval_DFT     =  l_sc_is_libDFT
 eval_HARTREE =  l_sc_hartree
 !
 ! Integrator FLAGS
 !==================
 call RT_Integrator_init()
 !
 ! Global gauge
 !==============
 if(.not.any((/trim(global_gauge)=="velocity".or.trim(global_gauge)=="length"/)) ) then
   call warning(" Not a valid gauge in input. Switching to length gauge")
   global_gauge="length"
 endif
 if(l_velocity_gauge_corr) l_velocity_gauge_corr=(trim(global_gauge)=="velocity")
 if(l_velocity_gauge_diam) l_velocity_gauge_diam=(trim(global_gauge)=="velocity")
 !
 ! Use NE_tot_time if provided
 !==============================
 NE_i_time=1
 if (NE_tot_time>0._SP) NE_steps=max(NE_steps,nint(NE_tot_time/RT_step)+1)
 !
 ! Initialize RT_dyn_step
 !========================
 RT_dyn_step=RT_step
 !
 ! Set the I/O steps: (J,P,CARRIERs) (GF) (output)
 !=================================================
 call SETUP_RT_IO_type(OBS_RT_IO_t,.TRUE.)
 call SETUP_RT_IO_type(CARR_RT_IO_t,.TRUE.)
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
 ! GPL_EXCLUDE_START
 !
 if (.not.RT_is_statically_dephased) then
   !
   if (l_elph_scatt.or.l_elel_scatt) then
     call warning('No Dephasing provided. E-E and/or E-P scatterings switched off')
     l_elph_scatt=.FALSE.
     l_elel_scatt=.FALSE.
   endif
   !
 endif
 !
 ! Memory Depth (Lifetimes) & extrapolation steps
 !================================================
 if (RT_is_statically_dephased) then
   !
   RT_life_extrap_times=RT_life_extrap_times_SAVE
   !
   if (all(RT_life_extrap_times>0)) then
     Life_MEM_steps=nint(RT_life_extrap_times(1)/RT_life_extrap_times(2))
     if (Life_MEM_steps<3) Life_MEM_steps=3
     if (RT_life_extrap_times(2)<RT_step) RT_life_extrap_times(2)=RT_step
     RT_life_extrap_times(2)=max(TIME_adjust(RT_life_extrap_times(2)),RT_step)
     RT_life_extrap_times(1)=RT_life_extrap_times(2)*Life_MEM_steps
   endif
   !
   RT_life_extrap_times_SAVE=RT_life_extrap_times
   !
 endif
 !
 ! Memory Depth (G_lesser)
 !=========================
 T_memory=0._SP
 if (l_elph_scatt.and.NE_MEM_treshold>0._SP) then
   T_memory=-1._SP/minval( abs(E%W(RT_bands(1):RT_bands(2),:,:)))*log(NE_MEM_treshold)
   G_MEM_steps=nint(T_memory/RT_step)
 endif
 !
 ! Update time Step
 !==================
 !
 if (NE_initial_time_step_update>0._SP) then
   !
   if (G_MEM_steps<3) G_MEM_steps=3
   !
   if (all(RT_life_extrap_times>0)) then
     NE_time_step_update_jump_SAVE = TIME_adjust(NE_time_step_update_jump_SAVE,2._SP*RT_life_extrap_times(1))
     if(NE_time_step_update_jump_SAVE<2._SP*RT_life_extrap_times(1)) NE_time_step_update_jump_SAVE=2._SP*RT_life_extrap_times(1) 
   endif
   !
   NE_time_step_update_jump=NE_time_step_update_jump_SAVE
   !
   OBS_RT_IO_t%INTERVAL_time_SAVE=TIME_adjust(OBS_RT_IO_t%INTERVAL_time_SAVE,NE_time_step_update_jump_SAVE)
   if(OBS_RT_IO_t%INTERVAL_time_SAVE>NE_time_step_update_jump_SAVE) OBS_RT_IO_t%INTERVAL_time_SAVE=NE_time_step_update_jump_SAVE
   if(OBS_RT_IO_t%INTERVAL_time_SAVE<RT_step*10._SP) OBS_RT_IO_t%INTERVAL_time_SAVE=NE_time_step_update_jump_SAVE
   call SETUP_RT_IO_type(OBS_RT_IO_t,.TRUE.)
   !
   if (RT_MAX_step<1.E-5) RT_MAX_step=10._SP*RT_step
   !
 endif
 !
 ! GPL_EXCLUDE_END
 !
 ! J, P and M (spin) cache Time size
 !=========================
 IO_OBS_cache_steps=OBS_RT_IO_t%INTERVAL_steps
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
     if ( ((Efield(i1)%ef_name=='QSSIN') .or. (Efield(i1)%ef_name=='GAUSS') .or. (Efield(i1)%ef_name=='PULSE')) &
&        .and. Efield(i1)%width<epsilon(1.)  ) &
&      call error("External "//trim(Efield(i1)%ef_name)//" field has undefined duration.")
     !
     Efield(i1)%versor   =normalize_v(Efield(i1)%versor)
     Efield(i1)%amplitude=Efield_strength(Efield(i1)%intensity,'AU')
     !
     Efield(i1)%t_initial_indx=max(nint(Efield(i1)%t_initial/RT_step)+1,2)
     Efield(i1)%t_initial=real(Efield(i1)%t_initial_indx-1)*RT_step
     !
     call Check_symmetries(Efield(i1)%versor)
     !
     if ( Efield(i1)%ef_name=='DELTA' ) then
       Efield(i1)%t_final_indx=Efield(i1)%t_initial_indx+2
       Efield(i1)%t_final=RT_step*(Efield(i1)%t_final_indx-1)       
     else if ( (Efield(i1)%ef_name=='QSSIN') .or. (Efield(i1)%ef_name=='GAUSS') .or. (Efield(i1)%ef_name=='PULSE') ) then
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
 ! GPL_EXCLUDE_START
 !
 ! Check on the PROBE/PUMP frequencies
 !====================================
 call RT_Field_Commensurable_Frequencies()
 !
 ! GPL_EXCLUDE_END
 !
 if ( .not. l_NE_dynamics ) call error('No external fields nor manual pumping provided')
 !
 ! Number of K-points
 !====================
 call RT_k_grid(E,k,q)
 !
 ! GPL_EXCLUDE_START
 !
 ! Memory Depth & Simulation points (Lifetimes)
 !=============================================
 if (l_elph_scatt.or.l_elel_scatt) then
   if (.not.allocated(E%W)) then
     call warning("NO QP damping provided. E-E and/or E-P switched off")
     l_elph_scatt=.FALSE.
     l_elel_scatt=.FALSE.
   endif
 endif
 !
 ! GPL_EXCLUDE_END
 !
 ! Checking extended COLLISIONS
 !==============================
 !
 if (l_elel_scatt.or.l_sc_sex.or.l_sc_fock.or.l_sc_hartree.or.l_sc_is_libDFT) then
   !
   call section('=','Electronic Collisions loading and setup')
   !
   ! GPL_EXCLUDE_START
   if (l_elel_scatt) then
     call PLASMA_build_up(E,k,q)
     call PLASMA_parallel_setup(EH_gas)
   endif
   ! GPL_EXCLUDE_END
   !
   if (io_COLLs) call COLLISIONS_load(.TRUE.)
   !
 endif
 !
 ! GPL_EXCLUDE_START
 !
#if defined _ELPH
 !
 ! Electron-phonon
 ! ===============
 !
 if (l_elph_scatt) then
   !
   call section('=','El-Ph matrix elements loading and setup')
   !                 =======================================
   call RT_ELPH_initialize(E,k,q)
   !
 endif
 !
#endif
 !
 ! GPL_EXCLUDE_END
 !
 l_RT_is_WFfree = (l_use_Hxc_collisions .or. trim(RT_potential)=="IP")   .and.     &
&                 (.not.eval_energy) .and. (.not.l_velocity_gauge_corr)  .and.     &
&                 (COLLISIONS_have_HARTREE.or.index(trim(RT_potential),"HARTREE")==0)
 !
 ! GPL_EXCLUDE_START
 !
 RT_is_dynamically_dephased=l_elel_scatt.or.l_elph_scatt.or.l_elphoton_scatt
 !
 if (     RT_is_dynamically_dephased) N_T_samplings = RT_Lifetimes_evaluation_plan( )
 if (.not.RT_is_dynamically_dephased) N_T_samplings = 0
 !
 call RT_adjust_equilibrium()
 !
 ! GPL_EXCLUDE_END
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
 call msg( 'r', '           steps           :',NE_steps)
 !
 ! GPL_EXCLUDE_START
 !
 call msg( 'r', '      time step   (now)[fs]:',RT_dyn_step*AUT2FS)
 !
 if (NE_initial_time_step_update>0.) then
   call msg( 'r','---Time Step Update---')
   call msg( 'r', 'Initial time           [fs]:',NE_initial_time_step_update*AUT2FS)
   call msg( 'r', 'Time Interval          [fs]:',NE_time_step_update_jump*AUT2FS)
   call msg( 'r', 'Treshold              [o/o]:',NE_step_update_treshold)
   call msg( 'r',  'dT maximum value      [fs]:',RT_MAX_step*AUT2FS)
 endif
 !
 call msg( 'r','---Memory---')
 call msg( 'r', 'Memory depth(G)        [fs]:',T_memory*AUT2FS)
 call msg( 'r', 'Memory steps(G)            :',G_MEM_steps)
 !
 if (RT_is_statically_dephased) then
   call msg( 'nr','---Lifetimes---')
   if (all(RT_life_extrap_times>0)) then
     call msg( 'r', 'Point groups (inter/intra) [fs]:',RT_life_extrap_times*AUT2FS)
     call msg( 'r', 'Extrapolation                  :',l_RT_lifetimes_extrapolation)
     call msg( 'r', 'Memory steps                   :',Life_MEM_steps)
   endif
   call msg( 'r', 'Evaluated Time points          :',N_T_samplings)
   call msg( 'r', 'Relaxation time approximation  :',l_RT_relax_time_approx)
   call msg( 'r', 'Equilibrium is zero temperature:',l_RT_eq_is_zero_temp)
   call msg( 'r', 'Equilibrium scattering included:',l_RT_include_eq_scatt)
 endif
 !
 ! GPL_EXCLUDE_END
 !
 call msg( 'nr','---Flags (if any)---')
 !
 call msg( 'r','Hxc approximation          :',trim(RT_potential))
 !
 ! GPL_EXCLUDE_START
 !
 if (l_elph_scatt)         call msg( 'r','El-Ph         scattering   :',l_elph_scatt)
 if (l_elel_scatt)         call msg( 'r','El-El         scattering   :',l_elel_scatt)
 if (l_elphoton_scatt)     call msg( 'r','El-Photon     scattering   :',l_elphoton_scatt)
 if (l_RT_update_Energies) then
   call msg( 'r','Update QP Energies         :',l_RT_update_Energies)
 endif
 if (l_update_SOC)         call msg( 'r','Update SOC Interaction     :',l_update_SOC)
 !
 ! GPL_EXCLUDE_END
 !
 call msg( 'nr','---I/O---')
 call                          msg('r', 'T between I/O of   OBS [fs]:',OBS_RT_IO_t%INTERVAL_time*AUT2FS)
 call                          msg('r', 'T between I/O of   CAR [fs]:',CARR_RT_IO_t%INTERVAL_time*AUT2FS)
 call                          msg('r', '          I/O of   GFs [fs]:',GF_RT_IO_t%INTERVAL_time*AUT2FS)
 call                          msg('r', '               outputs [fs]:',OUTPUT_RT_IO_t%INTERVAL_time*AUT2FS)
 if (l_use_collisions)    call msg( 'r','Extended collisions   [I/O]:',io_COLLs)
 !
 ! GPL_EXCLUDE_START
 !
 if(Phase_LifeTime>thresh.or.RAD_LifeTime>thresh) then
   call msg( 'nr','---Damping---')
   if(Phase_LifeTime>thresh) &
   & call msg( 'r', 'Damping (Phase)    [fs/meV]:',(/Phase_LifeTime*AUT2FS,HBAR_eVfs/Phase_LifeTime*AUT2FS*1000._SP/))
   if(RAD_LifeTime>thresh) &
   & call msg( 'r', '        (Radiative)[fs/meV]:',(/RAD_LifeTime  *AUT2FS,HBAR_eVfs/RAD_LifeTime  *AUT2FS*1000._SP/))
 endif
 !
 call msg( 'nr','---Retarded GF---')
 call msg( 'r', 'Retarded Green`s function  :',Gr_kind)
 call msg( 'r', '        Energy ref  (c)[eV]:',QP_ctl_user(3,1)%Wc_E_ref*HA2EV)
 call msg( 'r', '                    (v)[eV]:',QP_ctl_user(3,1)%Wv_E_ref*HA2EV)
 call msg( 'r', '        Analytic term (c)[eV,adim,eV^-1]:',QP_ctl_user(3,1)%Wc)
 call msg( 'r', '                      (v)[eV,adim,eV^-1]:',QP_ctl_user(3,1)%Wv)
 !
 if ((l_elel_scatt.or.l_elph_scatt).and.(.not.l_RT_impose_N_dN.or.l_RT_impose_E)) then
   call msg( 'nr','---Conservation Rules---')
   call msg( 'r',                  'dN (e-p) and N (e-p + e-e)  :',l_RT_impose_N_dN)
   if (l_elel_scatt) call msg( 'r','Energy (e-e)                :',l_RT_impose_E)
 endif
 !
 call msg( 'nr','---K Grid---')
 call msg( 'r',      'Double Grid support        :',l_RT_uses_E_DbGd)
 if (l_RT_uses_E_DbGd) then
   call msg( 'r',    'K-points            (TOTAL):',RT_DbGd_nk)
   call msg( 'r',    '                  (EVOLVED):',RT_nk)
   if (l_elel_scatt) then
     call msg( 'r',  'Percentual used in e-e[o/o]:',DbGd_EE_percent)
     call msg( 'r',  'Max e-e scattering states  :',maxval(DbGd_EE_Nk_table))
   endif
 else
   call msg( 'r',    'K-points            (TOTAL):',RT_nk)
 endif
 !
 ! GPL_EXCLUDE_END
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
 contains
   !
   subroutine RT_adjust_equilibrium()
     !
     use electrons,          ONLY:spin_occ,n_sp_pol,BZ_DbGd_tot_nk_levels
     use real_time,          ONLY:RT_ibz_coo,DbGd_ibz_coo
     !
     integer :: ik,ik_RT_DbGd,ik_E_DbGd,ib
     !
     if(.not.l_RT_eq_is_zero_temp) return
     !
     if(.not.allocated(E%fo)) then
       YAMBO_ALLOC(E%fo,(E%nb,E%nk,n_sp_pol))
       E%fo=E%f
     endif
     !
     if(.not.allocated(E%df)) then
       YAMBO_ALLOC(E%df,(E%nb,E%nk,n_sp_pol))
       E%df=0._SP
     endif
     !
     if (l_RT_uses_E_DbGd.and..not.allocated(E%df_DbGd)) then
       YAMBO_ALLOC(E%df_DbGd,(E%nb,BZ_DbGd_tot_nk_levels,n_sp_pol))
       E%df_DbGd=0._SP
     endif
     !
     ! Defined from occupations %f
     ! In case RT_apply has been used the reference is
     ! defined from the %fo occupations
     !
     do ib=RT_bands(1),RT_bands(2)
       do ik=1,k%nibz
         do ik_RT_DbGd=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
           ik_E_DbGd=ik_RT_DbGd-RT_ibz_coo(ik,1)+DbGd_ibz_coo(ik,1)
           !
           if (l_RT_uses_E_DbGd) then
             if(E%E_DbGd(ib,ik_E_DbGd,1)<=E%E_Fermi) then
               E%df_DbGd(ib,ik_E_DbGd,1)=(E%f_DbGd(ib,ik_E_DbGd,1)-spin_occ)
             else
               E%df_DbGd(ib,ik_E_DbGd,1)= E%f_DbGd(ib,ik_E_DbGd,1)
             endif
           else
             if(E%E(ib,ik,1)<=E%E_Fermi) then
               E%df(ib,ik,1)=(E%f(ib,ik,1)-spin_occ)
               E%fo(ib,ik,1)= spin_occ
             else
               E%df(ib,ik,1)= E%f(ib,ik,1)
               E%fo(ib,ik,1)= 0._SP
             endif
           endif
           !
         enddo
       enddo
     enddo
     !
   end subroutine RT_adjust_equilibrium
   !
end subroutine RT_initialize
