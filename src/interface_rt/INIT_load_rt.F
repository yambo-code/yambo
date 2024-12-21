!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine INIT_load_rt(defs,en,q,k,X,Xw,Dip)
 !
 use drivers,       ONLY:infile_editing
 use pars,          ONLY:schlen,SP
 use electrons,     ONLY:levels
 use frequency,     ONLY:w_samp
 use it_tools,      ONLY:it
 use it_m,          ONLY:initdefs,initmode,V_sc,V_real_time,V_parallel, &
 &                       E_unit,I_unit,T_unit,Time_unit
 use X_m,           ONLY:X_t
 use DIPOLES,       ONLY:DIPOLE_t
 use R_lattice,     ONLY:bz_samp
#if defined _QED
 use QP_m,          ONLY:QP_QED_ng
#endif
 use hamiltonian,   ONLY:H_potential
 use collision_ext, ONLY:COLLISIONS_cutoff,COLL_bands
 use RT_control,    ONLY:CARR_RT_IO_t,OBS_RT_IO_t,OUTPUT_RT_IO_t,Gless_RESTART_RT_IO_t,     &
&                        CACHE_OBS_INTERVAL_time_INPUT,RT_BSE_Occ_Mode,Gless_RT_IO_t
 use stderr,        ONLY:intc
 use RT_occupations,ONLY:RT_RTA_temp,RT_RTA_chem
 use RT_lifetimes,  ONLY:RT_life_extrap_times_INPUT
 use real_time,     ONLY:RT_step, Integrator_name, RAD_LifeTime, Phase_LifeTime, &
&                        NE_tot_time,RT_bands,RT_deph_range,                     &
&                        RT_ave_dephasing,RT_eh_en,RT_scatt_tresh,Gr_kind,       &
&                        NE_MEM_treshold,NE_time_step_update_jump_INPUT,         &
&                        two_alpha,DbGd_EE_percent,NE_time_step_update_jump,     &
&                        NE_initial_time_step_update,NE_step_update_treshold,    &
&                        RT_MAX_step,RAD_magnification,  &
&                        RT_deph_deg_thresh,RT_step_manual_prefactor,NE_time_step_update_manual
 use fields,        ONLY:Efield,n_ext_fields_max,n_ext_fields
 use plasma,        ONLY:PLASMA_redux_percent
#if defined _OPENMP
 use openmp,        ONLY:n_threads_RT
#endif
 !
 implicit none
 !
 type(initdefs)::defs
 type(levels)  ::en          
 type(bz_samp) ::q,k
 type(X_t)     ::X(5)
 type(DIPOLE_t)::Dip
 type(w_samp)  ::Xw(5)
 !
 ! Work Space
 !
 integer  :: i_field
 real(SP) :: IO_times(3)
 logical  :: l_USER_fields(16)
 !
 ! RunLevels...
 !==============
 !
 call INIT_load(defs,en,q,k,X,Xw,Dip)
 !
 ! ... list of USER fields 
 call LEVELS_mirror_rt("USER_fields",en,X,Dip,l_USER_fields)
 !
#if defined _RT_SCATT
 call it('r',defs,'el_el_scatt','[R] Electron-Electron Scattering')
 call it('r',defs,'el_photon_scatt','[R] Electron-Photon   Scattering')   
#endif
#if defined _QED
 call it('r',defs,'el_photon_corr','[R] Electron-Photon Correlation') 
#endif
 !
 !=======
 ! CPU's
 !=======
 !
#if defined _MPI 
 call CPU_structure_load( )
#endif
 !
 ! Parallel Setup
 !
#if defined _OPENMP
 call it(defs,'RT_Threads',      '[OPENMP/RT] Number of threads for real-time',n_threads_RT)
#endif
 !
 ! S_xc
 !
#if defined _QED
 call it(defs,'QEDRLvcs', '[QED] Vector-Potential G-vectors components',QP_QED_ng,G_unit)
#endif
 !
 ! BSE: Real-Time
 !
 call it(defs,'RTOccMode','[RT-BSE] (K)ernel/(R)esiduals. BSE components to be corrected with the TD occupations',&
&             RT_BSE_Occ_Mode,verb_level=V_real_time)
 call it('f',defs,'ForceEqTrans','[RT-BSE] Use only equilibrium transitions',verb_level=V_real_time)
 !
 ! Real Time dynamics
 !
 call it(defs,'RTBands',    '[RT] Bands',RT_bands)
 !
 ! This call is a temporary fix due to the multiple definition of energy ranges.
 ! The call is needed to align the different values based on the USER definition.
 !
 call it(defs,'TwoAlpha',   '[RT] C_nk ~ alpha*Gamma_nk^2 ',two_alpha,Verb_level=V_real_time)
 call it(defs,'GrKind',     '[RT] G-ret kind: Lorentzian (QP)/ Hyperbolic QP_secant (HS)',Gr_kind,Verb_level=V_real_time)
 call it(defs,'RADLifeTime','[RT] Radiative life-time (if negative RADLifeTime=Phase_LifeTime)', &
&         RAD_LifeTime,unit=Time_unit(1))
 call it(defs,'RADmagnific','[RT] Radiative life-time magnification',RAD_magnification,Verb_level=V_real_time)
 call it(defs,'PhLifeTime', '[RT] Constant Dephasing Time',Phase_LifeTime,unit=Time_unit(1))
 call it(defs,'DephTRange', '[RT] Time range in which Dephasing is applied',RT_deph_range,unit=Time_unit(1),Verb_level=V_real_time)
 call it(defs,'DephEThresh', '[RT] Threshold on the energy difference between two states to dephase them',&
         &   RT_deph_deg_thresh,unit=E_unit,Verb_level=V_real_time)
 !
 ! Dynamics
 !
 call it(defs,'RTstep',      '[RT] Real Time step length',RT_step,unit=Time_unit(3))
 call it(defs,'NETime',      '[RT] Simulation Time',NE_tot_time,unit=Time_unit(2))
 call it(defs,'dTupdateTimeSet','[RT] Time for manual deltaT update',NE_time_step_update_manual,&
&                            unit=Time_unit(1),verb_level=V_real_time)
 call it(defs,'dTupdateTime','[RT] Initial Time for deltaT update (active only if non-zero) ',NE_initial_time_step_update,&
&                            unit=Time_unit(1),verb_level=V_real_time)
 call it(defs,'dTupdateJump','[RT] Time betweem two deltaT updates',  NE_time_step_update_jump_INPUT,&
&                            unit=Time_unit(1),verb_level=V_real_time)
 call it(defs,'dTupdateTresh','[RT][o/o] Treshold of deltaT updates',  NE_step_update_treshold ,verb_level=V_real_time)
 call it(defs,'dT_MAX',      '[RT] Maximum value for the time-dependent dT',  RT_MAX_step ,verb_level=V_real_time,unit=Time_unit(1))
 call it(defs,'dT_SET',      '[RT] Prefactor for manual dT update',  RT_step_manual_prefactor,verb_level=V_real_time)
 call it(defs,'Integrator',  '[RT] Integrator. Use keywords space separated  ( "EULER/EXPn/INV" "SIMPLE/RK2/RK4/HEUN" "RWA") ',&
&              Integrator_name)
 !
 IO_times=(/CARR_RT_IO_t%INTERVAL_time_INPUT,Gless_RESTART_RT_IO_t%INTERVAL_time_INPUT,OUTPUT_RT_IO_t%INTERVAL_time_INPUT/)
 call it(defs,'IOtime',      '[RT] Time between two consecutive I/O (CARRIERs - GF - OUTPUT)',IO_times,unit=Time_unit(1))
 CARR_RT_IO_t%INTERVAL_time_INPUT             =IO_times(1)
 Gless_RESTART_RT_IO_t%INTERVAL_time_INPUT    =IO_times(2)
 Gless_RT_IO_t%INTERVAL_time_INPUT            =IO_times(2)
 OUTPUT_RT_IO_t%INTERVAL_time_INPUT           =IO_times(3)
 !
 IO_times(1:2)=(/CACHE_OBS_INTERVAL_time_INPUT,OBS_RT_IO_t%INTERVAL_time_INPUT/)
 call it(defs,'IOCachetime', '[RT] Time between two consecutive (caching - I/O) of OBSERVABLES',IO_times(1:2),unit=Time_unit(1),&
 &       verb_level=V_real_time)
 CACHE_OBS_INTERVAL_time_INPUT  =IO_times(1)
 OBS_RT_IO_t%INTERVAL_time_INPUT=IO_times(2)
 !
 call it(defs,'RTehEny',     '[RT] Electron-hole energy range',RT_eh_en,unit=E_unit,verb_level=V_real_time)
 !
 ! ... flags ...
 call it('f',defs,'DephCVonly',    '[RT] Dephase only in the CV channel',Verb_level=V_real_time)
 call it('f',defs,'RTskipImposeN', '[RT] Conservation of N, dN  imposed by hand on-the-fly',Verb_level=V_real_time)
 call it('f',defs,'RTEvalEnergy',  '[RT] Energy variation computed on the fly',Verb_level=V_real_time)
 call it('f',defs,'RTEvalEntropy', '[RT] Entropy variation computed on the fly',Verb_level=V_real_time)
 call it('f',defs,'SaveGhistory',  '[RT] Save the history of the green function',Verb_level=V_real_time)
 !
 ! ... updates ...
 call it('f',defs,'RTUpdateSOC',     '[RT] Update the SOC interaction',Verb_level=V_real_time)
 call it('f',defs,'RTUpdateE',     '[RT] Update the Enery levels on-the-fly',Verb_level=V_real_time)
 !
 call it('f',defs,'RTEqScatt',     '[RT] Include Gamma0f0 term in scattering',Verb_level=V_real_time)
 call it('f',defs,'RTImpForMet',   '[RT] Impose structure optimized for metals',Verb_level=V_real_time)
 call it('f',defs,'RTzeroTempRef', '[RT] Use zero temperature Fermi districution as reference',Verb_level=V_real_time)
 call it('f',defs,'RTskipPHabs',   '[RT] Skip e-p Lifetimes due to phonon absorption',Verb_level=V_real_time)
 !
 ! Scattering
 !
 call it('f',defs,'LifeExtrapolation',    '[RT] Skipped Lifetimes are extrapolated')
 call it(defs,'LifeExtrapSteps',   '[RT] Step length between and inside two consecutive groups of lifetimes',&
&                                  RT_life_extrap_times_INPUT,unit=Time_unit(1))
 call it('f',defs,'RelaxTimeApprox',    '[RT] Skipped Lifetimes are extrapolated',Verb_level=V_real_time)
 call it(defs,'RTAtemp',   '[RT] Temperatures for relaxation time approximation',&
&              RT_RTA_temp,unit=T_unit,Verb_level=V_real_time)
 call it(defs,'RTAchem',   '[RT] Chemical potentials for relaxation time approximation',&
&              RT_RTA_chem,unit=E_unit,Verb_level=V_real_time)
 call it(defs,'ScattTresh', '[RT] Treshold on the eh energy to compute the scattering',RT_scatt_tresh,&
&                           unit=E_unit,verb_level=V_real_time)
 !
 ! EE scattering
 !
 call it(defs,'PlasmaPerc', '[RT] % (0-100) of PLASMA eh pair reduction. =0 correspond to no reduction.',&
&        PLASMA_redux_percent,Verb_level=V_real_time)
 call it(defs,'EERimPerc',  '[RT] EE Double Grid (0-100): % of the points used in EE scatt',DbGd_EE_percent,Verb_level=V_real_time)
 call it('f',defs,'RTskipImposeE',   '[RT] Conservation of E (e-e channel) imposed by hand on-the-fly')
 !
 ! ELPH
 !
 call it(defs,'MemTresh',   '[RT] Treshold on the decay of the retarded GF',NE_MEM_treshold,verb_level=V_real_time)
 call it('f',defs,'UseDebyeE',   '[RT] Use a single Debye energy for all phonon modes',verb_level=V_real_time)
 call it('f',defs,'RT_T_evol',   '[RT] Use a complete Time evolution instead of the CCA',verb_level=V_real_time)
 !
 call it('f',defs,'InducedField','[RT] Include induced field in coupling and current',verb_level=V_real_time)
 call it('f',defs,'VelGaugeCorr','[RT] Correct the non local term of the pseudo with the vector potential',verb_level=V_real_time)
 !
 ! OLD / EXPERIMENTAL
 !
 call it(defs,'RTAveDeph',    '[RT] Dephasing for all elements not included in RTDePhMatrix', &
&                             RT_ave_dephasing,verb_level=V_real_time,unit=Time_unit(1))
 call it('f',defs,'LifeFitTemp','[RT] Fit on the fly  lifetimes ratio to a Fermi distribution',verb_level=V_real_time)
 !
 ! External fields
 !
 if (initmode==4.and..not.infile_editing) n_ext_fields=0
 do i_field=1,n_ext_fields_max
   call Afield(i_field)
 enddo
 !
 ! RT ctl 
 !
 call INIT_RT_ctl_load(defs,1)
 call INIT_RT_ctl_load(defs,2)
 call INIT_RT_ctl_load(defs,3)
 !
 ! DIPOLES
 !
 call it('f',defs,'SPINprojected', '[DIP] Project the spin dipoles in the c/v channels ',verb_level=V_real_time)
 !
 ! SC and RT common
 !
 call it(defs,'COLLBands',   '[COLL] Bands for the collisions',COLL_bands)
 call it(defs,'HXC_Potential', '[SC] SC HXC Potential',H_potential,protect=.FALSE.,case="A")
 call it(defs,'COLLCut',    '[SC,RT] Cutoff on the collisions, 0=full 1=none',COLLISIONS_cutoff,verb_level=V_sc)
 !
 if (initmode==4) then
   call LEVELS_mirror_rt("MIRROR_q",en,X,Dip,l_USER_fields)
   call LEVELS_mirror_rt("MIRROR_bands",en,X,Dip,l_USER_fields)
 endif
 ! 
 contains 
   !
   subroutine Afield(i_field)
     use units,    ONLY  :AU2KWCMm2
     use parser_m, ONLY  : parser
     character(10)      :: name
     integer            :: i_field
     logical            :: is_def
     !
     name='Field'//trim(intc(i_field))
     !
     call it(defs,trim(name)//'_Freq',            '[RT '//trim(name)//'] Frequency',&
&            Efield(i_field)%frequency,unit=E_unit)
     call it(defs,trim(name)//'_NFreqs',            '[RT '//trim(name)//'] Frequency',&
&            Efield(i_field)%n_frequencies)
     call it(defs,trim(name)//'_DFreq',           '[RT '//trim(name)//'] Frequency step',&
&            Efield(i_field)%W_step,unit=E_unit,verb_level=V_real_time)
     call it(defs,trim(name)//'_Int',             '[RT '//trim(name)//'] Intensity',&
&            Efield(i_field)%intensity,unit=I_unit)
     call it(defs,trim(name)//'_Width',           '[RT '//trim(name)//'] Width',&
&            Efield(i_field)%width,unit=Time_unit(1))
     call it(defs,trim(name)//'_FWHM',            '[RT '//trim(name)//'] Full Width at Half Maximum (overwrite width if set)',&
&            Efield(i_field)%FWHM,unit=Time_unit(1),verb_level=V_real_time)
     call it(defs,trim(name)//'_kind',            '[RT '//trim(name)//'] Kind(SIN|SOFTSIN| see more on src/modules/mod_fields.F)',&
&            Efield(i_field)%ef_name)
     call it(defs,trim(name)//'_pol',             '[RT '//trim(name)//'] Pol(linear|circular)',&
&            Efield(i_field)%ef_pol)
     call it(defs,trim(name)//'_Dir',             '[RT '//trim(name)//'] Versor',&
&            Efield(i_field)%versor)
     call it(defs,trim(name)//'_Dir_circ',        '[RT '//trim(name)//'] Versor_circ',&
&            Efield(i_field)%versor_circ,verb_level=V_real_time)
     call it(defs,trim(name)//'_Tstart',          '[RT '//trim(name)//'] Initial Time',&
&            Efield(i_field)%t_initial,unit=Time_unit(1))
     !
     if (initmode==4.and..not.infile_editing) then
       !
       call parser(trim(name)//'_FWHM',is_def)
       if (     is_def) Efield(i_field)%width=Efield(i_field)%FWHM/(2._SP*sqrt(2._SP*log(2._SP)))
       if (.not.is_def) Efield(i_field)%FWHM=Efield(i_field)%width*(2._SP*sqrt(2._SP*log(2._SP)))
       !
       if (trim(Efield(i_field)%ef_name)/='none') n_ext_fields=n_ext_fields+1
       !
     endif
     !
   end subroutine
   !
#if defined _MPI 
   !
   subroutine CPU_structure_load()
     use parallel_m, ONLY:n_CPU_str_max,CPU_structure,PARALLEL_default_mode
     integer           :: i_s
     character(schlen) :: name,possible_fields
     !
     i_s=6
     !
     possible_fields='(k,b,q,qp)'  ! RT
     !
     if (len_trim(CPU_structure(i_s)%Short_Description)==0) return
     name=trim(CPU_structure(i_s)%Short_Description)//"_CPU"
     call it(defs,trim(name),'[PARALLEL] CPUs for each role',&
     &       CPU_structure(i_s)%CPU_string,verb_level=V_parallel)
     name=trim(CPU_structure(i_s)%Short_Description)//"_ROLEs"
     call it(defs,trim(name),'[PARALLEL] CPUs roles '//trim(possible_fields),&
     &       CPU_structure(i_s)%ROLE_string,verb_level=V_parallel)
     !
   end subroutine
   !
#endif
   !
end subroutine
