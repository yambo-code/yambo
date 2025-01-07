!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine INIT_ypp_load_rt(defs)
 !
 use it_tools,      ONLY:it
 use it_m,          ONLY:initdefs,E_unit,Time_unit,V_real_time
 use YPPm,          ONLY:Nel_fac
 use YPP_real_time, ONLY:RT_conf,Eh_pumped_Nel,Eh_pumped_pair_deltaE,Rho_deph,&
&                        Eh_pumped_pair_width,Probe_Keyword,Pump_Keyword,X_order,X_kind,Eh_pumped_pair_energy,&
&                        Eh_pumped_pair_BZ_width,Eh_mu,Eh_temp,h_mu_autotune_thr,TRabsWHAT,TRabsDIP_plane,&
&                        TRabsMODE,TRabsDIP_dir,TRabs_Eeh_treshold,Pump_path,RT_pol_mode
 use real_time,     ONLY:RT_bands
 !
 implicit none
 !
 type(initdefs) :: defs
 !
 ! RunLevels...
 !==============
 !
 call INIT_load_ypp(defs)
 !
 call it('r',defs,'RealTime',     '[R] TD Post-Processing')
 call it('r',defs,'RT_X',         '[R] Response functions Post-Processing')
 call it('r',defs,'RT_ph_osc',    '[R] Coherent phonon oscillations')
 !
 call it('r',defs,'Select_energy','[R] NEQ DBs from input energy window')
 call it('r',defs,'Select_manual','[R] NEQ DBs manually created')
 call it('r',defs,'Select_Fermi', '[R] NEQ DBs according to Fermi distribution')
 !
 call it('r',defs,'RTabs',        '[R] Transient absorption')
 call it('r',defs,'RToccupations','[R] Analize time-dependent occupations')
 call it('r',defs,'RTlifetimes',  '[R] Analize time-dependent lifetimes')
 call it('r',defs,'RTpol',        '[R] Analize time-dependent polarization')
 call it('r',defs,'RTdeltaRho',   '[R] Analize the density')
 call it('r',defs,'RTGtwotimes',  '[R] Construct G</G>/G(r)/G(a)/A(t,tp) from rho within GKBA')
 !
 call it('r',defs,'RTfitbands',   '[R] Post-Processing kind: fit on bands')
 call it('r',defs,'RTtime',       '[R] Post-Processing kind: function of time')
 call it('r',defs,'RTenergy',     '[R] Post-Processing kind: function of energy')
 call it('r',defs,'RTdos',        '[R] Post-Processing kind: 2D DOS, function of time and energy')
 !
 ! Variables...
 !==============
 !
 ! RT DBs create
 !
 call it(defs,'RTBands',    '[RT] Bands range',RT_bands)
 call it(defs,'RTpumpNel',  '[RT] Number of electrons excited by hand per unit cell',Eh_pumped_Nel)
 call it(defs,'RTpumpBZWd', '[RT][iku] K region size',Eh_pumped_pair_BZ_width)
 call it(defs,'RTpumpEhWd', '[RT] Max Energy window used in the levels pick-up',Eh_pumped_pair_width,unit=E_unit)
 ! Method 1
 call it(defs,'RTpumpEhEn', '[RT] Energy (relative to VBM/CBM) of the levels to be inverted by hand',&
&                            Eh_pumped_pair_energy,unit=E_unit)
 ! Method 2
 call it(defs,'RTpumpDE',   '[RT] Energy difference to be matched',Eh_pumped_pair_deltaE,unit=E_unit)
 !
 ! The idea for Fermi occupations is to start from the 'end' of a carrier relaxation run
 call it(defs,'RTmuEh', '[RT] Chemical potentials hole | electron',Eh_mu,unit=E_unit)
 call it(defs,'RTtempEh', '[RT] Effective temperature hole | electron',Eh_temp,unit=E_unit)
 call it(defs,'RTautotuneThr', '[RT] Threshold to match no. of pumped holes and electrons.',h_mu_autotune_thr)
 !
 call it('f',defs,'LoadGhistory','[NEGF] Build the NEQ density from G<(t,t)')
 call it('f',defs,'OCCgroup',    '[NEGF] Double grid states occupations are grouped')
 call it('f',defs,'IncludeEQocc','[NEGF] Include also equilibrium occupations')
 call it('f',defs,'BuildGles',   '[NEGF] Build G<(t,tp) with GKBA')
 call it('f',defs,'KeepCC',      '[NEGF] Use only the cc channel when reconstructuing G<(t,tp)')
 call it('f',defs,'NoOcc',       '[NEGF] Consider only the changes in the energy')
 call it('f',defs,'SkipFermiFIT','[NEGF] Do a Fermi Fit of occupations (and lifetimes ratio)')
 call it('f',defs,'SeparateEH',  '[NEGF] Separate electrons from holes in the data file') 
 !
 ! TRANSIENT ABSORPTION
 !
 call it(defs,'TRabsOBS',&
&             '[RT] RT obs. Use a combination of (abs)bsorption, (trans)mission, (refl)ectivity, (kerr)err',TRabsWHAT)
 call it(defs,'TRabsDIPplane', '[RT] Dipole rotation plane',TRabsDIP_plane)
 call it(defs,'TRabsDIPdir',   '[RT] Dipole reference direction. Defines the internal `x` axis',TRabsDIP_dir)
 call it(defs,'TRabsMODE',     '[RT] TRabs mode. It can be a combination of cv,vv,cc,ip,eq',TRabsMODE)
 call it(defs,'TRabsEtresh',   '[RT] Treshold imposed on the e/h pairs energy',TRabs_Eeh_treshold,unit=E_unit)
 !
 ! Bands
 !
 call it('f',defs,'NormN','Nel on the bands is renormalized to Nel in the IBZ')
 call it(defs,'NelFac'   ,'Rescale the total number of electrons',Nel_fac)
 !
 ! RT Post-Processing
 !
 call it(defs,'Xorder','Max order of the response/exc functions',X_order)
 call it(defs,'XKind' ,  'Which response function (rhorho|jj)',X_kind,verb_level=V_real_time)
 call it(defs,'EnRngeRt','Energy range',RT_conf%X_W_range,unit=E_unit)
 call it(defs,'CarrEnRnge','Carriers Energy range',RT_conf%Carriers_W_range,unit=E_unit,verb_level=V_real_time)
 call it(defs,'ETStpsRt','Total Energy steps',RT_conf%W_n_steps)
 call it(defs,'TimeRange','Time-window where processing is done',RT_conf%T_range,unit=Time_unit(1))
 call it(defs,'TimeStep','Time step',RT_conf%delta_T,unit=Time_unit(1))
 call it(defs,'DampMode','Damping type ( NONE | LORENTZIAN | GAUSSIAN )',RT_conf%damping)
 call it(defs,'DampFactor','Damping parameter',RT_conf%damp_factor,unit=E_unit) 
 call it(defs,'RhoDeph','Dephasing parameter for G_GKBA',Rho_deph,unit=E_unit) 
 call it(defs,'PolMode','Polarization analysis mode ("slice","transitions")',RT_pol_mode) 
 call it('f',defs,'UseFFT','Use Fast Fourier transform with EnRange taken from dynamics',verb_level=V_real_time)
 call it('f',defs,'PrintExc','Print excited states functions')
 !
 call it(defs,'ProbeKey','Keyword to select groups of PROBE databases',Probe_Keyword,verb_level=V_real_time)
 call it(defs,'PumpKey','Keyword to select groups of PUMP databases',Pump_Keyword,verb_level=V_real_time)
 !
 call it('f',defs,'FrMinDamp','Force minimal damping',verb_level=V_real_time)
 call it('f',defs,'SkipOBS_IO','Do not dump on file the RT observables (P(t),J(t),D(t)...)')
 call it('f',defs,'RmAsymLim','Remove Asymptotic limit from P or J',verb_level=V_real_time) 
 !
 call it('f',defs,'STD_style','Print occupations using the standard style',verb_level=V_real_time) 
 !
end subroutine
