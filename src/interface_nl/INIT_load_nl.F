!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine INIT_load_nl(defs,en,q,k,X,Xw,Dip)
 !
 use drivers,       ONLY:infile_editing
 use pars,          ONLY:schlen,SP
 use electrons,     ONLY:levels
 use frequency,     ONLY:w_samp
 use it_tools,      ONLY:it
 use it_m,          ONLY:initdefs,initmode,V_nl_optics,V_real_time,V_parallel,V_resp, &
 &                       E_unit,I_unit,T_unit,Time_unit
 use X_m,           ONLY:X_t
 use DIPOLES,       ONLY:DIPOLE_t
 use R_lattice,     ONLY:bz_samp
 use real_time,     ONLY:RT_step,Integrator_name,NE_time,NE_tot_time
 use nl_optics,     ONLY:NL_bands,NL_damping,NL_correlation,NL_er,n_frequencies,NL_LRC_alpha, &
&                        NL_verb_name,n_angles,NL_rot_axis 
#if defined _OPENMP
 use openmp,        ONLY:n_threads_OSCLL,n_threads_NL
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
 call INIT_load_rt(defs,en,q,k,X,Xw,Dip)
 !
 ! ... list of USER fields 
 call LEVELS_mirror_nl("USER_fields",en,X,Dip,l_USER_fields)
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
 call it(defs,'OSCLL_Threads',    '[OPENMP/X] Number of threads for Oscillators',n_threads_OSCLL)
 call it(defs,'NL_Threads',      '[OPENMP/NL] Number of threads for nl-optics',n_threads_NL)
#endif
 !
 ! Non-linear Optics
 !
 call it(defs,'NLBands',      '[NL] Bands range',NL_bands)
 call it(defs,'NLverbosity',  '[NL] Verbosity level (low | high)',NL_verb_name) 
 call it(defs,'NLstep',       '[NL] Time step length',RT_step,unit=Time_unit(1),Verb_level=V_nl_optics)
 call it(defs,'NLtime',       '[NL] Simulation Time',NE_tot_time,unit=Time_unit(1))
 call it(defs,'NLintegrator', '[NL] Integrator ("EULEREXP/RK2/RK4/RK2EXP/HEUN/INVINT/CRANKNIC")',Integrator_name)
 call it(defs,'NLCorrelation','[NL] Correlation ("IPA/HARTREE/TDDFT/LRC/LRW/JGM/SEX/LSEX/LHF") ',NL_correlation)
 call it(defs,'NLLrcAlpha',   '[NL] Long Range Correction',NL_LRC_alpha)
 call it(defs,'NLDamping',    '[NL] Damping (or dephasing)',NL_damping,unit=E_unit)
 call it(defs,'NLEnRange',    '[NL] Energy range (for loop on frequencies NLEnSteps/=0',NL_er,E_unit)
 call it(defs,'NLAngSteps',   '[NL] Angular steps (if NLAngSteps/=0 field versor will be ignored)',n_angles)
 call it(defs,'NLrotaxis',    '[NL] Rotation axis (for the loop on angles NLAngSteps/=0)',NL_rot_axis)
 call it(defs,'NLEnSteps',    '[NL] Energy steps for the loop on frequencies',n_frequencies)
 call it('f',defs,'UseDipoles','[NL] Use Covariant Dipoles (just for test purpose)',verb_level=V_nl_optics)
 call it('f',defs,'TestOSCLL','[NL] Test Oscillators vs Full Collisions [for testing purposes]',verb_level=V_nl_optics)
 call it('f',defs,'FrSndOrd','[NL] Force second order in Covariant Dipoles',verb_level=V_nl_optics)
 call it('f',defs,'NoComprCOLL','[NL] Load collisions in double-precision and do not remove small elements (default false)',&
&  verb_level=V_nl_optics)
 call it('f',defs,'EvalCurrent','[NL] Evaluate the current')
 !
 ! DIPOLES
 !
 call it('f',defs,'EvPolarization','[DIP] Evaluate Polarization (require DipApproach=Covariant)',verb_level=V_resp)
 call it('f',defs,'FrPolPerdic','[DIP] Force periodicity of polarization respect to the external field')
 !
 if (initmode==4) then
   call LEVELS_mirror_nl("MIRROR_q",en,X,Dip,l_USER_fields)
   call LEVELS_mirror_nl("MIRROR_bands",en,X,Dip,l_USER_fields)
 endif
 ! 
 contains 
   !
#if defined _MPI 
   !
   subroutine CPU_structure_load()
     use parallel_m, ONLY:n_CPU_str_max,CPU_structure,PARALLEL_default_mode
     integer           :: i1,i_s
     character(schlen) :: name,possible_fields
     !
     do i1=1,2
       !
       if (i1==1) i_s=8
       if (i1==2) i_s=11
       !
       if (i_s==8) possible_fields='(w,k) '      ! NL
       if (i_s==11)possible_fields='(k,b) '      ! OSCLL
       !
       if (len_trim(CPU_structure(i_s)%Short_Description)==0) cycle
       name=trim(CPU_structure(i_s)%Short_Description)//"_CPU"
       call it(defs,trim(name),'[PARALLEL] CPUs for each role',&
&              CPU_structure(i_s)%CPU_string,verb_level=V_parallel)
       name=trim(CPU_structure(i_s)%Short_Description)//"_ROLEs"
       call it(defs,trim(name),'[PARALLEL] CPUs roles '//trim(possible_fields),&
&              CPU_structure(i_s)%ROLE_string,verb_level=V_parallel)
       !
     enddo
     !
   end subroutine
   !
#endif
   !
end subroutine
