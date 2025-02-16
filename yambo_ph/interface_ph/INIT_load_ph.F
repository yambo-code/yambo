!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine INIT_load_ph(defs,en,q,k,X,Xw,Dip)
 !
 use pars,          ONLY:schlen,SP
 use electrons,     ONLY:levels
 use frequency,     ONLY:w_samp
 use it_tools,      ONLY:it
 use it_m,          ONLY:initdefs,E_unit,V_ph,V_qp,V_parallel
 use X_m,           ONLY:X_t
 use DIPOLES,       ONLY:DIPOLE_t
 use QP_m,          ONLY:QP_G_damp
 use R_lattice,     ONLY:bz_samp
 use ELPH,          ONLY:QP_PH_n_G_bands,elph_nQ_used,elph_Ham_ik,elph_Ham_bands,elph_branches,&
&                               RES_tresh,FAN_deltaE_treshold,DW_deltaE_treshold,E_kpq_sh_fact,gkkp_db
#if defined _PHEL
 use PHEL,          ONLY:PH_Self_Energy_mode,PH_DbGd_points,PH_Q,PHEL_broad_factor,PHEL_fit_Energies
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
 real(SP) :: IO_times(3)
 logical  :: l_USER_fields(16)
 !
 ! RunLevels...
 !==============
 !
 call INIT_load(defs,en,q,k,X,Xw,Dip)
 !
 call it('r',defs,'el_ph_corr',    '[R] Electron-Phonon Correlation')   
#if defined _PHEL
 call it('r',defs,'ph_el_corr',    '[R] Phonon-Electron Correlation')   
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
#if defined _PHEL
 call it(defs,'GDamping', '[GW] G[W] damping',QP_G_damp,E_unit)
#else
 call it(defs,'GDamping', '[GW] G[W] damping',QP_G_damp,E_unit,verb_level=V_qp)
#endif
 !
#if defined _PHEL 
 call it(defs,'PHDbGdsize', '[PHEL] Size of subset of double grid k-points',PH_DbGd_points)
 call it(defs,'PHELQpts',   '[PHEL] Q-points considered',PH_Q)
 call it(defs,'PHELTrans',  '[PHEL] Energy window around W_ph to select transitions (units of GDamping)',PHEL_broad_factor)
 call it(defs,'PHEL_QPH_En','[PHEL] Energy points to get the Quasi-Phonon solution (units of the bare PH energy)',PHEL_fit_Energies)
 call it(defs,'PH_SE_mode', '[PHEL] Self-Energy scattering mode ("bare-bare","dressed-bare","dressed-dressed")',PH_Self_Energy_mode)
#endif
 !
 ! El-Ph
 !
 call it(defs,'GphBRnge',  '[ELPH] G[W] bands range',QP_PH_n_G_bands)
 call it(defs,'ElPhModes', '[ELPH] Phonon modes included',elph_branches)
 call it(defs,'FANdEtresh','[ELPH] Energy treshold for Fan denominator',FAN_deltaE_treshold,verb_level=V_ph,unit=E_unit)
 call it(defs,'DWdEtresh', '[ELPH] Energy treshold for DW denominator',DW_deltaE_treshold,verb_level=V_ph,unit=E_unit)
 call it(defs,'GkkpDB',    '[ELPH] GKKP database (gkkp | gkkp_expanded | genFroh )',gkkp_db,verb_level=V_ph)
 call it(defs,'ElPhHBRnge','[ELPH] Hamiltonian bands range',elph_Ham_bands)
 call it(defs,'ElPhHKpt',  '[ELPH] Hamiltonian k-point',elph_Ham_ik)
 call it(defs,'REStresh',  '[ELPH] Residual treshold to report in output files',RES_tresh)
 call it('f',defs,'WRgFsq','[ELPH] Dump on file gFsq coefficients')
 ! 
 contains 
   !
#if defined _MPI 
   !
   subroutine CPU_structure_load()
     use parallel_m, ONLY:n_CPU_str_max,CPU_structure,PARALLEL_default_mode
     integer           :: i_s
     character(schlen) :: name,possible_fields
     !
     i_s=10
     !
     possible_fields='(q,k,b) '    ! PH_SE
     !
     if (len_trim(CPU_structure(i_s)%Short_Description)==0) return
     name=trim(CPU_structure(i_s)%Short_Description)//"_CPU"
     call it(defs,trim(name),'[PARALLEL] CPUs for each role',&
&            CPU_structure(i_s)%CPU_string,verb_level=V_parallel)
     name=trim(CPU_structure(i_s)%Short_Description)//"_ROLEs"
     call it(defs,trim(name),'[PARALLEL] CPUs roles '//trim(possible_fields),&
&            CPU_structure(i_s)%ROLE_string,verb_level=V_parallel)
     !
   end subroutine
   !
#endif
   !
end subroutine
