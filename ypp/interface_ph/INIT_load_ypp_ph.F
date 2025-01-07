!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine INIT_ypp_load_ph(defs)
 !
 use pars,          ONLY:SP
 use it_tools,      ONLY:it
 use it_m,          ONLY:initdefs,E_unit,G_unit,V_general,V_RL
 use YPP_ELPH,      ONLY:elph_dbs_path,elph_Ef,elph_gamma_broad,elph_residual_treshold,elph_steps,&
&                        ph_broad,ph_freqs_file,ph_modes_file
 use ELPH,          ONLY:EkplusQ_mode,elph_branches
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
 ! ... derived
 !=============
 !
 call it('r',defs,'eliashberg', '[R] Eliashberg')
 call it('r',defs,'gkkp_db',    '[R] GKKP database')
 call it('r',defs,'gkkp_dg',    '[R] GKKP double grid')
 call it('r',defs,'gkkp_plot',  '[R] GKKP plot')
 call it('r',defs,'ph_ass_dos', '[R] Phonon-assisted DOS')
 !
 ! Variables...
 !==============
 call it(defs,'ResTresh', '[o/o] Treshold on processed polaronic residuals',elph_residual_treshold)
 ! 
 !EL-PH
 !
 call it(defs,'DBsPATH',  'Path to the PW el-ph databases',elph_dbs_path)
 call it(defs,'PHfreqF',  'PWscf format file containing the phonon frequencies',ph_freqs_file)
 call it(defs,'PHmodeF',  'PWscf format file containing the phonon modes',ph_modes_file)
 call it(defs,'ElPhModes', '[ELPH] Phonon modes included',elph_branches)
 call it(defs,'EkplusQmode',  'E(k+q) energies calculation mode (interp | dftp)',EkplusQ_mode)
 call it(defs,'EfGbroad', 'Fermi surface broadening (Gamma)',elph_gamma_broad,E_unit)
 call it(defs,'PhBroad','Phonon broadening (Eliashberg & DOS)',ph_broad,E_unit)
 call it(defs,'EE_Fermi','External Fermi Level (Eliashberg)',elph_Ef,E_unit)
 call it(defs,'PhStps', 'Energy steps',elph_steps)
 call it('f',defs,'TestPHDGrid','Test double-grid: set all values of the fine grid equal to the couse ones')
 call it('f',defs,'GkkpReadBare','Read the bare gkkp')
 call it('f',defs,'GkkpExpand','Expand the gkkp in the whole BZ')
 call it('f',defs,'UseQindxB','Use qindx_B to expand gkkp (for testing purposes)')
 !
end subroutine
