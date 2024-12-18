!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): DSAM
!
subroutine YPP_PH_SET_defaults()
 !
 use YPP_ELPH,       ONLY:elph_dbs_path, elph_Ef, elph_gamma_broad, elph_steps, l_atomic_amplitude, l_eliashberg,&
&                         l_gkkp, l_phonons, ph_broad, ph_freqs_file, ph_modes_file
 use ELPH,           ONLY:gsqF_energy_steps,elph_branches
 !
 implicit none
 !
 call YPP_SET_defaults()
 !
 ! ELPH
 !
 l_phonons=.false.
 l_gkkp=.false.
 l_eliashberg=.false.
 l_atomic_amplitude=.false.
 elph_steps=200
 ph_broad=0.01_SP/HA2EV
 elph_gamma_broad=0._SP
 elph_Ef=0._SP
 elph_dbs_path='.'
 ph_freqs_file='none'
 ph_modes_file='none'
 gsqF_energy_steps=1
 elph_branches=(/-1,-1/)
 !
end subroutine YPP_PH_SET_defaults
