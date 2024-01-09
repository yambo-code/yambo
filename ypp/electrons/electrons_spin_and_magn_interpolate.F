! 
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine electrons_spin_and_magn_interpolate(what,SEED_k,OUT_k,BANDS_range,ID_spin,ID_magn)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:n_spinor
 use R_lattice,      ONLY:bz_samp
 use interpolate,    ONLY:INTERPOLATION_driver_seed,INTERPOLATION_driver_do
 !
 implicit none
 !
 character(*) , intent(in)    :: what
 type(bz_samp), intent(in)    :: SEED_k
 integer      , intent(in)    :: BANDS_range(2)
 integer      , intent(out)   :: ID_spin,ID_magn
 type(bz_samp), intent(inout) :: OUT_k
 !
 ! Work space
 !
 real(SP) :: spin_fac(BANDS_range(1):BANDS_range(2),SEED_k%nibz,n_spinor),&
&            magn_fac(BANDS_range(1):BANDS_range(2),SEED_k%nibz,3)
 !
 ID_spin=0
 ID_magn=0
 !
 ! SPIN
 !------
 if (index(what,"S") > 0) then
   call electrons_spin_factors((/1,SEED_k%nibz/),BANDS_range,spin_fac)
   call INTERPOLATION_driver_seed("spin",ID_spin,BANDS_range,SEED_k,SEED_R3=spin_fac)
   call INTERPOLATION_driver_do("spin",ID_spin,OUT_k)
 endif
 !
 ! MAGNETIZATION
 !---------------
 if (index(what,"M") > 0) then
   call electrons_magn_factors((/1,SEED_k%nibz/),BANDS_range,magn_fac)
   call INTERPOLATION_driver_seed("magnetization",ID_magn,BANDS_range,SEED_k,SEED_R3=magn_fac)
   call INTERPOLATION_driver_do("magnetization",ID_magn,OUT_k)
 endif
 !
end subroutine electrons_spin_and_magn_interpolate
