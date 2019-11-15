! 
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
 ! SPIN
 !------
 if (index(what,"S") > 0) then
   call electrons_spin_factors((/1,SEED_k%nibz/),BANDS_range,spin_fac)
   call INTERPOLATION_driver_seed(dims=BANDS_range,SEED_k=SEED_k,SEED_R3=spin_fac,operation="spin",&
&                            ID_op=ID_spin,NEAREST=.TRUE.)
   call INTERPOLATION_driver_do(OUT_k=OUT_k,operation="spin")
 endif
 !
 ! MAGNETIZATION
 !---------------
 if (index(what,"M") > 0) then
   call electrons_magn_factors((/1,SEED_k%nibz/),BANDS_range,magn_fac)
   call INTERPOLATION_driver_seed(dims=BANDS_range,SEED_k=SEED_k,SEED_R3=magn_fac,operation="magnetization",&
&                            ID_op=ID_magn,NEAREST=.TRUE.)
   call INTERPOLATION_driver_do("magnetization",OUT_k)
 endif
 !
end subroutine electrons_spin_and_magn_interpolate
