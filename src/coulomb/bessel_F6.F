!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DV
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
function bessel_F6(x,y)
 use pars,          ONLY:SP
 use R_lattice,     ONLY:cyl_ph_radius
 implicit none
 real(SP) :: x,y
 ! 
 ! Work Space
 !
 real(SP) :: arg1,arg2,bk0,bk1,bessel_F6
#if defined _DOUBLE
 real(SP), external :: DBESJ0_,DBESJ1_,DBESK0,DBESK1
#else
 real(SP), external :: BESJ0,BESJ1,BESK0,BESK1
#endif
 !
 arg1=x*cyl_ph_radius
 arg2=y*cyl_ph_radius
 if (arg2 > 80) then
   bk0=0.
   bk1=0.
 else
#if defined _DOUBLE
   bk0=DBESK0(arg2)
   bk1=DBESK1(arg2)
#else
   bk0=BESK0(arg2)
   bk1=BESK1(arg2)
#endif
 endif
#if defined _DOUBLE
 bessel_F6=arg1*DBESJ1_(arg1)*bk0-arg2*DBESJ0_(arg1)*bk1
 bessel_F6=bessel_F6+1
#else
 bessel_F6=arg1*BESJ1(arg1)*bk0-arg2*BESJ0(arg1)*bk1
 bessel_F6=bessel_F6+1
#endif
 !
end function
