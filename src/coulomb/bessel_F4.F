!
!        Copyright (C) 2000-2019 the YAMBO team
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
function bessel_F4(z)
 use pars,          ONLY:SP
 use R_lattice,     ONLY:cyl_vr_save,cyl_cut
 implicit none
 real(SP)  :: z
 ! 
 ! Work Space
 !
 real(SP)  :: arg,bessel_F4
#if defined _DOUBLE
 real(SP), external :: DBESJ0_
#else
 real(SP), external :: BESJ0
#endif
 if (z==0) then
   bessel_F4=0.
 else 
   arg=cyl_vr_save*z
#if defined _DOUBLE
   bessel_F4=DBESJ0_(arg)*z*log((cyl_cut+sqrt(cyl_cut**2+z**2))/z)
#else
   bessel_F4=BESJ0(arg)*z*log((cyl_cut+sqrt(cyl_cut**2+z**2))/z)
#endif
 endif
end function
