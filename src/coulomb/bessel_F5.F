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
function bessel_F5(z)
 use pars,          ONLY:SP
 use R_lattice,     ONLY:cyl_vr_save
 implicit none
 ! 
 ! Work Space
 !
 real(SP)           :: z,arg,bessel_F5
#if defined _DOUBLE
 real(SP), external :: DBESJ0_
#else
 real(SP), external :: BESJ0
#endif
 if (z==0) then
   bessel_F5=0.
 else
   arg=cyl_vr_save*z
#if defined _DOUBLE
   bessel_F5=DBESJ0_(arg)*z*log(z)
#else
   bessel_F5=BESJ0(arg)*z*log(z)
#endif
 endif
 !
end function
