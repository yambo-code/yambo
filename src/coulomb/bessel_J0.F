!
!        Copyright (C) 2000-2020 the YAMBO team
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
function bessel_J0(s)
 use pars,          ONLY:SP
 use R_lattice,     ONLY:cyl_vr_save,cyl_zz_save
 implicit none
 !
 real(SP) ::bessel_J0
 ! 
 ! Work Space
 !
 real(SP) ::  s,arg
#if defined _DOUBLE
 real(SP) :: DBESJ0_
#else
 real(SP) :: BESJ0
#endif
 !
 arg=cyl_vr_save*s
 if(cyl_zz_save==0) then
#if defined _DOUBLE
   bessel_J0=DBESJ0_(arg)
#else
   bessel_J0=BESJ0(arg)
#endif
 else
#if defined _DOUBLE
   bessel_J0=DBESJ0_(arg)*s/sqrt(s**2+cyl_zz_save**2)
#else
   bessel_J0=BESJ0(arg)*s/sqrt(s**2+cyl_zz_save**2)
#endif
 endif
end function
