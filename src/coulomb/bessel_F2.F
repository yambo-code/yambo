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
function bessel_F2(z)
 use pars,          ONLY:SP,rZERO
 use R_lattice,     ONLY:cyl_zz_save,cyl_ph_radius
 implicit none
 real(SP) :: z
 ! 
 ! Work Space
 !
 integer       :: neval,ier,last,iwork(3)
 real(SP)      :: abserr,G,work(12),bessel_F2
 real(SP), external :: bessel_J0
 cyl_zz_save=z
#if defined _DOUBLE
 call dqag(bessel_J0,rZERO,cyl_ph_radius,8.e-1,0.,4,G,abserr,neval,ier, &
&         3,12,last,iwork,work)
#else
 call qag(bessel_J0,rZERO,cyl_ph_radius,8.e-1,0.,4,G,abserr,neval,ier, &
&         3,12,last,iwork,work)
#endif
 bessel_F2=G
end function 
