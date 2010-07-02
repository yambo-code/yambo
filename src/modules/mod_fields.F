!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
module fields
 !
 use pars, ONLY:SP,schlen,lchlen
 !
 ! Main Gauge
 !
 character(schlen) :: global_gauge
 !
 ! Electric Field(s)
 ! 
 type ext_field
   real(SP) :: t_initial
   integer  :: int_t_initial
   real(SP) :: frequency
   real(SP) :: damping
   real(SP) :: intensity
   real(SP) :: amplitude
   real(SP) :: versor(3)
   character(schlen) :: ef_name
 end type ext_field
 !
 integer, parameter  :: n_ext_fields=2
 type(ext_field)     :: Efield(n_ext_fields)
 !
 ! Vector potential
 ! 
 type gauge_field
   complex(SP) :: vecpot(3)
   complex(SP) :: vecpot_vel(3)
   complex(SP) :: vecpot_acc(3)
 end type gauge_field
 !
 type(gauge_field) :: A_vecpot
 type(gauge_field) :: A_vecpot_previous
 !
 ! Path for the longitudinal gauge
 !
 character(lchlen) :: long_path
 !
end module
