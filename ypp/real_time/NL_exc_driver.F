!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG
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
subroutine NL_exc_driver(en,k)
 !
 use YPP_real_time,  ONLY:X_order
 use pars,           ONLY:SP,pi,cZERO,lchlen
 use units,          ONLY:HA2EV,AUT2FS
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use parser_m,       ONLY:parser
 use nl_optics,      ONLY:n_frequencies
 !
#include<memory.h>
 !
 type(levels)  :: en
 type(bz_samp) :: k
 !
 ! Working Space
 !
 integer, external::io_NL
 integer  :: io_NonLinear,ID
 !
 call section("*",'NL Exc Post Processing')
 !
 ! Input/Output
 !   
end subroutine NL_exc_driver
