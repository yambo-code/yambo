!
! Copyright (C) 2000-2009 A. Marini and the YAMBO team 
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
module pars
 !
 implicit none
 !
 ! KINDS
 !
 integer, parameter  :: DP = selected_real_kind(14,200)
#if defined _DOUBLE
 integer, parameter  :: SP = selected_real_kind(14,200)
#else
 integer, parameter  :: SP = selected_real_kind(6,30)
#endif
 integer, parameter  :: IP = selected_int_kind(9)
 !
 ! STRINGs
 !
 integer, parameter  :: schlen=100
 integer, parameter  :: lchlen=300
 !
 ! CONSTANTs
 !
 real(SP), parameter :: pi=3.141592653589793238462643383279502884197_SP
#if defined _MAGNETIC
 real(SP), parameter :: alpha_m1=137.0359997_SP
#endif
 real(SP), parameter :: zero_dfl=1.E-5
 !
end module 
