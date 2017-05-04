!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
 use netcdf
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
 integer, parameter  :: IP  = selected_int_kind(8)
 integer, parameter  :: IP4 = selected_int_kind(4)
 integer, parameter  :: IP2 = selected_int_kind(2)
 integer, parameter  :: IP1 = selected_int_kind(1)
 !
 ! NetCDF types
 !
#if defined _DOUBLE
 integer, parameter  :: nf90_SP = nf90_double
#else
 integer, parameter  :: nf90_SP = nf90_real
#endif
 !
 ! Equivalent to OneByteInt in NetCdf (normal integer is 4 byte)
 integer, parameter  :: LP = selected_int_kind(2)
 !
 ! STRINGs
 !
 integer, parameter  :: schlen=100
 integer, parameter  :: lchlen=300
 !
 ! CONSTANTs
 !
 real(SP),    parameter :: pi=3.141592653589793238462643383279502884197_SP
 real(DP),    parameter :: pi_DP=3.141592653589793238462643383279502884197_DP
 real(SP),    parameter :: zero_dfl=1.E-5_SP
 real(SP),    parameter :: infinity_dfl=HUGE(1._SP)
 real(SP),    parameter :: rZERO=0._SP
 real(SP),    parameter :: rONE =1._SP
 complex(SP), parameter :: cI   =(0._SP,1._SP)
 complex(SP), parameter :: cONE =(1._SP,0._SP)
 complex(SP), parameter :: cZERO=(0._SP,0._SP)
 complex(DP), parameter :: cI_DP   =(0._DP,1._DP)
 complex(DP), parameter :: cONE_DP =(1._DP,0._DP)
 complex(DP), parameter :: cZERO_DP=(0._DP,0._DP)
 !
end module 
