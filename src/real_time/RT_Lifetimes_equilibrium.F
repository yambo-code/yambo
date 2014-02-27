!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
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
real(SP) function RT_Lifetimes_equilibrium(i_n,i_k,Time)
 !
 use pars,           ONLY:SP
 use real_time,      ONLY:REF_e_lifetime,REF_h_lifetime
 use electrons,      ONLY:n_full_bands
 !
 implicit none
 !
 integer         :: i_n,i_k
 real(SP)        :: Time
 !
 ! Work Space
 !
 real(SP)        :: Gamma_ratio,E,f_ref
 !
 Gamma_ratio=REF_e_lifetime(i_n,i_k)/(REF_e_lifetime(i_n,i_k)+REF_h_lifetime(i_n,i_k))
 !
 f_ref=0._SP
 if (i_n<=n_full_bands) f_ref=1._SP
 !
 RT_Lifetimes_equilibrium=(Gamma_ratio-f_ref)*exp(-(REF_e_lifetime(i_n,i_k)+REF_h_lifetime(i_n,i_k))*Time)+Gamma_ratio
 !
end function RT_Lifetimes_equilibrium
