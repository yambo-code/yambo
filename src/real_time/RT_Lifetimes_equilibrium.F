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
 use real_time,      ONLY:REF_lifetime
 use electrons,      ONLY:n_full_bands
 !
 implicit none
 !
 integer         :: i_n,i_k
 real(SP)        :: Time
 !
 ! Work Space
 !
 integer         :: i1
 real(SP)        :: Gamma_ratio,E,f_ref
 real(SP)        :: Life_H,Life_E
 !
 Life_H=0._SP
 Life_E=0._SP
 do i1=1,7,2
   Life_H=Life_H+REF_lifetime(i_n,i_k,i1)
   Life_E=Life_E+REF_lifetime(i_n,i_k,i1+1)
 enddo
 !
 Gamma_ratio=Life_H/(Life_E+Life_H)
 !
 f_ref=0._SP
 if (i_n<=n_full_bands) f_ref=1._SP
 !
 RT_Lifetimes_equilibrium=(f_ref-Gamma_ratio)*exp(-(Life_H+Life_E)*Time)+Gamma_ratio
 !
end function RT_Lifetimes_equilibrium
