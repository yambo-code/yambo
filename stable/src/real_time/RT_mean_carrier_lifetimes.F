!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team
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
subroutine RT_mean_carrier_lifetimes(G_lesser,k,E)
 ! 
 use pars,           ONLY:SP,cI
 use real_time,      ONLY:E_P_Hole_lifetime,E_P_Electron_lifetime,E_P_Phonon_lifetime,&
&                         E_E_Hole_lifetime,E_E_Electron_lifetime,RT_mean_lifetimes
 use QP_m,           ONLY:QP_nk
 use SC,             ONLY:SC_bands
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,spin_occ
 implicit none
 !
 complex(SP),intent(in) :: G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk)
 type(levels)           :: E
 type(bz_samp)          :: k
 !
 ! Work space
 !
 integer            :: ib,ik,n_states
 real(SP)           :: f_occ
 real(SP),parameter :: f_tresh=1.E-5
 !
 n_states=0
 !
 RT_mean_lifetimes=0._SP
 !
 do ik=1,QP_nk
   do ib=1,SC_bands(2)
     if (ib< SC_bands(1)) then
       f_occ=spin_occ
     else 
       f_occ=-cI*G_lesser(ib,ib,ik)
     endif
     !
     if (abs(f_occ)>f_tresh.and.abs(f_occ-spin_occ)>f_tresh) then
       n_states=n_states+1
       if (allocated(E_P_Hole_lifetime)) then
         RT_mean_lifetimes(1)=RT_mean_lifetimes(1)+E_P_Hole_lifetime(ib,ik)
         RT_mean_lifetimes(2)=RT_mean_lifetimes(2)+E_P_Electron_lifetime(ib,ik)
         RT_mean_lifetimes(3)=RT_mean_lifetimes(3)+E_P_Phonon_lifetime(ib,ik)
       endif
       if (allocated(E_E_Hole_lifetime)) then
         RT_mean_lifetimes(4)=RT_mean_lifetimes(4)+E_E_Hole_lifetime(ib,ik)
         RT_mean_lifetimes(5)=RT_mean_lifetimes(5)+E_E_Electron_lifetime(ib,ik)
       endif
     endif
     !
   enddo
 enddo
 !
 if (n_states>0) RT_mean_lifetimes=RT_mean_lifetimes/float(n_states)
 !
end subroutine RT_mean_carrier_lifetimes
