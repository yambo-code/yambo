!
! Copyright (C) 2000-2010 D. Sangalli and the YAMBO team 
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
subroutine Boltzmann_occupations(E,K,vel_per_state)
 !
 use pars,          ONLY:SP
 use electrons,     ONLY:levels,n_sp_pol,spin_occ,Boltz_Efield,Boltz_tau
 use R_lattice,     ONLY:bz_samp,k_pt
 use D_lattice,     ONLY:Tel
 use functions,     ONLY:Fermi_fnc_derivative
 !
 implicit none
 !
 type(levels) ::E
 type(bz_samp)::K
 !
 real(SP)     :: vel_per_state(3,E%nb,E%nk,n_sp_pol)
 !
 ! Work space
 real(SP)            :: v_dot_E
 integer             :: is,ib,idir,ik
 !
 do ib=E%nbf+1,E%nbm
   do is=1,n_sp_pol
     do ik=1,K%nibz
       !
       v_dot_E=0.
       do idir=1,3
         v_dot_E=v_dot_E+vel_per_state(idir,ib,ik,is)*Boltz_Efield(idir)
       enddo
       !
       E%f(ib,ik,is)=E%f(ib,ik,is)-spin_occ*Fermi_fnc_derivative(E%E(ib,ik,is),Tel) &
&                     *v_dot_E*Boltz_tau
       if(E%f(ib,ik,is) < 0. )       E%f(ib,ik,is)=0.
       if(E%f(ib,ik,is) > spin_occ ) E%f(ib,ik,is)=spin_occ
       !
     enddo
   enddo
 enddo
 !
end subroutine
