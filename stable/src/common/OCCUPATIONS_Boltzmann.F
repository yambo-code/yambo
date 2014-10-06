!
! Copyright (C) 2000-2013 D. Sangalli and the YAMBO team 
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
subroutine OCCUPATIONS_Boltzmann(E,k,ik)
 !
 ! warning: to be fixed action of trev or spin_sop on rotated_v
 !
 use pars,          ONLY:SP
 use electrons,     ONLY:levels,n_sp_pol,spin_occ,Boltz_Efield,Boltz_tau,Boltz_f,Boltz_vel
 use R_lattice,     ONLY:bz_samp,nkibz,rl_sop
 use D_lattice,     ONLY:Tel,nsym
 use functions,     ONLY:Fermi_fnc_derivative
 !
 implicit none
 !
 type(levels) ::E
 type(bz_samp)::k
 integer      ::ik
 !
 ! Work space
 real(SP)            :: v_dot_E,rotated_v(3),f_occ
 integer             :: i_spin,ib,is,i1,ikbz
 !
 if (.not.associated(Boltz_f)) then
   allocate(Boltz_f(E%nbm,nkibz,nsym,n_sp_pol))
   Boltz_f=0.
 endif
 !
 do ib=E%nbf+1,E%nbm
   do i_spin=1,n_sp_pol
     do i1=1,k%nstar(ik)
        !
        is=k%star(ik,i1)
        rotated_v=matmul(rl_sop(:,:,is), Boltz_vel(:,ib,ik,i_spin) )
        v_dot_E= dot_product( rotated_v, Boltz_Efield ) 
        f_occ=E%f(ib,ik,i_spin)-spin_occ*Fermi_fnc_derivative(E%E(ib,ik,i_spin),Tel)*v_dot_E*Boltz_tau
        !
        if(f_occ < 0. )       f_occ=0.
        if(f_occ > spin_occ ) f_occ=spin_occ
        !
        Boltz_f(ib,ik,is,i_spin)=f_occ
        !
      enddo
      !
   enddo
 enddo
 !
end subroutine
