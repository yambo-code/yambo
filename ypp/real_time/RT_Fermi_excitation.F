!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
subroutine RT_Fermi_excitation(E,k)
 !
 use pars,           ONLY:SP
 use units,          ONLY:BO2ANG
 use D_lattice,      ONLY:DL_vol
 use R_lattice,      ONLY:bz_samp
 use stderr,         ONLY:intc
 use electrons,      ONLY:levels,spin_occ
 use com,            ONLY:msg
 use vec_operate,    ONLY:v_is_zero,c2a
 use real_time,      ONLY:RT_carriers,RT_bands,RT_Nk_ibz
 use YPP_real_time,  ONLY:Eh_pumped_pair_energy,Eh_pumped_pair_width,        &
&                         Eh_pumped_pair_BZ_reagions,Eh_pumped_pair_deltaE,  &
&                         Eh_pumped_pair_BZ_width,Eh_pumped_Nel,n_regions,   &
&                         l_RTpump_Fermi
 !
 implicit none
 !
 type(levels),  intent(in) :: E
 type(bz_samp), intent(in) :: k
 type                      :: fermi_energy(2) 
 !
 ! Work Space
 !
 !
 ! I use same variable to define the chemical potential
 ! check spin-occupation
 fermi_energy(1)=Eh_pumped_pair_energy(1)+E%E_VBM
 fermi_energy(2)=Eh_pumped_pair_energy(2)+E%E_CBm
 ! Check if we can correlate a density of carriers with the electron and hole
 ! fermi energy
 ! 
 !
 delta_Nel=0._SP
 RT_carriers%delta_f=0._SP
 !
 do i_RT=1,RT_carriers%nstates
   !
   ib     = RT_carriers%table(i_RT,1)
   ik     = RT_carriers%table(i_RT,2)
   !
   if(E%E(ib,ik,i_sp_pol)<=0._SP) RT_carriers%delta_f(i_RT)=-spin_occ*Fermi_fnc(-(E%E(ib,ik,i_sp_pol)-fermi_energy(1)),T_hole)
   if(E%E(ib,ik,i_sp_pol)> 0._SP) RT_carriers%delta_f(i_RT)= spin_occ*Fermi_fnc( (E%E(ib,ik,i_sp_pol)-fermi_energy(2)),T_elec)
   !
   if(E%E(ib,ik,i_sp_pol)> 0._SP) delta_Nel=delta_Nel+RT_carriers%delta_f(i_RT)*k%weight()
   !
 enddo
 !
 call msg('sr','[RT] Carrier number and density [none,cm-3]:',delta_Nel*(/1._SP,1._SP/(DL_vol*BO2ANG**3*1.E-24_SP)/))
 !
end subroutine RT_Fermi_excitation
