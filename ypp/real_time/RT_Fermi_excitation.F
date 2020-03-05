!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM FP AMS
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
 use units,          ONLY:BO2ANG,HA2EV
 use D_lattice,      ONLY:DL_vol
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,spin_occ
 use com,            ONLY:msg
 use functions,      ONLY:Fermi_fnc
 use real_time,      ONLY:RT_carriers
 use YPP_real_time,  ONLY:Eh_mu,Eh_temp,h_mu_autotune_thr
 !
 implicit none
 !
 type(levels),  intent(in) :: E
 type(bz_samp), intent(in) :: k
 integer  :: i_RT,ib,ik,i_mu
 real(SP) :: mu_elec,mu_hole,delta_Nel,delta_Nho,mu_step
 !
 ! Work Space
 !
 ! What I want is:
 ! [i]   Select arbitrarily chemical potentials for holes and electrons 
 ! [ii]  Put some checks to for the carrier density in conduction and valence
 ! [iii] Put some fictitious temperature for holes and electrons to model the Fermi distribution
 !
 RT_carriers%delta_f=0._SP
 mu_hole=Eh_mu(1)+E%E_VBM(1) ! Negative wrt VBM
 mu_elec=Eh_mu(2)+E%E_CBm(1)
 !
 if(h_mu_autotune_thr > 0._SP) then ! h_mu autotune loop at fixed e_mu
   delta_Nho=-1000._SP ! Fills up with negative sign
   delta_Nel=0._SP
   mu_step=0.01_SP/HA2EV
   i_mu=0
   do while (abs(-delta_Nho-delta_Nel)>h_mu_autotune_thr)
     delta_Nho = 0._SP
     delta_Nel = 0._SP
     !
     call pump_Fermi()
     !
     if(abs(Delta_Nho)<=Delta_Nel) then
       if(i_mu<0) mu_step=mu_step/10._SP
       mu_hole=mu_hole-mu_step
       i_mu=abs(i_mu)+1
     !
     else
       if(i_mu>0) mu_step=mu_step/10._SP
       mu_hole=mu_hole+mu_step
       i_mu=-abs(i_mu)-1
     endif ! increase or reduce h_mu
     !
     call msg('sr','[RT] Autotuning of mu_hole - step ',abs(i_mu))
     call msg('sr','[RT] Chemical potentials (holes, electrons)     [eV]', HA2EV*(/mu_hole-E%E_VBM(1),Eh_mu(2)/))
     call msg('sr','[RT] Pumped holes     number and density [none,cm-3]',delta_Nho*(/1._SP,1._SP/(DL_vol*BO2ANG**3*1.E-24_SP)/))
     call msg('sr','[RT] Pumped electrons number and density [none,cm-3]',delta_Nel*(/1._SP,1._SP/(DL_vol*BO2ANG**3*1.E-24_SP)/))
   enddo ! autotune loop
   !
 else  ! independent e_mu and h_mu   
   delta_Nel=0._SP
   delta_Nho=0._SP
   !
   call pump_Fermi()
   !
   call msg('sr','[RT] Pumped holes     number and density [none,cm-3]',delta_Nho*(/1._SP,1._SP/(DL_vol*BO2ANG**3*1.E-24_SP)/))
   call msg('sr','[RT] Pumped electrons number and density [none,cm-3]',delta_Nel*(/1._SP,1._SP/(DL_vol*BO2ANG**3*1.E-24_SP)/))
 endif ! autotune or not
 ! 
 contains
   !
   subroutine pump_Fermi()
     !  
     do i_RT=1,RT_carriers%nstates
       !
       ib = RT_carriers%table(i_RT,1)
       ik = RT_carriers%table(i_RT,2)
       !
       if(E%E(ib,ik,1)<=E%E_VBM(1)) then
         RT_carriers%delta_f(i_RT)=-spin_occ*Fermi_fnc(-(E%E(ib,ik,1)-mu_hole),Eh_temp(1))
         delta_Nho=delta_Nho+RT_carriers%delta_f(i_RT)*k%weights(ik)
       endif
       !
       if(E%E(ib,ik,1)>=E%E_CBm(1)) then !.and. i_mu==0) then
         RT_carriers%delta_f(i_RT)= spin_occ*Fermi_fnc( (E%E(ib,ik,1)-mu_elec),Eh_temp(2)) !k%weights is here non in Delta_Nel
         delta_Nel=delta_Nel+RT_carriers%delta_f(i_RT)*k%weights(ik)
       endif
       !
     enddo
   end subroutine
   !
end subroutine RT_Fermi_excitation
