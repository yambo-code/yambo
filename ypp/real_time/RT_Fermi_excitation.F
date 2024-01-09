!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): DS AM FP AMS
!
subroutine RT_Fermi_excitation(E,k,do_kpt,SUCCESS)
 !
 ! What I want is:
 ! [i]   Select arbitrarily chemical potentials for holes and electrons 
 ! [ii]  Put some checks to for the carrier density in conduction and valence
 ! [iii] Put some fictitious temperature for holes and electrons to model the Fermi distribution
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
 logical,       intent(in) :: do_kpt(RT_carriers%nk)
 logical,       intent(out):: SUCCESS
 !
 ! Work Space
 !
 integer  :: i_RT,ib,ik,i_mu
 real(SP) :: mu_elec,mu_hole,delta_Nel,delta_Nho,mu_step
 !
 RT_carriers%OCC_el%dN=0._SP
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
     else
       if(i_mu>0) mu_step=mu_step/10._SP
       mu_hole=mu_hole+mu_step
       i_mu=-abs(i_mu)-1
     endif ! increase or reduce h_mu
     !
     call msg('sr','[RT] Autotuning of mu_hole - step ',abs(i_mu))
     call msg('sr','[RT] Chemical potentials (holes, electrons)         ', HA2EV*(/mu_hole-E%E_VBM(1),Eh_mu(2)/),"[eV]")
     call msg('sr','[RT] Pumped holes     number and density            ',&
&                  delta_Nho*(/1._SP,1._SP/(DL_vol*BO2ANG**3*1.E-24_SP)/),'[none,cm-3]')
     call msg('sr','[RT] Pumped electrons number and density            ',&
&                  delta_Nel*(/1._SP,1._SP/(DL_vol*BO2ANG**3*1.E-24_SP)/),'[none,cm-3]')
   enddo ! autotune loop
   !
 else  ! independent e_mu and h_mu   
   delta_Nel=0._SP
   delta_Nho=0._SP
   !
   call pump_Fermi()
   !
   call msg('sr','[RT] Pumped holes     number and density            ',&
&                  delta_Nho*(/1._SP,1._SP/(DL_vol*BO2ANG**3*1.E-24_SP)/),'[none,cm-3]')
   call msg('sr','[RT] Pumped electrons number and density            ',&
&                  delta_Nel*(/1._SP,1._SP/(DL_vol*BO2ANG**3*1.E-24_SP)/),'[none,cm-3]')
 endif ! autotune or not
 !
 SUCCESS=.TRUE.
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
       if(.not.do_kpt(ik)) cycle
       !
       if(E%E(ib,ik,1)<=E%E_VBM(1)) then
         RT_carriers%OCC_el%dN(i_RT)=-spin_occ*Fermi_fnc(-(E%E(ib,ik,1)-mu_hole),Eh_temp(1))
         delta_Nho=delta_Nho+RT_carriers%OCC_el%dN(i_RT)*k%weights(ik)
       endif
       !
       if(E%E(ib,ik,1)>=E%E_CBm(1)) then !.and. i_mu==0) then
         RT_carriers%OCC_el%dN(i_RT)= spin_occ*Fermi_fnc( (E%E(ib,ik,1)-mu_elec),Eh_temp(2)) !k%weights is here non in Delta_Nel
         delta_Nel=delta_Nel+RT_carriers%OCC_el%dN(i_RT)*k%weights(ik)
       endif
       !
     enddo
     !
   end subroutine
   !
end subroutine RT_Fermi_excitation
