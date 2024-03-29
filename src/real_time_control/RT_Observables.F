!
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA DS
!
subroutine RT_Observables(en,k,dG_lesser,OBSERVABLES)
 !
 ! This subroutine compute the variation of the energy and its different components,
 ! the number of electrons injected in conduction and holes in valence,
 ! and finally the variation in entropy and the associated temperature
 ! which is compared with the temperature from the fits
 !
 use pars,           ONLY:SP
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use real_time,      ONLY:RT_nk,RT_bands
 use fields,         ONLY:A_tot
 use timing_m,       ONLY:timing
 !
 implicit none 
 !
 type(levels),  intent(inout) :: en
 type(bz_samp), intent(in)    :: k
 complex(SP),   intent(in)    :: dG_lesser(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 real(SP),      intent(inout) :: OBSERVABLES(25)
 !
 ! Workspace
 !
 real(SP) :: NCarr(3),TFit(2),EfFit(2),Ener(5),Entr(3),TEntr(3),EnEntr(3),E_IP
 logical, external :: RT_do_it
 !
 if ( .not. ( RT_do_it('OUT') .or. RT_do_it('OBS_cache') .or. RT_do_it('deltaT') ) ) return
 !
 call timing('RT observables',OPR='start')
 !
 ! Current, Polarization and Magnetization
 !=========================================
 ! (evaluated also in RT_integrator when an induced field is included, l_RT_induced_field=.true.)
 !
 call RT_carriers_number(en,k,NCarr)
 call RT_current_AND_polarization(k,en,dG_lesser,A_tot,Ncarr)
 !
 if (.not. RT_do_it('OUT') ) then
   call timing('RT observables',OPR='stop')
   return
 endif
 !
 ! Carriers number
 !================
 call RT_carriers_number(en,k,NCarr)
 !
 ! Temperature
 !=============
 call RT_carriers_temperature(TFit,EfFit)
 !
 ! Energy
 !========
 call RT_energy_components(en,k,dG_lesser,Ener,E_IP,'tot')
 !
 ! Entropy
 !=========
 call RT_entropy(en,k,dG_lesser,Entr,TEntr,EnEntr,E_IP)
 !
 OBSERVABLES(1:21)=(/NCarr(:),TFit(:),EfFit(:),Ener(:),Entr(:),TEntr(:),EnEntr(:)/)
 !
 call timing('RT observables',OPR='stop')
 !
end subroutine RT_Observables
