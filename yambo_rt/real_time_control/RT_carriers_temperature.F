!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DS AM
!
subroutine RT_carriers_temperature(Tfit,EfFit)
 !
 use pars,           ONLY:SP
 use units,          ONLY:HA2KEL,HA2EV
 use drivers,        ONLY:l_elel_scatt,l_elph_scatt
 use RT_occupations, ONLY:RT_el_occ
 !
 implicit none
 !
 real(SP),    intent(inout):: Tfit(2),EfFit(2)
 !
 ! Workspace
 !
 ! Electrons and holes temperatures
 !
 Tfit =0._SP
 EfFit=0._SP
 !
 if ( (l_elel_scatt.or.l_elph_scatt)) then
   !
   call RT_occupations_FIT(Tfit,EfFit,RT_el_occ%N)
   !
   if (abs(Tfit(1))>50000._SP/HA2KEL) then
     Tfit(1)=0._SP
     EfFit(1)=0._SP
   endif
   if (abs(Tfit(2))>50000._SP/HA2KEL) then
     Tfit(2)=0._SP
     EfFit(2)=0._SP
   endif
   !
   EfFit=EfFit*HA2EV
   Tfit =Tfit*HA2KEL
   !
 endif
 !
end subroutine RT_carriers_temperature
