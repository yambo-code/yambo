!
!        Copyright (C) 2000-2023 the YAMBO team
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
