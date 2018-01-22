!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine RT_carriers_number_and_temperature(en,k,Ncarr,Tfit,EfFit)
 !
 use pars,           ONLY:SP
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use units,          ONLY:HA2KEL,HA2EV
 use RT_control,     ONLY:NE_i_start_time
 use drivers,        ONLY:l_elel_scatt,l_elph_scatt
 use real_time,      ONLY:RT_ibz_coo,DbGd_ibz_coo,l_RT_uses_E_DbGd,RT_Nk_ibz,NE_i_time,       &
&                         RT_E_occupations,RT_H_occupations,RT_bands,RT_delta0_occupations
 !
 implicit none
 !
 type(levels),  intent(in) :: en
 type(bz_samp), intent(in) :: k
 real(SP),      intent(out):: Ncarr(3)
 real(SP),    intent(inout):: Tfit(2),EfFit(2)
 !
 ! Workspace
 !
 integer     :: ib,ik,ik_RT,ik_E
 real(SP)    :: tmp_E,VALUE_tmp(2)
 !
 ! Electrons and holes temperatures
 !
 Tfit=0._SP
 !
 if ( (l_elel_scatt.or.l_elph_scatt)) then
   !
   call RT_occupations_FIT(Tfit,EfFit,RT_E_occupations)
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
 ! Electrons and holes number
 !
 Ncarr = 0._SP
 !
 do ik=1,en%nk
   do ib=RT_bands(1),RT_bands(2)
     VALUE_tmp=0._SP
     do ik_RT=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       if (l_RT_uses_E_DbGd) then
         ik_E=ik_RT-RT_ibz_coo(ik,1)+DbGd_ibz_coo(ik,1)
         tmp_E=en%E_DbGd(ib,ik_E,1)
       else
         tmp_E=en%E(ib,ik,1)
       endif
       if ( tmp_E<=0._SP ) then
         if(NE_i_time==NE_i_start_time) VALUE_tmp(1)=VALUE_tmp(1)+RT_H_occupations(ib,ik_RT)
         if(NE_i_time/=NE_i_start_time) VALUE_tmp(1)=VALUE_tmp(1)-RT_delta0_occupations(ib,ik_RT)
       else
         if(NE_i_time==NE_i_start_time) VALUE_tmp(2)=VALUE_tmp(2)+RT_E_occupations(ib,ik_RT)
         if(NE_i_time/=NE_i_start_time) VALUE_tmp(2)=VALUE_tmp(2)+RT_delta0_occupations(ib,ik_RT)
       endif
     enddo
     Ncarr(2:3)=Ncarr(2:3)+VALUE_tmp*k%weights(ik)/real(RT_Nk_ibz(ik),SP)
   enddo
 enddo
 !
 Ncarr(1)=Ncarr(2)-Ncarr(3)
 !
end subroutine RT_carriers_number_and_temperature
