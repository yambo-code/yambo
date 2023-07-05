!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine RT_Fluence_and_PI_kind(En,i_field)
 !
 use pars,           ONLY:SP,pi
 use units,          ONLY:PS2AUT
 use real_time,      ONLY:RT_step,NE_steps,NE_i_time
 use fields,         ONLY:Efield,gauge_field
 use electrons,      ONLY:levels
 use parser_m,       ONLY:parser
 !
 implicit none
 !
 type(levels), intent(in) :: En
 integer, intent(in)      :: i_field
 ! 
 ! Work Space
 !
 type(gauge_field) :: A
 !
 logical      :: l_print_rabi
 integer      :: it0,it,last_it_evaluated,NE_i_time_save
 real(SP)     :: I(NE_steps),Omega(NE_steps),T(NE_steps)
 !
 real(SP),external :: RIntegrate,RT_Rabi_frequency
 !
 call parser("RabiAnalysis",l_print_rabi)
 !
 ! E(t)=-d A(t)/dt / SPEED_OF_LIGHT
 !
 ! From RT_Ext_fields we see that
 !
 ! A'(t)=-Efield(i_field)%amplitude*Efield(i_field)%versor(:)*SPEED_OF_LIGHT * (small_a' theta + small_a theta')
 !
 ! Thus
 ! 
 ! |E(t)|^2= Efield(i_field)%amplitude**2 * | small_a' theta + small_a theta' |^2
 !
 if ( Efield(i_field)%intensity<1.E-15_SP ) then
   Efield(i_field)%t_final=Efield(i_field)%t_initial
   Efield(i_field)%t_final_indx=Efield(i_field)%t_initial_indx
   Efield(i_field)%fluence=0._SP
   Efield(i_field)%pi_kind=0._SP
   return
 endif
 !
 I=0._SP
 T=0._SP
 last_it_evaluated=0
 NE_i_time_save=NE_i_time
 !
 Efield(i_field)%fluence=0._SP
 Efield(i_field)%pi_kind=0._SP
 !
 it0=Efield(i_field)%t_initial_indx
 !
 do it=it0,NE_steps
   !
   T(it)=it*RT_step
   !
   if ((it-it0)>6*nint(Efield(i_field)%width/RT_step)) exit
   !
   NE_i_time=it
   !
   call RT_Ext_fields(A,T(it),i_field)
   !
   I(it)    =A%intensity
   !
   Omega(it)=A%envelop
   !
   last_it_evaluated=it
   !
 enddo
 !
 NE_i_time=NE_i_time_save
 !
 ! Omega(t)
 ! I divide for |E| which is already accounted for in Raby frequency
 ! Rabi_frequency is in THz  
 !
 Omega(:)=Omega(:)*RT_Rabi_frequency(En,i_field,l_print_rabi)/Efield(i_field)%amplitude
 ! 
 ! To get the fluence in kJ/cm^2 I should translate T in sec.
 ! Then I would have a factor 10^-12 ( 1 nJ/cm^2 = 10^-12 kJ/cm^2 )
 ! thus I ruther convert time in ps
 !
 T(:)=T(:)/real(PS2AUT,SP)      ! au -> ps
 !
 Efield(i_field)%fluence=RIntegrate(I(it0:last_it_evaluated),T(it0:last_it_evaluated),last_it_evaluated-it0+1)
 !
 Efield(i_field)%pi_kind=RIntegrate(Omega(it0:last_it_evaluated),T(it0:last_it_evaluated),last_it_evaluated-it0+1)
 !
 Efield(i_field)%t_final_indx=last_it_evaluated
 Efield(i_field)%t_final=(Efield(i_field)%t_final_indx-1)*RT_step
 !
 if (l_print_rabi) call RT_Rabi_frequency_ALL_and_print(En,i_field)
 !
end subroutine RT_Fluence_and_PI_kind
