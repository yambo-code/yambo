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
subroutine RT_initialize()
 ! 
 use pars,           ONLY:SP,schlen,pi
 use units,          ONLY:SPEED_OF_LIGHT
 use com,            ONLY:msg
 use vec_operate,    ONLY:normalize_v
 use real_time,      ONLY:l_NE_simulate_RPA,Thermal_steps,RT_step,l_NE_with_fields,i_time, &
&                         l_NE_simulate_IP,l_RT_diagonalize,NE_steps,    &
&                         l_RT_no_diamagnetic,EField,n_ext_fields,max_rt_steps_disk,     &
&                         l_NE_simulate_Irredux,l_RT_needs_XC0,Hfrozen,XCfrozen,         &
&                         l_RT_induced_field,j_current_ext
 use parser_m,       ONLY:parser
 implicit none
 !
 ! Work space
 !
 integer                  :: i1,rest
 !
 ! Logicals
 !----------
 call parser('RTDiagonalize',l_RT_diagonalize)
 call parser('NoDiamagnetic',l_RT_no_diamagnetic)
 call parser('InducedField' ,l_RT_induced_field)
 call parser('RTfreezeH',    Hfrozen)
 call parser('RTfreezeXC',   XCfrozen)
 l_NE_simulate_RPA=.not.Hfrozen.and.XCfrozen
 l_NE_simulate_IP =     Hfrozen.and.XCfrozen
 l_NE_simulate_Irredux =Hfrozen.and..not.XCfrozen
 !
 l_RT_needs_XC0=l_NE_simulate_IP.or.l_NE_simulate_RPA.or.Thermal_steps>0
 !
 j_current_ext=(0._SP,0._SP)
 i_time=0
 !
 ! Set a number of steps compatible with the cache 
 !
 if(mod(Thermal_steps+NE_steps,max_rt_steps_disk)/=0) then
   rest=mod(Thermal_steps+NE_steps,max_rt_steps_disk)
   if(NE_steps/=0) then
     NE_steps = NE_steps+max_rt_steps_disk-rest
     call msg('snr','[RT] Number of steps set to                 :',NE_steps)
   elseif(NE_steps==0.and.Thermal_steps/=0) then
     Thermal_steps = Thermal_steps+max_rt_steps_disk-rest
     call msg('snr','[RT] Number of thermalization steps set to  :',Thermal_steps)
   endif
 endif
 !
 l_NE_with_fields=.false.
 !
 do i1=1,n_ext_fields
   !      
   Efield(i1)%ef_name=trim(Efield(i1)%ef_name)      
   !
   if((Efield(i1)%ef_name/='none').and.EField(i1)%intensity/=0.) then
     !      
     l_NE_with_fields=.true.
     !
     EField(i1)%versor         =normalize_v(Efield(i1)%versor)
     !
     EField(i1)%int_t_initial  =int(Efield(i1)%t_initial/RT_step)+1
     !
     EField(i1)%amplitude=sqrt(Efield(i1)%intensity*(8._SP*pi)/SPEED_OF_LIGHT) 
     !
     call SC_check_symmetries(Efield(i1)%versor)
     !
   endif
   !
 enddo
 !
end subroutine RT_initialize
