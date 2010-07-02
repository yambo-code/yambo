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
 use pars,           ONLY:SP,pi
 use units,          ONLY:SPEED_OF_LIGHT,FS2AUT,HARTREE
 use com,            ONLY:msg,warning
 use vec_operate,    ONLY:normalize_v
 use electrons,      ONLY:nel
 use real_time,      ONLY:l_NE_simulate_RPA,Thermal_steps,RT_step,l_NE_with_fields,i_time, &
&                         l_NE_simulate_IP,l_RT_diagonalize,NE_steps,    &
&                         l_RT_no_diamagnetic,max_rt_steps_disk,     &
&                         l_NE_simulate_Irredux,l_RT_needs_XC0,Hfrozen,XCfrozen,         &
&                         l_RT_induced_field,j_current_ext,Ext_fields_initial_time,RT_diagonalization_steps,&
&                         RT_is_dumped,QP_LifeTime,Phase_LifeTime
 use D_lattice,      ONLY:DL_vol
 use fields,         ONLY:global_gauge,Efield,n_ext_fields
 use parser_m,       ONLY:parser
 implicit none
 !
 ! Work space
 !
 integer                  :: i1,rest
 !
 ! Logicals
 !----------
 call parser('NoDiamagnetic',l_RT_no_diamagnetic)
 call parser('InducedField' ,l_RT_induced_field)
 call parser('RTfreezeH',    Hfrozen)
 call parser('RTfreezeXC',   XCfrozen)
 l_RT_diagonalize=RT_diagonalization_steps>1
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
   elseif (NE_steps==0.and.Thermal_steps/=0) then
     Thermal_steps = Thermal_steps+max_rt_steps_disk-rest
   endif
 endif
 !
 ! QP and Phase Lifetimes
 !
 RT_is_dumped=any((/QP_LifeTime,Phase_LifeTime/)>1.E-8_SP)
 !
 ! Earliest time when the external field is switched on
 !
 Ext_fields_initial_time=1.E10
 !
 l_NE_with_fields=.false.
 !
 do i1=1,n_ext_fields
   !      
   Efield(i1)%ef_name=trim(Efield(i1)%ef_name)      
   !
   if((Efield(i1)%ef_name/='none').and.Efield(i1)%intensity/=0.) then
     !      
     Ext_fields_initial_time=min(Efield(i1)%t_initial,Ext_fields_initial_time)
     !      
     l_NE_with_fields=.true.
     !
     Efield(i1)%versor         =normalize_v(Efield(i1)%versor)
     !
     Efield(i1)%int_t_initial  =int(Efield(i1)%t_initial/RT_step)+1
     !
     Efield(i1)%amplitude=sqrt(Efield(i1)%intensity*(8._SP*pi)/SPEED_OF_LIGHT) 
     !
     call SC_check_symmetries(Efield(i1)%versor)
     !
     if(Efield(i1)%ef_name/='DELTA'.and.(Efield(i1)%frequency==0._SP.and.Efield(i1)%damping==0)) &
&      call warning(" External field frequency and damping are zero! ")
     !
   endif
   !
 enddo
 !
 if (.not.l_NE_with_fields) then
   NE_steps=0
   if (Thermal_steps==0) call warning('Skipping simulation. No Ext fields or thermalization provided')
 endif
 !
 call msg( 'r', '[RT] NE Steps                   :',NE_steps)
 call msg( 'r', '[RT] Thermalization steps       :',Thermal_steps)
 call msg( 'r', '[RT] Step length            [fs]:',RT_step/FS2AUT)
 if(NE_steps>0) then
   call msg('sr', '[RT] Response energy window        [eV]:',&
&                  (/2.*pi/(RT_step*real(NE_steps)-Ext_fields_initial_time),pi/RT_step/)*HARTREE)
   call msg('rsn','[RT] Electron-gas plasmon frequency[eV]:',sqrt(4._SP*pi*nel/DL_vol)*HARTREE)
 endif
 call msg('rsn','[RT] Simulation time        [fs]:',(/-real(Thermal_steps)*RT_step,RT_step*real(NE_steps)/)/FS2AUT)
 !
 if(l_NE_with_fields) &
&   call msg('rsn','[RT] Gauge Type                 :',trim(global_gauge))
 !
 if(l_RT_induced_field.and.global_gauge/='velocity') then
   call warning(" Induced field not included in Maxwell when the gauge is not 'velocity'")
   l_RT_induced_field=.false.
 endif
 !
 if(.not.l_RT_no_diamagnetic.and.global_gauge/='velocity') then
   call warning(" Diamagnetic term not present when the gauge is not 'velocity'")
   l_RT_no_diamagnetic=.true.
 endif
 !
end subroutine RT_initialize
