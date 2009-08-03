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
 use pars,           ONLY:SP,lchlen,schlen,pi
 use units,          ONLY:SPEED_OF_LIGHT
 use com,            ONLY:msg,warning
 use vec_operate,    ONLY:normalize_v
 use drivers,        ONLY:l_sc_hf
 use QP_m,           ONLY:QP_Sx_low_band
 use real_time,      ONLY:l_RPA,Thermal_steps,Thermal_time,RT_step,l_rt_efield,i_time, &
&                         l_RT_no_updates,Integrator_Name,l_qp_analyze,Tot_steps,    &
&                         l_no_diamagnetic,l_always_relax,EField,n_ext_fields,         &
&                         max_rt_steps_disk
 use parser_m,       ONLY:parser
 use stderr,         ONLY:intc
 implicit none
 !
 ! Work space
 !
 character(schlen)        :: headings(10),hash_ch,where_ch
 integer                  :: i1,i2,rest
 !
 ! Logicals
 !----------
 !
 call parser('EFNoIntraband1',EField(1)%no_intraband)
 call parser('EFNoIntraband2',EField(2)%no_intraband)
 call parser('EFNoDiamagnetic1',EField(1)%no_diamagnetic)
 call parser('EFNoDiamagnetic2',EField(2)%no_diamagnetic)
 !
 call parser('RTQpAnalyze',l_qp_analyze)
 call parser('KeepVxc',l_RPA)
 call parser('NoDiamagnetic',l_no_diamagnetic)
 call parser('AlwaysRelax',l_always_relax)
 !
 i_time=0
 !
 if(l_sc_hf.and.QP_Sx_low_band/=1) then
   QP_Sx_low_band=1
   call warning('  Exchange summation low band set to 1 ')
 endif
 !
 ! Set a number of steps compatible with the cache 
 !
 if(mod(Thermal_steps+Tot_steps,max_rt_steps_disk)/=0) then
   rest=mod(Thermal_steps+Tot_steps,max_rt_steps_disk)
   if(Tot_steps/=0) then
     Tot_steps = Tot_steps+max_rt_steps_disk-rest
     call msg('snr','[RT] Number of steps set to                 :',Tot_steps)
   elseif(Tot_steps==0.and.Thermal_steps/=0) then
     Thermal_steps = Thermal_steps+max_rt_steps_disk-rest
     call msg('snr','[RT] Number of thermalization steps set to  :',Thermal_steps)
   endif
 endif
 !
 if(Thermal_steps/=0) then
   !
   Thermal_time=real(Thermal_steps)*RT_step
   !
 else
   !      
   Thermal_time=0._SP
   !
 endif
 !
 l_rt_efield=.false.
 !
 do i1=1,n_ext_fields
   !      
   Efield(i1)%ef_name=trim(Efield(i1)%ef_name)      
   !
   if((Efield(i1)%ef_name/='none'.and.EField(i1)%intensity/=0.)) then
     !      
     l_rt_efield=.true.
     !
     EField(i1)%versor   =normalize_v(Efield(i1)%versor)
     !
     EField(i1)%amplitude=sqrt( Efield(i1)%intensity*(8.0*pi)/SPEED_OF_LIGHT ) 
     !
     call SC_check_symmetries(Efield(i1)%versor)
     !
   endif
   !
 enddo
 !
end subroutine RT_initialize
