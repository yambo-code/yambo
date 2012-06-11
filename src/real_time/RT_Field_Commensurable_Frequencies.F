!
!         Copyright (C) 2000-2012 the YAMBO team 
!              http://www.yambo-code.org
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
subroutine RT_Field_Commensurable_Frequencies()
 !
 ! Check on the PROBE/PUMP frequencies. To be possible to find, in ypp_rt, the coefficients of the 
 ! Fourier series I need ALL the frequencies to be Wn=n deltaW. Therefore frequency(1) and
 ! frequency(2) must be choosed accordingly.
 ! 
 ! Moreover to extract the Fourier coefficients I need to integrate over aa Time range
 ! equal to 2*pi/deltaW. This must be not larger then NE_steps*RT_step 
 !
 use pars,           ONLY:SP
 use fields,         ONLY:Efield,n_ext_fields,i_Pump,i_Probe,EtoT
 use real_time,      ONLY:NE_steps,RT_step
 !
 implicit none
 !
 integer  :: i_master_field,i_f
 real(SP) :: W_step,T_treshold,T_step
 real(SP), parameter :: treshold=0.8
 !
 if (.not.any ((/trim(Efield(i_Pump)%ef_name)=='ANTIRES',&
&               trim(Efield(i_Probe)%ef_name)=='ANTIRES',&
&               trim(Efield(i_Probe)%ef_name)=='SIN'/))) return
 !
 i_master_field=0
 if ( abs( Efield(i_Pump)%frequency(2) -Efield(i_Pump)%frequency(1) ) > epsilon(1._SP)  )  i_master_field=i_Pump
 if ( abs( Efield(i_Probe)%frequency(2)-Efield(i_Probe)%frequency(1))  >&
&     abs( Efield(i_Pump)%frequency(2) -Efield(i_Pump)%frequency(1) ) ) i_master_field=i_Probe
 !
 if (i_master_field==0) return
 !
 W_step=Efield(i_master_field)%W_step
 !
 T_step=EtoT(E=W_step)
 T_treshold=treshold*NE_steps*RT_step
 if (T_step > treshold*NE_steps*RT_step) then
   T_step=T_treshold
   W_step=EtoT(T=T_step)
 endif
 !
 Efield(i_master_field)%W_step=W_step
 !
 do i_f=1,n_ext_fields
   !
   Efield(i_f)%frequency(1)=nint(Efield(i_f)%frequency(1)/W_step)*W_step
   Efield(i_f)%frequency(2)=nint(Efield(i_f)%frequency(2)/W_step)*W_step
   !
   if ( abs(Efield(i_f)%frequency(1)) < 1.E-5 ) Efield(i_f)%frequency(1)=W_step
   !
   Efield(i_f)%n_frequencies=nint((Efield(i_f)%frequency(2)-Efield(i_f)%frequency(1))/W_step)+1
   !
   if ( Efield(i_f)%n_frequencies == 1) Efield(i_f)%W_step=0.
   !
 enddo 
 !
end subroutine RT_Field_Commensurable_Frequencies
