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
subroutine RT_initialize(file_name)
 ! 
 use pars,           ONLY:SP,lchlen,schlen
 use vec_operate,    ONLY:normalize_v
 use drivers,        ONLY:l_sc_hf
 use QP_m,           ONLY:QP_Sx_low_band
 use com,            ONLY:msg,of_open_close,warning
 use real_time,      ONLY:l_RPA,Thermal_steps,Thermal_time,RT_step,l_rt_efield,i_time, &
&                         l_RT_no_updates,Integrator_Name,l_qp_analyze,          &
&                         l_no_diamagnetic,l_always_relax,EField,n_ext_fields
 use electrons,      ONLY:levels
 use parser_m,       ONLY:parser
 use stderr,         ONLY:intc
 implicit none
 !
 character(lchlen)        :: file_name(2)
 !
 ! Work space
 !
 character(schlen)        :: headings(10),hash_ch,where_ch
 integer                  :: i1,i2
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
     EField(i1)%versor=normalize_v(Efield(i1)%versor)
     !
     call SC_check_symmetries(Efield(i1)%versor)
     !
   endif
   !
 enddo
 !
 ! OUTPUT Files
 !--------------
 !
 file_name(1)='RT-dynamics'
 call of_open_close(file_name(1),'ot') 
 headings(1)="Time"
 headings(2)="E_kin"
 headings(3)="E_ion"
 if(.not.l_RT_no_updates) then
   !      
   if(l_sc_hf) then
     headings(4)="E_Hartree"
     headings(5)="E_x"
   else
     headings(4)="E_Hartree"
     headings(5)="E_xc"
   endif
   !
 else
   headings(4)="Eo_pot"
   headings(5)="none"
 endif
 headings(6)="N" 
 !
 file_name(2)="RT-qp"
 if(l_qp_analyze) call of_open_close(file_name(2),'ot')
 !
 do i1=1,2
   !
   where_ch='r'
   hash_ch ='[RT]'
   if (i1==2) where_ch='o '//file_name(1)//' '//file_name(2)
   if (i1==2) hash_ch ='# [RT]'
   !
   call msg(trim(where_ch),trim(hash_ch)//' Integrator            :',trim(Integrator_Name),INDENT=0)
   call msg(trim(where_ch),trim(hash_ch)//' RPA                   :',l_RPA,INDENT=0)
   call msg(trim(where_ch),trim(hash_ch)//' Diamagnetic term      :',.not.l_no_diamagnetic,INDENT=0)
   call msg(trim(where_ch),trim(hash_ch)//' Analyze QP-dynamics   :',l_qp_analyze,INDENT=0)
   !
   do i2=1,n_ext_fields
     call msg(trim(where_ch),trim(hash_ch))
     call msg(trim(where_ch),trim(hash_ch)//' External Field('//trim(intc(i2))//&
&                                           ')      :',trim(EField(i2)%ef_name),INDENT=0)
     call msg(trim(where_ch),trim(hash_ch)//' Intensity                  :',EField(i2)%intensity,INDENT=0)
     call msg(trim(where_ch),trim(hash_ch)//' Frequancy                  :',EField(i2)%frequency,INDENT=0)
     call msg(trim(where_ch),trim(hash_ch)//' Direction                  :',EField(i2)%versor,INDENT=0)
     call msg(trim(where_ch),trim(hash_ch)//' Intraband transitions      :',.not.EField(i2)%no_intraband,INDENT=0)
   enddo  
   !
   if (i1==2) call msg(trim(where_ch),'#')
   !
 enddo
 !
 call msg('o '//file_name(1),'#',headings(1:6),INDENT=0,USE_TABS=.TRUE.)
 call msg('o '//file_name(1),'#')
 !
end subroutine RT_initialize
