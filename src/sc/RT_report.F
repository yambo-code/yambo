!
! Copyright (C) 2000-2010 C. Attaccalite and the YAMBO team
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
subroutine RT_report(file_name,write_output_files)
 ! 
 use pars,           ONLY:lchlen,schlen
 use com,            ONLY:msg,of_open_close
 use real_time,      ONLY:l_NE_simulate_RPA,n_ext_fields, &
&                         Integrator_Name,l_RT_diagonalize,l_RT_no_diamagnetic,EField,          &
&                         l_RT_induced_field,l_RT_no_intraband,l_NE_with_fields,A_vecpot
 use stderr,         ONLY:intc
 implicit none
 !
 character(lchlen)        :: file_name(2)
 logical, intent(in)      :: write_output_files
 !
 ! Work space
 !
 character(schlen)        :: headings(10),hash_ch,where_ch
 integer                  :: i1,i2,n_output_files
 !
 ! OUTPUT Files
 !--------------
 !
 if(write_output_files) then
   !      
   file_name(1)='RT-dynamics'
   call of_open_close(file_name(1),'ot') 
   headings(1)="Time[fs]"
   headings(2)="E_kin[eV]"
   headings(3)="E_ion[eV]"
   headings(4)="E_Hartree[eV]"
   headings(5)="E_x[eV]"
   headings(6)="E_c[eV]"
   headings(7)="E_mg[eV]"
   headings(8)="N" 
   !
   n_output_files=1
   !
   file_name(2)="RT-qp"
   if(l_RT_diagonalize) then 
     n_output_files=n_output_files+1
     call of_open_close(file_name(2),'ot')
   endif
   !
 else
   n_output_files=0
 endif
 !
 do i1=1,n_output_files+1
   !
   where_ch='r'
   hash_ch ='[RT]'
   if (i1>1) where_ch='o '//file_name(i1-1)
   if (i1>1) hash_ch ='# [RT]'
   !
   call msg(trim(where_ch),trim(hash_ch)//' Integrator            :',trim(Integrator_Name),INDENT=0)
   call msg(trim(where_ch),trim(hash_ch)//' RPA                   :',l_NE_simulate_RPA,INDENT=0)
   call msg(trim(where_ch),trim(hash_ch)//' Diamagnetic term      :',.not.l_RT_no_diamagnetic,INDENT=0)
   call msg(trim(where_ch),trim(hash_ch)//' Intraband coupling    :',.not.l_RT_no_intraband,INDENT=0)
   call msg(trim(where_ch),trim(hash_ch)//' Induced Field         :',.not.l_RT_induced_field,INDENT=0)
   call msg(trim(where_ch),trim(hash_ch)//' Analyze QP-dynamics   :',l_RT_diagonalize,INDENT=0)
   !
   do i2=1,n_ext_fields
     call msg(trim(where_ch),trim(hash_ch))
     call msg(trim(where_ch),trim(hash_ch)//' External Field('//trim(intc(i2))//&
&                                           ')      :',trim(EField(i2)%ef_name),INDENT=0)
     call msg(trim(where_ch),trim(hash_ch)//' Intensity                  :',EField(i2)%intensity,INDENT=0)
     call msg(trim(where_ch),trim(hash_ch)//' Amplitude                  :',EField(i2)%amplitude,INDENT=0)
     call msg(trim(where_ch),trim(hash_ch)//' Frequancy                  :',EField(i2)%frequency,INDENT=0)
     call msg(trim(where_ch),trim(hash_ch)//' Direction                  :',EField(i2)%versor,INDENT=0)
   enddo  
   !
   if(l_NE_with_fields) then
     call msg(trim(where_ch),trim(hash_ch)//' Initial Vecpot             :',&
     &               (/real(A_vecpot%vecpot),aimag(A_vecpot%vecpot)/),INDENT=0)
     call msg(trim(where_ch),trim(hash_ch)//' Initial Vecpot Velocity    :',&
     &               (/real(A_vecpot%vecpot_vel),aimag(A_vecpot%vecpot_vel)/),INDENT=0)
   endif
   !
   if (i1>1) then 
     call msg(trim(where_ch),'#')
     call msg(trim(where_ch),'#',headings(1:8),INDENT=0,USE_TABS=.TRUE.)
     call msg(trim(where_ch),'#')
   endif
   !
 enddo
 !
end subroutine RT_report
