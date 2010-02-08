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
subroutine RT_report(i_t,values)
 ! 
 use pars,           ONLY:lchlen,schlen,SP
 use com,            ONLY:msg,of_open_close
 use SC,             ONLY:SC_description,n_SC_descriptions
 use real_time,      ONLY:l_RT_diagonalize,l_NE_with_fields,A_vecpot
 implicit none
 !
 integer :: i_t
 real(SP):: values(8)
 integer, parameter      :: n_output_files=3
 !
 ! Work space
 !
 character(lchlen), save  :: file_name(3)
 character(schlen)        :: headings(10),where_ch
 integer                  :: i1,i_d
 logical                  :: output_file(n_output_files)
 real(SP)                 :: gauge_field_values(10)
 !
 ! OUTPUT Files
 !--------------
 !
 output_file=.false.
 !
 output_file(1)=.true.  ! General dynamics output file
 if (l_RT_diagonalize)  output_file(2)=.true.
 if (l_NE_with_fields)  output_file(3)=.true.
 !
 if(i_t==1) then
   !      
   file_name(1)='RT-dynamics'
   file_name(2)="RT-qp"
   file_name(3)="RT-gauge_field"
   !
   do i1=1,n_output_files
     !
     if(.not.output_file(i1)) cycle
     !
     call of_open_close(file_name(i1),'ot') 
     !
     where_ch='o '//file_name(i1)
     !
     do i_d=1,n_SC_descriptions
       call msg(trim(where_ch),'#',trim(SC_description(i_d)),INDENT=0)
     enddo
     !
   enddo
   !
   where_ch='o '//file_name(1)
   !
   headings(1)="Time[fs]"
   headings(2)="E_kin[eV]"
   headings(3)="E_ion[eV]"
   headings(4)="E_Hartree[eV]"
   headings(5)="E_x[eV]"
   headings(6)="E_c[eV]"
   headings(7)="E_mg[eV]"
   headings(8)="N" 
   !
   call msg(trim(where_ch),'#')
   call msg(trim(where_ch),'#',headings(1:8),INDENT=0,USE_TABS=.TRUE.)
   call msg(trim(where_ch),'#')
   !
   if(l_NE_with_fields) then
     !
     where_ch='o '//file_name(3)
     !
     headings(1)="Time[fs]"
     headings(2)="A_x"
     headings(3)="A_y"
     headings(4)="A_z"
     headings(5)="A'_x"
     headings(6)="A'_y"
     headings(7)="A'_z"
     headings(8)="A''_x"
     headings(9)="A''_y"
     headings(10)="A''_z"
     !
     call msg(trim(where_ch),'#')
     call msg(trim(where_ch),'#',headings(1:10),INDENT=0,USE_TABS=.TRUE.)
     call msg(trim(where_ch),'#')
     !
   endif
   !
 else if (i_t<0) then
   !
   do i1=1,n_output_files
     if(.not.output_file(i1)) cycle
     call of_open_close(file_name(i1))
   enddo
   !
 endif
 !
 if (i_t>0) then
   call msg('o dynamics','',values(1:8),USE_TABS=.TRUE.)
   if(l_NE_with_fields) then
     gauge_field_values(1)   =values(1)
     gauge_field_values(2:4) =real(A_vecpot%vecpot(:))
     gauge_field_values(5:7) =real(A_vecpot%vecpot_vel(:))
     gauge_field_values(8:10)=real(A_vecpot%vecpot_acc(:))
     call msg('o gauge_field','',gauge_field_values(1:10),USE_TABS=.TRUE.)
   endif
 endif
 !
end subroutine RT_report
