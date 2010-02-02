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
 use real_time,      ONLY:l_RT_diagonalize
 implicit none
 !
 integer :: i_t
 real(SP):: values(8)
 !
 ! Work space
 !
 character(lchlen), save  :: file_name(2)
 character(schlen)        :: headings(10),where_ch
 integer                  :: i1,n_output_files,i_d
 !
 ! OUTPUT Files
 !--------------
 !
 n_output_files=1
 if (l_RT_diagonalize) n_output_files=2
 !
 if(i_t==1) then
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
   file_name(2)="RT-qp"
   if(l_RT_diagonalize) call of_open_close(file_name(2),'ot')
   !
   do i1=1,n_output_files
     !
     where_ch='o '//file_name(i1)
     !
     do i_d=1,n_SC_descriptions
       call msg(trim(where_ch),'#',trim(SC_description(i_d)),INDENT=0)
     enddo
     !
     call msg(trim(where_ch),'#')
     call msg(trim(where_ch),'#',headings(1:8),INDENT=0,USE_TABS=.TRUE.)
     call msg(trim(where_ch),'#')
     !
   enddo
   !
 else if (i_t<0) then
   !
   call of_open_close(file_name(1))
   call of_open_close(file_name(2))
   !
 endif
 !
 if (i_t>0) call msg('o dynamics','',values(1:8),USE_TABS=.TRUE.)
 !
end subroutine RT_report
