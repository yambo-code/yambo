!
!        Copyright (C) 2000-2022 the YAMBO team
!            http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine RT_load_field_from_file_init(filename)
 !
 use com,        ONLY:com_path
 use fields,     ONLY:field_from_file_steps
 !
 implicit none
 !
 character(*), intent(in)   :: filename
 !
 integer :: n_time_steps
 !
 open(unit=99,file=trim(filename))
 read(99,*) n_time_steps
 close(99)
 field_from_file_steps=max(field_from_file_steps,n_time_steps)
 !
end subroutine RT_load_field_from_file_init
!
!
subroutine RT_load_field_from_file(filename,i_field)
 !
 use real_time,  ONLY:RT_step
 use com,        ONLY:com_path
 use pars,       ONLY:n_ext_fields_max,SP
 use fields,     ONLY:Efield,field_from_file,field_from_file_fname,field_from_file_steps
 !
 implicit none
 !
 character(*), intent(in)   :: filename
 integer,      intent(in)   :: i_field
 !
 integer :: i1,i_field_file,n_time_steps
 !
 i_field_file=-1
 do i1=1,n_ext_fields_max
   if ( field_from_file_fname(i1)/="" ) cycle
   i_field_file=i1
   exit
 enddo
 !
 field_from_file_fname(i_field_file)=filename
 !
 if(i_field_file==1) then
   allocate(field_from_file(field_from_file_steps,7,n_ext_fields_max))
   field_from_file=0._SP
 endif
 open(unit=99,file=trim(filename))
 read(99,*) n_time_steps
 do i1=1,n_time_steps
   read(99,*) field_from_file(i1,:,i_field_file) 
 enddo
 close(99)
 !
 Efield(i_field)%t_final_indx=(n_time_steps-8)/2+Efield(i_field)%t_initial_indx-1
 Efield(i_field)%t_final=RT_step*(Efield(i_field)%t_final_indx-1)
 !
end subroutine RT_load_field_from_file
!
!
subroutine RT_print_field_to_file_init(i_field)
 !
 use stderr,         ONLY:intc
 use com,            ONLY:com_path
 use pars,           ONLY:schlen
 use fields,         ONLY:Efield
 !
 implicit none
 !
 integer, intent(in)   :: i_field
 !
 integer :: n_time_steps
 character(schlen) :: filename
 !
 filename="RT_EXTFIELD_"//trim(intc(i_field))//".time"
 !
 n_time_steps=(Efield(i_field)%t_final_indx-Efield(i_field)%t_initial_indx+1)*2+8
 !
 open(unit=99+i_field,file=trim(com_path)//"/"//trim(filename))
 write(99+i_field,*) n_time_steps
 !
end subroutine RT_print_field_to_file_init
