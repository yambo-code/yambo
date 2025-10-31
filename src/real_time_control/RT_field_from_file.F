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
subroutine RT_load_field_from_file_init(filename1,filename2)
 !
 use pars,       ONLY:SP
 use com,        ONLY:com_path
 use fields,     ONLY:field_from_file_steps,field_from_file_dt
 use real_time,  ONLY:RT_step
 !
 implicit none
 !
 character(*), intent(in)   :: filename1,filename2
 !
 integer :: n_time_steps1,n_time_steps2,ndt1
 real(SP):: dt1,dt2
 !
 call warning(" Loading Field from file: "//trim(filename1)//" [experimental]")
 !
 open(unit=99,file=trim(filename1))
 read(99,*) n_time_steps1,dt1
 close(99)
 if (filename2/="") then
   open(unit=99,file=trim(filename2))
   read(99,*) n_time_steps2,dt2
   close(99)
   if(n_time_steps1/=n_time_steps2) call error(" Field from file, polarizations with different dimensions")
   if(abs(dt1-dt2)/(dt1+dt2)>1.E-4) call error(" Field from file, polarizations with different time step")
 endif
 field_from_file_steps=max(field_from_file_steps,n_time_steps1)
 field_from_file_dt=max(field_from_file_dt,dt1)
 !
 dt1=RT_step/field_from_file_dt
 if (abs(dt1-real(nint(dt1),SP))>1.E-5) call error(" Field time step non consistent with time step")
 !
end subroutine RT_load_field_from_file_init
!
!
subroutine RT_load_field_from_file(filename1,filename2,i_field)
 !
 use real_time,  ONLY:RT_step
 use com,        ONLY:com_path
 use pars,       ONLY:n_ext_fields_max,SP
 use fields,     ONLY:Efield,field_from_file,field_from_file_fname, &
 &                    field_from_file_steps,field_from_file_dt
 !
 implicit none
 !
 character(*), intent(in)   :: filename1,filename2
 integer,      intent(in)   :: i_field
 !
 integer  :: i1,i_field_file,n_time_steps1,n_time_steps2,idata,i_pol1_data,i_pol2_data
 real(SP) :: tmp_data(4,2),dt1,dt2
 !
 i_field_file=-1
 do i1=1,n_ext_fields_max
   if ( field_from_file_fname(i1)/="" ) cycle
   i_field_file=i1
   exit
 enddo
 !
 field_from_file_fname(i_field_file)=filename1
 tmp_data=0._SP
 !
 if(i_field_file==1) then
   allocate(field_from_file(field_from_file_steps,7,n_ext_fields_max))
   field_from_file=0._SP
 endif
 open(unit= 99,file=trim(filename1))
 read( 99,*) n_time_steps1,dt1
 if (filename2/="") then
   open(unit=100,file=trim(filename2))
   read(100,*) n_time_steps2,dt2
 endif
 do i1=1,n_time_steps1
   read( 99,*) tmp_data(:,1)
   if (filename2/="") read(100,*) tmp_data(:,2)
   field_from_file(i1,1,i_field_file)=tmp_data(1,1)
   do idata=2,4
     i_pol1_data=2*idata-2
     i_pol2_data=2*idata-1
     field_from_file(i1,i_pol1_data,i_field_file)=tmp_data(idata,1)
     field_from_file(i1,i_pol2_data,i_field_file)=tmp_data(idata,2)
   enddo
 enddo
 close( 99)
 if (filename2/="") close(100)
 !
 i1=nint(RT_step/field_from_file_dt)
 Efield(i_field)%t_final_indx=(n_time_steps1/i1-4)+Efield(i_field)%t_initial_indx-1
 Efield(i_field)%t_final=RT_step*(Efield(i_field)%t_final_indx-1)
 !
end subroutine RT_load_field_from_file
!
!
subroutine RT_print_field_to_file_init(i_field)
 !
 ! I think this subroutine is not used
 !
 use stderr,         ONLY:intc
 use com,            ONLY:com_path
 use pars,           ONLY:schlen,SP
 use fields,         ONLY:Efield
 !
 implicit none
 !
 integer, intent(in)   :: i_field
 !
 integer :: n_time_steps
 real(SP):: dt
 character(schlen) :: filename1,filename2
 !
 filename1="RT_EXTFIELD"//trim(intc(i_field))//"_P1.time"
 filename2=""
 if ( trim(Efield(i_field)%ef_pol) == "circular" ) filename2="RT_EXTFIELD"//trim(intc(i_field))//"_P2.time"
 !
 n_time_steps=(Efield(i_field)%t_final_indx-Efield(i_field)%t_initial_indx+1)*2+8
 ! To be fixed !
 dt=0._SP
 !
 open(unit=100+i_field,file=trim(com_path)//"/"//trim(filename1))
 write(100+i_field,*) n_time_steps,dt
 !
 if (filename2/="") then
   open(unit=200+i_field,file=trim(com_path)//"/"//trim(filename2))
   write(200+i_field,*) n_time_steps,dt
 endif
 !
end subroutine RT_print_field_to_file_init
