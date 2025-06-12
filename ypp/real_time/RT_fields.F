!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
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
subroutine RT_fields()
 !
 use units,          ONLY:AUT2FS
 use stderr,         ONLY:intc
 use com,            ONLY:msg
 use pars,           ONLY:SP,schlen,pi,n_fields_defs_max
 use com,            ONLY:com_path
 use YPP_real_time,  ONLY:RT_conf,ElField,RT_time
 use fields,         ONLY:Efield
 use stderr,         ONLY:STRING_split
 !
#include<y_memory.h>
 !
 ! Work Space
 !
 integer           :: i_field,i_t
 logical           :: l_load_from_file,l_from_time_fft 
 character(schlen) :: field_defs(n_fields_defs_max),filename
 real(SP)          :: field_data(7),Field_time
 !
 RT_conf%T_n_steps=nint( (RT_conf%T_range(2)-RT_conf%T_range(1))/RT_conf%delta_T )+1
 !
 ! Time Initialization from input
 !call RT_time_configuration_setup('FIELDS',OBS_RT_IO_t)
 YAMBO_ALLOC(RT_time,(RT_conf%T_n_steps))
 do i_t=1,RT_conf%T_n_steps
   RT_time(i_t)=(i_t-1)*RT_conf%delta_T+RT_conf%T_range(1)
 enddo
 !
 call msg('s',' Time range         :',(/minval(RT_time),maxval(RT_time)/)*AUT2FS,"[fs]")
 call msg('s',' Time step          :',RT_conf%delta_T*AUT2FS,"[fs]")
 call msg('s',' Time steps         :',RT_conf%T_n_steps)
 !
 ! Analize the input variables for the external field
 call STRING_split(Efield(1)%ef_name,field_defs)
 !
 i_field=1
 !
 ! Field in frequency space from external file
 if ( field_defs(1)=='FROM_W_FILE') call RT_Field_w_load(i_field)
 ! 
 ! Field in frequency space from FT of real time expression
 if ( field_defs(1)/='FROM_W_FILE') call RT_Field_t_to_w(i_field)
 !
 ! Chirp the field and convert back into real time
 call RT_1D_Fourier_Chirp(ElField,RT_conf)
 !
 call RT_print_Elfield(1,"time-chirped",Elfield)
 !
 call RT_1D_Fourier_Transform('T2W',ElField,RT_conf,1)
 !
 call RT_print_Elfield(1,"freq-chirped",Elfield)
 !
end subroutine RT_fields
!
!
subroutine RT_Field_w_load(i_field)
 !
 use YPP_real_time,  ONLY:RT_conf,ElField
 !
 ! TODO
 !
 implicit none
 !
 integer, intent(in) :: i_field
 !
 call error(' Loading field frequency from file not yet coded')
 !
 call RT_print_Elfield(1,"freq",Elfield)
 !
end subroutine RT_Field_w_load
!
!
subroutine RT_Field_t_to_w(i_field)
 !
 use units,          ONLY:FS2AUT
 use pars,           ONLY:SP,schlen
 use fields_int,     ONLY:small_a
 use fields,         ONLY:Efield
 use YPP_real_time,  ONLY:RT_conf,ElField,RT_time
 !
 implicit none
 !
 integer, intent(in) :: i_field
 !
 integer   :: i_t
 real(SP)  :: Field_time,dT
 !
 ElField%title="Field"
 ElField%N    =6
 call RT_1D_Fourier_setup(ElField,RT_conf,6)
 !
 dT=RT_conf%delta_T
 !
 do i_t=1,RT_conf%T_n_steps
   Field_time=RT_time(i_t)-Efield(i_field)%t_initial
   ElField%F_t(i_t,1:2) =small_a(Field_time,dT,Efield(i_field),0)
   ElField%F_t(i_t,3:4) =small_a(Field_time,dT,Efield(i_field),1)
   ElField%F_t(i_t,5:6) =small_a(Field_time,dT,Efield(i_field),2)
 enddo
 !
 call RT_print_Elfield(1,"time",Elfield)
 !
 call RT_1D_Fourier_Transform('T2W',ElField,RT_conf,1)
 !
 call RT_print_Elfield(1,"freq",Elfield)
 !
end subroutine RT_Field_t_to_w
!
!
subroutine RT_print_Elfield(i_field,mode,Elfield)
 !
 use units,          ONLY:FS2AUT
 use YPP_real_time,  ONLY:RT_conf,OBS_el
 use pars,           ONLY:SP,schlen
 use stderr,         ONLY:intc
 use com,            ONLY:com_path
 !
 implicit none
 !
 integer,      intent(in) :: i_field
 character(*), intent(in) :: mode
 type(OBS_el), intent(in) :: ElField
 !
 integer           :: i_t,i_w,idata
 character(schlen) :: filename
 real(SP)          :: field_data(7),Field_t,Field_w
 !
 if (mode=="time")         filename="YPP_EXTFIELD_"//trim(intc(i_field))//".time"
 if (mode=="time-chirped") filename="YPP_EXTFIELD_LINCHIRP_"//trim(intc(i_field))//".time"
 if (mode=="freq")         filename="YPP_EXTFIELD_"//trim(intc(i_field))//".freq"
 if (mode=="freq-chirped") filename="YPP_EXTFIELD_LINCHIRP_"//trim(intc(i_field))//".freq"
 !
 open(unit=99+i_field,file=trim(com_path)//"/"//trim(filename))
 !
 if (index(mode,"time")>0) then
   !
   write(99+i_field,*) RT_conf%T_n_steps
   !
   do i_t=1,RT_conf%T_n_steps
     Field_t=RT_conf%T_range(1)+(i_t-1)*RT_conf%delta_t   !-Efield%t_initial
     field_data(1)=Field_t/FS2AUT
     do idata=1,6
       field_data(idata+1)=real(ElField%F_t(i_t,idata))
     enddo
     write(99+i_field,*) field_data
   enddo
   !
 else if (index(mode,"freq")>0) then
   !
   write(99+i_field,*) RT_conf%W_n_steps
   !
   do i_w=1,RT_conf%W_n_steps
     Field_w=RT_conf%X_w_range(1)+(i_w-1)*RT_conf%delta_W
     field_data(1)=Field_w !/FS2AUT
     do idata=1,6
       field_data(idata+1)=real(ElField%F_w(i_w,idata))
     enddo
     write(99+i_field,*) field_data
   enddo
   !
 endif
 !
 close(99+i_field)
 !
end subroutine RT_print_Elfield
