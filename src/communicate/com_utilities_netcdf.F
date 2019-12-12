!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine of_netcdf_shadow_real(i1,mesg,val,nval)
 !
 use pars,     ONLY:SP,schlen
#if defined _NC_OUTPUT
 use stderr,   ONLY:intc
 use com,      ONLY:of_nc_pos,of_nc_IDs,of_nc_var_names
 use netcdf,   ONLY:nf90_unlimited
 use IO_m,     ONLY:WR,NONE,io_control,io_variable_bulk,def_variable_bulk
#endif
 !
 implicit none
 !
 integer,      intent(in) :: i1,nval
 character(*), intent(in) :: mesg
 real(SP),     intent(in) :: val(nval)
 !
#if defined _NC_OUTPUT
 !
 logical               :: def_var
 integer, parameter    :: max_head_values_lines=50
 integer               :: VAR_ID,i_var,i_char,imsg,VAR_POS(2),VAR_SIZE(2),VAR_SIZE_STR(2),VAR_ID_STR
 real(SP)              :: nc_var(nval,1)
 character(schlen)     :: dim_names(2),dim_names_str(2),VAR_NAME
 character(1)          :: nc_str(schlen,1)
 !
 imsg=index(mesg,"#")
 !
 if(imsg==0) then
   VAR_ID  =4
   def_var=(of_nc_pos(VAR_ID,i1)==0)
   of_nc_pos(VAR_ID,i1)=of_nc_pos(VAR_ID,i1)+1
   dim_names(1)='n_values'
   dim_names(2)='unlim'
   VAR_NAME=trim(of_nc_var_names(i1))//'_values'
   VAR_SIZE=(/nval,nf90_unlimited/)
   VAR_POS =(/1,of_nc_pos(VAR_ID,i1)/)
 endif
 if(imsg> 0) then
   VAR_ID  =3
   def_var=(mod(of_nc_pos(VAR_ID,i1),max_head_values_lines)==0)
   of_nc_pos(VAR_ID,i1)=of_nc_pos(VAR_ID,i1)+1
   i_var=(of_nc_pos(VAR_ID,i1)-1)/max_head_values_lines
   dim_names(1)='max_head_values_n'
   dim_names(2)='max_head_values_lines'
   VAR_NAME=trim(of_nc_var_names(i1))//"_head_values_"//trim(intc(i_var))
   VAR_SIZE=(/10,max_head_values_lines/)
   VAR_POS =(/1,of_nc_pos(VAR_ID,i1)-i_var*max_head_values_lines/)
   !
   nc_str=''
   VAR_ID_STR=5
   dim_names_str(1)='schlen'
   dim_names_str(2)='max_head_values_lines'
   VAR_SIZE_STR=(/schlen,max_head_values_lines/)
   do i_char=1,len_trim(mesg)
     nc_str(i_char,1)=mesg(i_char:i_char)
   enddo
 endif
 !
 if(def_var) then
   call def_variable_bulk(of_nc_IDs(i1),trim(VAR_NAME),VAR_ID,VAR_SIZE,SP,dim_names)
   if(imsg> 0) call def_variable_bulk(of_nc_IDs(i1),trim(VAR_NAME)//"_titles",VAR_ID_STR,VAR_SIZE_STR,0,dim_names_str)
 endif
 !
 nc_var(:,1)=val
 !
 call io_control(ACTION=WR,COM=NONE,SEC=(/1/),ID=of_nc_IDs(i1))
 call io_variable_bulk(of_nc_IDs(i1),VAR_ID,R2=nc_var,IPOS=VAR_POS)
 if(imsg> 0) call io_variable_bulk(of_nc_IDs(i1),VAR_ID_STR,CH2=nc_str,IPOS=VAR_POS)
 !
#endif
 !
 return
 !
end subroutine of_netcdf_shadow_real
!
!
subroutine of_netcdf_shadow_char(i1,nval,val)
 !
 use pars,     ONLY:SP,schlen
#if defined _NC_OUTPUT
 use stderr,   ONLY:intc
 use com,      ONLY:of_nc_pos,of_nc_IDs,of_nc_var_names
 use netcdf,   ONLY:nf90_unlimited
 use IO_m,     ONLY:WR,NONE,io_control,io_variable_bulk,def_variable_bulk
#endif
 !
 implicit none
 !
 integer,      intent(in)  :: i1,nval
 character(*), intent(in)  :: val(nval)
 !
#if defined _NC_OUTPUT
 !
 logical               :: write_nc,def_var
 integer, parameter    :: max_headers_lines=50
 integer               :: i_var,i_char,strlen(nval),VAR_ID,VAR_POS(2),VAR_SZ(2)
 character(schlen)     :: VAR_NAME
 character(3)          :: end_string
 character(schlen)     :: dim_names(2)
 character(1), allocatable  :: nc_var(:,:)
 !
 write_nc=.false.
 do i_var=1,nval
   if(trim(val(i_var))=="") cycle
   write_nc=.true.
   exit
 enddo
 !
 if(.not.write_nc) return
 !
 if (nval==1) then
   VAR_ID  =2
   def_var=(mod(of_nc_pos(VAR_ID,i1),max_headers_lines)==0)
   of_nc_pos(VAR_ID,i1)=of_nc_pos(VAR_ID,i1)+1
   i_var=(of_nc_pos(VAR_ID,i1)-1)/max_headers_lines
   allocate(nc_var(schlen,nval))
   strlen(1)=len(trim(val(1)))
   dim_names(1)='schlen'
   dim_names(2)='max_headers_lines'
   VAR_NAME=trim(of_nc_var_names(i1))//"_headers_"//trim(intc(i_var))
   VAR_SZ  =(/schlen,max_headers_lines/)
   VAR_POS =(/1,of_nc_pos(VAR_ID,i1)-i_var*max_headers_lines/)
 endif
 !
 if (nval> 1) then
   VAR_ID  =1
   def_var=.true.
   of_nc_pos(VAR_ID,i1)=of_nc_pos(VAR_ID,i1)+1
   do i_var=1,nval
     strlen(i_var)=len(trim(val(i_var))) 
   enddo
   allocate(nc_var(maxval(strlen),nval))
   dim_names(1)='maxval_strin_len_'//trim(intc(of_nc_pos(VAR_ID,i1)))
   dim_names(2)='n_headers_'//trim(intc(of_nc_pos(VAR_ID,i1)))
   VAR_NAME=trim(of_nc_var_names(i1))//"_head_names_"//trim(intc(of_nc_pos(VAR_ID,i1)-1))
   VAR_SZ  =(/maxval(strlen),nval/)
   VAR_POS =(/1,1/)
 endif
 !
 if(def_var) call def_variable_bulk(of_nc_IDs(i1),trim(VAR_NAME),VAR_ID,VAR_SZ,0,dim_names)
 !
 end_string=" # "
 nc_var=""
 do i_var=1,nval
   do i_char=1,strlen(i_var)
     nc_var(i_char,i_var)=val(i_var)(i_char:i_char)
   enddo
 enddo
 !
 call io_control(ACTION=WR,COM=NONE,SEC=(/1/),ID=of_nc_IDs(i1))
 call io_variable_bulk(of_nc_IDs(i1),VAR_ID,CH2=nc_var,IPOS=VAR_POS)
 !
 deallocate(nc_var)
 !
#endif
 !
 return
 !
end subroutine of_netcdf_shadow_char

