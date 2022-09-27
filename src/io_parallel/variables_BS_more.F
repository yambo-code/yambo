!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
integer function variables_BS_more(ID,mode)
 !
 use pars,       ONLY:IP,SP,LP,schlen
 use BS,         ONLY:BS_K_cutoff,BS_K_cutoff_done,BS_max_val,l_BSE_kernel_complete
 use IO_m,       ONLY:nf90_sync,io_unit,write_is_on,read_is_on,&
&                     variable_exist,NF90_unlimited,netcdf_dim_size
 use IO_int,     ONLY:def_variable_elemental,io_variable_elemental,&
&                     def_variable_bulk,io_variable_bulk
 use IO_m,       ONLY:io_status
 !
 implicit none
 !
 integer,      intent(in) :: ID
 character(*), intent(in) :: mode
 !
 integer           :: N_cutoff,stat
 logical           :: io_var
 real(SP)          :: BS_K_cutoff_IO(1)
 character(schlen) :: kernel_dim_name(1)
 !
 variables_BS_more=0
 !
 if ((write_is_on(ID).and.trim(mode)=="end").or.variable_exist(ID,"BSE_KERNEL_MAXVAL")) then
   call def_variable_elemental(ID,"BSE_KERNEL_MAXVAL",1,SP,0)
   call  io_variable_elemental(ID,VAR=" Kernel maxval",R0=BS_max_val)
 endif
 !
 ! Check if BSE kernel is complete in reading mode, set the variable to false in writing mode
 !
 if(write_is_on(ID)) io_var=trim(mode)=="start".or.(trim(mode)=="end".and.l_BSE_kernel_complete)
 if( read_is_on(ID)) io_var=trim(mode)=="start".and.variable_exist(ID,"BSE_KERNEL_COMPLETE")
 if(io_var) then
   call def_variable_elemental(ID,"BSE_KERNEL_COMPLETE",1,LP+100,0)
   call  io_variable_elemental(ID,VAR=" Kernel complete",DB_L0=l_BSE_kernel_complete,L0=l_BSE_kernel_complete)
 endif
 !
 if(write_is_on(ID)) io_var=trim(mode)=="end".and.l_BSE_kernel_complete.and.BS_K_cutoff>0._SP
 if( read_is_on(ID)) io_var=variable_exist(ID,"BSE_KERNEL_CUTOFF").and..not.allocated(BS_K_cutoff_done)
 if (io_var) then
   kernel_dim_name="N_COMPRESSED_KERNELS"
   call def_variable_bulk(ID,"BSE_KERNEL_CUTOFF",1,(/NF90_unlimited/),SP,kernel_dim_name)
   N_cutoff=netcdf_dim_size(ID,kernel_dim_name(1))
   if(write_is_on(ID)) then
     BS_K_cutoff_IO=BS_K_cutoff
     call  io_variable_bulk(ID,1,R1=BS_K_cutoff_IO,IPOS=(/N_cutoff+1/))
   else if(read_is_on(ID)) then
     allocate(BS_K_cutoff_done(N_cutoff))
     call  io_variable_bulk(ID,1,R1=BS_K_cutoff_done)
   endif
 endif
 !
 if(write_is_on(ID)) stat = nf90_sync(io_unit(ID))
 !
 variables_BS_more=io_status(ID)
 !
end function
