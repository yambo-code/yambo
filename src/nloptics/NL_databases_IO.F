!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG CA
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
subroutine NL_databases_IO(i_f,read_or_write)
 !
 use electrons,     ONLY:levels
 use X_m,           ONLY:X_t
 use IO_m,          ONLY:io_control,REP,OP_WR_CL,VERIFY,OP_APP_WR_CL,OP_RD_CL         
 use nl_optics,     ONLY:ID
 use parallel_m,    ONLY:master_cpu
 !
 implicit none
 !
 integer, intent(in)      :: i_f
 character(5), intent(in) :: read_or_write
 !
 ! Working Space
 !
 integer              :: io_Nonlinear,IO_ACTION
 integer, external    :: io_NL
 logical              :: reading,writing
 !
 reading=trim(read_or_write)=="read"
 writing=trim(read_or_write)=="write"
 !    
 if(i_f==0) then
   if(master_cpu) then
     !
     if(reading) IO_ACTION=OP_RD_CL
     if(writing) IO_ACTION=OP_WR_CL
     !
     call io_control(ACTION=IO_ACTION,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID(1))
     io_Nonlinear=io_NL(ID(1))
   endif
 else
   !         
   if(reading) IO_ACTION=OP_RD_CL
   if(writing) IO_ACTION=OP_APP_WR_CL
   !
   call io_control(ACTION=IO_ACTION,COM=REP,SEC=(/i_f+1/),MODE=VERIFY,ID=ID(1))
   io_Nonlinear=io_NL(ID(1))
   !
 endif
 !
end subroutine NL_databases_IO
