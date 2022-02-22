!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine io_control(ACTION,MODE,COM,SEC,ID,COMM,WHO)
 !
 use parallel_int,  ONLY:PP_bcast
 use parallel_m,    ONLY:master_cpu,mpi_comm_null
 use IO_m,          ONLY:io_unit,io_mode,io_com,io_action,io_sec,max_io_units, &
&                        OP_APP,OP_APP_CL,OP_WR,OP_RD,OP_WR_CL,OP_RD_CL,&
&                        io_reset,io_PAR_comm,io_PAR_cpu,IO_read,IO_write,&
&                        read_is_on,write_is_on
 !
 implicit none
 !
 integer,intent(in)    :: ACTION
 integer,intent(inout) :: ID
 integer,optional,intent(in) :: MODE,COM,SEC(:)
 integer,optional,intent(in) :: COMM
 logical,optional,intent(in) :: WHO
 !
 ! Work Space
 !
 integer :: i1,COMM_
 logical :: starting_up
 !
 COMM_=mpi_comm_null
 if (present(COMM)) COMM_=COMM
 !
 ! Assign a new unit if the unit is not already open
 !
 starting_up=any((/ACTION==OP_RD_CL, ACTION==OP_WR_CL,    &
&                  ACTION==OP_APP_CL,ACTION==OP_RD,      &
&                  ACTION==OP_APP,   ACTION==OP_WR/))
 !
 if (  starting_up .and. (master_cpu .or. COMM_==mpi_comm_null) ) then
   ID=-1
   do i1=1,max_io_units
     if (io_unit(i1)==0) then
       ID=i1
       call io_reset(ID)
       io_unit(ID)=40+i1  ! This is not used with netcdf since nf90_create assigns the unit
       exit
     endif
   enddo
   if(ID==-1) call error(" Recompile Yambo with a larger: max_io_units")
 endif
 !
 ! Define the parallel COMM needed to perform parallel I/O
 !                    ------
 if (starting_up) then
   io_PAR_comm(ID)=mpi_comm_null
#if defined _PAR_IO
   if (present(COMM)) then
     call PP_bcast(ID,         0,COMM)
     call PP_bcast(io_unit(ID),0,COMM)
     ! This MUST be after the BCAST
     io_PAR_comm(ID)=COMM
   endif
#endif
 endif
 !
 io_action(ID)=ACTION
 if (present(MODE)) io_mode(ID)=MODE
 if (present(COM )) io_com(ID)=COM
 if (present(SEC)) then
   io_sec(ID,:)=0
   io_sec(ID,:size(SEC))=SEC
 endif
 !
 ! Define the processors allowed to do the specific action
 !           ------------
 if (starting_up) then
   io_PAR_cpu(ID)=0
   if ( read_is_on(ID).and.IO_read ) io_PAR_cpu(ID)=1
   if (write_is_on(ID).and.IO_write) io_PAR_cpu(ID)=1
 endif
 if (present(WHO).and.io_PAR_comm(ID)/=mpi_comm_null) then
   io_PAR_cpu(ID)=0
   if (WHO) io_PAR_cpu(ID)=1
 endif
 !
end subroutine
