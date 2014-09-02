!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
module fragments
 ! 
 use pars,   ONLY:schlen,lchlen
 use IO_m,   ONLY:close_is_on,io_connect,io_resetable,io_file,&
&                 io_sec,io_extension,io_action,io_fragmented,read_is_on,write_is_on,&
&                 OP_RD_CL,OP_WR_CL,io_type,io_disconnect,io_netcdf_support,io_folder
 use stderr, ONLY:intc,write_fragments_IO_log,logfile
 !
 implicit none
 !
 integer, allocatable :: Fragment_status(:)
 logical              :: Parallel_Fragments
 integer              :: last_syncronized
 !
 character(schlen), private :: subfolder
 !
 ! Interface
 !
 interface 
   !
   subroutine Fragments_Restart(ID,current_fragment,fragments_todo)
     integer          ::ID
     integer, optional::current_fragment,fragments_todo
   end subroutine
   !
   subroutine Fragments_Synchronize(ID,db_name,i_fragment,j_fragment)
     integer          ::ID
     character(*)     ::db_name
     integer          ::i_fragment
     integer, optional::j_fragment
   end subroutine
   !
 end interface
 !
 contains
   !
   logical function fragment_exists(ID)
     !
     use com,       ONLY:file_exists,all_locations,num_of_alternative_locations
     !
     implicit none
     !
     integer                :: ID
     !
     ! Work Space
     !
     character(lchlen) :: fragment_filenames(num_of_alternative_locations)
     integer           :: i_f
     !
     fragment_filenames=all_locations(trim(io_extension(ID)),trim(io_folder(ID)),io_netcdf_support(ID))
     !
     fragment_exists=.FALSE.
     !
     do i_f=1,num_of_alternative_locations
       if (file_exists(trim(fragment_filenames(i_f)))) then
         fragment_exists=.TRUE.
         return
       endif
     enddo
     !
   end function
   !
   subroutine io_fragment(ID,i_fragment,j_fragment,ierr)
     !
     ! This routine checks if the DB is fragmented. If it is
     ! it closes the DB and opens the fragment corresponding
     ! to the i_fragment (j_fragment) pointers.
     !
     ! Note that, as io_disconnect is called with a negative
     ! argument io_reset is no called. This is needed to save
     ! the prperties of the DB that have been read previously 
     ! (like io_nectdf_support or io_code_version/revision).
     !
     use interfaces, ONLY:PARALLEL_live_message
     use parallel_m, ONLY:ncpu
     !
     implicit none
     integer           :: ID
     integer, optional :: i_fragment,j_fragment,ierr
     !
     ! Work Space
     !
     integer           :: ierr_,i_frag
     logical           :: close_is_on_save
     character(schlen) :: fragment_filename
     !
     if (.not.io_fragmented(ID)) return
     !
     close_is_on_save=close_is_on(ID)
     !
     if (read_is_on(ID))  io_action(ID)=OP_RD_CL
     if (write_is_on(ID)) io_action(ID)=OP_WR_CL
     !
     if (present(i_fragment))      i_frag=i_fragment
     if (.not.present(i_fragment)) i_frag=maxval(io_sec(ID,:))
     !
     ! Disconnect without calling io_reset
     !
     io_resetable(ID)=.false.
     !
     call io_disconnect(ID)
     !
     ! io_file is redefined by io_connect
     !
     io_file(ID)=' '
     !
     fragment_filename=trim(io_extension(ID))//"_fragment_"//trim(intc(i_frag))
     if (present(j_fragment)) then
       fragment_filename=trim(io_extension(ID)) &
&                        //"_fragments_"//trim(intc(i_frag))//"_"//trim(intc(j_fragment))
     endif
     ierr_=io_connect(fragment_filename,type=io_type(ID),subfolder=trim(io_folder(ID)),ID=ID)
     !
     if (ncpu>1.and.write_fragments_IO_log.and.index(logfile,"stderr")==0) then
       if (write_is_on(ID)) call PARALLEL_live_message("Writing "//trim(fragment_filename))
       if (read_is_on(ID) ) call PARALLEL_live_message("Reading "//trim(fragment_filename))
     endif
     !
     if (present(ierr)) ierr=ierr_
     !
     ! io_action must be saved otherwise any io_disconnect call would
     ! call io_reset (like in ioWF) 
     !
     if (close_is_on_save) io_resetable(ID)=.true.
     !
   end subroutine 
   !
end module 
