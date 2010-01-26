!
! Copyright (C) 2000-2009 A. Marini and the YAMBO team 
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
module fragments
 ! 
 use pars,   ONLY:schlen
 use IO_m,   ONLY:close_is_on,io_connect,ver_is_gt_or_eq,io_resetable,io_file,&
&                 io_sec,io_extension,io_action,io_fragmented,read_is_on,write_is_on,&
&                 OP_RD_CL,OP_WR_CL,io_type,io_disconnect,io_netcdf_support
 use stderr, ONLY:intc
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
   logical function fragment_exists(ID,i_fragment,j_fragment)
    !
    use com,       ONLY:get_name,file_exists
    implicit none
    integer                :: ID
    integer                :: i_fragment
    integer,optional       :: j_fragment
    !
    ! Work Space
    !
    character(schlen) :: fragment_filename
    ! 
    fragment_filename=trim(io_extension(ID))//"_fragment_"//trim(intc(i_fragment))
    if (present(j_fragment)) fragment_filename=trim(io_extension(ID))//"_fragments_"//trim(intc(i_fragment))//"_"//trim(intc(j_fragment))
    fragment_filename=get_name(fragment_filename,type=io_type(ID),CORE_IO=.TRUE.,MORE_IO=.FALSE.,COM_IO=.FALSE.,NETCDF=io_netcdf_support(ID))
    !
    fragment_exists=file_exists(trim(fragment_filename))
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
     if (write_is_on(ID).or.ver_is_gt_or_eq(ID,revision=518)) then
       !
       fragment_filename=trim(io_extension(ID))//"_fragment_"//trim(intc(i_frag))
       if (present(j_fragment)) fragment_filename=trim(io_extension(ID))//"_fragments_"//trim(intc(i_frag))//"_"//trim(intc(j_fragment))
       ierr_=io_connect(fragment_filename,type=io_type(ID),ID=ID)
       !
     else
       write (subfolder,'(2a,i5.5)') trim(io_extension(ID)),'_',i_frag
       if (present(j_fragment)) write (subfolder,'(2a,i5.5,a,i5.5)') &
&                              trim(io_extension(ID)),'_',i_frag,'_',j_fragment
       !
       ! Note that the fragment DB keeps the same type of the DB core (io_type(ID))
       !
       ierr_=io_connect('fragment',subfolder=subfolder,type=io_type(ID),ID=ID)
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
