!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine of_open_close(of_name_extension,mode)
 !
 use pars,            ONLY:lchlen
 use com,             ONLY:of_name,of_opened,of_unit,write_to_of,n_ofs_defined
 use IO_m,            ONLY:get_name
#if defined _NC_OUTPUT
 use stderr,          ONLY:STRING_remove
 use com,             ONLY:of_nc_pos,of_nc_var_names,of_nc_IDs
 use IO_m,            ONLY:io_control,io_connect,io_disconnect,OP_WR,WR_CL,NONE
#endif
#if defined _YAML_OUTPUT
 use f_utils,         ONLY:f_get_free_unit
 use com,             ONLY:of_yaml_IDs
 use yaml_output,     ONLY:yaml_set_stream,yaml_close_stream,yaml_stream_connected
#endif
 !
 implicit none
 !
 character(*), intent(in)            :: of_name_extension
 character(*), intent(in), optional  :: mode
 !
 integer, external  :: get_file_index
 ! 
 ! Work Space
 !
#if defined _NC_OUTPUT
 integer          :: io_out
#endif
 integer          :: i2,file_index
 character(lchlen):: of_name_complete
 !
 if (len_trim(of_name_extension)==0) return
 !
 if (present(mode)) then
   !
   ! Get full filename, including path and jobname, from extension
   !  
   of_name_complete=get_name(desc=trim(of_name_extension),type=3,CORE_IO=.false.,MORE_IO=.false.,COM_IO=.true.)
   !
   ! Get file index from file name
   !
   file_index=get_file_index(of_name_complete)
   !
   ! Flush output if file is opened
   !
   if (index(mode,'f')/=0.and.(len_trim(of_opened(file_index))>0)) then
     flush(abs(of_unit(file_index)))
     if (trim(mode)=='f') return
   endif
   !
   if (index(mode,'a')==0.and.index(mode,'O')==0) then
     ! Check if filename exists from previous run.
     ! In case _0n needs to be appended when file is first created
     call rename_file(of_name_complete)
     of_name(file_index)=of_name_complete
   else if(len_trim(of_name(file_index))>0) then
     ! In case _0n needs to be appendend, it is done checking the filename saved
     of_name_complete=of_name(file_index)
   endif
   !
   if ( (index(mode,'o')==0.and.index(mode,'O')==0) .or. .not.write_to_of) return
   !
   ! Here I open the unit using file_name to define the name
   !
   ! Check first if already open
   !
   do i2=1,n_ofs_defined
     if (trim(of_opened(i2))==of_name_complete) return
   enddo
   !
   ! Find the first free unit
   !
#if defined _YAML_OUTPUT
   of_unit(file_index)=f_get_free_unit()
#else
   of_unit(file_index)=file_index+20
#endif
   of_opened(file_index)=of_name_complete
   !
   if (index(mode,'a')==0) open(of_unit(file_index),file=trim(of_opened(file_index)))
   if (index(mode,'a')/=0) open(of_unit(file_index),file=trim(of_opened(file_index)),position='append')
   if (index(mode,"t")>0) call write_the_logo(of_unit(file_index),'#')
   !
   ! A opened file has a unit assigned
   ! However the unit is kept negative
   ! Only msg_manager switchis it to positive just before writing to file
   ! Immediately after writing the msg_interfaces will switch it back to negative
   !
   of_unit(file_index)=-of_unit(file_index)
   !
#if defined _NC_OUTPUT
   call io_control(ACTION=OP_WR,COM=NONE,SEC=(/1/),ID=of_nc_IDs(file_index))
   io_out=io_connect(desc="output_"//trim(of_name_extension),type=2, ID=of_nc_IDs(file_index))
   of_nc_var_names(file_index)=trim(STRING_remove(trim(of_name_extension),"-",replace="_"))
   of_nc_pos(:,file_index)=0
#endif
#if defined _YAML_OUTPUT
   call yaml_set_stream(filename=trim(of_name_complete)//".yaml",record_length=200)
   call yaml_stream_connected(trim(of_name_complete)//".yaml",of_yaml_IDs(file_index))
#endif
   !
 else
   !
   ! Close file
   !  
   do i2=1,n_ofs_defined
     if (index(of_opened(i2),trim(of_name_extension))/=0) then
       close(iabs(of_unit(i2)))
       of_unit(i2)=0
#if defined _NC_OUTPUT
       call io_control(ACTION=WR_CL,COM=NONE,SEC=(/1/),ID=of_nc_IDs(i2))
       call io_disconnect(ID=of_nc_IDs(i2)) 
#endif
#if defined _YAML_OUTPUT
       call yaml_close_stream(unit=of_yaml_IDs(i2))
#endif
       of_opened(i2)=' '
       exit
     endif
   enddo
   !
 endif
 !
end subroutine of_open_close
!
subroutine write_the_logo(unit_,comment_)
 !
 use LIVE_t,       ONLY:LIVE_message
 use LOGO,         ONLY:pickup_a_logo,logo_line,n_logo_lines
 use pars,         ONLY:schlen
 !
 implicit none
 !
 integer     ::unit_
 character(1)::comment_
 character(schlen)::space_,ch ! Work Space
 integer          ::i1        !
 !
 call pickup_a_logo(unit_)
 !
 if (unit_<0) return
 !
 if (trim(comment_)=='')  space_='(t5,3a)'
 if (trim(comment_)=='#') then
   space_='(t1,3a)'
   do i1=1,n_logo_lines
      ch='# '//logo_line(i1)
      logo_line(i1)=trim(ch)
   enddo
 endif
 !
 do i1=1,n_logo_lines
   if (unit_/=6) write (unit_,trim(space_)) logo_line(i1)
   if (unit_==6.and.i1>=n_logo_lines-3) cycle
   if (unit_==6) call LIVE_message(logo_line(i1),"n","","%s",CPU_TIME=.false.,CPU_ID=.false.)
 enddo
 if (unit_/=6.and.trim(comment_)=='')  write (unit_,'(a)') ' '
 if (unit_/=6.and.trim(comment_)=='#') write (unit_,'(a)') '#'
 !
end subroutine write_the_logo
!
subroutine rename_file(name)
 !
 use pars,       ONLY:lchlen
 !
 implicit none
 !
 character(lchlen):: name
 character(lchlen):: ch
 integer          :: i1
 logical, external:: file_exists
 !
 if (.not.file_exists(name)) return
 i1=0
 ch=name
 do while (file_exists(ch))
   i1=i1+1
   if (i1< 99) write (ch,'(2a,i2.2)') trim(name),'_',i1
   if (i1> 99) write (ch,'(2a,i3.3)') trim(name),'_',i1
 enddo
 name=ch
 !
end subroutine rename_file
!
subroutine warning(mesg)
 !
 use LIVE_t,     ONLY:LIVE_message
 use stderr,     ONLY:STRING_pack
 use com,        ONLY:msg
 !
 implicit none
 !
 character(*) :: mesg
 call msg('nr',STRING_pack('[WARNING] ',trim(mesg)))
 call LIVE_message(STRING_pack('[WARNING] ',trim(mesg)),"n","","%s")
 !
end subroutine warning
!
subroutine error(mesg)
 !
 use LIVE_t,     ONLY:LIVE_message,live_timing_is_on
 use stderr,     ONLY:STRING_pack,logfile_unit,log_as_a_file
 use com,        ONLY:secnm,msg
#if defined _MPI
 use parallel_m, ONLY:mpi_comm_world,ncpu
#endif
 !
 implicit none
 !
#if defined _MPI
 integer         :: ierr
#endif
 character(*)    :: mesg
 !
 live_timing_is_on=.FALSE.
 !
 if (len_trim(secnm)>0) then
   call msg('nr',STRING_pack('[ERROR] STOP signal received while in ',trim(secnm)))
   call LIVE_message(STRING_pack('[ERROR] STOP signal received while in ',trim(secnm)),"nn","","%s")
 endif
 call msg('nr',STRING_pack('[ERROR] ',trim(mesg)))
 call LIVE_message(STRING_pack('[ERROR] ',trim(mesg)),"n","","%s")
 call LIVE_message("","n","","%s")
 if(log_as_a_file) flush(logfile_unit)
 !
#if defined _MPI
 if (ncpu>1) call MPI_Abort(mpi_comm_world,1,ierr)
#endif
 stop
 !
end subroutine error
!
subroutine msg_manager(how,msg_for_out,msg_for_rep,msg_for_log)
 !
 ! how = '(n)s(n)' '(n)r(n)' '(n)l(n)' 'o PATTERN'
 !
 ! r(eport)
 ! s(tderr)
 ! oN=msgunits(N) 
 !
 ! This subroutine switches to positive the value of of_unit just before writing
 ! plus extra operations
 !
 use pars,    ONLY:schlen,lchlen,repfile_index
 use stderr,  ONLY:STRING_split,STRING_remove
 use com,     ONLY:terminator,of_opened,of_name,of_unit,n_ofs_defined,jobstr,&
&                  write_to_report,write_to_of
 !
 implicit none
 !
 character(*), intent(in)  :: how
 logical,      intent(out) :: msg_for_out,msg_for_rep,msg_for_log
 ! 
 ! Work Space
 !
 integer          ::i1,i2
 character(schlen)::ch_piece(10)
 character(lchlen)::stripped_file_name
 !
 terminator(1)='('
 terminator(2)=')'
 !
 msg_for_out=.false.
 msg_for_rep=.false.
 msg_for_log=.false.
 !
 ! o. files
 !
 if (how(:1)=="o".and.write_to_of) then
   call STRING_split(how,ch_piece)
   do i2=2,10
     if (len_trim(ch_piece(i2))==0) cycle
     do i1=1,n_ofs_defined
       if (len_trim(of_name(i1))==0) cycle
       stripped_file_name=STRING_remove(of_opened(i1),"-"//trim(jobstr))
       if (index( stripped_file_name,trim(ch_piece(i2)) )/=0) then
         if (len_trim(of_opened(i1))==0) call error(' Trying to write on closed file '//trim(of_name(i1)))
         of_unit(i1) =-of_unit(i1)
       endif
     enddo
   enddo
   msg_for_out=.true.
   return
 endif
 !
 ! Report
 !
 if (index(how,'r' )/=0.and.write_to_report) then
   of_unit(repfile_index) =-of_unit(repfile_index)
   msg_for_rep=.true.
   if (index(how,'nr')/=0) terminator(1)='(/'
   if (index(how,'rn')/=0) terminator(2)='/)'
 endif
 !
 ! Log 
 !
 if (index(how,'l' )/=0) then
   msg_for_log=.true.
   terminator(3:4)=' '
   if (index(how,'nl')/=0) terminator(3)='n'
   if (index(how,'ln')/=0) terminator(4)='n'
 endif
 !
end subroutine msg_manager
