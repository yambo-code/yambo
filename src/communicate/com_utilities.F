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
!
! SUBROUTINES
!=============
!
subroutine of_open_close(of_name,mode)
 !
 use pars,            ONLY:lchlen,max_open_ofs
 use com,             ONLY:of,nofs,opened_of,of_unit,write_to_of
 use IO_m,            ONLY:get_name
#if defined _NC_OUTPUT
 use com,             ONLY:of_nc_IDs,of_nc_pos
 use IO_m,            ONLY:io_control,io_connect,io_disconnect,OP_WR,WR_CL,NONE
#endif
 !
 implicit none
 !
 character(*)           :: of_name
 character(*),optional  :: mode
 ! 
 ! Work Space
 !
#if defined _NC_OUTPUT
 integer          :: io_out
 character(lchlen):: of_nc_name
#endif
 integer          :: i2,i3,file_index
 character(lchlen):: local_file_name
 !
 if (len_trim(of_name)==0) return
 !
 if (present(mode)) then
   !
   ! Open file
   !  
   local_file_name=of_name
#if defined _NC_OUTPUT
   of_nc_name=of_name
#endif
   !
   ! Flush output
   !
   if (index(mode,'f')/=0) then
     file_index=-1
     do i2=1,max_open_ofs
        if (trim(opened_of(i2))==local_file_name) file_index=i2
     enddo
     if(file_index==-1) return
     flush(abs(of_unit(file_index)))
   endif
   !
   if ( (index(mode,'o')==0.and.index(mode,'O')==0) .or. .not.write_to_of) return
   !
   ! Here I open the unit using file_name to define the name
   !
   if (index(mode,'a')==0.and.index(mode,'O')==0) then
     local_file_name=get_name(desc=trim(of_name),type=3,CORE_IO=.false.,MORE_IO=.false.,COM_IO=.true.)
     call rename_file(local_file_name)
   endif
   !
   ! Check first if already open
   !
   do i2=1,max_open_ofs
     if (trim(opened_of(i2))==local_file_name) return
   enddo
   !
   ! Find the first free unit
   !
   do i2=1,max_open_ofs
     if (of_unit(i2)==0) then
       file_index=i2
       of_unit(file_index)=20+i2
       exit
     endif
   enddo
   of_name=local_file_name
   opened_of(file_index)=local_file_name
   !
   if (index(mode,'a')==0) open(of_unit(file_index),file=trim(opened_of(file_index)))
   if (index(mode,'a')/=0) open(of_unit(file_index),file=trim(opened_of(file_index)),position='append')
   if (index(mode,"t")>0) call write_the_logo(of_unit(file_index),'#')

   of_unit(file_index)=-of_unit(file_index)
   !
#if defined _NC_OUTPUT
   call io_control(ACTION=OP_WR,COM=NONE,SEC=(/1/),ID=of_nc_IDs(file_index))
   io_out=io_connect(desc=trim(of_nc_name),type=2,   ID=of_nc_IDs(file_index))
   of_nc_pos(file_index)=0
#endif
   !
 else
   !
   ! Close file
   !  
   do i2=1,max_open_ofs
     if (index(opened_of(i2),trim(of_name))/=0) then
       close(iabs(of_unit(i2)))
       of_unit(i2)=0
#if defined _NC_OUTPUT
       call io_control(ACTION=WR_CL,COM=NONE,SEC=(/1/),ID=of_nc_IDs(i2))
       call io_disconnect(ID=of_nc_IDs(i2)) 
#endif
       if (.not.any(of==of_name)) then
         do i3=1,nofs
           if (len_trim(of(i3))==0) then
             of(i3)=opened_of(i2)
             exit
           endif
         enddo
       endif
       opened_of(i2)=' '
       exit
     endif
   enddo
   !
 endif
 !
end subroutine of_open_close
!
!
subroutine write_the_logo(unit_,comment_)
 !
 use LIVE_t,       ONLY:LIVE_message
 use LOGO,         ONLY:pickup_a_logo,logo_line,n_logo_lines
 use pars,         ONLY:schlen
 use com,          ONLY:repfile
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
 if (unit_==11) open(unit=11,file=trim(repfile),position='append')
 !
 do i1=1,n_logo_lines
   if (unit_/=6) write (unit_,trim(space_)) logo_line(i1)
   if (unit_==6.and.i1>=n_logo_lines-3) cycle
   if (unit_==6) call LIVE_message(logo_line(i1),"n","","%s",CPU_TIME=.false.,CPU_ID=.false.)
 enddo
 if (unit_/=6.and.trim(comment_)=='')  write (unit_,'(a)') ' '
 if (unit_/=6.and.trim(comment_)=='#') write (unit_,'(a)') '#'
 !
 if (unit_==11) close(11)
 !
end subroutine write_the_logo
!
!
subroutine rename_file(name)
 use pars,       ONLY:schlen,lchlen
 implicit none
 character(lchlen):: name
 character(lchlen):: ch 
 integer          :: i1
 logical, external:: file_exists
 if (.not.file_exists(name)) return
 i1=0
 ch=name
 do while (file_exists(ch))
   i1=i1+1
   if (i1< 99) write (ch,'(2a,i2.2)') trim(name),'_',i1
   if (i1> 99) write (ch,'(2a,i3.3)') trim(name),'_',i1
 enddo
 name=ch
end subroutine rename_file
!
!
subroutine warning(mesg)
 use LIVE_t,     ONLY:LIVE_message
 use stderr,     ONLY:STRING_pack
 use com,        ONLY:msg
 implicit none
 character(*) :: mesg
 call msg('nr',STRING_pack('[WARNING] ',trim(mesg)))
 call LIVE_message(STRING_pack('[WARNING] ',trim(mesg)),"n","","%s")
end subroutine warning
!
!
subroutine error(mesg)
 use LIVE_t,     ONLY:LIVE_message,live_timing_is_on
 use stderr,     ONLY:STRING_pack
 use com,        ONLY:secnm,msg
#if defined _MPI
 use parallel_m, ONLY:mpi_comm_world,ncpu
 integer         :: ierr
#endif
 character(*)    :: mesg
 !
 live_timing_is_on=.FALSE.
 !
 if (len_trim(secnm)>0) then
   call msg('nr',STRING_pack('[ERROR] STOP signal received while in :',trim(secnm)))
   call LIVE_message(STRING_pack('[ERROR] STOP signal received while in :',trim(secnm)),"nn","","%s")
 endif
 call msg('nr',STRING_pack('[ERROR] ',trim(mesg)))
 call LIVE_message(STRING_pack('[ERROR] ',trim(mesg)),"n","","%s")
 call LIVE_message("","n","","%s")
 flush(6)
 !
#if defined _MPI
 if (ncpu>1) call MPI_Abort(mpi_comm_world,1,ierr)
#endif
 stop
 !
end subroutine error
!
!
subroutine msg_deliver(of_name,formatted_msg)
 use pars,      ONLY:max_open_ofs
 use com,       ONLY:opened_of,repfile,of_unit
 implicit none
 character(*) :: of_name,formatted_msg
 integer      :: i_unit
 do i_unit=1,max_open_ofs
   if (index(opened_of(i_unit),trim(of_name))==0) cycle
   if (of_unit(i_unit)<=0) cycle
   if (of_unit(i_unit)==11) open(unit=11,file=trim(repfile),position='append')
   write (of_unit(i_unit),'(a)') trim(formatted_msg)
   if (of_unit(i_unit)==11) close(11)
   of_unit(i_unit)=-of_unit(i_unit)
 enddo
 !
end subroutine msg_deliver
!
!
subroutine msg_manager(how)
 !
 ! how = '(n)s(n)' '(n)r(n)' '(n)l(n)' 'o PATTERN'
 !
 ! r(eport)
 ! s(tderr)
 ! oN=msgunits(N) 
 !
 use pars,    ONLY:schlen,lchlen
 use stderr,  ONLY:STRING_split,STRING_remove
 use com,     ONLY:terminator,opened_of,of_unit,max_open_ofs,write_to_report,jobstr
 !
 implicit none
 !
 character(*) how
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
 ! o. files
 !
 if (how(:1)=="o") then
   call STRING_split(how,ch_piece)
   do i2=2,10
     if (len_trim(ch_piece(i2))==0) cycle
     do i1=1,max_open_ofs-1
       if (len_trim(opened_of(i1))==0) cycle
       stripped_file_name=STRING_remove(opened_of(i1),trim(jobstr))
       if (index( stripped_file_name,trim(ch_piece(i2)) )/=0) then
         of_unit(i1) =-of_unit(i1)
       endif
     enddo
   enddo
   return
 endif
 !
 ! Report
 !
 if (index(how,'r' )/=0.and.write_to_report) then
   of_unit(max_open_ofs) =-of_unit(max_open_ofs)
   if (index(how,'nr')/=0) terminator(1)='(/'
   if (index(how,'rn')/=0) terminator(2)='/)'
 endif
 !
 ! Log 
 !
 if (index(how,'l' )/=0) then
   terminator(3:4)=' '
   if (index(how,'nl')/=0) terminator(3)='n'
   if (index(how,'ln')/=0) terminator(4)='n'
 endif
 !
end subroutine msg_manager
!
! FUNCTIONS
!===========
!
logical function OF_exists(name)
 use pars,      ONLY:max_open_ofs
 use com,       ONLY:opened_of
 implicit none
 character(*) ::name
 integer      ::i_f
 OF_exists=.false. 
 do i_f=1,max_open_ofs
   if (index(opened_of(i_f),name)/=0) OF_exists=.TRUE.
 enddo
end function OF_exists
!
!
logical function file_exists(name)
 implicit none
 character(*) name
 file_exists=.false.
 if (len_trim(name)==0) return
 inquire(file=name,exist=file_exists)
end function file_exists
!
!
function composed_fmt(basic_fmt,n,USE_TABS,INDENT)
 !
 use pars,   ONLY:lchlen
 use com,    ONLY:terminator,depth
 use stderr, ONLY:STRING_pack,of_tabs
 !
 implicit none
 !
 character(lchlen) :: composed_fmt
 !
 character(*)     :: basic_fmt
 integer          :: n,INDENT
 logical          :: USE_TABS
 ! 
 ! Work Space
 !
 integer           :: i1
 character(lchlen) :: lch
 !
 if (USE_TABS) then
   composed_fmt=STRING_pack(terminator(1),'a,')
   lch=composed_fmt
   do i1=1,n-1
     write (lch,'(2a,i3.3,3a)') trim(composed_fmt),'t',of_tabs(i1)+2+INDENT,',',basic_fmt,','
     composed_fmt=lch
   enddo
   write (composed_fmt,'(2a,i3.3,3a)') trim(lch),'t',of_tabs(n)+2+INDENT,',',basic_fmt,trim(terminator(2))
 else
   write (composed_fmt,'(a,i2.2,a,i5.4,4a)') trim(terminator(1)),depth+1,'x,a,',n,'(',basic_fmt,',1x)',trim(terminator(2))
   if (INDENT==0) write (composed_fmt,'(2a,i5.4,4a)')  trim(terminator(1)),'a,',n,'(',basic_fmt,',1x)',trim(terminator(2))
 endif
 if (n<0) write (composed_fmt,'(a,i5.4,3a)') '(a,',iabs(n),'(',basic_fmt,',1x))'
end function composed_fmt
!
!
function filename(name)
 use stderr,     ONLY:STRING_split,STRING_remove
 use pars,       ONLY:schlen,lchlen
 character(lchlen) :: filename
 character(*)      :: name
 integer           :: N
 character(schlen) :: names(20)
 character(lchlen) :: name_
 name_=name
 name_=STRING_remove(name_,"/",replace=" ")
 call STRING_split(name_,names,n_non_empty_strings=N)
 filename=trim(names(N))
end function filename
