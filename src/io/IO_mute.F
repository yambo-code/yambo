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
subroutine IO_mute(what)
 !
 use pars,           ONLY:schlen
 use IO_m,           ONLY:IO_write,IO_read
 use com,            ONLY:write_to_report
 use par_proc_m,     ONLY:master_cpu
 use stderr,         ONLY:string_split,write_to_log
 implicit none
 !
 character(*) :: what
 !
 ! Work Space
 !
 integer          ::ich
 character(schlen)::ch_piece(10)
 !
 write_to_log   =master_cpu
 write_to_report=master_cpu
 IO_write       =master_cpu
 IO_read        =.true.
 !
 if (len_trim(what)>0) then
   call string_split(what,ch_piece)
   do ich=1,10
     if (trim(ch_piece(ich))=="report".or.trim(ch_piece(ich))=="all") write_to_report=.false.
     if (trim(ch_piece(ich))=="log"   .or.trim(ch_piece(ich))=="all") write_to_log=.false.
     if (trim(ch_piece(ich))=="io_in" .or.trim(ch_piece(ich))=="all") IO_read=.false.
     if (trim(ch_piece(ich))=="io_out".or.trim(ch_piece(ich))=="all") IO_write=.false.
   enddo
 endif
 !
end subroutine IO_mute
