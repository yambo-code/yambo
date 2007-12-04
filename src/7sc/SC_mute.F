!
! Copyright (C) 2000-2005 A. Marini and the SELF team
!         http://www.fisica.uniroma2.it/~self
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
subroutine SC_mute(what)
 !
 use pars,           ONLY:schlen
 use IO_m,           ONLY:write_to_dbs,read_dbs
 use com,            ONLY:write_to_report
 use par_proc_m,     ONLY:master_node
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
 if (len_trim(what)>0) then
   call string_split(what,ch_piece)
   !
   do ich=1,10
     if (trim(ch_piece(ich))=="report") write_to_report=.false.
     if (trim(ch_piece(ich))=="log")    write_to_log=.false.
     if (trim(ch_piece(ich))=="io_in")  read_dbs=.false.
     if (trim(ch_piece(ich))=="io_out") write_to_dbs=.false.
   enddo
 else
   write_to_log=.true.
   write_to_report=.true.
   write_to_dbs=master_node
   read_dbs=.true.
 endif
 !
end subroutine SC_mute
