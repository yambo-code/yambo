!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team
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
subroutine IO_and_Messaging_switch(what)
 !
 use pars,           ONLY:schlen
 use IO_m,           ONLY:IO_write,IO_read,Fragmented_IO_save,Fragmented_IO
 use com,            ONLY:write_to_report
 use parallel_m,     ONLY:local_master_cpu,myid,master_cpu
 use stderr,         ONLY:string_split,write_to_log
 use fragments,      ONLY:Parallel_Fragments,last_syncronized
 use timing,         ONLY:live_timing_is_on
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
   do ich=1,10
     !
     ! Complete Reset
     !
     if (trim(ch_piece(ich))=="RESET") then
       write_to_log   =master_cpu
       write_to_report=master_cpu
       IO_write       =local_master_cpu(myid+1)
       IO_read        =.true.
       Fragmented_IO  =Fragmented_IO_save
       Parallel_Fragments  =.FALSE.
       last_syncronized    =0
       live_timing_is_on   =.TRUE.
       return
     endif
     !
     ! Messages
     !
     if (trim(ch_piece(ich))=="-report") write_to_report=.false.
     if (trim(ch_piece(ich))=="-log"   ) write_to_log=.false.
     if (trim(ch_piece(ich))=="-timing") live_timing_is_on=.false.
     if (trim(ch_piece(ich))=="+report") write_to_report=master_cpu
     if (trim(ch_piece(ich))=="+log"   ) write_to_log=master_cpu
     if (trim(ch_piece(ich))=="+timing") live_timing_is_on=.true.
     !
     ! I/O
     !
     if (trim(ch_piece(ich))=="-io_in")  IO_read=.false.
     if (trim(ch_piece(ich))=="-io_out") IO_write=.false.
     if (trim(ch_piece(ich))=="+io_in")  IO_read=.true.
     if (trim(ch_piece(ich))=="+io_out") IO_write=local_master_cpu(myid+1)
     !
     if (trim(ch_piece(ich))=="+all_write_fragments") then
       Parallel_Fragments=.TRUE.
       IO_write          =.TRUE.
       last_syncronized    =0
     endif
     if (trim(ch_piece(ich))=="-all_write_fragments") then
       Parallel_Fragments=.FALSE.
       IO_write          =local_master_cpu(myid+1)
     endif
     !
     if (trim(ch_piece(ich))=="+DB_is_fragmented") Fragmented_IO=.TRUE.
     if (trim(ch_piece(ich))=="-DB_is_fragmented") Fragmented_IO=Fragmented_IO_save
     !
   enddo
 endif
 !
end subroutine IO_and_Messaging_switch
