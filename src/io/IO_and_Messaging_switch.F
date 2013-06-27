!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team
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
subroutine IO_and_Messaging_switch(what,CONDITION)
 !
 use pars,           ONLY:schlen
 use IO_m,           ONLY:IO_write,IO_read,Fragmented_IO_save,Fragmented_IO
 use com,            ONLY:write_to_report,write_to_of
 use parallel_m,     ONLY:IO_write_default,myid,master_cpu
 use stderr,         ONLY:string_split
 use fragments,      ONLY:Parallel_Fragments,last_syncronized
 use LIVE_t,         ONLY:live_timing_is_on
 use parser_m,       ONLY:parser
 use stderr,         ONLY:write_fragments_IO_log
 implicit none
 !
 character(*)      :: what
 logical, optional :: CONDITION
 !
 ! Work Space
 !
 integer          ::ich
 character(schlen)::ch_piece(10)
 logical          ::local_TRUE,local_FALSE,local_no_WRITE,local_master
 !
 call parser('SkipWRITE',local_no_WRITE)
 !
 local_TRUE  =.TRUE.
 local_FALSE =.FALSE.
 local_master=master_cpu
 if (present(CONDITION)) then
   local_TRUE =CONDITION
   local_FALSE=CONDITION
   local_master=CONDITION
 endif
 !
 if (len_trim(what)>0) then
   call string_split(what,ch_piece)
   do ich=1,10
     !
     ! Complete Reset
     !
     if (trim(ch_piece(ich))=="RESET") call SET_logicals()
     !
     ! Messages
     !
     if (trim(ch_piece(ich))=="-report") write_to_report=local_FALSE
     if (trim(ch_piece(ich))=="-timing") live_timing_is_on=local_FALSE
     if (trim(ch_piece(ich))=="+report") write_to_report=local_master
     if (trim(ch_piece(ich))=="+timing") live_timing_is_on=local_TRUE
     !
     ! Ouput files
     !
     if (trim(ch_piece(ich))=="-output") write_to_of=local_FALSE
     if (trim(ch_piece(ich))=="+output") write_to_of=local_TRUE
     !
     ! I/O
     !
     if (trim(ch_piece(ich))=="-io_in" ) IO_read=local_FALSE
     if (trim(ch_piece(ich))=="-io_out") IO_write=local_FALSE
     if (trim(ch_piece(ich))=="+io_in" ) IO_read=local_TRUE
     if (trim(ch_piece(ich))=="+io_out") then
       if (local_no_WRITE)      IO_write=.FALSE.
       if (.not.local_no_WRITE) IO_write=local_TRUE
     endif
     !
     if (trim(ch_piece(ich))=="+all_write_fragments") then
       Parallel_Fragments=local_TRUE
       IO_write          =local_TRUE
       last_syncronized  =0
     endif
     if (trim(ch_piece(ich))=="-all_write_fragments") then
       Parallel_Fragments=local_FALSE
       IO_write          =IO_write_default(myid+1)
     endif
     !
     if (trim(ch_piece(ich))=="+DB_is_fragmented") Fragmented_IO=local_TRUE
     if (trim(ch_piece(ich))=="-DB_is_fragmented") Fragmented_IO=Fragmented_IO_save
     !
     if (trim(ch_piece(ich))=="+fragments_IO_log") write_fragments_IO_log=local_TRUE
     if (trim(ch_piece(ich))=="-fragments_IO_log") write_fragments_IO_log=local_FALSE
     !
   enddo
 endif
 !
end subroutine IO_and_Messaging_switch
