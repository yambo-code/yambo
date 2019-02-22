!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine IO_and_Messaging_switch(what,CONDITION)
 !
 use pars,           ONLY:schlen
 use IO_m,           ONLY:IO_write,IO_read
 use com,            ONLY:write_to_report,write_to_of
 use parallel_m,     ONLY:IO_write_default,myid,master_cpu
 use stderr,         ONLY:STRING_split,write_to_log
 use LIVE_t,         ONLY:live_timing_is_on,what_is_running
 use stderr,         ONLY:write_fragments_IO_log,write_to_log_default
 use timing_m,       ONLY:timing
 implicit none
 !
 character(*)      :: what
 logical, optional :: CONDITION
 !
 ! Work Space
 !
 integer          ::ich
 character(schlen)::ch_piece(10)
 logical          ::local_TRUE,local_FALSE,local_master,local_write_to_report
 !
 logical,    SAVE ::IO_read_now,IO_write_now,write_to_of_now,write_to_report_now,write_to_log_now,live_timing_is_on_now
 !
 call timing('IO_and_Messaging_switch',OPR='start')
 !
 local_TRUE            = .TRUE.
 local_FALSE           = .FALSE.
 local_master          = master_cpu
 local_write_to_report = write_to_report
 !
 if (present(CONDITION)) then
   local_TRUE  =CONDITION
   local_FALSE =CONDITION
   local_master=CONDITION
 endif
 !
 if (len_trim(what)>0) then
   !
   call STRING_split(what,ch_piece)
   !
   do ich=1,10
     !
     ! SAVEs used later for a RESTORE
     !
     if (trim(ch_piece(ich))=="SAVE") then
       IO_read_now           =IO_read
       IO_write_now          =IO_write
       write_to_of_now       =write_to_of
       write_to_report_now   =write_to_report
       write_to_log_now      =write_to_log
       live_timing_is_on_now =live_timing_is_on
     endif
     !
     ! Restore 
     !
     if (trim(ch_piece(ich))=="RESTORE") then
       IO_read           =IO_read_now
       IO_write          =IO_write_now
       write_to_of       =write_to_of_now
       write_to_report   =write_to_report_now
       write_to_log      =write_to_log_now
       live_timing_is_on =live_timing_is_on_now
     endif
     !
     ! Complete Reset
     !
     if (trim(ch_piece(ich))=="RESET") then
       IO_read           =.TRUE.
       IO_write          =IO_write_default(myid+1)
       write_to_of       =master_cpu
       write_to_report   =master_cpu
       write_to_log      =write_to_log_default
       live_timing_is_on =write_to_log_default
     endif
     !
     ! Mute
     !
     if (trim(ch_piece(ich))=="MUTE") then
       IO_read           =.FALSE.
       IO_write          =.FALSE.
       write_to_of       =.FALSE.
       write_to_report   =.FALSE.
       write_to_log      =.FALSE.
       live_timing_is_on =.FALSE.
     endif
     !
     ! Messages
     !
     if (trim(ch_piece(ich))=="-report") write_to_report=local_FALSE
     if (trim(ch_piece(ich))=="-timing") live_timing_is_on=local_FALSE
     if (trim(ch_piece(ich))=="+report") write_to_report=local_master
     if (trim(ch_piece(ich))=="+timing") live_timing_is_on=(local_TRUE.and.write_to_log_default)
     !
     ! Log 
     !
     if (trim(ch_piece(ich))=="-log") write_to_log=local_FALSE
     if (trim(ch_piece(ich))=="+log") write_to_log=(local_TRUE.and.write_to_log_default)
     !
     ! Ouput files
     !
     if (trim(ch_piece(ich))=="-output") write_to_of=local_FALSE
     if (trim(ch_piece(ich))=="+output") write_to_of=local_master
     !
     ! I/O
     !
     if (trim(ch_piece(ich))=="-io_in" ) IO_read=local_FALSE
     if (trim(ch_piece(ich))=="-io_out") IO_write=local_FALSE
     if (trim(ch_piece(ich))=="+io_in" ) IO_read=local_TRUE
     if (trim(ch_piece(ich))=="+io_out") IO_write=local_master
     if (trim(ch_piece(ich))=="-fragments_IO_log") write_fragments_IO_log=local_FALSE
     if (trim(ch_piece(ich))=="+fragments_IO_log") write_fragments_IO_log=(local_TRUE.and.write_to_log_default)
     !
   enddo
   !
 endif
 !
 ! YPP cannot set to .TRUE. the write_to_report
 !
 if (index(what_is_running,'YPP')>0) write_to_report = local_write_to_report
 !
 call timing('IO_and_Messaging_switch',OPR='stop')
 !
end subroutine IO_and_Messaging_switch
