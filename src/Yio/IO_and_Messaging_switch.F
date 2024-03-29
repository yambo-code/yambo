!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine IO_and_Messaging_switch(what,CONDITION)
 !
 use pars,           ONLY:schlen
 use IO_m,           ONLY:IO_write,IO_read
 use com,            ONLY:write_to_report,write_to_of,fat_log
 use parallel_m,     ONLY:IO_write_default,myid,master_cpu
 use stderr,         ONLY:STRING_split,write_to_log,&
&                         write_fragments_IO_log,write_to_log_default
#if ! defined _io_lib
 use LIVE_t,         ONLY:live_timing_is_on
 use timing_m,       ONLY:timing
#endif
 !
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
#if ! defined _io_lib
 call timing('IO_and_Messaging_switch',OPR='start')
#endif
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
#if ! defined _io_lib
       live_timing_is_on_now =live_timing_is_on
#endif
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
#if ! defined _io_lib
       live_timing_is_on =live_timing_is_on_now
#endif
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
#if ! defined _io_lib
       live_timing_is_on =write_to_log_default
#endif
     endif
     !
     ! Mute
     !
     if (trim(ch_piece(ich))=="MUTE") then
       write_to_of       =.FALSE.
       write_to_report   =.FALSE.
       write_to_log      =.FALSE.
#if ! defined _io_lib
       live_timing_is_on =.FALSE.
#endif
     endif
     !
     ! Messages
     !
     if (trim(ch_piece(ich))=="-report") write_to_report=local_FALSE
     if (trim(ch_piece(ich))=="+report") write_to_report=local_master
#if ! defined _io_lib
     if (trim(ch_piece(ich))=="-timing") live_timing_is_on=local_FALSE
     if (trim(ch_piece(ich))=="+timing") live_timing_is_on=(local_TRUE.and.write_to_log_default)
#endif
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
     if (trim(ch_piece(ich))=="+fragments_IO_log") write_fragments_IO_log=(local_TRUE.and.write_to_log_default.and.fat_log)
     !
   enddo
   !
 endif
 !
#if ! defined _io_lib
 call timing('IO_and_Messaging_switch',OPR='stop')
#endif
 !
end subroutine IO_and_Messaging_switch
