!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine msg_integers_vector_int(how,mesg,val,after_msg,USE_TABS,INDENT,MSG_string)
 !
 use pars,         ONLY:lchlen,schlen,max_open_ofs,repfile_index
 use stderr,       ONLY:gen_fmt,STRING_remove
 use stderr,       ONLY:c_print,write_to_log,log_as_a_file,logfile_unit
 use LIVE_t,       ONLY:LIVE_message
 use com,          ONLY:repfile,of_unit,terminator
 use com_interfcs, ONLY:composed_fmt
#if defined _YAML_OUTPUT
 use com,          ONLY:of_yaml_IDs
 use yaml_output,  ONLY:yaml_map
#endif
 !
 implicit none
 !
 character(*)                :: how
 character(*)                :: mesg
 integer                     :: val(:)
 character(*),      optional :: after_msg
 logical,           optional :: USE_TABS
 integer,           optional :: INDENT
 character(lchlen), optional :: MSG_string
 ! 
 ! Work Space
 !
 integer :: i1,l_indent,i1_limits(2),nm
 character(schlen):: after_msg_,yaml_after_msg
 character(lchlen):: msgfmt,lch,yaml_mesg,mesg_plus
 logical          :: l_tabs,OUT_msg,REP_msg,LOG_msg,STD_msg,&
&                    l_new_line(2) !left and right
 !
 if (len_trim(how)==0) return
 !
 ! General setup
 !
 after_msg_=""
 if(present(after_msg)) after_msg_=after_msg
 ! switches to positive the value of of_unit which matches the given filename
 call msg_manager(how,OUT_msg,REP_msg,LOG_msg,STD_msg,l_new_line)
 l_tabs    = .false.
 l_indent  = -1
 if (present(USE_TABS))   l_tabs   = USE_TABS
 if (present(INDENT))     l_indent = INDENT
 !
 ! Define format
 !
 msgfmt=composed_fmt(trim(gen_fmt(val)),size(val),l_tabs,l_indent)
 !
 if (len_trim(after_msg_)>0) call fix_the_new_line(how,msgfmt,l_new_line(2))
 !
 ! Write to o-file or to report
 !
 i1_limits=(/1,-1/)
 if(OUT_msg) i1_limits=(/1,max_open_ofs/)
 if(REP_msg) i1_limits=(/repfile_index,repfile_index/)
 !
 if (present(MSG_string)) then
   call add_the_semi_column(mesg,nm,mesg_plus)
   if (len_trim(after_msg_)==0) write (MSG_string,trim(msgfmt)) mesg_plus(1:nm),val
   if (len_trim(after_msg_)> 0) write (MSG_string,trim(msgfmt)) mesg_plus(1:nm),val,trim(after_msg_)
   return
 endif
 !
 do i1=i1_limits(1),i1_limits(2)
   !
   if (of_unit(i1)<=0) cycle
   !
   call add_the_semi_column(mesg,nm,mesg_plus)
   !
   if (len_trim(after_msg_)==0) write (of_unit(i1),trim(msgfmt)) mesg_plus(1:nm),val
   if (len_trim(after_msg_)> 0) write (of_unit(i1),trim(msgfmt)) mesg_plus(1:nm),val,trim(after_msg_)
   !
   ! after writing the value of of_unit is switched back to negative
   of_unit(i1)=-of_unit(i1)
   !
#if defined _YAML_OUTPUT
   yaml_mesg=STRING_remove(STRING_remove(trim(adjustl(mesg)),'[',replace='('),']',replace=')')
   if (len_trim(after_msg_)==0) then
     call yaml_map(trim(yaml_mesg),val,unit=of_yaml_IDs(i1))
   else
     yaml_after_msg=STRING_remove(STRING_remove(trim(adjustl(after_msg_)),'[',replace='('),']',replace=')')
     call yaml_map(trim(yaml_mesg),val,unit=of_yaml_IDs(i1),advance="no")
     call yaml_map("",(/trim(yaml_after_msg)/),unit=of_yaml_IDs(i1))
   endif
#endif
   !
 enddo
 if (OUT_msg) return
 !
 ! Write to logfile or to report
 !
 if (.not.write_to_log.or.(.not.STD_msg.and..not.LOG_msg)) return
 !
 msgfmt=composed_fmt(trim(gen_fmt(val)),-size(val),.false.,-1)
 !
 if (len_trim(after_msg_)>0) call fix_the_new_line(how,msgfmt,l_new_line(2))
 !
 call add_the_semi_column(mesg,nm,mesg_plus)
 !
 if (len_trim(after_msg_)==0) write (lch,trim(msgfmt)) mesg_plus(1:nm),val
 if (len_trim(after_msg_)> 0) write (lch,trim(msgfmt)) mesg_plus(1:nm),val,trim(after_msg_)
 !
 if (LOG_msg) then
   if(.not.log_as_a_file) call c_print(terminator(3),lch,terminator(4),"%s")
   if(     log_as_a_file) call yambo_flush(logfile_unit)
 endif
 if (STD_msg) call LIVE_message(trim(lch),"n","","%s")
 !
end subroutine
