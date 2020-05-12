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
subroutine msg_integers_vector_int(how,mesg,val,after_msg,USE_TABS,INDENT)
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
 character(*)     :: mesg
 character(*)     :: how
 integer          :: val(:)
 character(*), optional :: after_msg
 integer,optional :: INDENT
 logical,optional :: USE_TABS
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
   if(     log_as_a_file) flush(logfile_unit)
 endif
 if (STD_msg) call LIVE_message(trim(lch),"n","","%s")
 !
end subroutine
