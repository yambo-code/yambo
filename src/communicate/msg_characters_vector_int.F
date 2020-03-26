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
subroutine msg_characters_vector_int(how,mesg,val,USE_TABS,INDENT)
 !
 use pars,         ONLY:lchlen,max_open_ofs,repfile_index
 use stderr,       ONLY:c_print,write_to_log,log_as_a_file,STRING_remove
 use LIVE_t,       ONLY:LIVE_message
 use com,          ONLY:repfile,terminator,of_unit
 use com_interfcs, ONLY:composed_fmt
#if defined _YAML_OUTPUT
 use com,          ONLY:of_yaml_IDs
 use yaml_output,  ONLY:yaml_map,yaml_comment
#endif
 !
 implicit none
 !
 character(*)     :: mesg
 character(*)     :: how
 character(*)     :: val(:)
 integer,optional :: INDENT
 logical,optional :: USE_TABS
 ! 
 ! Work Space
 !
 integer          :: i1,i2,l_indent,i1_limits(2)
 character(lchlen):: msgfmt,lch,yaml_mesg,yaml_val(size(val))
 logical          :: l_tabs,msg_for_out,msg_for_rep,msg_for_log
#if defined _YAML_OUTPUT
 logical          :: l_yaml_map,l_yaml_com
#endif
 !
 if (len_trim(how)==0) return
 ! switches to positive the value of of_unit which matches the given filename
 call msg_manager(how,msg_for_out,msg_for_rep,msg_for_log)
 l_tabs=.false.
 l_indent=-1
 if (present(USE_TABS)) l_tabs   = USE_TABS
 if (present(INDENT))   l_indent = INDENT
 msgfmt=composed_fmt('a',size(val),l_tabs,l_indent)
 if(trim(msgfmt)=="(/00x,a, 01(a,1x))") msgfmt="(/99x,a, 01(a,1x))"
#if defined _YAML_OUTPUT
 l_yaml_map= len_trim(val(1))> 0
 l_yaml_com= len_trim(val(1))<=0 .and. len_trim(mesg)>1 .and.              &
 &           index(trim(mesg),'[0')==0 .and. index(trim(mesg),'[1')==0 .and. &
 &           index(trim(mesg),'==')==0
 do i1=1,size(val)
   yaml_val(i1)=STRING_remove(STRING_remove(trim(adjustl(val(i1))),'[',replace="("),']',replace=")")
 enddo
#endif
 !
 ! Write to o-file or to report
 !
 i1_limits=(/1,-1/)
 if(msg_for_out) i1_limits=(/1,max_open_ofs/)
 if(msg_for_rep) i1_limits=(/repfile_index,repfile_index/)
 do i1=i1_limits(1),i1_limits(2)
   if (of_unit(i1)<=0) cycle
   if (len_trim(mesg)>0.and.len_trim(val(1))>0.and.trim(mesg)/="#") then
     write (of_unit(i1),trim(msgfmt)) mesg//": ",(trim(val(i2)),i2=1,size(val))
   else
     write (of_unit(i1),trim(msgfmt)) mesg,(trim(val(i2)),i2=1,size(val))
   endif
   ! after writing the value of of_unit is switched back to negative
   of_unit(i1)=-of_unit(i1)
   if (.not.msg_for_rep) call of_netcdf_shadow_char(i1,size(val),val)
#if defined _YAML_OUTPUT
   yaml_mesg=STRING_remove(STRING_remove(trim(adjustl(mesg)),'[',replace='('),']',replace=')')
   if(l_yaml_map) call yaml_map(trim(yaml_mesg),yaml_val,unit=of_yaml_IDs(i1))
   if(l_yaml_com) call yaml_comment(trim(mesg),unit=of_yaml_IDs(i1))
#endif
 enddo
 if (msg_for_out) return
 !
 ! Write to logfile or to report
 !
 if (.not.write_to_log.or.(index(how,'s')==0.and.index(how,'l')==0)) return
 msgfmt=composed_fmt('a',-size(val),.false.,-1)
  if (len_trim(mesg)>0.and.len_trim(val(1))>0.and.trim(mesg)/="#") then
   write (lch,trim(msgfmt)) mesg//": ",(trim(val(i2)),i2=1,size(val))
 else
   write (lch,trim(msgfmt)) mesg,(trim(val(i2)),i2=1,size(val))
 endif
 if (index(how,'l')/=0.and..not.log_as_a_file) call c_print(terminator(3),lch,terminator(4),"%s")
 if (index(how,'s')/=0) call LIVE_message(trim(lch),"n","","%s")
 !
end subroutine
!
subroutine add_the_semi_column(string,n,string_plus)
 use pars,      ONLY:lchlen
 implicit none
 character(*)      :: string
 character(lchlen) :: string_plus
 integer           :: n
 n=len_trim(string)
 string_plus=string
 if (n>0) then
   string_plus=string//": "
   n=len(string)+2
 endif
end subroutine
!
subroutine fix_the_new_line(how,string)
 implicit none
 integer      :: n
 character(*) :: how,string
 n=len_trim(string)
 if (index(how,"n")>0) then
   string=trim(string(:n-2))//',a'//trim(string(n-1:n))
 else
   string=trim(string(:n-1))//',a'//trim(string(n:n))
 endif
end subroutine

