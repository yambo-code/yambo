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
subroutine msg_characters_vector_int(how,mesg,val,USE_TABS,INDENT)
 !
 use pars,         ONLY:lchlen,max_open_ofs
 use stderr,       ONLY:c_print,write_to_log,log_as_a_file
 use LIVE_t,       ONLY:LIVE_message
 use com,          ONLY:repfile,repfile_unit,terminator,of_unit
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
 integer          :: i1,i2,l_indent,irep
 character(lchlen):: msgfmt,lch
 character(2)     :: dots_string
 logical          :: l_tabs
 !
 if (len_trim(how)==0) return
 dots_string=""
 if(len_trim(val(1))>0) dots_string=": "
 call msg_manager(how,irep)
 l_tabs=.false.
 l_indent=-1
 if (present(USE_TABS)) l_tabs   = USE_TABS
 if (present(INDENT))   l_indent = INDENT
 msgfmt=composed_fmt('a',size(val),l_tabs,l_indent)
 if(trim(msgfmt)=="(/00x,a, 01(a,1x))") msgfmt="(/99x,a, 01(a,1x))"
 do i1=1,max_open_ofs-irep
   if (of_unit(i1)<=0) cycle
   write (of_unit(i1),trim(msgfmt)) mesg//trim(dots_string),(trim(val(i2)),i2=1,size(val))
   if (of_unit(i1)/=repfile_unit) of_unit(i1)=-of_unit(i1)
   if (of_unit(i1)/=repfile_unit) call of_netcdf_shadow_char(i1,size(val),val)
#if defined _YAML_OUTPUT
   if(len_trim(val(1))> 0) call yaml_map(mesg,val,unit=of_yaml_IDs(i1))
   if(len_trim(val(1))<=0) call yaml_comment(mesg,unit=of_yaml_IDs(i1))
#endif
 enddo
 if (index(how,'o')>0.or.index(how,'O')>0) return
 if (.not.write_to_log.or.(index(how,'s')==0.and.index(how,'l')==0)) return
 msgfmt=composed_fmt('a',-size(val),.false.,-1)
 write (lch,trim(msgfmt)) mesg,(trim(val(i2)),i2=1,size(val))
 if (index(how,'l')/=0.and..not.log_as_a_file) &
 &     call c_print(terminator(3),lch,terminator(4),"%s")
 if (index(how,'s')/=0) call LIVE_message(trim(lch),"n","","%s")
 !
end subroutine
!
!
subroutine msg_integers_vector_int(how,mesg,val,USE_TABS,INDENT)
 !
 use pars,         ONLY:lchlen,max_open_ofs
 use stderr,       ONLY:gen_fmt
 use stderr,       ONLY:c_print,write_to_log,log_as_a_file
 use LIVE_t,       ONLY:LIVE_message
 use com,          ONLY:repfile,repfile_unit,of_unit,terminator
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
 integer,optional :: INDENT
 logical,optional :: USE_TABS
 ! 
 ! Work Space
 !
 integer :: i1,l_indent,irep
 logical :: l_tabs
 character(lchlen)::lch,msgfmt
 !
 if (len_trim(how)==0) return
 call msg_manager(how,irep)
 l_tabs    = .false.
 l_indent  = -1
 if (present(USE_TABS))   l_tabs   = USE_TABS
 if (present(INDENT))     l_indent = INDENT
 msgfmt=composed_fmt(trim(gen_fmt(val)),size(val),l_tabs,l_indent)
 do i1=1,max_open_ofs-irep
   if (of_unit(i1)<=0) cycle
   write (of_unit(i1),trim(msgfmt)) mesg//": ",val
   if (abs(of_unit(i1))/=repfile_unit) of_unit(i1)=-of_unit(i1)
#if defined _YAML_OUTPUT
   call yaml_map(mesg,val,unit=of_yaml_IDs(i1))
   !write(*,*) 'msg_int',trim(mesg),val 
#endif
 enddo
 if (index(how,'o')>0.or.index(how,'O')>0) return
 if (.not.write_to_log.or.(index(how,'s')==0.and.index(how,'l')==0)) return
 msgfmt=composed_fmt(trim(gen_fmt(val)),-size(val),.false.,-1)
 write (lch,trim(msgfmt)) mesg,val 
 if (index(how,'l')/=0.and..not.log_as_a_file) &
 &    call c_print(terminator(3),lch,terminator(4),"%s")
 if (index(how,'s')/=0) call LIVE_message(trim(lch),"n","","%s")
 !
end subroutine
!
!
subroutine msg_reals_vector_int(how,mesg,val,USE_TABS,INDENT,EXT_FORMAT,formatted_msg)
 !
 use pars,         ONLY:SP,schlen,lchlen,max_open_ofs
 use stderr,       ONLY:c_print,write_to_log,log_as_a_file,gen_fmt
 use LIVE_t,       ONLY:LIVE_message
 use com,          ONLY:repfile,repfile_unit,of_unit,terminator
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
 real(SP)         :: val(:)
 integer,optional :: INDENT
 logical,optional :: USE_TABS
 logical,optional :: EXT_FORMAT
 character(lchlen), optional :: formatted_msg
 ! 
 ! Work Space
 !
 integer  :: i1,l_indent,imsg,irep
 character(schlen):: gen_fmt_local
 character(lchlen):: lch,msgfmt
 logical          :: l_tabs,l_ext_fmt
 !
 if (len_trim(how)==0) return
 call msg_manager(how,irep)
 l_ext_fmt = .false.
 l_tabs    = .false.
 l_indent  = -1
 if (present(EXT_FORMAT)) l_ext_fmt = EXT_FORMAT
 if (present(USE_TABS))   l_tabs    = USE_TABS
 if (present(INDENT))     l_indent  = INDENT
 if (.not.l_ext_fmt) msgfmt=composed_fmt(trim(gen_fmt(r_v=val)),size(val),l_tabs,l_indent)
 if (     l_ext_fmt) then
   write(gen_fmt_local,'(a)') 'F11.06'
   msgfmt=composed_fmt(trim(gen_fmt_local),size(val),l_tabs,l_indent)
 endif
 do i1=1,max_open_ofs-irep
   if (of_unit(i1)<=0) cycle
   if (present(formatted_msg)) then
     write (formatted_msg,trim(msgfmt)) mesg//": ",val
   else
     write (of_unit(i1),trim(msgfmt)) mesg//": ",val
     if (of_unit(i1)/=repfile_unit) of_unit(i1)=-of_unit(i1)
   endif
   if (of_unit(i1)/=repfile_unit) call of_netcdf_shadow_real(i1,mesg,val,size(val))
#if defined _YAML_OUTPUT
   call yaml_map(mesg,val,unit=of_yaml_IDs(i1))
   !write(*,*) 'msg_real',trim(mesg),val 
#endif
 enddo
 if (index(how,'o')>0.or.index(how,'O')>0) return
 if (.not.write_to_log.or.(index(how,'s')==0.and.index(how,'l')==0)) return
 if (.not.l_ext_fmt) msgfmt=composed_fmt(trim(gen_fmt(r_v=val)),-size(val),.false.,-1)
 if (     l_ext_fmt) then
   write(gen_fmt_local,'(a)') 'F11.06'
   msgfmt=composed_fmt(trim(gen_fmt_local),-size(val),.false.,-1)
 endif
 write (lch,trim(msgfmt)) mesg//": ",val 
 if (index(how,'l')/=0.and..not.log_as_a_file) call &
&       c_print(terminator(3),lch,terminator(4),"%s")
 if (index(how,'s')/=0) call LIVE_message(trim(lch),"n","","%s")
 !
end subroutine
