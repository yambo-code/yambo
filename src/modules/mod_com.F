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
module com
 !
 use pars,       ONLY:lchlen,SP,schlen,nofs,max_open_ofs,n_max_jobstr
 use stderr,     ONLY:gen_fmt
 !
 implicit none
 !
 ! PATHS
 !
 character(lchlen)  :: core_io_path
 character(lchlen)  :: more_io_path
 character(lchlen)  :: com_path
 character(lchlen)  :: repfile
 !
 ! Path for the shifted grids
 !
 character(lchlen) :: grid_paths
 !
 character(3)      :: terminator(4)
 !
 ! JOB strings
 !
 integer            :: n_alt_jobstr
 character(lchlen)  :: jobstr
 character(lchlen)  :: alt_jobstr(n_max_jobstr)
 character(lchlen)  :: jobdir
 character(lchlen)  :: alt_jobdir(n_max_jobstr)
 !
 ! Sectioning
 !
 integer           :: depth,isec(5)
 character(1)      :: previous_secmode
 character(schlen) :: secnm
 ! 
 ! Output Files/Report File
 !
 integer, parameter :: num_of_alternative_locations=4+2*n_max_jobstr
 character(lchlen)  :: of(nofs)
 character(lchlen)  :: opened_of(max_open_ofs)
 integer            :: of_unit(max_open_ofs)
 logical            :: write_to_of
 logical            :: write_to_report
 !
 interface
   !
   subroutine of_open_close(of_name,mode)
     character(*)           :: of_name
     character(*),optional  :: mode
   end subroutine of_open_close
   !
 end interface
 !
 interface msg
   module procedure msg_novalue,                        &
   &                msg_logical,                        &
   &                msg_character,msg_characters_vector,&
   &                msg_integer,  msg_integers_vector,  &
   &                msg_real,     msg_reals_vector
 end interface
 !
 contains
   !
   !#######################
   ! INTERFACE TO MSG
   !#######################
   !
   subroutine msg_novalue(how,mesg)
     character(*)     :: mesg
     character(*)     :: how
     character        :: crap(1)
     integer          :: INDENT
     crap(1)=" "
     INDENT=-1
     if(len_trim(mesg)==0) then
       call msg_characters_vector(how," ",crap,INDENT=INDENT)
       return
     endif
     if (mesg(1:1)=="#") INDENT=0
     call msg_characters_vector(how,trim(mesg),crap,INDENT=INDENT)
   end subroutine
   !
   subroutine msg_logical(how,mesg,val,USE_TABS,INDENT)
     character(*)     :: mesg
     character(*)     :: how
     logical          :: val
     integer,optional :: INDENT
     logical,optional :: USE_TABS
     if (     val) call msg_character(how,mesg,"yes",USE_TABS=USE_TABS,INDENT=INDENT)
     if (.not.val) call msg_character(how,mesg,"no" ,USE_TABS=USE_TABS,INDENT=INDENT)
   end subroutine
   !
   subroutine msg_character(how,mesg,val,USE_TABS,INDENT)
     character(*)     :: mesg
     character(*)     :: how
     character(*)     :: val
     integer,optional :: INDENT
     logical,optional :: USE_TABS
     call msg_characters_vector(how,mesg,(/val/),USE_TABS=USE_TABS,INDENT=INDENT)
   end subroutine
   !
   subroutine msg_integer(how,mesg,val,USE_TABS,INDENT)
     character(*)     :: mesg
     character(*)     :: how
     integer          :: val
     integer,optional :: INDENT
     logical,optional :: USE_TABS
     call msg_integers_vector(how,mesg,(/val/),USE_TABS=USE_TABS,INDENT=INDENT)
   end subroutine
   !
   subroutine msg_real(how,mesg,val,USE_TABS,INDENT,EXT_FORMAT)
     character(*)     :: mesg
     character(*)     :: how
     real(SP)         :: val
     integer,optional :: INDENT
     logical,optional :: USE_TABS
     logical,optional :: EXT_FORMAT
     call msg_reals_vector(how,mesg,(/val/),USE_TABS=USE_TABS,INDENT=INDENT,EXT_FORMAT=EXT_FORMAT)
   end subroutine
   !
   subroutine msg_characters_vector(how,mesg,val,USE_TABS,INDENT)
     use stderr,   ONLY: c_print,write_to_log,log_as_a_file
     use LIVE_t,   ONLY: LIVE_message
     character(*)     :: mesg
     character(*)     :: how
     character(*)     :: val(:)
     integer,optional :: INDENT
     logical,optional :: USE_TABS
     ! 
     ! Work Space
     !
     integer          :: i1,i2,l_indent 
     character(lchlen):: msgfmt,lch
     logical          :: l_fmt
     !
     if (len_trim(how)==0) return
     call msg_manager(how)
     l_fmt=.false.
     l_indent=-1
     if (present(USE_TABS)) l_fmt=.true.
     if (present(INDENT)) l_indent=INDENT
     msgfmt=composed_fmt('a',size(val),l_fmt,l_indent)
     if(trim(msgfmt)=="(/00x,a, 01(a,1x))") msgfmt="(/99x,a, 01(a,1x))"
     do i1=1,max_open_ofs
       if (of_unit(i1)<=0) cycle
       if (of_unit(i1)==11) then
         if (len_trim(repfile)==0) cycle
         open(unit=11,file=trim(repfile),position='append')
       endif
       write (of_unit(i1),trim(msgfmt)) mesg,(trim(val(i2)),i2=1,size(val))
       if (of_unit(i1)==11) close(11)
       of_unit(i1)=-of_unit(i1)
     enddo
     if (index(how,'o')>0.or.index(how,'O')>0) return
     if (.not.write_to_log.or.(index(how,'s')==0.and.index(how,'l')==0)) return
     msgfmt=composed_fmt('a',-size(val),.false.,-1)
     write (lch,trim(msgfmt)) mesg,(trim(val(i2)),i2=1,size(val))
     if (index(how,'l')/=0.and..not.log_as_a_file) &
&       call c_print(terminator(3),lch,terminator(4),"%s")
     if (index(how,'s')/=0) call LIVE_message(trim(lch),"n","","%s")
   end subroutine
   !
   subroutine msg_integers_vector(how,mesg,val,USE_TABS,INDENT)
     use stderr,   ONLY: c_print,write_to_log,log_as_a_file
     use LIVE_t,   ONLY: LIVE_message
     character(*)     :: mesg
     character(*)     :: how
     integer          :: val(:)
     integer,optional :: INDENT
     logical,optional :: USE_TABS
     ! 
     ! Work Space
     !
     integer :: i1,l_indent
     logical :: l_fmt
     character(lchlen)::lch,msgfmt
     !
     if (len_trim(how)==0) return
     call msg_manager(how)
     l_fmt     = .false.
     l_indent  = -1
     if (present(USE_TABS))   l_fmt    = .true.
     if (present(INDENT))     l_indent = INDENT
     msgfmt=composed_fmt(trim(gen_fmt(val)),size(val),l_fmt,l_indent)
     do i1=1,max_open_ofs
       if (of_unit(i1)<=0) cycle
       if (of_unit(i1)==11) open(unit=11,file=trim(repfile),position='append')
       write (of_unit(i1),trim(msgfmt)) mesg,val
       if (of_unit(i1)==11) close(11)
       of_unit(i1)=-of_unit(i1)
     enddo
     if (index(how,'o')>0.or.index(how,'O')>0) return
     if (.not.write_to_log.or.(index(how,'s')==0.and.index(how,'l')==0)) return
     msgfmt=composed_fmt(trim(gen_fmt(val)),-size(val),.false.,-1)
     write (lch,trim(msgfmt)) mesg,val 
     if (index(how,'l')/=0.and..not.log_as_a_file) &
&       call c_print(terminator(3),lch,terminator(4),"%s")
     if (index(how,'s')/=0) call LIVE_message(trim(lch),"n","","%s")
   end subroutine
   !
   subroutine msg_reals_vector(how,mesg,val,USE_TABS,INDENT,EXT_FORMAT,formatted_msg)
     use stderr,   ONLY: c_print,write_to_log,log_as_a_file
     use LIVE_t,   ONLY: LIVE_message
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
     integer  :: i1,l_indent
     character(schlen):: gen_fmt_local
     character(lchlen):: lch,msgfmt
     logical          :: l_fmt,l_ext_fmt
     if (len_trim(how)==0) return
     call msg_manager(how)
     l_ext_fmt = .false.
     l_fmt     = .false.
     l_indent  = -1
     if (present(EXT_FORMAT)) l_ext_fmt = EXT_FORMAT
     if (present(USE_TABS))   l_fmt     = .true.
     if (present(INDENT))     l_indent  = INDENT
     if (.not.l_ext_fmt) write(*,*) trim(gen_fmt(r_v=val)),size(val),l_fmt,l_indent
     if (.not.l_ext_fmt) msgfmt=composed_fmt(trim(gen_fmt(r_v=val)),size(val),l_fmt,l_indent)
     if (.not.l_ext_fmt) write(*,*) 'done'
     if (     l_ext_fmt) then
       write(gen_fmt_local,'(a)') 'F11.06'
       msgfmt=composed_fmt(trim(gen_fmt_local),size(val),l_fmt,l_indent)
     endif
     do i1=1,max_open_ofs
       if (of_unit(i1)<=0) cycle
       if (of_unit(i1)==11) open(unit=11,file=trim(repfile),position='append')
       if (present(formatted_msg)) then
         write (formatted_msg,trim(msgfmt)) mesg,val
       else
         write (of_unit(i1),trim(msgfmt)) mesg,val
         if (of_unit(i1)==11) close(11)
         of_unit(i1)=-of_unit(i1)
       endif
     enddo
     if (index(how,'o')>0.or.index(how,'O')>0) return
     if (.not.write_to_log.or.(index(how,'s')==0.and.index(how,'l')==0)) return
     if (.not.l_ext_fmt) msgfmt=composed_fmt(trim(gen_fmt(r_v=val)),-size(val),.false.,-1)
     if (     l_ext_fmt) then
       write(gen_fmt_local,'(a)') 'F11.06'
       msgfmt=composed_fmt(trim(gen_fmt_local),-size(val),.false.,-1)
     endif
     write (lch,trim(msgfmt)) mesg,val 
     if (index(how,'l')/=0.and..not.log_as_a_file) call &
&       c_print(terminator(3),lch,terminator(4),"%s")
     if (index(how,'s')/=0) call LIVE_message(trim(lch),"n","","%s")
   end subroutine
   !
   character(lchlen) function composed_fmt(basic_fmt,n,USE_TABS,INDENT)
     use stderr, ONLY:STRING_pack,of_tabs
     character(*)     :: basic_fmt
     integer          :: n,INDENT
     logical          :: USE_TABS
     ! 
     ! Work Space
     !
     integer           :: i1
     character(lchlen) :: lch
     !
     if (USE_TABS) then
       composed_fmt=STRING_pack(terminator(1),'a,')
       lch=composed_fmt
       do i1=1,n-1
         write(*,*) i1,trim(composed_fmt),'t',of_tabs(i1)+2+INDENT,basic_fmt
         write (lch,'(2a,i3.3,3a)') trim(composed_fmt),'t',&
&                                   of_tabs(i1)+2+INDENT,',',basic_fmt,','
         composed_fmt=lch
       enddo
       write (composed_fmt,'(2a,i3.3,3a)') trim(lch),'t',of_tabs(n)+2+INDENT,&
&                                        ',',basic_fmt,trim(terminator(2))
     else
       write (composed_fmt,'(a,i2.2,a,i3.2,4a)') trim(terminator(1)),depth+1,'x,a,',n,&
&                                             '(',basic_fmt,',1x)',trim(terminator(2))
       if (INDENT==0) write (composed_fmt,'(2a,i2.2,4a)') &
&         trim(terminator(1)),'a,',n,'(',basic_fmt,',1x)',trim(terminator(2))
     endif
     if (n<0) write (composed_fmt,'(a,i3.2,3a)') '(a,',iabs(n),'(',basic_fmt,',1x))'
   end function
   !
   subroutine msg_manager(how)
     !
     ! how = '(n)s(n)' '(n)r(n)' '(n)l(n)' 'o PATTERN'
     !
     ! r(eport)
     ! s(tderr)
     ! oN=msgunits(N) 
     !
     use stderr,  ONLY:STRING_split,STRING_remove
     character(*) how
     ! 
     ! Work Space
     !
     integer          ::i1,i2
     character(schlen)::ch_piece(10)
     character(lchlen)::stripped_file_name
     !
     terminator(1)='('
     terminator(2)=')'
     !
     ! o. files
     !
     if (how(:1)=="o") then
       call STRING_split(how,ch_piece)
       do i2=2,10
         if (len_trim(ch_piece(i2))==0) cycle
         do i1=1,max_open_ofs-1
           if (len_trim(opened_of(i1))==0) cycle
           stripped_file_name=STRING_remove(opened_of(i1),trim(jobstr))
           if (index( stripped_file_name,trim(ch_piece(i2)) )/=0) then
             of_unit(i1) =-of_unit(i1)
           endif
         enddo
       enddo
       return
     endif
     !
     ! Report
     !
     if (index(how,'r' )/=0.and.write_to_report) then
       of_unit(max_open_ofs) =-of_unit(max_open_ofs)
       if (index(how,'nr')/=0) terminator(1)='(/'
       if (index(how,'rn')/=0) terminator(2)='/)'
     endif
     !
     ! Log 
     !
     if (index(how,'l' )/=0) then
       terminator(3:4)=' '
       if (index(how,'nl')/=0) terminator(3)='n'
       if (index(how,'ln')/=0) terminator(4)='n'
     endif
     !
   end subroutine
   !
end module com
