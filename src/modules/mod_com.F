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
 character(lchlen)  :: of(nofs)
 character(lchlen)  :: opened_of(max_open_ofs)
 integer            :: of_unit(max_open_ofs)
#if defined _NC_OUTPUT
 integer, parameter :: of_nc_max_size=10000
 integer            :: of_nc_IDs(max_open_ofs)
 integer            :: of_nc_pos(max_open_ofs)
#endif
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
   function composed_fmt(basic_fmt,n,USE_TABS,INDENT)
     use pars,   ONLY:lchlen
     !
     character(lchlen) :: composed_fmt 
     !
     character(*)     :: basic_fmt
     integer          :: n,INDENT
     logical          :: USE_TABS
   end function composed_fmt
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
     logical          :: l_tabs
     !
     if (len_trim(how)==0) return
     call msg_manager(how)
     l_tabs=.false.
     l_indent=-1
     if (present(USE_TABS)) l_tabs   = USE_TABS
     if (present(INDENT))   l_indent = INDENT
     msgfmt=composed_fmt('a',size(val),l_tabs,l_indent)
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
     logical :: l_tabs
     character(lchlen)::lch,msgfmt
     !
     if (len_trim(how)==0) return
     call msg_manager(how)
     l_tabs    = .false.
     l_indent  = -1
     if (present(USE_TABS))   l_tabs   = USE_TABS
     if (present(INDENT))     l_indent = INDENT
     msgfmt=composed_fmt(trim(gen_fmt(val)),size(val),l_tabs,l_indent)
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
#if defined _NC_OUTPUT
     use netcdf,   ONLY: nf90_unlimited
     use IO_m,     ONLY: WR,NONE,io_control,def_variable_bulk,io_variable_bulk
#endif
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
     logical          :: l_tabs,l_ext_fmt
#if defined _NC_OUTPUT
     real(SP), allocatable :: nc_var(:,:)
#endif
     if (len_trim(how)==0) return
     call msg_manager(how)
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
#if defined _NC_OUTPUT
       if (abs(of_unit(i1))/=11) then
         allocate(nc_var(size(val),1))
         if(of_nc_pos(i1)==0) call def_variable_bulk(of_nc_IDs(i1),"output",1,(/size(val),nf90_unlimited/),SP)
         of_nc_pos(i1)=of_nc_pos(i1)+1
         nc_var(:,1)=val
         call io_control(ACTION=WR,COM=NONE,SEC=(/1/),ID=of_nc_IDs(i1))
         call io_variable_bulk(of_nc_IDs(i1),1,R2=nc_var,IPOS=(/1,of_nc_pos(i1)/))
         deallocate(nc_var)
       endif
#endif
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
end module com
