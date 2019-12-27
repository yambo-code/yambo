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
module com
 !
 use pars,          ONLY:SP,lchlen,schlen,max_n_ofs,max_open_ofs,n_max_jobstr
 use com_interfcs,  ONLY:msg_characters_vector_int,msg_integers_vector_int,msg_reals_vector_int
 !
 implicit none
 !
 ! PATHS
 !
 character(lchlen)  :: core_io_path
 character(lchlen)  :: more_io_path
 character(lchlen)  :: com_path
 !
 character(lchlen)  :: repfile
 !
 integer            :: repfile_unit
 integer            :: inpfile_unit
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
 character(lchlen)  :: of_name(max_n_ofs)
 character(lchlen)  :: of_opened(max_open_ofs)
 integer            :: of_unit(max_open_ofs+2)
 integer            :: n_ofs_defined
#if defined _NC_OUTPUT
 integer            :: of_nc_IDs(max_open_ofs)
 integer            :: of_nc_pos(4,max_open_ofs)
 character(schlen)  :: of_nc_var_names(max_open_ofs)
#endif
#if defined _YAML_OUTPUT
 integer            :: of_yaml_IDs(max_open_ofs+2)
#endif
 !
 logical            :: write_to_of
 logical            :: write_to_report
 !
 interface
   !
   ! This interfaces is not really needed and could be easely
   ! dropped splitting of_open_close in two different subroutines:
   ! of_open and of_close
   !
   subroutine of_open_close(of_name,mode)
     character(*), intent(in)            :: of_name
     character(*), intent(in), optional  :: mode
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
     call msg_characters_vector_int(how,mesg,(/val/),USE_TABS=USE_TABS,INDENT=INDENT)
   end subroutine
   !
   subroutine msg_integer(how,mesg,val,USE_TABS,INDENT)
     character(*)     :: mesg
     character(*)     :: how
     integer          :: val
     integer,optional :: INDENT
     logical,optional :: USE_TABS
     call msg_integers_vector_int(how,mesg,(/val/),USE_TABS=USE_TABS,INDENT=INDENT)
   end subroutine
   !
   subroutine msg_real(how,mesg,val,USE_TABS,INDENT,EXT_FORMAT)
     character(*)     :: mesg
     character(*)     :: how
     real(SP)         :: val
     integer,optional :: INDENT
     logical,optional :: USE_TABS
     logical,optional :: EXT_FORMAT
     call msg_reals_vector_int(how,mesg,(/val/),USE_TABS=USE_TABS,INDENT=INDENT,EXT_FORMAT=EXT_FORMAT)
   end subroutine
   !
   subroutine msg_characters_vector(how,mesg,val,USE_TABS,INDENT)
     character(*)     :: mesg
     character(*)     :: how
     character(*)     :: val(:)
     integer,optional :: INDENT
     logical,optional :: USE_TABS
     call msg_characters_vector_int(how,mesg,val,USE_TABS=USE_TABS,INDENT=INDENT)
   end subroutine
   !
   subroutine msg_integers_vector(how,mesg,val,USE_TABS,INDENT)
     character(*)     :: mesg
     character(*)     :: how
     integer          :: val(:)
     integer,optional :: INDENT
     logical,optional :: USE_TABS
     call msg_integers_vector_int(how,mesg,val,USE_TABS=USE_TABS,INDENT=INDENT)
   end subroutine
   !
   subroutine msg_reals_vector(how,mesg,val,USE_TABS,INDENT,EXT_FORMAT,after_msg)
     character(*)     :: mesg
     character(*)     :: how
     real(SP)         :: val(:)
     integer,optional :: INDENT
     logical,optional :: USE_TABS
     logical,optional :: EXT_FORMAT
     character(schlen), optional :: after_msg
     call msg_reals_vector_int(how,mesg,(/val/),USE_TABS=USE_TABS,INDENT=INDENT,EXT_FORMAT=EXT_FORMAT,after_msg=after_msg)
   end subroutine
   !
end module com
