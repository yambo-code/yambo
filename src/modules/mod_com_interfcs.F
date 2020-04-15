!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
module com_interfcs
 !
 interface
   !
   subroutine msg_characters_vector_int(how,mesg,val,USE_TABS,INDENT)
     character(*)     :: mesg
     character(*)     :: how
     character(*)     :: val(:)
     integer,optional :: INDENT
     logical,optional :: USE_TABS
   end subroutine
   !
   subroutine msg_integers_vector_int(how,mesg,val,after_msg,USE_TABS,INDENT)
     character(*)     :: mesg
     character(*)     :: how
     integer          :: val(:)
     character(*), optional :: after_msg
     integer,optional :: INDENT
     logical,optional :: USE_TABS
   end subroutine
   !
   subroutine msg_reals_vector_int(how,mesg,val,after_msg,USE_TABS,INDENT,EXT_FORMAT)
     use pars,     ONLY:SP,schlen
     character(*)     :: mesg
     character(*)     :: how
     real(SP)         :: val(:)
     character(*), optional :: after_msg
     integer,optional :: INDENT
     logical,optional :: USE_TABS
     logical,optional :: EXT_FORMAT
   end subroutine
   !
   function composed_fmt(basic_fmt,n,USE_TABS,INDENT)
     use pars,   ONLY:lchlen
     character(lchlen) :: composed_fmt 
     character(*)      :: basic_fmt
     integer           :: n,INDENT
     logical           :: USE_TABS
   end function composed_fmt
   !
 end interface
 !
end module com_interfcs
