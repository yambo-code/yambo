!
! License-Identifier: GPL
!
! Copyright (C) 2019 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
module com_interfcs
 !
 interface
   !
   subroutine msg_characters_vector_int(how,mesg,val,after_msg,USE_TABS,INDENT,MSG_string)
     use pars,     ONLY:lchlen
     character(*)                :: how
     character(*)                :: mesg
     character(*)                :: val(:)
     character(*),      optional :: after_msg
     logical,           optional :: USE_TABS
     integer,           optional :: INDENT
     character(lchlen), optional :: MSG_string
   end subroutine
   !
   subroutine msg_integers_vector_int(how,mesg,val,after_msg,USE_TABS,INDENT,MSG_string)
     use pars,     ONLY:lchlen
     character(*)                :: how
     character(*)                :: mesg
     integer                     :: val(:)
     character(*),      optional :: after_msg
     logical,           optional :: USE_TABS
     integer,           optional :: INDENT
     character(lchlen), optional :: MSG_string
   end subroutine
   !
   subroutine msg_reals_vector_int(how,mesg,val,after_msg,USE_TABS,INDENT,EXT_FORMAT,MSG_string)
     use pars,     ONLY:SP,lchlen
     character(*)                :: how
     character(*)                :: mesg
     real(SP)                    :: val(:)
     character(*),      optional :: after_msg
     logical,           optional :: USE_TABS
     integer,           optional :: INDENT
     logical,           optional :: EXT_FORMAT
     character(lchlen), optional :: MSG_string
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
   function fsync (fd) bind(c,name="fsync")
     use iso_c_binding, only: c_int
     integer(c_int), value :: fd
     integer(c_int) :: fsync
   end function fsync
   !
 end interface
 !
end module com_interfcs
