!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
! Copyright (C) 2002 M. Marques, A. Castro, A. Rubio, G. Bertsch
!
! Authors (see AUTHORS file for details): AM
!
module PARSER_lib
 !
 implicit none
 !
 private :: iparse_string, iparse_block_string
 !
 interface 
   !
   integer function iparse_init(file_in, file_out)
     character(*), intent(in)  :: file_in, file_out
   end function iparse_init
   !
   subroutine iparse_end()
   end subroutine iparse_end
   !
   integer function iparse_isdef(name)
     character(*), intent(in) :: name
   end function iparse_isdef
   !
   subroutine iparse_int(name, def, res)
     character(*), intent(in) :: name
     integer, intent(in)          :: def
     integer, intent(out)         :: res
   end subroutine iparse_int
   !
   subroutine iparse_double(name, def, res) 
     use pars, ONLY:DP
     character(*), intent(in) :: name
     real(DP), intent(in)          :: def
     real(DP), intent(out)         :: res
   end subroutine iparse_double
   !
   subroutine iparse_complex(name, def, res)
     use pars, ONLY:DP
     character(*), intent(in) :: name
     complex(DP), intent(in)       :: def
     complex(DP), intent(out)      :: res
   end subroutine iparse_complex
   !
   subroutine iparse_string(name, def, res)
     character(*), intent(in) :: name, def
     character(*), intent(out):: res
   end subroutine iparse_string
   !
   integer function iparse_block_n(name)
     character(*), intent(in) :: name
   end function iparse_block_n
   !
   subroutine iparse_block_int(name, l, c, res)
     character(*), intent(in) :: name
     integer, intent(in)          :: l, c
     integer, intent(out)         :: res
   end subroutine iparse_block_int
   !
   subroutine iparse_block_double(name, l, c, res)
     use pars, ONLY:DP
     character(*), intent(in) :: name
     integer, intent(in)          :: l, c
     real(DP), intent(out)         :: res
   end subroutine iparse_block_double
   !
   subroutine iparse_block_complex(name, l, c, res)
     use pars, ONLY:DP
     character(*), intent(in) :: name
     integer, intent(in)          :: l, c
     complex(DP), intent(out)      :: res
   end subroutine iparse_block_complex
   !
   subroutine iparse_block_string(name, l, c, res)
     character(*), intent(in) :: name
     integer, intent(in)          :: l, c
     character(*), intent(out):: res
   end subroutine iparse_block_string
   !
 end interface 
 !
 contains
   !
   subroutine iparse_logical(name, def, res)
     !
     ! logical is a FORTRAN type, so we emulate the routine with integers
     !
     use stderr, ONLY : cstr
     character(*), intent(IN) :: name
     logical, intent(in) :: def
     logical, intent(out) :: res
     integer :: idef, ires
     idef = 0
     if(def) idef = 1
     call iparse_int(cstr(name), idef, ires)
     res = (ires .ne. 0)
   end subroutine iparse_logical
   !
   subroutine iparse_str(name, def, res)
     !
     ! to avoid errors
     !
     use stderr, ONLY : cstr,clear_str
     character(*), intent(in)  :: name, def
     character(*), intent(out) :: res
     call clear_str(res)
     call iparse_string(cstr(name), cstr(def), res)
   end subroutine iparse_str
   !
   subroutine iparse_block_logical(name, l, c, res)
     use stderr, ONLY : cstr
     character(*), intent(IN) :: name
     integer, intent(in)          :: l, c
     logical, intent(out)         :: res
     integer :: ires
     call iparse_block_int(cstr(name), l, c, ires)
     res = (ires .ne. 0)
   end subroutine iparse_block_logical
   !
   subroutine iparse_block_str(name, l, c, res)
     use pars, ONLY:lchlen
     use stderr, ONLY : cstr,clear_str
     character(*), intent(in)  :: name
     integer, intent(in) :: l, c
     character(*), intent(out) :: res
     character(lchlen) :: name1
     name1 = cstr(name)
     call clear_str(res)
     call iparse_block_string(name1, l, c, res)
   end subroutine iparse_block_str
   !
end module PARSER_lib
