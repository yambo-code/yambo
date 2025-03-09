!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!    
subroutine IO_make_directories(what)
 !
 use com,        ONLY:com_path,more_io_path
 use IO_m,       ONLY:mk_dir
 use parallel_m, ONLY:ncpu
 !
 implicit none
 !
 character(*) :: what
 !
 if (index(what,"MORE")>0.or.index(what,"SAVE")>0) call mk_dir(more_io_path)
 if (index(what,"SAVE")>0)                         call mk_dir(trim(more_io_path)//"/SAVE")
 if (index(what,"COM")>0.or.index(what,"LOG")>0)   call mk_dir(com_path)
 if (index(what,"LOG")>0 .and. ncpu>1 )            call mk_dir(trim(com_path)//"/LOG")
 !
end subroutine
