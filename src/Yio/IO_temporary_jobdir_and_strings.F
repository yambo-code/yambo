!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine IO_temporary_jobdir_and_strings(what,value)
 !
 use pars,           ONLY:lchlen
 use com,            ONLY:n_max_jobstr,jobstr,jobdir,alt_jobstr,alt_jobdir
 !
 implicit none
 !
 character(*)      :: what,value
 !
 ! Work Space
 !
 character(lchlen), SAVE :: jobstr_save,jobdir_save,alt_jobstr_save(n_max_jobstr),alt_jobdir_save(n_max_jobstr)
 !
 select case(value)
   case("SAVE")
     jobstr_save=jobstr
     jobdir_save=jobdir
     alt_jobstr_save=alt_jobstr
     alt_jobdir_save=alt_jobdir
     return
   case("RESTORE")
     jobstr=jobstr_save
     jobdir=jobdir_save
     alt_jobstr=alt_jobstr_save
     alt_jobdir=alt_jobdir_save
     return
 end select
 !
 select case(what)
   case("JOBSTR")
     jobstr=trim(value)
     jobdir=" "
     alt_jobstr=" "
     alt_jobdir=" "
     return
   case("JOBDIR")
     jobstr=" "
     jobdir=trim(value)
     alt_jobstr=" "
     alt_jobdir=" "
     return
 end select
 !
end subroutine IO_temporary_jobdir_and_strings
