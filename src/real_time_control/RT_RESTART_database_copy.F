!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA
!
subroutine RT_RESTART_database_copy(db)
 !
 use pars,           ONLY:lchlen,schlen,n_max_jobstr
 use IO_m,           ONLY:cp_file,mk_dir
 use RT_control,     ONLY:RT_DB_file_name
 use stderr,         ONLY:n_max_jobstr,STRING_split
 use com,            ONLY:jobdir,jobstr,more_io_path
 use parallel_m,     ONLY:master_cpu
 !
 implicit none
 !
 character(*) :: db
 !
 ! Work Space
 !
 integer           :: i_db,n_s,i_err
 character(lchlen) :: db_to_cp,destination
 character(schlen) :: components(20)
 !
 logical, external :: file_exists
 !
 if(.not.master_cpu) return
 !
 db_to_cp=" "
 !
 do i_db=1,size(RT_DB_file_name)
   if (index(RT_DB_file_name(i_db),db)>0) then
     db_to_cp=RT_DB_file_name(i_db)
   endif
 enddo
 !
 if (len_trim(db_to_cp)==0) return
 !
 call STRING_split(trim(db_to_cp),components,"/",n_non_empty_strings=n_s)
 !
 if (len_trim(jobdir)>0)  destination=trim(more_io_path)//trim(jobdir)//"/"//trim(jobstr)//"/"//trim(components(n_s))
 if (len_trim(jobdir)==0) destination=trim(more_io_path)              //"/"//trim(jobstr)//"/"//trim(components(n_s))
 !
 if (.not.file_exists(trim(destination))) then
   call mk_dir(trim(more_io_path)//"/"//trim(jobdir)//"/"//trim(jobstr))
   call cp_file(trim(db_to_cp),trim(destination),i_err)
 endif
 !
end subroutine RT_RESTART_database_copy
