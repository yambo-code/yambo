!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!    
subroutine SET_job_strings_and_dirs(JS)
 !
 use pars,       ONLY:schlen
 use stderr,     ONLY:STRING_split,STRING_remove
 use com,        ONLY:jobstr,jobdir,alt_jobstr,alt_jobdir,n_alt_jobstr,&
&                     n_max_jobstr,com_path,more_io_path
 !
 implicit none
 !
 character(*) :: JS
 !
 ! Work Space
 !
 integer           :: i_j,n_s
 character(schlen) :: string(10),dir(10),job,path,ch_tmp
 !
 call STRING_split(JS,string,",")
 !
 jobstr    =" "
 alt_jobstr=" "
 jobdir    =" "
 alt_jobdir=" "
 !
 do i_j=1,n_max_jobstr
   !
   ! remove final /
   !
   ch_tmp=string(i_j)
   !
   if (len_trim(ch_tmp)==0) cycle
   !
   if (ch_tmp(len_trim(ch_tmp):len_trim(ch_tmp))=="/") ch_tmp(len_trim(ch_tmp):len_trim(ch_tmp))=" "
   !
   call STRING_split(trim(ch_tmp),dir,"/",n_non_empty_strings=n_s)
   !
   if (n_s==1) then
     job =dir(1)
     path=" "
   else
     job =dir(n_s)
     path=trim(STRING_remove(ch_tmp,"/"//trim(job)))
   endif
   !
   if (i_j==1) then
     jobstr=job
     jobdir=path
     if (len_trim(path)>0) then
       if (len_trim(com_path)>0) then
         com_path=trim(com_path)//"/"//trim(path)
       else if (len_trim(more_io_path)>0) then
         com_path=trim(more_io_path)//"/"//trim(path)
       else
         com_path=trim(path)
       endif
     endif
   else
     alt_jobstr(i_j-1)=job
     alt_jobdir(i_j-1)=path
     n_alt_jobstr=i_j-1
   endif
   !
 enddo
 !
end subroutine
