!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine SET_job_strings_and_dirs(JS)
 !
 use pars,       ONLY:schlen
 use stderr,     ONLY:string_split,string_remove
 use com,        ONLY:jobstr,jobdir,alt_jobstr,alt_jobdir,n_alt_jobstr,&
&                     n_max_jobstr,com_path
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
 call string_split(JS,string,",")
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
   call string_split(trim(ch_tmp),dir,"/",n_non_empty_strings=n_s)
   !
   if (n_s==1) then
     job =dir(1)
     path=" "
   else
     job =dir(n_s)
     path=trim(string_remove(ch_tmp,"/"//trim(job)))
   endif
   !
   if (i_j==1) then
     jobstr=job
     jobdir=path
     if (len_trim(path)>0) com_path=path
   else
     alt_jobstr(i_j-1)=job
     alt_jobdir(i_j-1)=path
     n_alt_jobstr=i_j-1
   endif
   !
 enddo
 !
end subroutine
