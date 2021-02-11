!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA
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
subroutine RT_RESTART_database_copy(db)
 !
 use pars,           ONLY:lchlen,schlen,n_max_jobstr
 use IO_m,           ONLY:cp_file,mk_dir
 use RT_control,     ONLY:RT_DB_file_name
 use stderr,         ONLY:n_max_jobstr,STRING_split
 use com,            ONLY:jobdir,jobstr
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
 destination=trim(jobdir)//"/"//trim(jobstr)//"/"//trim(components(n_s))
 !
 if (.not.file_exists(trim(destination))) then
   call mk_dir(trim(jobdir)//"/"//trim(jobstr))
   call cp_file(trim(db_to_cp),trim(destination),i_err)
 endif
 !
end subroutine RT_RESTART_database_copy
