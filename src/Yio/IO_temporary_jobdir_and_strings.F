!
!        Copyright (C) 2000-2022 the YAMBO team
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
