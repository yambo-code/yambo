!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 integer function f2s_file_name(inf,file_name)
!=====================
 use pars,             ONLY:lchlen,schlen
 use com,              ONLY:file_exists,msg 
 implicit none
 character(*)              :: inf
 character(lchlen)         :: file_name
!ws
 integer                   :: i1
 logical                   :: sfile
 character(lchlen)         ::fname,msgt
 character(4)         ::ext(4)

 data ext/'','.inp','.wvf','.rl'/

 f2s_file_name=0 
 do i1=1,4
  write (file_name,'(2a)') trim(inf),trim(ext(i1))
   if (.not. file_exists(trim(file_name))) then
    f2s_file_name=f2s_file_name-1
    write(msgt,'(2a)') "f2s: missing file:",trim(file_name)
    call msg('nl',trim(msgt))
   endif
 enddo
 file_name=trim(inf)
 return
 end function

