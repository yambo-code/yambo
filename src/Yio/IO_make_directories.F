!
!        Copyright (C) 2000-2020 the YAMBO team
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
