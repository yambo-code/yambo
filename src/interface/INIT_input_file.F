!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine INIT_input_file()
 !
 use pars,           ONLY:lchlen
 use drivers,        ONLY:infile_editing
 use it_m,           ONLY:infile
 use com,            ONLY:com_path,jobstr
 use IO_m,           ONLY:cp_file
 !
 implicit none
 !
 character(lchlen) :: infile_copy
 integer           :: io_err
 !
 character(lchlen), external :: filename
 !
 if (len_trim(com_path)==0) return
 if (infile_editing) return
 if (trim(com_path)==".") return
 !
 call IO_make_directories("COM")
 infile_copy=trim(com_path)//"/"//trim(filename(infile))
 if (trim(jobstr)/=''.and.index(infile,trim(jobstr))==0) then
   infile_copy=trim(com_path)//"/"//trim(filename(infile))//"_"//trim(jobstr)
 endif
 call cp_file(trim(infile),trim(infile_copy),io_err)
 infile=infile_copy 
 !
end subroutine
