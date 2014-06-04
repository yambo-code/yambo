!
!        Copyright (C) 2000-2014 the YAMBO team
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
subroutine YDB_scan_file(file_name)
 !
 use pars,                ONLY:schlen,lchlen
 use IO_m,                ONLY:mk_dir 
 use com,                 ONLY:file_exists
 use stderr,              ONLY:string_split,write_to_log,log_as_a_file
 use LIVE_t,              ONLY:LIVE_message
 use parallel_m,          ONLY:ncpu
 !
 implicit none
 character(*), intent(in) :: file_name
 !
 ! Work Space
 !
 integer          ::n_pieces,i1
 !
 !
end subroutine
