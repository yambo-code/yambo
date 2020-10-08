!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DV
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
integer function c2y_file_name(inf,file_name_)
 !
 use pars,             ONLY:lchlen
 implicit none
 character(*)              :: inf
 character(lchlen)         :: file_name_
 !
 logical, external  :: file_exists
 !
 ! Work Space
 !
 c2y_file_name=0
 !
 file_name_=inf
 if (file_exists (file_name_) ) return
 !
 c2y_file_name=-1
 !
end function
