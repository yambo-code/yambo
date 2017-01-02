!
!        Copyright (C) 2000-2017 the YAMBO team
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
logical function ver_is_gt_or_eq(ID,version,revision)
 !
 use IO_m,    ONLY:io_code_version,io_code_revision
 use LOGO,    ONLY:code_version,code_revision
 implicit none
 !
 integer, intent(in), optional     :: revision,version(3)
 integer, intent(in)  :: ID
 integer              :: revision2comp,version2comp(3)
 !
 ver_is_gt_or_eq=.false.
 !
 if (ID>0) version2comp =io_code_version(ID,:)
 if (ID>0) revision2comp=io_code_revision(ID,1)
 if (ID<0) version2comp =code_version
 if (ID<0) revision2comp=code_revision
 !
 if (present(version)) then
   if (version2comp(1)<version(1)) return
   if (version2comp(1)==version(1).and.version2comp(2)<version(2)) return
   if (version2comp(1)==version(1).and.version2comp(2)==version(2)&
&      .and.version2comp(3)<version(3)) return
 endif
 !
 if (present(revision)) then
   if (revision2comp<revision) return
 endif
 !
 ver_is_gt_or_eq=.true.
 !
end function
