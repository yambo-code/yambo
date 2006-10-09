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
 logical function qpapplydone(br,kr,en,what)
!===========================================
!
! (E) qp_done = 1 3 5 7
! (W) qp_done = 2 3 6 7
! (Z) qp_done = 4 5 6 7
!
 use electrons,     ONLY:levels
 implicit none
 type(levels) ::en
 integer      ::br(2),kr(2)
 character(1) ::what
!ws
 integer :: i1,i2,i3,iref(4)
!
 qpapplydone=.true.
 if (what=='E') iref=(/1,3,5,7/)
 if (what=='W') iref=(/2,3,6,7/)
 if (what=='Z') iref=(/3,5,6,7/)
 do i1=br(1),br(2)
   do i2=kr(1),kr(2)
     if (en%qp_done(i1,i2)/=iref(1).and.&
&        en%qp_done(i1,i2)/=iref(2).and.&
&        en%qp_done(i1,i2)/=iref(3).and.&
&        en%qp_done(i1,i2)/=iref(4)) qpapplydone=.false.
   enddo
 enddo
 end function
