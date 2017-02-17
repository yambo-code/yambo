!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
logical function QP_apply_done(band_range,k_range,spin_range,en,what)
 !
 ! (E) qp_done = 1 3 5 7
 ! (W) qp_done = 2 3 6 7
 ! (Z) qp_done = 4 5 6 7
 !
 use electrons,     ONLY:levels,n_sp_pol
 implicit none
 type(levels) ::en
 integer      ::band_range(2),k_range(2),spin_range(2)
 character(1) ::what
 ! 
 ! Work Space
 !
 integer :: i1,i2,i_sp,iref(4)
 !
 QP_apply_done=.true.
 if (what=='E') iref=(/1,3,5,7/)
 if (what=='W') iref=(/2,3,6,7/)
 if (what=='Z') iref=(/3,5,6,7/)
 do i1=band_range(1),band_range(2)
   do i2=k_range(1),k_range(2)
     do i_sp=spin_range(1),spin_range(2)
       if (en%QP_corrected(i1,i2,i_sp)/=iref(1).and.&
&          en%QP_corrected(i1,i2,i_sp)/=iref(2).and.&
&          en%QP_corrected(i1,i2,i_sp)/=iref(3).and.&
&          en%QP_corrected(i1,i2,i_sp)/=iref(4)) QP_apply_done=.false.
     enddo
   enddo
 enddo
 !
end function
