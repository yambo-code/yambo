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
logical function QP_check_if_corrected(band_range,k_range,spin_range,en,what)
 !
 ! E,W and Z corrections are identified with specfic integers
 !
 ! E is 1
 ! W is 2
 ! Z is 4
 ! G is 8
 !
 ! so that a sum of all possible combinations is uniquely defined
 !
 ! E     = 1 
 ! E + W = 3 
 ! E + Z = 5 
 ! E+W+Z = 7 
 ! W     = 2 
 ! W + Z = 6 
 ! Z     = 4 
 ! G     = 8 
 !
 use electrons,     ONLY:levels
 implicit none
 type(levels) ::en
 integer      ::band_range(2),k_range(2),spin_range(2)
 character(1) ::what
 ! 
 ! Work Space
 !
 integer :: ib,ik,i_sp,any_of(4)
 !
 QP_check_if_corrected=.true.
 !
 if (what=='E') any_of=(/1,3,5,7/)
 if (what=='W') any_of=(/2,3,6,7/)
 if (what=='Z') any_of=(/3,5,6,7/)
 if (what=='G') any_of=(/8,8,8,8/)
 !
 do ib=band_range(1),band_range(2)
   do ik=k_range(1),k_range(2)
     do i_sp=spin_range(1),spin_range(2)
       if (en%QP_corrected(ib,ik,i_sp)/=any_of(1).and.&
&          en%QP_corrected(ib,ik,i_sp)/=any_of(2).and.&
&          en%QP_corrected(ib,ik,i_sp)/=any_of(3).and.&
&          en%QP_corrected(ib,ik,i_sp)/=any_of(4)) QP_check_if_corrected=.false.
     enddo
   enddo
 enddo
 !
end function
