!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
integer function RT_Lifetimes_evaluation_plan( )
 !
 return
 !
end function RT_Lifetimes_evaluation_plan
!
subroutine RT_ELPH_initialize(E,k,q)
 ! 
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 !
 type(levels)             :: E
 type(bz_samp),intent(in) :: k,q
 !
end subroutine RT_ELPH_initialize
!
subroutine RT_ELPH_zeroing()
 !
 return
 !
end subroutine RT_ELPH_zeroing
!
subroutine RT_relaxation_logicals( )
 !
 return
 !
end subroutine RT_relaxation_logicals
