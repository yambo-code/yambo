!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine build_xc_spin_map()
 !
 ! see http://www.tddft.org/programs/libxc/manual/
 !
 ! 1 for fxc and gradients
 ! 2 for kxc = the derivative of fxc, at present not used in yambo
 !
 use electrons,            ONLY:n_spin
 use xc_functionals,       ONLY:xc_spin,xc_spin_map1,xc_spin_map2
 !
 implicit none
 !
 if (n_spin==1) then
   xc_spin=1
   allocate(xc_spin_map1(1,1))
   allocate(xc_spin_map2(1,1,1))
   xc_spin_map1=1
   xc_spin_map2=1
   return
 endif
 !
 ! fxc and grad
 !
 xc_spin(1)=3
 !
 allocate(xc_spin_map1(n_spin,n_spin))
 !
 xc_spin_map1(1,1)=1
 xc_spin_map1(1,2)=2
 xc_spin_map1(2,1)=2
 xc_spin_map1(2,2)=3
 !
 ! kxc
 !
 xc_spin(2)=4
 !
 allocate(xc_spin_map2(n_spin,n_spin,n_spin))
 !
 xc_spin_map2(1,1,1)=1
 xc_spin_map2(2,1,1)=2
 xc_spin_map2(1,2,1)=2
 xc_spin_map2(1,1,2)=2
 xc_spin_map2(1,2,2)=3
 xc_spin_map2(2,1,2)=3
 xc_spin_map2(2,2,1)=3
 xc_spin_map2(2,2,2)=4
 !
end subroutine build_xc_spin_map
