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
 real function kmin_distance(k,p) ! k/p are iku 
!================================
 use pars
 use vec_operate,  ONLY:iku_v_norm,c2a
 use R_lattice,    ONLY:b
 implicit none
 real(SP) :: k(3),p(3)
!ws
 integer :: i1,i2,i3
 integer,parameter :: ni=2
 real(SP) :: di(3),da(3)
 di=k-p
 call c2a(b,di,da,'ki2a')
 kmin_distance=iku_v_norm(di)
 do i1=-ni,ni
  do i2=-ni,ni
   do i3=-ni,ni
    call c2a(b,da(:)-(/i1,i2,i3/),di,'ka2c')
    kmin_distance=min(kmin_distance,iku_v_norm(di))
   enddo
  enddo
 enddo
 end function
