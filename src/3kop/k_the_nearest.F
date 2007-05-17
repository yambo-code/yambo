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
integer function k_the_nearest(ko_,k,n_k)
 !
 use com,            ONLY:error
 use pars,           ONLY:SP,zero_dfl
 use vec_operate,    ONLY:k2bz,iku_v_norm
 implicit none
 !
 integer :: n_k
 real(SP):: ko_(3),k(n_k,3)
 !
 ! Work Space
 !
 integer :: i1,i2,is
 real(SP):: v(3),ko(3),k_distance
 !
 k_the_nearest=0
 !
 ! As the k array SHOULD BE obtained using the TRUE flag
 ! in k_ibz2bz I can avoid loop on the G-vectors moving the
 ! ko in the BZ 
 !
 call k2bz(ko_,v_out=ko)
 !
 k_distance=10000.
 do i1=1,n_k
   v=ko_-k(i1,:)
   if (iku_v_norm(v)<k_distance) then
     k_distance=iku_v_norm(v)
     k_the_nearest=i1
   endif
   if (k_distance<zero_dfl) exit
 enddo
 !
 if (k_the_nearest==0) call error('in k_the_nearest. Nearest point not found.')
 !
end function
