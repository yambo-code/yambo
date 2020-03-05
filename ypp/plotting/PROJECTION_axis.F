! 
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine PROJECTION_axis( n_axis, axis, O )
 !
 use pars,           ONLY:SP
 use vec_operate,    ONLY:cross_product,c2a,normalize_v,v_is_zero,iku_v_norm
 use YPPm,           ONLY:CIRCUIT_k,CIRCUIT_k_label
 !
 implicit none
 !
 integer   :: n_axis
 real(SP)  :: axis(n_axis,3),O(3)
 !
 ! Work Space
 !
 integer  :: i_k,i_a,corners(n_axis),n_corners,n_axis_here
 real(SP) :: perp_vector(n_axis,3)
 !
 ! Find the circuit segments...
 !=============================
 corners=0
 n_corners=0
 do i_k=1,CIRCUIT_k%nbz
   if (len_trim(CIRCUIT_k_label(i_k))>0) then
     n_corners=n_corners+1
     corners(n_corners)=i_k
   endif
 enddo
 !
 if (n_corners==0) return
 !
 n_axis_here=n_corners
 !
 ! ...plane segments...
 do i_a=1,n_corners-1
   axis(i_a,:)=CIRCUIT_k%pt(corners(i_a+1),:)-CIRCUIT_k%pt(corners(i_a),:)
   if (i_a==1) axis(i_a,:)=-axis(i_a,:)
   if (i_a==2) O=CIRCUIT_k%pt( corners(i_a) ,:)
 enddo 
 !
 ! ...perp versors...
 do i_a=1,n_corners-2
   call c2a(v_in= axis(i_a,:),mode='ki2c')
   perp_vector(i_a,:)=normalize_v( cross_product(axis(i_a+1,:),axis(i_a,:)))
   call c2a(v_in= axis(i_a,:),mode='kc2i')
   if (i_a==1) then
     axis(n_axis_here,:)=perp_vector(1,:)
   else
     if (.not.v_is_zero( cross_product(axis(n_axis_here,:),perp_vector(i_a,:)) )) then
       call error("More than one perpendicular direction found")
     endif
   endif
 enddo
 !
 call c2a(v_in= axis(n_axis_here,:),mode='kc2i')
 axis(n_axis_here,:)=axis(n_axis_here,:)/iku_v_norm( axis(n_axis_here,:) )
 !
end subroutine
