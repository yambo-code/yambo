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
subroutine PROJECTION_axis( axis, O )
 !
 use pars,           ONLY:SP
 use stderr,         ONLY:intc
 use com,            ONLY:msg
 use vec_operate,    ONLY:cross_product,c2a,normalize_v,v_is_zero
 use YPPm,           ONLY:CIRCUIT_k,CIRCUIT_k_label,coo_out,K_transform
 !
 implicit none
 !
 real(SP)  :: axis(3,3),O(3)
 !
 ! Work Space
 !
 integer, parameter :: n_max_axis=20
 integer  :: n_axis,i_k,i_a,corner(n_max_axis),n_perps
 real(SP) :: axis_vector(n_max_axis,3),perp_vector(3),cross(3),try(3)
 !
 if (CIRCUIT_k%nbz==0) return
 !
 ! iku->cc
 !=========
 do i_k=1,CIRCUIT_k%nbz
   call c2a(v_in= CIRCUIT_k%pt(i_k,:),mode='ki2c')
 enddo
 !
 ! Find the circuit segments...
 !=============================
 n_axis=1
 corner(1)=1
 axis_vector(1,:)=normalize_v(CIRCUIT_k%pt(2,:)-CIRCUIT_k%pt(1,:))
 K_LOOP: do i_k=2,CIRCUIT_k%nbz
   try(:)=normalize_v(CIRCUIT_k%pt(i_k,:)-CIRCUIT_k%pt(corner(n_axis),:))
   do i_a=1,n_axis
     if (abs( dot_product(try,axis_vector(i_a,:))-1._SP) < 1.E-5 ) cycle K_LOOP
   enddo
   n_axis=n_axis+1
   corner(n_axis)=i_k-1
   axis_vector(n_axis,:)=normalize_v(CIRCUIT_k%pt(i_k,:)-CIRCUIT_k%pt(i_k-1,:))
 enddo K_LOOP
 corner(n_axis+1)=CIRCUIT_k%nbz
 do i_a=1,n_axis+1
   try=CIRCUIT_k%pt(corner(i_a),:)
   call K_transform(try,in_mode="cc")
   if (len_trim(CIRCUIT_k_label(corner(i_a)))/=0) then
     call msg('s','Corner #'//trim(intc(i_a))//'['//trim(coo_out)//']:',try,&
&             after_msg='['//trim(CIRCUIT_k_label(corner(i_a)))//']')
   else
     call msg('s','Corner #'//trim(intc(i_a))//'['//trim(coo_out)//']:',try)
   endif
 enddo
 !
 if (n_axis==1) call error("[PROJECTION axis search] Only one axis found.")
 !
 ! ...perp versors...
 n_perps=1
 perp_vector(:)=normalize_v( cross_product(axis_vector(2,:),axis_vector(1,:)))
 do i_a=3,n_axis
   try(:)=normalize_v( cross_product(axis_vector(i_a,:),axis_vector(1,:)))
   cross=cross_product(try,perp_vector(:))
   if (.not.v_is_zero( cross )) then
     call error("More than one perpendicular direction found")
   endif
 enddo
 !
 ! Final transfer
 !
 axis(1,:)=-(CIRCUIT_k%pt(corner(2),:)-CIRCUIT_k%pt(corner(1),:))
 axis(2,:)=CIRCUIT_k%pt(corner(3),:)-CIRCUIT_k%pt(corner(2),:)
 axis(3,:)=perp_vector
 O=CIRCUIT_k%pt(corner(2),:)
 call c2a(v_in= O,mode='kc2i')
 do i_a=1,3
   call c2a(v_in= axis(i_a,:),mode='kc2i')
 enddo
 !
 ! cc->iku
 !=========
 do i_k=1,CIRCUIT_k%nbz
   call c2a(v_in= CIRCUIT_k%pt(i_k,:),mode='kc2i')
 enddo
 !
end subroutine
