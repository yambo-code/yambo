! 
!        Copyright (C) 2000-2019 the YAMBO team
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
 integer  :: i_k,i_l
 real(SP) :: line_vector(n_axis,3),perp_vector(n_axis,3),versor(3)
 !
 ! Find the versor perpendicular to the circuit...
 !================================================
 !
 ! ...line versors...
 i_l=0
 K_LOOP: do i_k=1,CIRCUIT_k%nbz-1
   if (len_trim(CIRCUIT_k_label(i_k))>0) then
     i_l=i_l+1
     line_vector(i_l,:)=CIRCUIT_k%pt(i_k+1,:)-CIRCUIT_k%pt(i_k,:)
     line_vector(i_l,:)=line_vector(i_l,:)/iku_v_norm( line_vector(i_l,:) )
     !
     if (i_l==2) O=CIRCUIT_k%pt(i_k,:)
     if (i_l==n_axis-1) exit K_LOOP
   endif
 enddo K_LOOP
 !
 ! ...perp versors...
 do i_l=1,n_axis-2
   call c2a(v_in= line_vector(i_l,:),mode='ki2c')
   perp_vector(i_l,:)=normalize_v( cross_product(line_vector(i_l+1,:),line_vector(i_l,:)))
   call c2a(v_in= line_vector(i_l,:),mode='kc2i')
   if (i_l==1) then
     versor=perp_vector(1,:)
   else
     if (.not.v_is_zero( cross_product(versor,perp_vector(i_l,:)) )) then
       call error("More than one perpendicular direction found")
     endif
   endif
 enddo
 !
 axis=0._SP
 do i_l=1,n_axis-1
   axis(i_l,:)=line_vector(i_l,:)
 enddo
 axis(n_axis,:)=versor
 !
end subroutine
