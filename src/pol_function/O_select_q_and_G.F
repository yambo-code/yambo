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
integer function O_select_q_and_G(iq,q,X,Q_plus_G_pt,Q_plus_G_sq_modulus)
 !
 use pars,           ONLY:SP,schlen
 use X_m,            ONLY:q_plus_G_direction,X_t,Q_Shift_Order
 use R_lattice,      ONLY:bz_samp,g_vec,q_norm
 use vec_operate,    ONLY:v_norm,v_is_zero,iku_v_norm
 use stderr,         ONLY:intc
 use com,            ONLY:msg
 !
 implicit none
 !
 integer      ::iq
 type(X_t)    ::X
 type(bz_samp)::q
 real(SP)     ::Q_plus_G_pt(3),Q_plus_G_sq_modulus
 !
 ! Work Space
 !
 integer ::ig,i_found
 real(SP):: v(3),projection
 character(schlen):: RL_msg(2)
 !
 O_select_q_and_G=1 
 !
 ! IBZ q-point
 !
 if (iq==1) then
   Q_plus_G_pt=X%q0/v_norm(X%q0)
   Q_plus_G_sq_modulus=q_norm(iq)**2
 else 
   Q_plus_G_pt=q%pt(iq,:)
   Q_plus_G_sq_modulus=iku_v_norm(Q_plus_G_pt)**2
 endif
 !
 ! Wrong input paramaters
 !
 if (v_is_zero(q_plus_G_direction)) return
 !
 q_plus_G_direction=q_plus_G_direction/v_norm(q_plus_G_direction)
 !
 if (Q_Shift_Order<=1) then
   !
   ! In this case check only the the given q-point lays on the user defined direction
   !
   v=Q_plus_G_pt/v_norm(Q_plus_G_pt)
   projection=dot_product(q_plus_G_direction,v)-1._SP
   if (abs(projection)>1.E-5_SP) O_select_q_and_G=-1
   !
   return
   !
 endif
 !
 ! If, instead, Q_Shift_Order is given I check that 
 ! q + G are in the correct direction
 !
 if (iq==1) then
   projection=dot_product(q_plus_G_direction,Q_plus_G_pt)-1._SP
   if (abs(projection)>1.E-5_SP) then
     call warning(' Field direction and Q drection are different. q=0 case not computed')
     O_select_q_and_G=-1
     return
   endif
   Q_plus_G_pt=0._SP
 endif
 !
 RL_msg(1)='Compatible RL shift of @ Q('//trim(intc(iq))//'):'
 !
 O_select_q_and_G=-1 
 i_found=1
 !
 do ig=2,X%ng
   v=Q_plus_G_pt+g_vec(ig,:)
   v=v/v_norm(v)
   projection=dot_product(q_plus_G_direction,v)-1._SP
   if (abs(projection)<1.E-5) then
     i_found=i_found+1
     if (i_found==Q_Shift_Order) O_select_q_and_G=ig
     RL_msg(2)=trim(RL_msg(1))//' '//trim(intc(ig)) 
     RL_msg(1)=RL_msg(2)
   endif
 enddo
 !
 if (O_select_q_and_G/=-1) then
   Q_plus_G_pt=Q_plus_G_pt+g_vec(O_select_q_and_G,:)
   Q_plus_G_sq_modulus=iku_v_norm(Q_plus_G_pt)**2
   call msg('rs',trim(RL_msg(2)))
   call msg('r','Momentum shift [cc]:',g_vec(O_select_q_and_G,:))
   call msg('r','Total Momentum [cc]:',Q_plus_G_pt(:))
 endif
 !
end function
