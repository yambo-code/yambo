!
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
! This subroutine checks:
! [i]  which q[iq] from the input list is consistent with the Qdirection 
! (value of Q=q+G) provided in input by the user.
! [ii] which is the index ig of the G-vector corresponding to the value
! of QShiftOrder given in input [1: first BZ, 2: nearest-neighbor shell of the 
! first BZ, 3: 2nd-NN shell, etc] and along Qdirection.
!
! Example: Qdirection=(0,1,0) and QShiftOrder=2 in a hexagonal system
! means the second BZ along the GM direction. 
!
! [iii] The code wll proceed with the optics calculation only if  q[iq]||Q,
! otherwise skip the q-point.
!
integer function OPTICS_select_q_and_G(iq,q,X)
 !
 use pars,           ONLY:SP,schlen
 use X_m,            ONLY:q_plus_G_direction,X_t,Q_Shift_Order,&
&                         Q_plus_G_pt,Q_plus_G_sq_modulus 
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
 !
 ! Work Space
 !
 integer ::ig,i_found
 real(SP):: v(3),projection
 character(schlen):: RL_msg(2)
 !
 OPTICS_select_q_and_G=1 
 !
 ! IBZ q-point
 !
 if (iq==1) then ! Q_plus_G = q0/|q0|
   Q_plus_G_pt=X%q0/v_norm(X%q0)
   Q_plus_G_sq_modulus=q_norm(iq)**2
 else ! Q_plus_G = q[iq] 
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
   if (abs(projection)>1.E-5_SP) OPTICS_select_q_and_G=-1
   !
   return
   !
 endif
 !
 ! If, instead, Q_Shift_Order is given I check that 
 ! q + G are in the correct direction
 !
 if (iq==1) then ! Q_plus_G = q0/|q0|
   projection=dot_product(q_plus_G_direction,Q_plus_G_pt)-1._SP
   if (abs(projection)>1.E-5_SP) then
     call warning(' Field direction and Q drection are different. q=0 case not computed')
     OPTICS_select_q_and_G=-1
     return
   endif
   Q_plus_G_pt=0._SP
 endif
 !
 RL_msg(1)='Compatible RL shift of @ Q('//trim(intc(iq))//')'
 !
 ! Now all the checks are done
 !
 OPTICS_select_q_and_G=-1 
 i_found=1
 !
 ! Recall that Q_Shift_Order>1 here
 !
 ! This loop checks q+G for each G, with the present q.
 ! It finds a list of G satisfying q+G || Qdirection
 !
 ! The G selected from the list is the one with index Q_Shift_Order,
 ! i.e., if Q_Shift_Order=N select N-1 matching G, etc. 
 !
 do ig=2,X%ng
   v=Q_plus_G_pt+g_vec(ig,:) ! v is actually Q+G
   v=v/v_norm(v)             ! v = (Q[iq]+G[ig])/|Q[iq]+G[ig]|
   ! An index ig is selected if v || q_plus_G_direction
   projection=dot_product(q_plus_G_direction,v)-1._SP
   if (abs(projection)<1.E-5) then
     i_found=i_found+1
     if (i_found==Q_Shift_Order) OPTICS_select_q_and_G=ig
     RL_msg(2)=trim(RL_msg(1))//' '//trim(intc(ig)) 
     RL_msg(1)=RL_msg(2)
   endif
 enddo
 !
 if (OPTICS_select_q_and_G/=-1) then
   Q_plus_G_pt=Q_plus_G_pt+g_vec(OPTICS_select_q_and_G,:) ! Final Q+G
   Q_plus_G_sq_modulus=iku_v_norm(Q_plus_G_pt)**2
   ! This prints the legal ig 
   call msg('rs',trim(RL_msg(2)))
   call msg('r','Momentum shift     ',g_vec(OPTICS_select_q_and_G,:),"[c.c.]")
   call msg('r','Total Momentum     ',Q_plus_G_pt(:),"[c.c.]")
 endif
 !
end function
