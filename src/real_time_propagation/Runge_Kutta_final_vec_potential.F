!
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA DS
!
subroutine RT_Runge_Kutta_final_vec_potential()
! !
! do i_step=1,Integrator_nsteps
!   if (a_tableau(i_step)==0._SP) cycle 
!   A_tmp(i_step)%vecpot    =(A_tmp(i_step)%vecpot    -A_old%vecpot)/a_tableau(i_step)
!   A_tmp(i_step)%vecpot_vel=(A_tmp(i_step)%vecpot_vel-A_old%vecpot_vel)/a_tableau(i_step)
!   A_tmp(i_step)%vecpot_acc=(A_tmp(i_step)%vecpot_acc-A_old%vecpot_acc)/a_tableau(i_step)
! enddo
! A_tmp(Integrator_nsteps+1)%vecpot    =A_tmp(Integrator_nsteps+1)%vecpot    -A_old%vecpot
! A_tmp(Integrator_nsteps+1)%vecpot_vel=A_tmp(Integrator_nsteps+1)%vecpot_vel-A_old%vecpot_vel
! A_tmp(Integrator_nsteps+1)%vecpot_acc=A_tmp(Integrator_nsteps+1)%vecpot_acc-A_old%vecpot_acc
! !
! ! Finally I average using b_tableau
! !
! A_new=A_old
! do i_step=1,Integrator_nsteps
!   A_new%vecpot    =A_new%vecpot    +A_tmp(i_step+1)%vecpot    *b_tableau(i_step)
!   A_new%vecpot_vel=A_new%vecpot_vel+A_tmp(i_step+1)%vecpot_vel*b_tableau(i_step)
!   A_new%vecpot_acc=A_new%vecpot_acc+A_tmp(i_step+1)%vecpot_acc*b_tableau(i_step)
! enddo
 !
end subroutine RT_Runge_Kutta_final_vec_potential
