!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA DS
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
subroutine RT_Integrator(G_new,dG_new,dG_old,A_new,A_old,E,k,q)
 !
 !  This subroutine perform integration using different methods: EULER / EXP / INV
 !        G(t),H[G(t')],dt  --> G(t+dt)
 !  * Explicit methods available
 !    1 - Euler's method:    G(t+dt) = G(t) - i*dt *[H(t'),G(t)]
 !    2 - Exp integrator:    G(t+dt) = G(t) - i*dt *[H(t'),G(t)] - dt^2 *[H(t'),[H(t'),G(t)]] + ....         [Can be expanded up to order 6]
 !  * Implicit methods available
 !    3 - Inv integrator:    (1+iH(t')dt)*G(t+dt)+G(t+dt)(1-iH(t')dt) = (1-iH(t')dt)*G(T)+G(T)(1+iH(t')dt)
 !                                                                    = 2G(t) -i*dt *[H(t'),G(t)]
 !
 !  Each method can be used with the following "multi step" approaches
 !
 !  * Single step approach:
 !    1 - "SIMPLE":  G(t),H[G(t)],dt  --> G(t+dt)
 !  * Two steps approaches:
 !    2 - "RK2" : (Runge-Kutta 2nd order)
 !                G(t),H[G(t)],dt/2      --> G1(t+dt/2)
 !                G(t),H[G1(t+dt/2)),dt  --> G2(t+dt)
 !                      G(t+dt)=G2
 !    3 - "HEUN":
 !                G(t), H[G(t)],dt     --> G1(t+dt)
 !                G(t), H[G1(t+dt)],dt --> G2(t+dt)
 !                      G(t+dt)=G1(t+dt)/2+G2(t+dt)/2
 !
 !  * Four steps approaches:
 !    4 - "RK4" : (Runge-Kutta 4th order)
 !                G(t),H[G(t)],dt/2        --> G1(t+dt/2)
 !                G(t),H[G1(t+dt/2)],dt/2  --> G2(t+dt/2)
 !                G(t),H[G2(t+dt/2)),dt    --> G3(t+dt)
 !                G(t),H[G3(t+dt)),dtcccc  --> G4(t+dt)
 !                      G(t+dt)=G1(t+dt)/6+G2(t+dt)/3+G3(t+dt)/3+G2(t+dt)/6
 !
 !  In addition the IP oscillations and dephasing are "analytically integrated out"
 !
 ! Notice that dG_in is never overwritten 
 ! --------------------------------------
 !
 use pars,           ONLY:SP
 use R_lattice,      ONLY:bz_samp
 use RT_control,     ONLY:TIME_adjust
 use parallel_m,     ONLY:PAR_G_kpts
 use real_time,      ONLY:Integrator_step,RT_nk,RT_bands,a_tableau,b_tableau,&
&                         c_tableau,RWA_table,Integrator_nsteps,G_lesser_reference,&
&                         NE_time,NE_i_time,RT_step,RT_dyn_step
 use fields,         ONLY:gauge_field
 use electrons,      ONLY:levels
 use timing_m,       ONLY:timing
 !
 implicit none
 !
 type(bz_samp),        intent(in) :: k,q
 type(levels),      intent(inout) :: E
 complex(SP),       intent(inout) :: G_new(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_kpts(1):PAR_G_kpts(2))
 complex(SP),          intent(in) :: dG_old(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_kpts(1):PAR_G_kpts(2))
 complex(SP),         intent(out) :: dG_new(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_kpts(1):PAR_G_kpts(2))
 type(gauge_field),    intent(in) :: A_old
 type(gauge_field),   intent(out) :: A_new
 !
 ! Work Space
 !
 integer     :: i_step
 real(SP)    :: delta_T1,delta_T2,delta_T3
 complex(SP) :: dG_tmp(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_kpts(1):PAR_G_kpts(2),Integrator_nsteps+1)
 !
 type(gauge_field) :: A_tmp(Integrator_nsteps+1)
 !
 call timing('RT integrator',OPR='start')
 !
 dG_tmp(:,:,:,1)=dG_old
 A_tmp(1)       =A_old
 !
 Integrator_step=1
 !
 do i_step=1,Integrator_nsteps
   !
   if (a_tableau(i_step)==0._SP) cycle 
   !  
   delta_T1=RT_dyn_step*a_tableau(i_step)
   delta_T2=RT_dyn_step*c_tableau(i_step)
   delta_T3=RT_dyn_step*RWA_table(i_step)
   !
   ! Evaluate @ T+dt1
   !
   call RT_Glesser_evolve(E,k,dG_old,dG_tmp(:,:,:,i_step-1),dG_tmp(:,:,:,i_step),delta_T1,delta_T3)
   !
   ! GPL_EXCLUDE_START
   !
   ! ... Update of logicals before the occupations update
   !
   call RT_relaxation_logicals( )
   !
   call RT_occupations(dG_tmp(:,:,:,i_step))
   !
   ! GPL_EXCLUDE_END
   !
   ! Fields @ T+dt2
   !
   call RT_propagate_fields(E,k,A_old,A_tmp(i_step),dG_tmp(:,:,:,i_step),NE_time,delta_T2)
   !
   Integrator_step=Integrator_step+1
   !
   call timing('RT integrator',OPR='stop')
   !
   ! Recalculate H and S using G(t+dt1) 
   !
   call RT_Hamiltonian(dG_tmp(:,:,:,i_step),A_tmp(i_step),E,k)             ! The time specifications here is useless
   !
   ! GPL_EXCLUDE_START
   !
   call RT_relaxation(E,k,q,NE_time+delta_T1)                                 ! since nothing explicitely depends on time
   !
   ! GPL_EXCLUDE_END
   !
   call timing('RT integrator',OPR='start')
   !
 enddo
 !
 ! final step @ T+dt
 !
 call RT_Glesser_evolve(E,k,dG_old,dG_tmp(:,:,:,Integrator_nsteps),dG_tmp(:,:,:,Integrator_nsteps+1), &
&                                               RT_dyn_step,RT_dyn_step*RWA_table(Integrator_nsteps+1))
 !
 call RT_build_dG_new()
 !
 ! GPL_EXCLUDE_START
 !
 ! Rotate the G_lesser in the instaneous basis
 !
 call RT_Glesser_rotate(dG_new)
 !
 ! Occupations @ T+dT
 !
 call RT_occupations(dG_new)
 !
 ! GPL_EXCLUDE_END
 !
 ! Fields @ T+dT
 !
 call RT_propagate_fields(E,k,A_old,A_tmp(Integrator_nsteps+1),dG_new,NE_time,RT_dyn_step)
 !
 call RT_build_A_new()
 !
 ! Reset integrator time-point and update time
 !=============================================
 !
 Integrator_step=1
 !
 NE_i_time       = NE_i_time+nint(RT_dyn_step/RT_step)
 NE_time         = TIME_adjust((NE_i_time-1)*RT_step)
 !
 call timing('RT integrator',OPR='stop')
 !
 contains
   !
   subroutine RT_build_dG_new()
     !
     use parallel_m,     ONLY:PAR_IND_Xk_ibz
     use real_time,      ONLY:RT_all2ibz
     !
     ! Now I remove dG_old from dG_tmp and rescale to a_tableau
     !
     integer  :: ik
     !
     do ik=1,RT_nk
       !
       if (.not.PAR_IND_Xk_ibz%element_1D(RT_all2ibz(ik)) ) cycle
       !
       do i_step=1,Integrator_nsteps
         if (a_tableau(i_step)==0._SP) cycle 
         dG_tmp(:,:,ik,i_step)=(dG_tmp(:,:,ik,i_step)-dG_old(:,:,ik))/a_tableau(i_step)
       enddo
       dG_tmp(:,:,ik,Integrator_nsteps+1)=dG_tmp(:,:,ik,Integrator_nsteps+1)-dG_old(:,:,ik)
       !
       ! Finally I average using b_tableau
       !
       dG_new(:,:,ik)=dG_old(:,:,ik)
       do i_step=1,Integrator_nsteps
         dG_new(:,:,ik)=dG_new(:,:,ik)+dG_tmp(:,:,ik,i_step+1)*b_tableau(i_step)
       enddo
       !
     enddo
     !
     G_new=G_lesser_reference(:,:,PAR_G_kpts(1):PAR_G_kpts(2))+dG_new
     !
   end subroutine RT_build_dG_new
   !
   subroutine RT_build_A_new()
     !
     do i_step=1,Integrator_nsteps
       if (a_tableau(i_step)==0._SP) cycle 
       A_tmp(i_step)%vecpot    =(A_tmp(i_step)%vecpot    -A_old%vecpot)/a_tableau(i_step)
       A_tmp(i_step)%vecpot_vel=(A_tmp(i_step)%vecpot_vel-A_old%vecpot_vel)/a_tableau(i_step)
       A_tmp(i_step)%vecpot_acc=(A_tmp(i_step)%vecpot_acc-A_old%vecpot_acc)/a_tableau(i_step)
     enddo
     A_tmp(Integrator_nsteps+1)%vecpot    =A_tmp(Integrator_nsteps+1)%vecpot    -A_old%vecpot
     A_tmp(Integrator_nsteps+1)%vecpot_vel=A_tmp(Integrator_nsteps+1)%vecpot_vel-A_old%vecpot_vel
     A_tmp(Integrator_nsteps+1)%vecpot_acc=A_tmp(Integrator_nsteps+1)%vecpot_acc-A_old%vecpot_acc
     !
     ! Finally I average using b_tableau
     !
     A_new=A_old
     do i_step=1,Integrator_nsteps
        A_new%vecpot    =A_new%vecpot    +A_tmp(i_step+1)%vecpot    *b_tableau(i_step)
        A_new%vecpot_vel=A_new%vecpot_vel+A_tmp(i_step+1)%vecpot_vel*b_tableau(i_step)
        A_new%vecpot_acc=A_new%vecpot_acc+A_tmp(i_step+1)%vecpot_acc*b_tableau(i_step)
     enddo
     !
   end subroutine RT_build_A_new
   !
end subroutine RT_Integrator
