!
!        Copyright (C) 2000-2019 the YAMBO team
!            http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AC MG 
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
subroutine NL_Integrator(E,k,q,X,Integrator,i_time,V_bands)
 !
 !  This subroutine perform integration using different methods
 !
 ! if(Integrator=="EULER") simple Euler's method
 !
 !    y(t+dt) = y(t) + dt * f(t,y(t))
 !
 ! elseif(Integrator=="RK2")  Runge-Kutta 2nd order
 !
 !    y(t+dt) = y(t) + dt * f(t+dt/2,y(t+dt/2))
 !
 ! elseif(Integrator=="INVINT")  
 !
 ! see reference  
 ! Koonin Meredith 
 ! Computational Physics, Addison-Wesley, Reading, MA, 1990
 !
 ! ------------------------------------------------------------
 !
 use pars,           ONLY:SP,cZERO
 use QP_m,           ONLY:QP_nk
 use nl_optics,      ONLY:RK2,EULEREXP,RK2EXP,INVINT,RK4,RK4EXP,HEUN,CRANK_NICOLSON,I_relax,&
&                         NL_bands 
 use real_time,      ONLY:RT_step,Ho_plus_Sigma
 use electrons,      ONLY:levels,n_sp_pol
 use X_m,            ONLY:X_t
 use R_lattice,      ONLY:bz_samp
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX,PAR_Xk_ibz_index
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 ! 
 implicit none
 !
 integer,              intent(in) :: Integrator
 type(bz_samp),        intent(in) :: k,q
 type(X_t),            intent(in) :: X
 type(levels),         intent(in) :: E
 integer,              intent(in) :: i_time
 complex(SP),       intent(inout) :: V_bands(NL_bands(2),E%nbf,QP_nk,n_sp_pol)
 !
 ! Working space
 !
 integer :: ik,ib,is,ik_mem
 complex(SP) :: K_all(NL_bands(2),E%nbf,QP_nk,n_sp_pol),Ktmp(NL_bands(2),E%nbf)
 complex(SP), allocatable   :: U_tot(:,:,:,:,:),U_tmp(:,:)
 complex(SP), allocatable   :: V_tmp(:,:,:,:)
 real(SP) :: Time
 !
 ! parameters
 !
 real(SP), parameter:: RK2_apar= 1._SP/2._SP
 real(SP), parameter:: RK2_bpar= 1._SP
 real(SP),dimension(2), parameter:: Heun_bpar=(/ 1._SP/4._SP, 3._SP/4._SP /)
 real(SP),dimension(2), parameter:: Heun_apar=(/ 1._SP/3._SP, 2._SP/3._SP /)
 real(SP),dimension(4), parameter:: RK4_bpar=(/ 1._SP/6._SP, 1._SP/3._SP, 1._SP/3._SP, 1._SP/6._SP /)
 real(SP),dimension(3), parameter:: RK4_apar=(/ 1._SP/2._SP, 1._SP/2._SP, 1._SP /)
 !
#if defined _TIMING
 call timing('NL Integrator',OPR='start')
#endif
 !
 Time=real(i_time-1._SP)*RT_step
 !  
 select case(Integrator)
 case(EULEREXP)
   !
   ! |V(T+dt)> = |V(T)> - i dt * H |V >
   !
   do ik=1,QP_nk
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) then
       V_bands(:,:,ik,:)=cZERO
       cycle
     endif
     ik_mem=PAR_Xk_ibz_index(ik)
     do is=1,n_sp_pol
       call EXP_step(Ktmp,V_bands(:,:,ik,is),Ho_plus_Sigma(:,:,ik_mem,is),I_relax(:,:,:,ik_mem,is),E%nbf,RT_step)
       V_bands(:,:,ik,is) = V_bands(:,:,ik,is) + Ktmp(:,:)
     enddo
   enddo
   call PP_redux_wait(V_bands,COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
   !
 case(INVINT)
   !  
   do ik=1,QP_nk
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) then
       V_bands(:,:,ik,:)=cZERO
       cycle
     endif
     ik_mem=PAR_Xk_ibz_index(ik)
     do is=1,n_sp_pol
       Ktmp=V_bands(:,:,ik,is)
       call INVINT_step(V_bands(:,:,ik,is),Ktmp,Ho_plus_Sigma(:,:,ik_mem,is),I_relax(:,:,:,ik_mem,is),E%nbf,RT_step)
     enddo
   enddo
   call PP_redux_wait(V_bands,COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
   !
 case(CRANK_NICOLSON) 
   !  
   ! Evaluate V(t+dt/2)
   !
   K_all=cZERO
   do ik=1,QP_nk
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle 
     ik_mem=PAR_Xk_ibz_index(ik)
     do is=1,n_sp_pol
       call INVINT_step(K_all(:,:,ik,is),V_bands(:,:,ik,is),Ho_plus_Sigma(:,:,ik_mem,is), &
&                                                                 I_relax(:,:,:,ik_mem,is),E%nbf,RT_step/2._SP)
     enddo
   enddo
   call PP_redux_wait(K_all,COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
   !
   ! Recalculate H(t+dt/2) using  V(t+dt/2) 
   !
#if defined _TIMING
 call timing('NL Integrator',OPR='stop')
#endif
   call NL_Hamiltonian(E,k,q,X,Time+RT_step/2._SP,i_time,K_all)
#if defined _TIMING
 call timing('NL Integrator',OPR='start')
#endif
   !
   ! final step |V(t+dt)>=[ (1 - dt/2 H(t +dt/2)) / ( 1 + dt/2 H(t+dt/2)) ] |V(t)> 
   ! see 
   !
   K_all=V_bands
   do ik=1,QP_nk
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) then
       V_bands(:,:,ik,:)=cZERO
       cycle
     endif
     ik_mem=PAR_Xk_ibz_index(ik)
     do is=1,n_sp_pol
       call INVINT_step(V_bands(:,:,ik,is),K_all(:,:,ik,is),Ho_plus_Sigma(:,:,ik_mem,is), &
&                                                                 I_relax(:,:,:,ik_mem,is),E%nbf,RT_step)
     enddo
   enddo
   call PP_redux_wait(V_bands,COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
   !
 case(RK2EXP) 
   !
   ! Evaluate V(t+dt/2)
   !
   K_all=cZERO
   do ik=1,QP_nk
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     ik_mem=PAR_Xk_ibz_index(ik)
     do is=1,n_sp_pol
       call EXP_step(Ktmp,V_bands(:,:,ik,is),Ho_plus_Sigma(:,:,ik_mem,is),I_relax(:,:,:,ik_mem,is),E%nbf,RT_step/2._SP)
       K_all(:,:,ik,is) = V_bands(:,:,ik,is) + Ktmp(:,:)
     enddo
   enddo
   call PP_redux_wait(K_all,COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
   !
   ! Recalculate H(t) using  V(t+dt/2) 
   !
#if defined _TIMING
 call timing('NL Integrator',OPR='stop')
#endif
   call NL_Hamiltonian(E,k,q,X,Time+RT_step/2._SP,i_time,K_all)
#if defined _TIMING
 call timing('NL Integrator',OPR='start')
#endif
   !
   ! final step |V(t+dt)>=|V(t)> - i dt * H(t+dt/2) | V(t+dt/2)>
   !
   do ik=1,QP_nk
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) then
       V_bands(:,:,ik,:)=cZERO
       cycle
     endif
     ik_mem=PAR_Xk_ibz_index(ik)
     do is=1,n_sp_pol
       call RK_basestep(Ktmp(:,:),K_all(:,:,ik,is),Ho_plus_Sigma(:,:,ik_mem,is),I_relax(:,:,:,ik_mem,is),E%nbf)
       V_bands(:,:,ik,is) = V_bands(:,:,ik,is) + RK2_bpar*RT_step*Ktmp(:,:)
     enddo
   enddo
   call PP_redux_wait(V_bands,COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
   !
 case(RK2) 
   !
   ! Evaluate V(t+dt/2)
   !
   K_all=cZERO
   do ik=1,QP_nk
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     ik_mem=PAR_Xk_ibz_index(ik)
     do is=1,n_sp_pol
       call RK_basestep(Ktmp(:,:),V_bands(:,:,ik,is),Ho_plus_Sigma(:,:,ik_mem,is),I_relax(:,:,:,ik_mem,is),E%nbf)
       K_all(:,:,ik,is) = V_bands(:,:,ik,is) + RK2_apar*RT_step*Ktmp(:,:)
     enddo
   enddo
   call PP_redux_wait(K_all,COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
   !
   ! Recalculate H(t) using  V(t+dt/2) 
   !
#if defined _TIMING
 call timing('NL Integrator',OPR='stop')
#endif
   call NL_Hamiltonian(E,k,q,X,Time+RT_step/2._SP,i_time,K_all)
#if defined _TIMING
 call timing('NL Integrator',OPR='start')
#endif
   !
   ! final step |V(t+dt)>
   !
   do ik=1,QP_nk
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) then
       V_bands(:,:,ik,:)=cZERO
       cycle
     endif
     ik_mem=PAR_Xk_ibz_index(ik)
     do is=1,n_sp_pol
       call RK_basestep(Ktmp(:,:),K_all(:,:,ik,is),Ho_plus_Sigma(:,:,ik_mem,is),I_relax(:,:,:,ik_mem,is),E%nbf)
       V_bands(:,:,ik,is) = V_bands(:,:,ik,is) + RK2_bpar*RT_step*Ktmp(:,:)
     enddo
   enddo
   call PP_redux_wait(V_bands,COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
   !
 case(HEUN) 
   !
   allocate(V_tmp(NL_bands(2),E%nbf,QP_nk,n_sp_pol))
   !
   ! Evaluate V(t+dt/3)
   !
   do ik=1,QP_nk
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) then
       V_tmp(:,:,ik,:)=cZERO
       K_all(:,:,ik,:)=cZERO
       cycle
     endif
     ik_mem=PAR_Xk_ibz_index(ik)
     do is=1,n_sp_pol
       call RK_basestep(Ktmp(:,:),V_bands(:,:,ik,is),Ho_plus_Sigma(:,:,ik_mem,is),I_relax(:,:,:,ik_mem,is),E%nbf)
       V_tmp(:,:,ik,is) = V_bands(:,:,ik,is) + Heun_apar(1)*RT_step*Ktmp(:,:)
       K_all(:,:,ik,is) = Heun_bpar(1)*Ktmp(:,:)
     enddo
   enddo
   !
   ! Recalculate H(t) using  V(t+dt/3) 
   !
#if defined _TIMING
 call timing('NL Integrator',OPR='stop')
#endif
   call NL_Hamiltonian(E,k,q,X,Time+RT_step*Heun_apar(1),i_time,V_tmp)
#if defined _TIMING
 call timing('NL Integrator',OPR='start')
#endif
   !
   ! Evaluate V(t+2dt/3)
   !
   do ik=1,QP_nk
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) then
       V_tmp(:,:,ik,:)=cZERO
       K_all(:,:,ik,:)=cZERO
       cycle
     endif
     ik_mem=PAR_Xk_ibz_index(ik)
     do is=1,n_sp_pol
       call RK_basestep(Ktmp(:,:),V_tmp(:,:,ik,is),Ho_plus_Sigma(:,:,ik_mem,is),I_relax(:,:,:,ik_mem,is),E%nbf)
       V_tmp(:,:,ik,is) = V_bands(:,:,ik,is) + Heun_apar(2)*RT_step*Ktmp(:,:)
     enddo
   enddo
   call PP_redux_wait(V_tmp,COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
   !
   ! Recalculate H(t) using  V(t+2dt/3) 
   !
   call NL_Hamiltonian(E,k,q,X,Time+RT_step*Heun_apar(2),i_time,V_tmp)
   !
   ! final step |V(t+dt)>
   !
   do ik=1,QP_nk
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) then
       V_bands(:,:,ik,:)=cZERO
       K_all(:,:,ik,:)=cZERO
       cycle
     endif
     ik_mem=PAR_Xk_ibz_index(ik)
     do is=1,n_sp_pol
       call RK_basestep(Ktmp(:,:),V_tmp(:,:,ik,is),Ho_plus_Sigma(:,:,ik_mem,is),I_relax(:,:,:,ik_mem,is),E%nbf)
       K_all(:,:,ik,is)   = K_all(:,:,ik,is) + Heun_bpar(2)*Ktmp(:,:)
       V_bands(:,:,ik,is) = V_bands(:,:,ik,is) + RT_step*K_all(:,:,ik,is)
     enddo
   enddo
   call PP_redux_wait(V_bands,COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
   !
   deallocate(V_tmp)
   !
 case(RK4EXP)
   !
   !
   allocate(U_tot(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),E%nbf,QP_nk,n_sp_pol))
   allocate(U_tmp(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2)))
   allocate(V_tmp(NL_bands(2),E%nbf,QP_nk,n_sp_pol))
   !
   ! Evaluate V(t+dt/2)
   !
   U_tot=cZERO
   V_tmp=cZERO
   do ik=1,QP_nk
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     ik_mem=PAR_Xk_ibz_index(ik)
     do is=1,n_sp_pol
       do ib=1,E%nbf
         call EXP_op(U_tmp(:,:),Ho_plus_Sigma(:,:,ik_mem,is)+I_relax(:,:,ib,ik_mem,is),RT_step*RK4_apar(1))
         V_tmp(NL_bands(1):,ib,ik,is)=matmul(U_tmp,V_bands(NL_bands(1):,ib,ik,is))
         U_tot(:,:,ib,ik,is) =RK4_bpar(1)*(Ho_plus_Sigma(:,:,ik_mem,is)+I_relax(:,:,ib,ik_mem,is))
       enddo
     enddo
   enddo
   call PP_redux_wait(V_tmp,COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
   if(NL_bands(1)/=1) V_tmp(1:NL_bands(1)-1,:,:,:)=V_bands(1:NL_bands(1)-1,:,:,:)
   !
   ! Recalculate H(t) using  V(t+dt/2) 
   !
#if defined _TIMING
 call timing('NL Integrator',OPR='stop')
#endif
   call NL_Hamiltonian(E,k,q,X,Time+RT_step*RK4_apar(1),i_time,V_tmp)
#if defined _TIMING
 call timing('NL Integrator',OPR='start')
#endif
   !
   ! Evaluate V'(t+dt/2)
   !
   V_tmp=cZERO
   do ik=1,QP_nk
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     ik_mem=PAR_Xk_ibz_index(ik)
     do is=1,n_sp_pol
       do ib=1,E%nbf
         call EXP_op(U_tmp(:,:),Ho_plus_Sigma(:,:,ik_mem,is)+I_relax(:,:,ib,ik_mem,is),RT_step*RK4_apar(2))
         V_tmp(NL_bands(1):,:,ik,is)=matmul(U_tmp,V_bands(NL_bands(1):,:,ik,is))
         U_tot(:,:,ib,ik,is) = U_tot(:,:,ib,ik,is) + RK4_bpar(2)*(Ho_plus_Sigma(:,:,ik_mem,is)+I_relax(:,:,ib,ik_mem,is)) 
       enddo
     enddo
   enddo
   call PP_redux_wait(V_tmp,COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
   if(NL_bands(1)/=1) V_tmp(1:NL_bands(1)-1,:,:,:)=V_bands(1:NL_bands(1)-1,:,:,:)
   !
   ! Recalculate H(t) using  V'(t+dt/2) 
   !
#if defined _TIMING
 call timing('NL Integrator',OPR='stop')
#endif
   call NL_Hamiltonian(E,k,q,X,Time+RT_step*RK4_apar(2),i_time,V_tmp)
#if defined _TIMING
 call timing('NL Integrator',OPR='start')
#endif
   !
   ! Evaluate V'(t+dt)
   !
   V_tmp=cZERO
   do ik=1,QP_nk
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     ik_mem=PAR_Xk_ibz_index(ik)
     do is=1,n_sp_pol
       do ib=1,E%nbf
         call EXP_op(U_tmp(:,:),Ho_plus_Sigma(:,:,ik_mem,is)+I_relax(:,:,ib,ik_mem,is),RT_step*RK4_apar(3))
         V_tmp(NL_bands(1):,:,ik,is)=matmul(U_tmp,V_bands(NL_bands(1):,:,ik,is))
         U_tot(:,:,ib,ik,is) = U_tot(:,:,ib,ik,is) + RK4_bpar(3)*(Ho_plus_Sigma(:,:,ik_mem,is)+I_relax(:,:,ib,ik_mem,is))
       enddo
     enddo
   enddo
   call PP_redux_wait(V_tmp,COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
   if(NL_bands(1)/=1) V_tmp(1:NL_bands(1)-1,:,:,:)=V_bands(1:NL_bands(1)-1,:,:,:)
   !
   ! Recalculate H(t) using  V'(t+dt) 
   !
#if defined _TIMING
 call timing('NL Integrator',OPR='stop')
#endif
   call NL_Hamiltonian(E,k,q,X,Time+RT_step*RK4_apar(3),i_time,V_tmp)
#if defined _TIMING
 call timing('NL Integrator',OPR='start')
#endif
   !
   ! final step |V(t+dt)>
   !
   V_tmp=V_bands
   !
   do ik=1,QP_nk
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) then
       V_bands(:,:,ik,:)=cZERO
       cycle
     endif
     ik_mem=PAR_Xk_ibz_index(ik)
     do is=1,n_sp_pol
       do ib=1,E%nbf
         U_tot(:,:,ib,ik,is) = U_tot(:,:,ib,ik,is) + RK4_bpar(4)*(Ho_plus_Sigma(:,:,ik_mem,is)+I_relax(:,:,ib,ik_mem,is))
         call EXP_op(U_tmp(:,:),U_tot(:,:,ib,ik,is),RT_step)
         V_bands(NL_bands(1):,ib,ik,is)=matmul(U_tmp,V_tmp(NL_bands(1):,ib,ik,is))
       enddo
     enddo
   enddo
   call PP_redux_wait(V_bands,COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
   !
   deallocate(U_tot,U_tmp,V_tmp)
   !
 case(RK4)
   !
   allocate(V_tmp(NL_bands(2),E%nbf,QP_nk,n_sp_pol))
   !
   ! Evaluate V(t+dt/2)
   !
   V_tmp=cZERO
   K_all=cZERO
   do ik=1,QP_nk
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     ik_mem=PAR_Xk_ibz_index(ik)
     do is=1,n_sp_pol
       call RK_basestep(Ktmp(:,:),V_bands(:,:,ik,is),Ho_plus_Sigma(:,:,ik_mem,is),I_relax(:,:,:,ik_mem,is),E%nbf) 
       V_tmp(:,:,ik,is) = V_bands(:,:,ik,is) + RK4_apar(1)*RT_step*Ktmp(:,:)
       K_all(:,:,ik,is) = RK4_bpar(1)*Ktmp(:,:)
     enddo
   enddo
   call PP_redux_wait(V_tmp,COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
   !
   ! Recalculate H(t) using  V(t+dt/2) 
   !
#if defined _TIMING
 call timing('NL Integrator',OPR='stop')
#endif
   call NL_Hamiltonian(E,k,q,X,Time+RT_step*RK4_apar(1),i_time,V_tmp)
#if defined _TIMING
 call timing('NL Integrator',OPR='start')
#endif
   !
   ! Evaluate V'(t+dt/2)
   !
   do ik=1,QP_nk
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) then
       V_tmp(:,:,ik,:)=cZERO
       cycle
     endif
     ik_mem=PAR_Xk_ibz_index(ik)
     do is=1,n_sp_pol
       call RK_basestep(Ktmp(:,:),V_tmp(:,:,ik,is),Ho_plus_Sigma(:,:,ik_mem,is),I_relax(:,:,:,ik_mem,is),E%nbf) 
       V_tmp(:,:,ik,is) = V_bands(:,:,ik,is) + RK4_apar(2)*RT_step*Ktmp(:,:)
       K_all(:,:,ik,is) = K_all(:,:,ik,is)   + RK4_bpar(2)*Ktmp(:,:)
     enddo
   enddo
   call PP_redux_wait(V_tmp,COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
   !
   ! Recalculate H(t) using  V'(t+dt/2) 
   !
#if defined _TIMING
 call timing('NL Integrator',OPR='stop')
#endif
   call NL_Hamiltonian(E,k,q,X,Time+RT_step*RK4_apar(2),i_time,V_tmp)
#if defined _TIMING
 call timing('NL Integrator',OPR='start')
#endif
   !
   ! Evaluate V'(t+dt)
   !
   do ik=1,QP_nk
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) then
       V_tmp(:,:,ik,:)=cZERO
       cycle
     endif
     ik_mem=PAR_Xk_ibz_index(ik)
     do is=1,n_sp_pol
       call RK_basestep(Ktmp(:,:),V_tmp(:,:,ik,is),Ho_plus_Sigma(:,:,ik_mem,is),I_relax(:,:,:,ik_mem,is),E%nbf) 
       V_tmp(:,:,ik,is) = V_bands(:,:,ik,is) + RK4_apar(3)*RT_step*Ktmp(:,:)
       K_all(:,:,ik,is) = K_all(:,:,ik,is) + RK4_bpar(3)*Ktmp(:,:)
     enddo
   enddo
   call PP_redux_wait(V_tmp,COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
   !
   ! Recalculate H(t) using  V'(t+dt) 
   !
#if defined _TIMING
 call timing('NL Integrator',OPR='stop')
#endif
   call NL_Hamiltonian(E,k,q,X,Time+RT_step*RK4_apar(3),i_time,V_tmp)
#if defined _TIMING
 call timing('NL Integrator',OPR='start')
#endif
   !
   ! final step |V(t+dt)>
   !
   do ik=1,QP_nk
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) then
       V_bands(:,:,ik,:)=cZERO
       cycle
     endif
     ik_mem=PAR_Xk_ibz_index(ik)
     do is=1,n_sp_pol
       call RK_basestep(Ktmp(:,:),V_tmp(:,:,ik,is),Ho_plus_Sigma(:,:,ik_mem,is),I_relax(:,:,:,ik_mem,is),E%nbf)
       K_all(:,:,ik,is) = K_all(:,:,ik,is) + RK4_bpar(4)*Ktmp(:,:)
       V_bands(:,:,ik,is) = V_bands(:,:,ik,is) + RT_step*K_all(:,:,ik,is)
     enddo
   enddo
   call PP_redux_wait(V_bands,COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
   ! 
   deallocate(V_tmp)
   !
 case default
   call error("Unknown integrator! ")
 end select
 !
#if defined _TIMING
 call timing('NL Integrator',OPR='stop')
#endif
 ! 
end subroutine NL_Integrator
