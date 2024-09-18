!
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA DS
!
subroutine Runge_Kutta_driver(G_tpdt,dG_tpdt,dG_t,A_tpdt,A_t,E,k,q)
 !
 ! O_tpdt=O^<(t+dt)
 ! O_t   =O^<(t)
 !
 ! where O=G,A,dG
 !
 use pars,           ONLY:SP,cI
 use R_lattice,      ONLY:bz_samp
 use RT_control,     ONLY:TIME_adjust
 use real_time,      ONLY:RT_nk,RT_bands,RTibz,G_lesser_reference,&
&                         NE_time,NE_i_time,RT_step,RT_dyn_step,RK_integrator,H_RF
 use fields,         ONLY:gauge_field
 use electrons,      ONLY:levels
 use timing_m,       ONLY:timing
 use parallel_m,     ONLY:PAR_G_k_range,PAR_IND_Xk_ibz
 !
 implicit none
 !
 type(bz_samp),        intent(in) :: k,q
 type(levels),      intent(inout) :: E
 complex(SP),       intent(inout) :: G_tpdt(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2))
 complex(SP),       intent(inout) :: dG_t(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2))
 complex(SP),         intent(out) :: dG_tpdt(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2))
 type(gauge_field),    intent(in) :: A_t
 type(gauge_field),   intent(out) :: A_tpdt
 !
 ! Work Space
 !
 integer           :: i_step,j_step,ik
 complex(SP)       :: RK_kernel(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2),RK_integrator%N)
 complex(SP)       :: RK_dG(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2),RK_integrator%N)
 real(SP)          :: eval_T
 !
 call timing('RT integrator',OPR='start')
 !
 do i_step=1,RK_integrator%N
   !
   ! Intermediate RK time
   !----------------------
   eval_T=NE_time+RK_integrator%c(i_step)*RT_dyn_step
   !
   do ik=1,RT_nk
     !
     if( .not.PAR_IND_Xk_ibz%element_1D(RTibz%k_map(ik)) ) cycle
     !
     ! dG=dG_t (in the rotating frame (RF))
     !---------
     RK_dG(:,:,ik,i_step)=dG_t(:,:,ik)*exp(cI*H_RF(:,:,ik,1)*eval_T) !RF
     !
     if (i_step>1) then
       !
       ! K_m (m<n) => dG_n (in the RF)
       !-------------------------------
       do j_step=1,i_step-1
         RK_dG(:,:,ik,i_step)=RK_dG(:,:,ik,i_step)+RK_integrator%a(i_step,j_step)*RK_kernel(:,:,ik,j_step)*RT_dyn_step ! RF
       enddo
       !
     endif
     !
     ! Back to the still basis (SB)
     !------------------------------
     RK_dG(:,:,ik,i_step)=RK_dG(:,:,ik,i_step)*exp(-cI*H_RF(:,:,ik,1)*eval_T)  ! SB
     !
   enddo
   !
   if (i_step>1) then
       !
       call timing('RT integrator',OPR='stop')
       !
       ! dG_n => New Hamiltonian@(t+c_n dt) Eq. 11b of Overleaf Notes (in the SB)
       !---------------------------------------------------------------------------
       !
       ! ... E_ext(t+c*dt) (needed by RT_Hamiltonian)
       call RT_Ext_fields(A_t,eval_T,-1)
       !
       ! ... H(t+c*dt)
       call RT_Hamiltonian(RK_dG(:,:,:,i_step),A_t,E,k)
       !
       call timing('RT integrator',OPR='start')
       !
   endif
   !
   do ik=1,RT_nk
     !
     if( .not.PAR_IND_Xk_ibz%element_1D(RTibz%k_map(ik)) ) cycle
     !
     ! K_n (in the SB)
     !-----------------
     call Runge_Kutta_kernel(ik,RK_dG(:,:,ik,i_step),RK_kernel(:,:,ik,i_step))
     !
     ! K_n must be rotated in the RF
     !-------------------------------
     RK_kernel(:,:,ik,i_step)=RK_kernel(:,:,ik,i_step)*exp(cI*H_RF(:,:,ik,1)*eval_T) ! RF
     !
   enddo
   !
 enddo
 !
 ! Final t -> t+dt step ...
 !==========================
 !
 ! ... G_lesser(t+dt)
 eval_T=NE_time+RT_dyn_step
 !
 do ik=1,RT_nk
   !
   if( .not.PAR_IND_Xk_ibz%element_1D(RTibz%k_map(ik)) ) cycle
   !
   ! The following step(s) are in the RF
   !-------------------------------------
   dG_tpdt(:,:,ik)=dG_t(:,:,ik)*exp(cI*H_RF(:,:,ik,1)*NE_time)
   do i_step=1,RK_integrator%N
     dG_tpdt(:,:,ik)=dG_tpdt(:,:,ik)+RK_integrator%b(i_step)*RK_kernel(:,:,ik,i_step)*RT_dyn_step
   enddo
   !
   ! Final rotation and sum in the SB
   !----------------------------------
   dG_tpdt(:,:,ik)=dG_tpdt(:,:,ik)*exp(-cI*H_RF(:,:,ik,1)*eval_T)
   G_tpdt(:,:,ik) =G_lesser_reference(:,:,ik)+dG_tpdt(:,:,ik)
   !
   call RT_G_symmetrization(ik,G_tpdt)
   !
 enddo
 !
 ! ... E_ext(t+dt)
 call RT_Ext_fields(A_t,eval_T,-1)
 !
 ! New occupations @ t+dt
 !========================
 call RT_occupations_eval(dG_tpdt)
 !
 ! New external field @ t+dt
 !===========================
 !
 ! Update time
 !=============================================
 NE_i_time       = NE_i_time+nint(RT_dyn_step/RT_step)
 NE_time         = TIME_adjust((NE_i_time-1)*RT_step)
 !
 call timing('RT integrator',OPR='stop')
 !
end subroutine Runge_Kutta_driver
