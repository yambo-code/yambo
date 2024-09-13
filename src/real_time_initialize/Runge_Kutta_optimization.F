!
! License-Identifier: GPL
!
! Copyright (C) 2024 the Yambo Team
!
! Authors (see AUTHORS file for details): AM 
!
subroutine Runge_Kutta_optimization(E)
 !
 use pars,           ONLY:SP,cI
 use real_time,      ONLY:RT_bands,RT_step,NE_tot_time,NE_i_time,RT_dyn_step,&
&                         RK_integrators,N_RK_integrators,N_RK_integrators
 use stderr,         ONLY:STRING_same
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:nXkibz
 use fields,         ONLY:gauge_field,Efield,n_ext_fields,Efield_strength
 use units,          ONLY:AUT2FS,SPEED_OF_LIGHT
 use vec_operate,    ONLY:normalize_v
 use OUTPUT,         ONLY:OUTPUT_driver
 use LIVE_t,         ONLY:live_timing
 !
#include<memory.h>
 !
 type(levels)      :: E
 !
 ! Work Space 
 !
 type(gauge_field)   :: A
 real(SP)            :: Damp,delta_E_max,delta_E
 real(SP),allocatable:: Time(:),Ef(:)
 integer             :: N_steps,i_f,ik,ib,ibp
 !
 ! .. iterative dT optimization
 integer                :: i_iter,N_iter_steps,it,itp,i_best_int,i_best_iter,i_int,m,n
 integer,parameter      :: N_max_iterations=20
 complex(SP),allocatable:: Y(:,:,:)
 real(SP)               :: TRESH,ERR(N_RK_integrators,N_max_iterations),dT_iter
 complex(SP)            :: Y_trial,M_trial
 !
 call section('=','Better Runge-Kutta integrator estimation')
 !############################################################
 !
 ! Initial step & damping
 !------------------------
 RT_step=1./AUT2FS/1.E3_SP ! 1as 
 TRESH=10.E-3
 RT_dyn_step=RT_step
 !Damp=100.0_SP/HA2EV/1000. ! 50 meV
 !
 ! Fields init
 !-------------
 do i_f=1,n_ext_fields
   if (STRING_same(Efield(i_f)%ef_name,'none')) cycle
   Efield(i_f)%versor        =normalize_v(Efield(i_f)%versor)
   Efield(i_f)%amplitude     =Efield_strength(Efield(i_f)%intensity,'AU')
   Efield(i_f)%t_initial_indx=max(nint(Efield(i_f)%t_initial/RT_step)+1,2)
   Efield(i_f)%t_initial     =real(Efield(i_f)%t_initial_indx-1)*RT_step
 enddo
 !
 ! Steps & Allocs
 !----------------
 N_steps=NE_tot_time/RT_step
 YAMBO_ALLOC(Time,(N_steps+1))
 YAMBO_ALLOC(Ef,(N_steps+1))
 YAMBO_ALLOC(Y,(N_RK_integrators,N_max_iterations,N_steps+1))
 !
 ! Elemental frequencies
 !=======================
 !
 ! .. Electronic transitions
 !----------------------------
 delta_E_max=-1.
 do ik=1,nXkibz
   do ib=RT_bands(1),RT_bands(2)
     do ibp=RT_bands(1),RT_bands(2)
       if (ib==ibp) cycle
       delta_E=abs(E%E(ib,ik,1)-E%E(ibp,ik,1))
       delta_E_max=max(delta_E,delta_E_max)
     enddo
   enddo
 enddo 
 !
 ! .. Efield
 !-----------
 NE_i_time=0
 do it=1,N_steps+1
   Time(it)=RT_step*(it-1)
   NE_i_time=NE_i_time+1
   call RT_Ext_fields(A,Time(it),-1)
   Ef(it)=dot_product(real(A%vecpot_vel),(/1.,1.,1./))/SPEED_OF_LIGHT/sqrt(3._SP)
 enddo
 !
 ! RK integrators
 !----------------
 call Runge_Kutta_integrators(.FALSE.)
 !
 ! Integrations
 !--------------
 call live_timing('RK integrations',N_RK_integrators)
 !
 ERR=10._SP*TRESH
 !
 do i_int=1,N_RK_integrators
   !
   ITER_LOOP: do i_iter=1,N_max_iterations
     !
     Y(i_int,i_iter,1)=1._SP
     !
     if (i_iter==1) dT_iter=RT_step
     if (i_iter> 1) dT_iter=5._SP*real(i_iter-1,SP)*RT_step
     !
     it=1
     do while (it<N_steps)
       call SOLVE_it(i_int,Y(i_int,i_iter,it),it,Y_trial)
       N_iter_steps=int(dT_iter/RT_step)
       M_trial=(Y_trial-Y(i_int,i_iter,it))/dT_iter
       do itp=1,N_iter_steps
         if (it+itp>=N_steps) cycle
         Y(i_int,i_iter,it+itp)=Y(i_int,i_iter,it)+M_trial*itp*RT_step 
       enddo
       it=it+N_iter_steps
     enddo
     !
     if (i_iter>1) then
       call EVAL_the_diff(i_int,i_iter,ERR(i_int,i_iter))
       if (ERR(i_int,i_iter)>TRESH) exit ITER_LOOP
     endif
     !
   enddo ITER_LOOP
   !
   write (*,*) i_int,ERR(i_int,:)
   call live_timing(steps=1)
   !
 enddo
 call live_timing( )
 !
 do i_int=N_RK_integrators,1,-1
   do i_iter=2,N_max_iterations-1
     if (ERR(i_int,i_iter)<TRESH.and.ERR(i_int,i_iter+1)>TRESH) then
       i_best_int=i_int 
       i_best_iter=i_int 
     endif
   enddo
 enddo
 !
 write (*,*) trim(RK_integrators(i_best_int)%short) 
 write (*,*) 5._SP*real(i_best_iter-1,SP)*RT_step
 stop
 !
 call  OUTPUT_driver("RK_check",action="open")
 do it=1,N_steps+1 
   call  OUTPUT_driver("RK_check",TITLES=(/"Time"/),R_VALUES=(/Time(it)/),UNIT="fs",Kind="Time")
   do i_int=1,N_RK_integrators
     call  OUTPUT_driver("RK_check",TITLES=(/trim(RK_integrators(i_int)%short)//" dT=1as"/),R_VALUES=(/aimag(Y(i_int,1,it))/))
     !call  OUTPUT_driver("RK_check",TITLES=(/trim(RK_integrators(i_int)%short)//" dT=1as"/),R_VALUES=(/aimag(Y(i_int,1,it))/))
   enddo
   call  OUTPUT_driver("RK_check",action="write")
 enddo
 call  OUTPUT_driver("RK_check",action="close")
 !
 stop
 !
 contains
   !
   real(SP) function S(T)
     !
     real(SP)::T
     integer ::itp
     !
     do itp=1,N_steps+1
       if (T>=Time(itp).and.T<Time(itp+1)) then
         S=Ef(itp)*sin(delta_E_max*Time(itp))*exp(-Damp*Time(itp))
       endif
     enddo
     !
   end function
   !
   subroutine EVAL_the_diff(I,iter,ERR)
     integer  :: I,iter
     real(SP) :: ERR,DIFF_max,VAL_max,DIFF
     DIFF_max=-1.
     VAL_max =-1.
     do it=1,N_steps
       DIFF=abs(aimag(Y(I,iter,it))-aimag(Y(I,1,it)))
       if (DIFF>DIFF_max) DIFF_max=DIFF
       if (abs(aimag(Y(I,1,it)))>VAL_max) VAL_max=abs(aimag(Y(I,1,it)))
     enddo
     ERR=DIFF_max/VAL_max
   end subroutine
   !
   subroutine SOLVE_it(I,F,it,Fp)
     !
     real(SP)    :: T_eval
     integer     :: I,it
     complex(SP) :: K(4),Y_RK(4),F,Fp
     !
     Y_RK=0._SP
     K   =0._SP
     !
     do n=1,RK_integrators(I)%N
       !
       Y_RK(n)=F
       if (n>1) then
         do m=1,n-1
           Y_RK(n)=Y_RK(n)+dT_iter*RK_integrators(I)%a(n,m)*K(m)
         enddo
       endif
       !
       T_eval=Time(it)+RK_integrators(I)%c(n)*dT_iter
       K(n) =-cI*S(T_eval)*Y_RK(n)
       !
     enddo
     !
     Fp=F
     do n=1,RK_integrators(I)%N
       Fp=Fp+RK_integrators(I)%b(n)*K(n)*dT_iter
     enddo
     !
   end subroutine
   !
end subroutine Runge_Kutta_optimization
