!
! License-Identifier: GPL
!
! Copyright (C) 2024 the Yambo Team
!
! Authors (see AUTHORS file for details): AM 
!
subroutine RT_Runge_Kutta_Fehlberg(E)
 !
 use pars,           ONLY:SP,schlen,cI
 use real_time,      ONLY:RT_bands,RT_step,NE_tot_time,NE_i_time,RT_dyn_step,&
&                         RK_integrators,N_RK_integrators
 use stderr,         ONLY:STRING_same
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:nXkibz
 use fields,         ONLY:gauge_field,Efield,n_ext_fields,Efield_strength
 use units,          ONLY:AUT2FS,SPEED_OF_LIGHT
 use vec_operate,    ONLY:normalize_v
 use OUTPUT,         ONLY:OUTPUT_driver
 !
#include<memory.h>
 !
 type(levels)      :: E
 !
 ! Work Space 
 !
 type(gauge_field)   :: A
 real(SP)            :: Damp,delta_E_max,delta_E
 real(SP),allocatable:: F(:),Time(:),Ef(:),dT_fehlberg(:,:)
 integer             :: N_steps,i_f,ik,ib,ibp
 !
 ! .. Runge-Kutta
 integer                :: i_Selected_RK_int(4),n,m,it
 character(schlen)      :: Selected_RK_int(4)
 !
 ! .. iterative dT optimization
 integer                :: i_int,i_iter,N_iter_steps,itp,it_trial
 integer,parameter      :: N_max_iterations=20
 complex(SP),allocatable:: Y(:,:),Y_converged(:)
 real(SP)               :: TRESH,ERR,dT_iter,dT_iter_prev
 complex(SP)            :: Y_trial,M_trial
 !
 call section('=','Time-Step evaluation via Runge-Kutta-Fehlber')
 !################################################################
 !
 ! Initial step & damping
 !------------------------
 RT_step=1./AUT2FS/1.E3_SP ! 1as 
 TRESH=1.E-1
 RT_dyn_step=RT_step
 Damp=0._SP
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
 !
 ! The elemental step here is 1as just defined in order to 
 ! define a max number of steps
 !---------------
 N_steps=NE_tot_time/RT_step
 YAMBO_ALLOC(F,(N_steps+1))
 YAMBO_ALLOC(Time,(N_steps+1))
 YAMBO_ALLOC(Ef,(N_steps+1))
 YAMBO_ALLOC(dT_fehlberg,(4,N_steps+1))
 YAMBO_ALLOC(Y,(4,N_steps+1))
 YAMBO_ALLOC(Y_converged,(N_steps+1))
 Y=0._SP
 dT_fehlberg=0._SP
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
 Selected_RK_int(1)  ='EULER'
 Selected_RK_int(2:3)=(/'RK2','RK3'/)
 Selected_RK_int(4)  ='RK41'
 !
 call Runge_Kutta_integrators(.FALSE.)
 !
 do it=1,N_RK_integrators
   do n=1,4
     if (STRING_same(Selected_RK_int(n),RK_integrators(it)%short)) i_Selected_RK_int(n)=it
   enddo
 enddo
 !
 ! Iterative solution
 !--------------------
 !
 ! ... reference (=RK4 with 1as time-step)
 Y(:,1)=1._SP
 it=1
 i_int=2
 do while (it<N_steps)
   dT_iter=RT_step
   call GIMME_val(i_int,it)
   Y(i_int,it+1)=Y_trial
   it=it+1
 enddo 
 Y_converged(:)=Y(i_int,:)
 call  OUTPUT_driver("converged",action="open")
 do it=1,N_steps+1 
   call  OUTPUT_driver("converged",TITLES=(/"Time"/),R_VALUES=(/Time(it)/),UNIT="fs",Kind="Time")
   call  OUTPUT_driver("converged",TITLES=(/trim(RK_integrators(i_Selected_RK_int(i_int))%short)//" [dT=1as]"/),&
&                      R_VALUES=(/aimag(Y_converged(it))/))
   call  OUTPUT_driver("converged",action="write")
 enddo
 call  OUTPUT_driver("converged",action="close")
 !
 ! ... iterative 
 do i_int=2,2
   !
   Y(i_int,1)=1._SP
   it=1
   do while (it<N_steps)
     !
     dT_iter     =RT_step
     dT_iter_prev=RT_step
     !
     ITER: do i_iter=1,N_max_iterations
       !
       ! T -> T+dT_iter (in Y_trial)
       call GIMME_val(i_int,it)
       !
       it_trial=it+int(dT_iter/RT_step)
       !
       ERR=abs(aimag(Y_trial-Y_converged(it_trial)))/abs(aimag(Y_converged(it_trial)))
       !
       if (ERR>TRESH.or.dT_iter>1._SP/AUT2FS.or.i_iter==N_max_iterations) then
         if (i_iter==1) write (*,*) it,ERR
         if (i_iter==1) STOP
         dT_iter=dT_iter_prev
         N_iter_steps=int(dT_iter/RT_step)
         it=it+N_iter_steps
         exit ITER
       endif
       !
       N_iter_steps=int(dT_iter/RT_step)
       dT_iter_prev=dT_iter
       dT_fehlberg(i_int,it:it+N_iter_steps-1)=dT_iter
       !
       M_trial=(Y_trial-Y(i_int,it))/dT_iter
       do itp=1,N_iter_steps
         Y(i_int,it+itp)=Y(i_int,it)+M_trial*itp*RT_step 
         write (100,*) Time(it+itp)*AUT2FS,aimag(Y(i_int,it+itp)),aimag(Y_converged(it+itp))
       enddo
       !
       dT_iter=dT_iter*2._SP
       !
     enddo ITER
     !
   enddo 
   !
 enddo
 !
 call  OUTPUT_driver("iterative",action="open")
 call  OUTPUT_driver("time_step",action="open")
 do it=1,N_steps+1 
   call  OUTPUT_driver("iterative",TITLES=(/"Time"/),R_VALUES=(/Time(it)/),UNIT="fs",Kind="Time")
   call  OUTPUT_driver("time_step",TITLES=(/"Time"/),R_VALUES=(/Time(it)/),UNIT="fs",Kind="Time")
   do i_int=1,1
     call  OUTPUT_driver("iterative",TITLES=(/trim(RK_integrators( i_Selected_RK_int(i_int) )%short)/),&
&                        R_VALUES=(/aimag(Y(i_int,it))/))
     call  OUTPUT_driver("time_step",TITLES=(/trim(RK_integrators( i_Selected_RK_int(i_int) )%short)/),&
&                        R_VALUES=(/dT_fehlberg(i_int,it)/),UNIT="as",Kind="Time")
   enddo
   call  OUTPUT_driver("iterative",action="write")
   call  OUTPUT_driver("ime_step",action="write")
 enddo
 call  OUTPUT_driver("iterative",action="close")
 call  OUTPUT_driver("time_step",action="close")
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
   subroutine GIMME_val(i_int,it)
     !
     real(SP)    :: T_eval
     integer     :: i_int,it
     complex(SP) :: K(4),Y_RK(4)
     !
     Y_RK=0._SP
     K   =0._SP
     !
     do n=1,RK_integrators( i_Selected_RK_int(i_int) )%N
       !
       Y_RK(n)=Y(i_int,it)
       if (n>1) then
         do m=1,n-1
           Y_RK(n)=Y_RK(n)+dT_iter*RK_integrators( i_Selected_RK_int(i_int) )%a(n,m)*K(m)
         enddo
       endif
       !
       T_eval=Time(it)+RK_integrators( i_Selected_RK_int(i_int) )%c(n)*dT_iter
       K(n) =-cI*S(T_eval)*Y_RK(n)
       !
     enddo
     !
     Y_trial=Y(i_int,it)
     do n=1,RK_integrators( i_Selected_RK_int(i_int) )%N
       Y_trial=Y_trial+RK_integrators( i_Selected_RK_int(i_int) )%b(n)*K(n)*dT_iter
     enddo
     !
   end subroutine
   !
end subroutine RT_Runge_Kutta_Fehlberg
