!
! License-Identifier: GPL
!
! Copyright (C) 2024 the Yambo Team
!
! Authors (see AUTHORS file for details): AM 
!
subroutine Runge_Kutta_step_and_order_estimation(E)
 !
 use pars,           ONLY:SP,cI,lchlen
 use real_time,      ONLY:RT_bands,RT_step,NE_tot_time,NE_i_time,RT_dyn_step,Integrator_name,&
&                         RK_integrators,N_RK_integrators,N_RK_integrators,RK_quest_treshold
 use stderr,         ONLY:STRING_same,real2ch,set_real_printed_length,intc
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:nXkibz
 use fields,         ONLY:gauge_field,Efield,n_ext_fields,Efield_strength
 use units,          ONLY:AUT2FS,SPEED_OF_LIGHT
 use vec_operate,    ONLY:normalize_v
 use OUTPUT,         ONLY:OUTPUT_driver
 use com,            ONLY:msg
 use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
 use LIVE_t,         ONLY:live_timing
 !
#include<memory.h>
 !
 type(levels)      ::E
 !
 ! Work Space 
 !
 type(gauge_field)   ::A
 real(SP)            ::Damp,delta_E_max,delta_E
 real(SP),allocatable::Time(:),Ef(:)
 integer             ::N_steps,i_f,ik,ib,ibp
 !
 ! .. iterative dT optimization
 integer,parameter      ::N_max_iterations=20
 integer                ::i_iter,N_iter_steps,it,itp,i_int,m,n,i_int_ref(1),dT_table(N_max_iterations),i_best_int
 complex(SP),allocatable::Y(:,:,:)
 real(SP)               ::ERR(N_RK_integrators,N_max_iterations),dT_iter(N_max_iterations),&
&                         dT(N_RK_integrators),TRESH
 complex(SP)            ::Y_trial,M_trial
 character(lchlen)      ::string
 type(PP_indexes)       ::px
 !
 call section('=','Runge-Kutta order and dT estimation')
 !#######################################################
 !
 ! Initial step & damping
 !------------------------
 if (abs(RT_step)>0) then
   RT_step=abs(RT_step)
 else
   RT_step=1./AUT2FS/1.E3_SP ! 1as 
 endif
 RT_dyn_step=RT_step
 TRESH=RK_quest_treshold/1000._SP
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
 ! Test time-steps
 !-----------------
 do i_iter=1,N_max_iterations
   if (i_iter==1) dT_iter(i_iter)=RT_step
   if (i_iter> 1) dT_iter(i_iter)=5._SP*real(i_iter-1,SP)*RT_step
 enddo
 !
 ! Integrations
 !==============
 ERR      =0._SP
 i_int_ref=0
 !
 ! Parallel distribution
 !-----------------------
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/N_RK_integrators/))
 if (px%n_of_elements(myid+1)>0) call live_timing('dT optimization',px%n_of_elements(myid+1))
 !
 do i_int=1,N_RK_integrators
   !
   if (.not.px%element_1D(i_int)) cycle
   !
   ERR(i_int,2:)=10._SP*TRESH
   !
   if (.not.STRING_same(Integrator_name,"best")) then
     if (.not.STRING_same(Integrator_name,RK_integrators(i_int)%short)) then
       call live_timing(steps=1)
       cycle
     endif
     i_int_ref=i_int
   endif
   !
   ITER_LOOP: do i_iter=1,N_max_iterations
     !
     Y(i_int,i_iter,1)=1._SP
     !
     it=1
     do while (it<N_steps)
       call SOLVE_it(i_int,it,Y(i_int,i_iter,it),Y_trial,dT_iter(i_iter))
       N_iter_steps=int(dT_iter(i_iter)/RT_step)
       M_trial=(Y_trial-Y(i_int,i_iter,it))/dT_iter(i_iter)
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
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing(steps=1)
 !
 call PP_redux_wait(ERR)
 call PP_redux_wait(i_int_ref)
 if (i_int_ref(1)==0) i_int_ref=7
 !
 if (all(ERR(:,2:)>TRESH)) call warning("Impossible to reduce the time-step. Using user provided.")
 !
 ! dT table
 !----------
 dT=-1._SP
 dT_table=0
 do i_int=1,N_RK_integrators
   do i_iter=2,N_max_iterations-1
     if (ERR(i_int,i_iter)<TRESH.and.ERR(i_int,i_iter+1)>TRESH) then
       dT(i_int)=dT_iter(i_iter)
       dT_table(i_int)=i_iter
     endif
   enddo
 enddo
 !
 ! Output...
 !-----------
 !
 ! ... CHECK file
 string="RK_dT_and_order_estimation"
 call  OUTPUT_driver(trim(string),action="open")
 n=nint(NE_tot_time/RT_step)
 do it=1,N_steps+1 
   call  OUTPUT_driver(trim(string),TITLES=(/"Time"/),R_VALUES=(/Time(it)/),UNIT="fs",Kind="Time")
   call set_real_printed_length(f_length=5)
   call  OUTPUT_driver(trim(string),TITLES=(/trim(RK_integrators(i_int_ref(1))%short)//" [dT=1as]"/),&
&                      R_VALUES=(/aimag(Y(i_int_ref(1),1,it))/))
   call set_real_printed_length( )
   do i_int=1,N_RK_integrators
     if (dT_Table(i_int)==0) cycle
     if (nint(NE_tot_time/dT(i_int))*RK_integrators(i_int)%N<n) then
       n=nint(NE_tot_time/dT(i_int))*RK_integrators(i_int)%N
       i_best_int=i_int
     endif
     call  OUTPUT_driver(trim(string),&
&                        TITLES=(/trim(RK_integrators(i_int)%short)//" [dT="//trim(real2ch(dT(i_int)*AUT2FS*1000._SP))//"as]"/),&
&                        R_VALUES=(/aimag(Y(i_int,1,it))/))
   enddo
   call  OUTPUT_driver(trim(string),action="write")
 enddo
 call  OUTPUT_driver(trim(string),action="close")
 !
 ! ... report
 call msg("r","Integrator   'dT [as]'  'ERROR [o/oo]'   '#of Collision calls (T/dTxORDER)'")
 call set_real_printed_length(f_length=5)
 do i_int=1,N_RK_integrators
   if (dT_Table(i_int)==0) cycle
   n=nint(NE_tot_time/dT(i_int))*RK_integrators(i_int)%N
   string=trim(RK_integrators(i_int)%short)
   string =string(1:5)//"        "//trim(real2ch(dT(i_int)*AUT2FS*1000_SP))
   string=trim(string)//"      "//trim(real2ch(ERR(i_int,dT_Table(i_int))*1000._SP))
   string=trim(string)//"             "//trim(intc(n))
   if (i_int==i_best_int) then
     string=trim(string)//" [SELECTED]"
     RT_step=dT(i_int)
     Integrator_name=trim(RK_integrators(i_int)%short)
   endif
   call msg("r",string)
 enddo
 call msg("nrs","RK best integrator-dT is: "//trim(Integrator_name)//" - "//trim(real2ch(RT_step*AUT2FS*1000_SP))//" [as]")
 call set_real_printed_length( )
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
   subroutine SOLVE_it(I,it,F,Fp,dT_)
     !
     real(SP)    :: T_eval,dT_
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
           Y_RK(n)=Y_RK(n)+dT_*RK_integrators(I)%a(n,m)*K(m)
         enddo
       endif
       !
       T_eval=Time(it)+RK_integrators(I)%c(n)*dT_
       K(n) =-cI*S(T_eval)*Y_RK(n)
       !
     enddo
     !
     Fp=F
     do n=1,RK_integrators(I)%N
       Fp=Fp+RK_integrators(I)%b(n)*K(n)*dT_
     enddo
     !
   end subroutine
   !
end subroutine Runge_Kutta_step_and_order_estimation
