!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): DS AM
!
subroutine Runge_Kutta_initialize()
 ! 
 use pars,           ONLY:SP,cONE,cZERO
 use stderr,         ONLY:intc,STRING_match
 use parser_m,       ONLY:parser
 use real_time,      ONLY:l_RT_EXP,l_RT_INV,l_RT_ACC,l_RT_DIAG,l_RT_LINEAR_RESP, &
&                         Integrator_name,Integrator_exp_order,RK_integrators,RK_integrator, &
&                         I1_matrix,RT_bands,Integrator_slow_approx,l_slow_TE,N_RK_integrators,&
&                         RK_integrator_alloc
 !
#include<memory.h>
 !
 ! Work Space
 !
 integer           :: ib,is,i_active_integrator
 !
 ! Load the RK integrators
 !=========================
 call Runge_Kutta_integrators(.FALSE.)
 !
 ! Find the user defined 
 !=======================
 do is=1,N_RK_integrators
   if (STRING_match(RK_integrators(is)%short,Integrator_name)) i_active_integrator=is
 enddo
 !
 if (.not.STRING_match(Integrator_slow_approx,"no")) then
   !
   ! Slow dynamics specific approximations
   !===============
   !
   l_RT_EXP=STRING_match(Integrator_slow_approx,'EXP') 
   !
   l_RT_INV=.not.l_RT_EXP.and.STRING_match(Integrator_slow_approx,'INV')  
   !
   if (l_RT_EXP) then
     Integrator_exp_order=3
     if ( STRING_match(Integrator_slow_approx,'EXP1') )  Integrator_exp_order=1
     if ( STRING_match(Integrator_slow_approx,'EXP2') )  Integrator_exp_order=2
     if ( STRING_match(Integrator_slow_approx,'EXP3') )  Integrator_exp_order=3
     if ( STRING_match(Integrator_slow_approx,'EXP4') )  Integrator_exp_order=4
     if ( STRING_match(Integrator_slow_approx,'EXP5') )  Integrator_exp_order=5
     if ( STRING_match(Integrator_slow_approx,'EXP6') )  Integrator_exp_order=6
   endif
   !
   l_RT_ACC  =(l_RT_INV.or.l_RT_EXP).and.STRING_match(Integrator_slow_approx,'acc')
   !
   l_RT_DIAG =l_RT_INV.and.STRING_match(Integrator_slow_approx,'diag')
   !
   l_slow_TE=any((/l_RT_EXP,l_RT_INV/))
   !
   !
   if (l_RT_INV.or.l_RT_EXP) then
     YAMBO_ALLOC(I1_matrix,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2)))
     I1_matrix=cZERO
     do ib=RT_bands(1),RT_bands(2)
       I1_matrix(ib,ib)=cONE
     enddo
   endif
   !
 endif
 !
 ! Defaults...
 ! ... INV is alternative to EULER. In this case set the active to be RK2 (Nr. 3)
 !=============
 if (i_active_integrator<0.or.(l_RT_INV.and.i_active_integrator==1)) i_active_integrator=3
 !
 ! Linear regime
 !===============
 if (i_active_integrator==1) call parser('linearRegime',l_RT_LINEAR_RESP)
 !
 ! Rebuild Integrator Name(s)
 !===========================
 Integrator_name=RK_integrators(i_active_integrator)%short
 if (.not.STRING_match(Integrator_slow_approx,"no")) then
   Integrator_slow_approx=""
   if (l_slow_TE) then
     if (l_RT_EXP  ) Integrator_slow_approx = trim(Integrator_slow_approx)//" EXP"//trim(intc(Integrator_exp_order))
     if (l_RT_INV  ) Integrator_slow_approx = trim(Integrator_slow_approx)//" INV"
     if (l_RT_ACC  ) Integrator_slow_approx = trim(Integrator_slow_approx)//"+ACCURATE"
     if (l_RT_DIAG ) Integrator_slow_approx = trim(Integrator_slow_approx)//"+DIAGO"
   endif
 endif
 !
 ! Copy the active integrator in the RT_integrator type
 !======================================================
 call RK_integrator_alloc(RK_integrator,RK_integrators(i_active_integrator)%N,&
&                         trim(RK_integrators(i_active_integrator)%short),&
&                         trim(RK_integrators(i_active_integrator)%name))
 RK_integrator%a=RK_integrators(i_active_integrator)%a
 RK_integrator%b=RK_integrators(i_active_integrator)%b
 RK_integrator%c=RK_integrators(i_active_integrator)%c
 !
end subroutine Runge_Kutta_initialize
