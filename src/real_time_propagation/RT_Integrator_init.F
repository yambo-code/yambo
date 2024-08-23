!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): DS AM
!
subroutine RT_Integrator_init()
 ! 
 use pars,           ONLY:SP,cONE,cZERO
 use stderr,         ONLY:intc,STRING_match
 use parser_m,       ONLY:parser
 use real_time,      ONLY:l_RT_EXP,l_RT_INV,l_RT_ACC,l_RT_DIAG,l_RT_LINEAR_RESP,l_RT_RWA, &
&                         Integrator_name,Integrator_exp_order,    &
&                         a_tableau,b_tableau,c_tableau,RWA_table, &
&                         I1_matrix,Integrator_nsteps,RT_bands,Integrator_slow_approx,l_slow_TE
 !
#include<memory.h>
 !
 ! Work Space
 !
 integer           :: ib
 !
 ! ... EXACT (EULER+HEUN) https://en.wikipedia.org/wiki/Heun%27s_method
 logical  :: l_RT_EULER
 logical  :: l_RT_HEUN
 !
 ! ... EXACT (RKN)
 logical  :: l_RT_RK2
 logical  :: l_RT_RK4
 !
 ! Integrator Kind (in the differential equations sense)
 !=================
 !
 l_RT_EULER=STRING_match(Integrator_name,'euler') 
 !
 l_RT_RK2  =STRING_match(Integrator_name,'rk2') 
 !
 l_RT_RK4  =STRING_match(Integrator_name,'rk4').and..not.l_RT_RK2
 !
 l_RT_HEUN =STRING_match(Integrator_name,'heun') .and..not.(l_RT_RK2.or.l_RT_RK4) 
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
   ! INV is alternative to EULER
   !------------------------------
   if (l_RT_INV) l_RT_EULER=.FALSE.
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
 ! Integrators tables
 !====================
 !
 a_tableau(1) = 0._SP
 c_tableau(1) = 0._SP    ! These are non-zero only for implicit methods
 RWA_table(1) = 0._SP
 !
 if (l_RT_RK2   ) then
   Integrator_nsteps = 2
   c_tableau(2) = 1._SP/2._SP
   a_tableau(2) = 1._SP/2._SP
   b_tableau(1:2) = (/0._SP      ,1._SP      /)
   RWA_table(2:3) = (/1._SP/2._SP,1._SP/2._SP/)
 else if (l_RT_HEUN  ) then
   Integrator_nsteps = 2
   c_tableau(2) = 1._SP
   a_tableau(2) = 1._SP
   b_tableau(1:2) = (/1._SP/2._SP,1._SP/2._SP/)
   RWA_table(2:3) = (/1._SP      ,0._SP      /)
 else if (l_RT_RK4   ) then
   Integrator_nsteps = 4
   c_tableau(2:4) = (/1._SP/2._SP,1._SP/2._SP,1._SP      /)
   a_tableau(2:4) = (/1._SP/2._SP,1._SP/2._SP,1._SP      /)
   b_tableau(1:4) = (/1._SP/6._SP,1._SP/3._SP,1._SP/3._SP,1._SP/6._SP/)
   RWA_table(2:3) = 0._SP
 else 
   Integrator_nsteps = 1
   b_tableau(1) = 1._SP
   c_tableau(2) = 1._SP
   RWA_table(2) = 1._SP
 endif
 ! 
 ! Linear regime
 !===============
 if (l_RT_EULER) call parser('linearRegime',l_RT_LINEAR_RESP)
 !
 ! RWA
 !=====
 l_RT_RWA=STRING_match(Integrator_name,'rwa') 
 !
 ! Rebuild Integrator Name
 !========================
 Integrator_name=""
 if (l_RT_EULER) Integrator_name  = "EULER"
 if (l_RT_RK2   ) Integrator_name = "RK2"
 if (l_RT_RK4   ) Integrator_name = "RK4"
 if (l_RT_HEUN  ) Integrator_name = "HEUN"
 if (l_RT_RWA   ) Integrator_name = trim(Integrator_name)//" RWA"
 !
 Integrator_slow_approx=""
 if (l_slow_TE) then
   if (l_RT_EXP  ) Integrator_slow_approx = trim(Integrator_slow_approx)//" EXP"//trim(intc(Integrator_exp_order))
   if (l_RT_INV  ) Integrator_slow_approx = trim(Integrator_slow_approx)//" INV"
   if (l_RT_ACC  ) Integrator_slow_approx = trim(Integrator_slow_approx)//"+ACCURATE"
   if (l_RT_DIAG ) Integrator_slow_approx = trim(Integrator_slow_approx)//"+DIAGO"
 endif
 !
end subroutine RT_Integrator_init
