!
!        Copyright (C) 2000-2017 the YAMBO team
!            http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine RT_Integrator_init()
 ! 
 use pars,           ONLY:SP,cONE,cZERO
 use com,            ONLY:msg
 use stderr,         ONLY:intc
 use real_time,      ONLY:l_RT_EULER,l_RT_EXP,l_RT_INV,            &
&                         l_RT_RWA,l_RT_ACC,l_RT_DIAG,l_RT_PERT,   &
&                         l_RT_simple,l_RT_HEUN,l_RT_RK2,l_RT_RK4, &
&                         Integrator_exp_order,Integrator_name,    &
&                         a_tableau,b_tableau,c_tableau,RWA_table, &
&                         I1_matrix,Integrator_nsteps,RT_bands
 !
#include<memory.h>
 !
 integer  :: ib
 !
 ! a) Propagation kind: Euler / Exp /
 !
 l_RT_EXP  =(index(Integrator_name,'EXP'  )/=0.or.index(Integrator_name,'exp'  )/=0)
 !
 l_RT_EULER=(index(Integrator_name,'EULER')/=0.or.index(Integrator_name,'euler')/=0).and..not. l_RT_EXP
 !
 l_RT_INV  =(index(Integrator_name,'INV'  )/=0.or.index(Integrator_name,'inv'  )/=0).and..not.(l_RT_EXP.or.l_RT_EULER)
 !
 if ( .not.(l_RT_EXP.or.l_RT_EULER.or.l_RT_INV) ) l_RT_EULER=.TRUE.
 !
 if (l_RT_EXP) then
   integrator_exp_order=3
   if ( index(Integrator_name,'EXP1')/=0 .or. index(Integrator_name,'exp1')/=0 )  integrator_exp_order=1
   if ( index(Integrator_name,'EXP2')/=0 .or. index(Integrator_name,'exp2')/=0 )  integrator_exp_order=2
   if ( index(Integrator_name,'EXP3')/=0 .or. index(Integrator_name,'exp3')/=0 )  integrator_exp_order=3
   if ( index(Integrator_name,'EXP4')/=0 .or. index(Integrator_name,'exp4')/=0 )  integrator_exp_order=4
   if ( index(Integrator_name,'EXP5')/=0 .or. index(Integrator_name,'exp5')/=0 )  integrator_exp_order=5
   if ( index(Integrator_name,'EXP6')/=0 .or. index(Integrator_name,'exp6')/=0 )  integrator_exp_order=6
 endif
 !
 ! b) Multi step approach: RK2 / HEUN
 !
 l_RT_RK2  = (index(Integrator_name,'RK2' )/=0.or.index(Integrator_name,'rk2'  )/=0)
 !
 l_RT_RK4  = (index(Integrator_name,'RK4' )/=0.or.index(Integrator_name,'rk4'  )/=0).and..not. l_RT_RK2
 !
 l_RT_HEUN = (index(Integrator_name,'HEUN')/=0.or.index(Integrator_name,'heun' )/=0).and..not.(l_RT_RK2.or.l_RT_RK4)
 !
 l_RT_SIMPLE=.not.(l_RT_RK2.or.l_RT_RK4.or.l_RT_HEUN)
 !
 ! c) Extra
 !
 l_RT_RWA = index(Integrator_name,'RWA'  )/=0.or.index(Integrator_name,'rwa'  )/=0
 !
 l_RT_ACC = (l_RT_INV  .or. l_RT_EXP) .and. &
            index(Integrator_name,'ACC'  )/=0.or.index(Integrator_name,'acc'  )/=0
 !
 l_RT_DIAG=  l_RT_INV                 .and. &
&           index(Integrator_name,'DIAG' )/=0.or.index(Integrator_name,'diag' )/=0
 !
 l_RT_PERT= (l_RT_EULER.and.l_RT_RWA) .and. &
&           index(Integrator_name,'PERT'  )/=0.or.index(Integrator_name,'pert' )/=0
 !
 ! d) Reset integrator name
 !
 Integrator_name=""
 if (l_RT_EULER) Integrator_name = "EULER"
 if (l_RT_EXP  ) Integrator_name = "EXP"//trim(intc(integrator_exp_order))
 if (l_RT_INV  ) Integrator_name = "INV"
 !
 if (l_RT_SIMPLE) Integrator_name = trim(Integrator_name)//" + SIMPLE"
 if (l_RT_RK2   ) Integrator_name = trim(Integrator_name)//" + RK2"
 if (l_RT_RK4   ) Integrator_name = trim(Integrator_name)//" + RK4"
 if (l_RT_HEUN  ) Integrator_name = trim(Integrator_name)//" + HEUN"
 !
 if (l_RT_RWA   ) Integrator_name = trim(Integrator_name)//" + RWA"
 if (l_RT_ACC   ) Integrator_name = trim(Integrator_name)//" + ACC"
 if (l_RT_DIAG  ) Integrator_name = trim(Integrator_name)//" + DIAG"
 if (l_RT_PERT  ) Integrator_name = trim(Integrator_name)//" + PERT"
 !
 call msg('s','Integrator: '//trim(Integrator_name))
 !
 ! e) Define integrators tables
 !
 a_tableau(1) = 0._SP
 c_tableau(1) = 0._SP    ! These are non-zero only for implicit methods
 RWA_table(1) = 0._SP
 !
 if (l_RT_SIMPLE) then
   Integrator_nsteps = 1
   b_tableau(1) = 1._SP
   c_tableau(2) = 1._SP
   RWA_table(2) = 1._SP
 endif
 !
 if (l_RT_RK2   ) then
   Integrator_nsteps = 2
   c_tableau(2) = 1._SP/2._SP
   a_tableau(2) = 1._SP/2._SP
   b_tableau(1:2) = (/0._SP      ,1._SP      /)
   RWA_table(2:3) = (/1._SP/2._SP,1._SP/2._SP/)
 endif
 !
 if (l_RT_HEUN  ) then
   Integrator_nsteps = 2
   c_tableau(2) = 1._SP
   a_tableau(2) = 1._SP
   b_tableau(1:2) = (/1._SP/2._SP,1._SP/2._SP/)
   RWA_table(2:3) = (/1._SP      ,0._SP      /)
 endif
 !
 if (l_RT_RK4   ) then
   Integrator_nsteps = 4
   c_tableau(2:4) = (/1._SP/2._SP,1._SP/2._SP,1._SP      /)
   a_tableau(2:4) = (/1._SP/2._SP,1._SP/2._SP,1._SP      /)
   b_tableau(1:4) = (/1._SP/6._SP,1._SP/3._SP,1._SP/3._SP,1._SP/6._SP/)
   !RWA_table(2:3) = NOT DEFINED
 endif
 !
 ! This is needed by some integrators
 !
 YAMBO_ALLOC(I1_matrix,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2)))
 !
 I1_matrix=cZERO
 do ib=RT_bands(1),RT_bands(2)
   I1_matrix(ib,ib)=cONE
 enddo
 !
end subroutine RT_Integrator_init
