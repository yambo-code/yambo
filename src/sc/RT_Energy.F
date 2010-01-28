!
! Copyright (C) 2000-2010 A. Marini, C. Attaccalite and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine RT_Energy(en,k,E_kin,E_ion,E_h,Ex,Exc,E_mg,Trace_Gf)
 !
 use pars,           ONLY:SP,DP,pi
 use units,          ONLY:SPEED_OF_LIGHT
 use drivers,        ONLY:l_sc_hf,l_sc_contains_lda,l_sc_chosex
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:DL_vol
 use SC,             ONLY:V_hartree_sc,Ho
 use real_time,      ONLY:XC0,G_lesser,RT_average_operator,XCfrozen, &
&                         l_NE_with_fields,A_vecpot
 use X_m,            ONLY:P_square
 use electrons,      ONLY:levels
 use QP_m,           ONLY:QP_Vnl_xc,QP_Sc
 use xc_functionals, ONLY:E_xc
 use global_XC,      ONLY:WF_kind,WF_xc_functional
 !
 implicit none 
 !
 type(levels),  intent(in) :: en
 type(bz_samp), intent(in) :: k
 complex(SP),   intent(out):: E_kin,E_ion,E_h,Trace_Gf,Exc,Ex,E_mg
 !
 ! Non-interaction kinetc energy
 !
 ! E_Kin = - i Tr { P^2 G^<}/2  
 !
 call RT_average_operator(VALUE=E_kin,O_MATRIX=P_Square,G=G_lesser,k=k)
 !
 E_kin=E_kin/2.
 !  
 ! E_ion = - i Tr {Ho G^<} - Ekin
 !
 call RT_average_operator(VALUE=E_ion,O_MATRIX=Ho,G=G_lesser,k=k)
 !
 E_ion = E_ion - E_kin
 !
 ! Hartree potential Energy
 !
 call RT_average_operator(VALUE=E_h,O_REAL=V_hartree_sc,G=G_lesser,k=k)
 !
 E_h=E_h/2._SP
 !
 if(l_sc_hf.or.l_sc_chosex) then
   !
   call RT_average_operator(VALUE=Ex,O_QP=QP_Vnl_xc,G=G_lesser,k=k)
   !
   Ex=Ex/2._SP
   !
 else
   !
   Ex=(0._SP,0._SP)
   !
 endif
 !
 if(XCfrozen) then
   !
   call RT_average_operator(VALUE=Exc,O_MATRIX=XC0,G=G_lesser,k=k)
   !
 else
   !
   if(l_sc_contains_lda) then 
     !        
     ! E_xc Energy 
     !
     call IO_and_Messaging_switch("report log io_in io_out")
     call xc_lda_driver(en,k,WF_KIND,WF_xc_functional,0,G_lesser)
     call IO_and_Messaging_switch("  ")
     !
     call RT_average_operator(VALUE=Exc,O_REAL=E_xc,G=G_lesser,k=k)
     !
   elseif(l_sc_chosex) then
     !      
     call RT_average_operator(VALUE=Exc,O_QP=QP_Sc,G=G_lesser,k=k)
     !
   else
     !
     Exc=(0._SP,0._SP)
     !
   endif
   !
 endif
 !
 ! Energy of the Electro-Magnetic Field
 !
 if(l_NE_with_fields) then
   !                    
   E_mg=DL_vol/(8._SP*pi*SPEED_OF_LIGHT**2)*abs(sum(A_vecpot%vecpot))
   !
 else
   E_mg=(0._SP,0._SP)
 endif
 !
 ! Total number of electrons
 !
 call RT_average_operator(VALUE=Trace_Gf,G=G_lesser,k=k)
 !
end subroutine RT_Energy

