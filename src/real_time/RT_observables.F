!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA
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
subroutine RT_Observables(en,k,dG_lesser,on_the_fly_operators)
 !
 use pars,           ONLY:SP
 use units,          ONLY:HA2EV,HA2KEL
 use drivers,        ONLY:l_sc_hf,l_sc_contains_lda,l_sc_cohsex,l_elel_scatt,l_elph_scatt
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:V_hartree_sc,Ho,SC_bands,RT_nk,RT_Nk_ibz,RT_ibz_coo
 use real_time,      ONLY:RT_Tfit_occupations,RT_Efit_occupations
 use X_m,            ONLY:P_square
 use real_time,      ONLY:XC0_and_HARTREE0,RT_average_operator,XCfrozen,           &
&                         l_NE_simulate_IP,RIM_ibz_coo,l_RT_uses_E_RIM,RT_do_IO,   &
&                         RT_Tfit_occupations,RT_Efit_occupations,RT_E_occupations,&
&                         RT_H_occupations,G_lesser_reference
 use electrons,      ONLY:levels,spin_occ
 use QP_m,           ONLY:QP_Vnl_xc
 use xc_functionals, ONLY:E_xc,XC_potential_driver
 use global_XC,      ONLY:WF_kind,WF_xc_functional
 use IO_m,           ONLY:IO_and_Messaging_switch
 use parallel_m,     ONLY:PAR_Xk_nibz,master_cpu
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none 
 !
 type(levels),  intent(in) :: en
 type(bz_samp), intent(in) :: k
 complex(SP),   intent(in) :: dG_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 complex(SP),   intent(out):: on_the_fly_operators(10)
 !
 ! WorkSpace
 !
 integer     :: ib,ik,ik_rim,iE_rim
 real(SP)    :: Energy
 complex(SP) :: G_full(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 complex(SP) :: E_tot,E_kin,E_ion,E_h,Trace_Gf,Exc,N_holes,N_elecs,VALUE_tmp(2)
 !
 if ( .not.RT_do_IO('OUT') ) return      
 !
 G_full=dG_lesser+G_lesser_reference
 !
#if defined _TIMING
 call timing('RT observables',OPR='start')
#endif
 !
 E_tot    =(0._SP,0._SP)
 E_kin    =(0._SP,0._SP)
 E_ion    =(0._SP,0._SP)
 E_h      =(0._SP,0._SP)
 Exc      =(0._SP,0._SP)
 Trace_Gf =(0._SP,0._SP)
 N_holes  =(0._SP,0._SP)
 N_elecs  =(0._SP,0._SP)
 !
 ! Non-interaction kinetc energy
 !
 ! E_Kin = - i Tr { P^2 G^<}/2  
 !
 call RT_average_operator(VALUE=E_kin,O_MATRIX=P_square,G=dG_lesser,en=en,k=k,TRANS=.TRUE.,nK_mem=PAR_Xk_nibz)
 !
 E_kin=E_kin/2.
 !  
 ! E_ion = - i Tr {Ho G^<} - Ekin
 !
 call RT_average_operator(VALUE=E_ion,O_MATRIX=Ho,G=dG_lesser,en=en,k=k,RIM=l_RT_uses_E_RIM,nK_mem=k%nibz)
 !
 E_ion = E_ion - E_kin
 !
 if (.not.l_NE_simulate_IP) then
   !
   ! Hartree potential Energy
   !
   call RT_average_operator(VALUE=VALUE_tmp(1),O_REAL=V_hartree_sc,G=G_lesser_reference,en=en,k=k,nK_mem=k%nibz)
   call RT_average_operator(VALUE=VALUE_tmp(2),O_REAL=V_hartree_sc,G=dG_lesser,en=en,k=k,nK_mem=k%nibz)
   !
   ! delta E_H ~ rho_0*delta_rho + 0.5*delta_rho**2
   E_h=VALUE_tmp(1)+VALUE_tmp(2)/2._SP
   !
 endif
 !
 if (XCfrozen) then
   !
   ! XC0_and_HARTREE0 contains the Hartree part only in case
   ! XC is not frozen. Here we are in the case XC is frozen,
   ! so it contais both V_H and V_xc
   !
   call RT_average_operator(VALUE=Exc,O_MATRIX=XC0_and_HARTREE0,G=dG_lesser,en=en,k=k,nK_mem=k%nibz)
   !
   ! If Hartreee is evolving the change in the Hartree energy has been already
   ! stored in E_h, so I remove from here the term rho_0*delta_rho
   if(.not.l_NE_simulate_IP) Exc=Exc-VALUE_tmp(1)
   !
 else
   !
   if(l_sc_contains_lda) then 
     !        
     ! Exc Energy from E_xc[rho_n] energy density
     !
     call XC_potential_driver(en,k,WF_KIND,WF_xc_functional,0,&
&                             MUTE="+report +log +io_in +io_out",unMUTE=master_cpu)
     !
     call RT_average_operator(VALUE=Exc,O_REAL=E_xc,G=dG_lesser,en=en,k=k,nK_mem=k%nibz)
     !
   else if (l_sc_hf.or.l_sc_cohsex) then
     !
     ! HF or COHSEX energy.
     ! Davide: Is this correctly defined also in the COHSEX case 
     !         or should I use the Galitskii Migdal equation ?
     !
     call RT_average_operator(VALUE=Exc,O_QP=QP_Vnl_xc,G=G_full,en=en,k=k,nK_mem=k%nibz)
     !
   endif
   !
 endif
 !
 E_tot = E_ion + E_kin + E_h + Exc
 !
 ! Change in the total number of electrons
 !
 call RT_average_operator(VALUE=Trace_Gf,G=dG_lesser,en=en,k=k,nK_mem=k%nibz)
 !
 ! Electrons and holes temperatures
 !
 if (l_elel_scatt.or.l_elph_scatt) then
   call RT_occupations_FIT_and_lock(en,k,RT_Tfit_occupations,RT_Efit_occupations,RT_E_occupations)
   if (RT_Tfit_occupations(1)>50000/HA2KEL.or.RT_Tfit_occupations(1)<0.) RT_Tfit_occupations(1)=0.
   if (RT_Tfit_occupations(2)>50000/HA2KEL.or.RT_Tfit_occupations(2)<0.) RT_Tfit_occupations(2)=0.
 endif
 !
 ! Number of Holes in the valence band and
 ! Number of Electrons in the conduction band
 ! Should be extended for metals...
 !
 do ik=1,en%nk
   do ib=SC_bands(1),SC_bands(2)
     VALUE_tmp=(0._SP,0._SP)
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       if(l_RT_uses_E_RIM) then
         iE_rim=ik_rim-RT_ibz_coo(ik,1)+RIM_ibz_coo(ik,1)
         Energy=en%E_RIM(ib,iE_rim,1)
       else
         Energy=en%E(ib,ik,1)
       endif
       if ( Energy<=en%Efermi(1) ) VALUE_tmp(1)=VALUE_tmp(1)+RT_H_occupations(ib,ik_rim)
       if ( Energy> en%Efermi(1) ) VALUE_tmp(2)=VALUE_tmp(2)+RT_E_occupations(ib,ik_rim)
     enddo
     N_holes=N_holes+VALUE_tmp(1)*k%weights(ik)/real(RT_Nk_ibz(ik))
     N_elecs=N_elecs+VALUE_tmp(2)*k%weights(ik)/real(RT_Nk_ibz(ik))
   enddo
 enddo
 !
 ! Transfer the shadow variables
 !
 on_the_fly_operators(1:4)=(/E_kin,E_ion,E_h,Exc/)*HA2EV
 on_the_fly_operators(5)=E_tot*HA2EV
 on_the_fly_operators(6:8)=(/Trace_Gf,N_holes,N_elecs/)
 on_the_fly_operators(9:10)=cmplx(RT_Tfit_occupations(:),0._SP)*HA2KEL
 !
#if defined _TIMING
 call timing('RT observables',OPR='stop')
#endif
 !
end subroutine RT_Observables
