!
! Copyright (C) 2000-2014 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_Observables(en,k,dG_lesser,on_the_fly_operators)
 !
 use pars,           ONLY:SP
 use units,          ONLY:HA2EV,HA2KEL
 use drivers,        ONLY:l_sc_hf,l_sc_contains_lda,l_sc_cohsex,l_elel_scatt,l_elph_scatt
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:V_hartree_sc,Ho,SC_bands,RT_nk,RT_Nk_ibz,RT_ibz_coo
 use real_time,      ONLY:RT_Tfit_occupations,RT_Efit_occupations
 use X_m,            ONLY:P_square
 use real_time,      ONLY:XC0_and_HARTREE0,RT_average_operator,XCfrozen,Hfrozen,   &
&                         l_NE_simulate_IP,RIM_ibz_coo,l_RT_uses_E_RIM,RT_do_IO,   &
&                         RT_Tfit_occupations,RT_Efit_occupations,RT_E_occupations,&
&                         RT_H_occupations,G_lesser_reference,l_RT_needs_XC0
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
 complex(SP) :: E_tot,E_kin,E_ion,E_hxc_0,d_E_h,Trace_Gf,d_E_xc,E_mg,N_holes,N_elecs,VALUE_tmp(2)
 !
 if ( .not.RT_do_IO('OUT') ) return      
 !
#if defined _TIMING
 call timing('RT observables',OPR='start')
#endif
 !
 E_tot    =(0._SP,0._SP)
 E_kin    =(0._SP,0._SP)
 E_ion    =(0._SP,0._SP)
 E_hxc_0  =(0._SP,0._SP)
 d_E_h    =(0._SP,0._SP)
 d_E_xc   =(0._SP,0._SP)
 Trace_Gf =(0._SP,0._SP)
 E_mg     =(0._SP,0._SP)
 N_holes  =(0._SP,0._SP)
 N_elecs  =(0._SP,0._SP)
 !
 ! Non-interaction kinetc energy
 !
 ! E_Kin = - i Tr { P^2 G^<}/2  
 !
 call RT_average_operator(VALUE=E_kin,O_MATRIX=P_square,G=dG_lesser+G_lesser_reference,en=en,k=k,TRANS=.TRUE.,nK_mem=PAR_Xk_nibz)
 !
 E_kin=E_kin/2.
 !  
 ! E_ion = - i Tr {Ho G^<} - Ekin
 !
 call RT_average_operator(VALUE=E_ion,O_MATRIX=Ho,G=dG_lesser+G_lesser_reference,en=en,k=k,RIM=l_RT_uses_E_RIM,nK_mem=k%nibz)
 !
 E_ion = E_ion - E_kin
 !
 ! Hartree and XC
 !================
 !
 ! Note that both Hartree and XC (local) are based on change of densities.
 ! For example:
 !
 ! V_hartree(G) = V_hartree(rho-rho_0) + V_hartree(rho_0)
 !
 ! V_hartree(rho_0)+V_xc(rho_0)
 !------------------------------
 !
 if (l_RT_needs_XC0) then
   call RT_average_operator(VALUE=E_hxc_0,O_MATRIX=XC0_and_HARTREE0,G=dG_lesser+G_lesser_reference,en=en,k=k,nK_mem=k%nibz)
   E_hxc_0=E_hxc_0/2.
 endif
 !
 ! delta V_hartree(rho_0)
 !------------------------
 !
 if (.not.Hfrozen) then
   call RT_average_operator(VALUE=d_E_h,O_REAL=V_hartree_sc,G=dG_lesser+G_lesser_reference,en=en,k=k,nK_mem=k%nibz)
 endif
 !
 ! delta V_xc(rho_0)
 !-------------------
 !
 if (.not.XCfrozen) then
   !
   if(l_sc_contains_lda) then 
     !        
     ! E_xc Energy 
     !
     call IO_and_Messaging_switch("-report -log -io_in -io_out")
     call XC_potential_driver(en,k,WF_KIND,WF_xc_functional,0)
     call IO_and_Messaging_switch("+report +log +io_in +io_out",CONDITION=master_cpu)
     !
     call RT_average_operator(VALUE=d_E_xc,O_REAL=E_xc,G=dG_lesser+G_lesser_reference,en=en,k=k,nK_mem=k%nibz)
     !
   else if (l_sc_hf.or.l_sc_cohsex) then
     !
     call RT_average_operator(VALUE=d_E_xc,O_QP=QP_Vnl_xc,G=dG_lesser+G_lesser_reference,en=en,k=k,nK_mem=k%nibz)
     !
   endif
   !
 endif
 !
 E_tot = E_ion + E_kin + E_hxc_0+d_E_xc+d_E_h
 !
 ! Change in the total number of electrons
 !
 call RT_average_operator(VALUE=Trace_Gf,G=dG_lesser(:,:,:),en=en,k=k,nK_mem=k%nibz)
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
 on_the_fly_operators(1:4)=(/E_kin,E_ion,d_E_h,d_E_xc/)*HA2EV
 on_the_fly_operators(5)=E_tot*HA2EV
 on_the_fly_operators(6:8)=(/Trace_Gf,N_holes,N_elecs/)
 on_the_fly_operators(9:10)=cmplx(RT_Tfit_occupations(:),0._SP)*HA2KEL
 !
#if defined _TIMING
 call timing('RT observables',OPR='stop')
#endif
 !
end subroutine RT_Observables
