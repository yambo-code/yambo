!
! Copyright (C) 2000-2013 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_Observables(en,k,G_lesser,on_the_fly_operators)
 !
 use pars,           ONLY:SP,cI
 use units,          ONLY:HA2EV,HA2KEL
 use drivers,        ONLY:l_sc_hf,l_sc_contains_lda,l_sc_cohsex
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:V_hartree_sc,Ho,SC_bands,RT_nk,RT_Nk_ibz,RT_ibz_coo
 use X_m,            ONLY:P_square
 use real_time,      ONLY:XC0_and_HARTREE0,RT_average_operator,XCfrozen, &
&                         l_NE_simulate_IP,RIM_ibz_coo,l_RT_uses_E_RIM,RT_do_IO,&
&                         RT_IO_output_steps,NE_i_time
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
 complex(SP),   intent(in) :: G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 complex(SP),   intent(out):: on_the_fly_operators(9)
 !
 ! WorkSpace
 !
 integer     :: ib,ik,ik_rim,iE_rim
 real(SP)    :: Energy,T_fit(2),Ef_fit(2),Occupations(SC_bands(1):SC_bands(2),RT_nk)
 complex(SP) :: E_kin,E_ion,E_h,Trace_Gf,Exc,E_mg,N_holes,N_elecs
 !
 if ( .not.RT_do_IO('OUT') ) return      
 !
#if defined _TIMING
 call timing('RT observables',OPR='start')
#endif
 !
 E_kin    =(0._SP,0._SP)
 E_ion    =(0._SP,0._SP)
 E_h      =(0._SP,0._SP)
 Trace_Gf =(0._SP,0._SP)
 Exc      =(0._SP,0._SP)
 E_mg     =(0._SP,0._SP)
 N_holes  =(0._SP,0._SP)
 N_elecs  =(0._SP,0._SP)
 Ef_fit   = 0._SP
 T_fit    = 0._SP
 if(NE_i_time-1>=RT_IO_output_steps) T_fit=real(on_the_fly_operators(8:9))/HA2KEL
 !
 ! Non-interaction kinetc energy
 !
 ! E_Kin = - i Tr { P^2 G^<}/2  
 !
 call RT_average_operator(VALUE=E_kin,O_MATRIX=P_square,G=G_lesser(:,:,:),en=en,k=k,TRANS=.TRUE.,nK_mem=PAR_Xk_nibz)
 !
 E_kin=E_kin/2.
 !  
 ! E_ion = - i Tr {Ho G^<} - Ekin
 !
 call RT_average_operator(VALUE=E_ion,O_MATRIX=Ho,G=G_lesser(:,:,:),en=en,k=k,nK_mem=k%nibz)
 !
 E_ion = E_ion - E_kin
 !
 if (.not.l_NE_simulate_IP) then
   !
   ! Hartree potential Energy
   !
   call RT_average_operator(VALUE=E_h,O_REAL=V_hartree_sc,G=G_lesser(:,:,:),en=en,k=k,nK_mem=k%nibz)
   !
   E_h=E_h/2.
   !
 endif
 !
 if (XCfrozen) then
   !
   ! Notice that XC0_and_HARTREE0 contains the Hartree part only in case
   ! of indipendent particles
   !
   call RT_average_operator(VALUE=Exc,O_MATRIX=XC0_and_HARTREE0,G=G_lesser(:,:,:),en=en,k=k,nK_mem=k%nibz)
   !
 else
   !
   if(l_sc_contains_lda) then 
     !        
     ! E_xc Energy 
     !
     call IO_and_Messaging_switch("-report -log -io_in -io_out")
     call XC_potential_driver(en,k,WF_KIND,WF_xc_functional,0)
     call IO_and_Messaging_switch("+report +log +io_in +io_out",CONDITION=master_cpu)
     !
     call RT_average_operator(VALUE=Exc,O_REAL=E_xc,G=G_lesser(:,:,:),en=en,k=k,nK_mem=k%nibz)
     !
   else if (l_sc_hf.or.l_sc_cohsex) then
     !
     call RT_average_operator(VALUE=Exc,O_QP=QP_Vnl_xc,G=G_lesser(:,:,:),en=en,k=k,nK_mem=k%nibz)
     !
   endif
   !
 endif
 !
 ! Total number of electrons
 !
 call RT_average_operator(VALUE=Trace_Gf,G=G_lesser(:,:,:),en=en,k=k,nK_mem=k%nibz)
 !
 ! Electrons and holes temperatures
 !
 forall(ib=SC_bands(1):SC_bands(2)) Occupations(ib,:)=aimag(G_lesser(ib,ib,:))
 call RT_occupations_FIT_and_lock(en,k,T_fit,Ef_fit,Occupations)
 !
 ! Number of Holes in the valence band and
 ! Number of Electrons in the conduction band
 ! Should be extended for metals...
 !
 do ik=1,en%nk
   do ib=SC_bands(1),SC_bands(2)
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       if(l_RT_uses_E_RIM) then
         iE_rim=ik_rim-RT_ibz_coo(ik,1)+RIM_ibz_coo(ik,1)
         Energy=en%E_RIM(ib,iE_rim,1)
       else
         Energy=en%E(ib,ik,1)
       endif
       if(Energy<=en%Efermi(1) ) N_holes=N_holes+&
         &  (spin_occ-aimag(G_lesser(ib,ib,ik_rim)))*k%weights(ik)/real(RT_Nk_ibz(ik))
       if(Energy> en%Efermi(1) ) N_elecs=N_elecs+&
         &            aimag(G_lesser(ib,ib,ik_rim)) *k%weights(ik)/real(RT_Nk_ibz(ik))
     enddo
   enddo
 enddo
 !
 ! Transfer the shadow variables
 !
 on_the_fly_operators(1:4)=(/E_kin,E_ion,E_h,Exc/)*HA2EV
 on_the_fly_operators(5)=(E_kin+E_ion+E_h+Exc)*HA2EV
 on_the_fly_operators(6:7)=(/Trace_Gf,Trace_Gf-N_holes-N_elecs/)
 on_the_fly_operators(8:9)=cmplx(T_fit(:),0._SP)*HA2KEL
 !
#if defined _TIMING
 call timing('RT observables',OPR='stop')
#endif
 !
end subroutine RT_Observables
