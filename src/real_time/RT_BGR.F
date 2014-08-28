!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine RT_BGR(G,E,k)
 !
 ! This routine calculates the BGR
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_sc_hf,l_sc_cohsex
 use SC,             ONLY:SC_bands,V_hartree_sc,V_xc_sc,rho_n
 use R_lattice,      ONLY:nkibz,bz_samp
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 use electrons,      ONLY:levels,n_sp_pol,E_duplicate
 use parallel_m,     ONLY:master_cpu
 use QP_m,           ONLY:QP_Vnl_xc,QP_table,QP_n_states
 use wave_func,      ONLY:WF
 use real_time,      ONLY:G_lesser_reference,l_initial_equilibrium,REF_V_xc_sc,BGR,&
&                         REF_V_hartree_sc,REF_QP_Vnl_xc,BGR_Hartree,BGR_V_xc,BGR_Self_Energy,&
&                         l_RT_uses_E_RIM,RIM_ibz_coo,E_reference,RT_nk,RT_ibz_coo,&
&                         eval_DENSITY,eval_HARTREE,eval_DFT,RT_ibz_coo,RT_all2ibz
 use xc_functionals, ONLY:V_xc,XC_potential_driver
 use global_XC,      ONLY:WF_kind,WF_xc_functional 
 use parallel_m,     ONLY:PP_redux_wait
 use interfaces,     ONLY:OCCUPATIONS_Gaps
 !
 implicit none
 !
 type(levels),  intent(inout) :: E
 type(bz_samp), intent(in)    :: k
 complex(SP),   intent(in)    :: G(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 !
 ! Work Space
 !
 complex(SP)  :: G_hf(SC_bands(2),SC_bands(2),nkibz),G_cohsex(SC_bands(2),SC_bands(2),nkibz),&
&                H(SC_bands(2),SC_bands(2),n_sp_pol)
 real(SP)     :: deltaE
 integer      :: ib,ik,ik_ref,i_qp,ibp,I_dir(2),ik_ibz,ik_RIM
 !
#if defined _TIMING
 call timing('RT BGR',OPR='start')
#endif
 !
 BGR_Hartree    =0.
 BGR_V_xc       =0.
 BGR_Self_Energy=0.
 !
 if (l_initial_equilibrium) then
   call E_duplicate(E,E_reference)
 endif
 !
 if (.not.l_initial_equilibrium) then
   !
   ! From RT_Collision I get rho_n to calculate the new Hartree and XC potential
   !
   if (eval_HARTREE) then
     !
     ! Hartree
     !
     call V_Hartree(rho_n,V_hartree_sc)
     !
     V_hartree_sc=V_hartree_sc-REF_V_hartree_sc
     !
   endif
   !
   if (eval_DFT) then
     !
     ! V_xc
     !
     call XC_potential_driver(E,k,WF_KIND,WF_xc_functional,1,&
&                             MUTE="-report -log -io_in -io_out",unMUTE=master_cpu)
     V_xc_sc=V_xc-REF_V_xc_sc
     !
   endif
   !
   do ik=1,nkibz
     !
     ! Hartree contribution to BGR
     !
     if (eval_HARTREE) then
       H=(0._SP,0._SP)
       call V_real_space_to_H(ik,V_hartree_sc,H,WF,'def')
       do ib=1,SC_bands(2)
         BGR_Hartree(ib,ik,1)=H(ib,ib,1)
       enddo
     endif
     !
     ! XC contribution to BGR
     !
     if (eval_DFT) then
       H=(0._SP,0._SP)
       call V_real_space_to_H(ik,V_xc_sc,H,WF,'xc ')
       do ib=1,SC_bands(2)
         BGR_V_xc(ib,ik,1)=H(ib,ib,1)
       enddo
     endif
     !
   enddo
   !
   ! All to All
   !
   call PP_redux_wait(BGR_Hartree)
   call PP_redux_wait(BGR_V_xc)
   !
 endif
 !
 if (l_sc_hf.or.l_sc_cohsex) then
   !
   G_hf    =(0._SP,0._SP)
   G_cohsex=(0._SP,0._SP)
   !
   do ik=1,nkibz
     !
     ik_ref=RT_ibz_coo(ik,1)
     !
     do ib=1,SC_bands(2)
       G_hf(ib,ib,ik)    =(0._SP,1._SP)*E%f(ib,ik,1)
       G_cohsex(ib,ib,ik)=(0._SP,1._SP)*(E%f(ib,ik,1)-1.)
       if (ib>=SC_bands(1).and..not.l_initial_equilibrium) then
         G_hf(ib,ib,ik)    =G(ib,ib,ik_ref)+G_lesser_reference(ib,ib,ik_ref)
         G_cohsex(ib,ib,ik)=G(ib,ib,ik_ref)+G_lesser_reference(ib,ib,ik_ref)- (0._SP,1._SP)
       endif
     enddo
   enddo
   !
   call COLLISIONS_compose(G_hf,G_cohsex)
   !
   if (l_initial_equilibrium) then
     !
     REF_QP_Vnl_xc=QP_Vnl_xc
     !
   else
     !
     do i_qp=1,QP_n_states
       ib =QP_table(i_qp,1)
       ibp=QP_table(i_qp,2)
       ik =QP_table(i_qp,3)
       !
       if (ib/=ibp) cycle
       !
       BGR_Self_Energy(ib,ik,1)=real(QP_Vnl_xc(i_qp)-REF_QP_Vnl_xc(i_qp))
       !
     enddo
     !
   endif
   !
 endif
 !
 if (.not.l_initial_equilibrium) then
   !
   ! Corrections of the Energy Levels
   !
   do ik=1,RT_nk
     !
     ik_ibz=RT_all2ibz(ik)
     ik_RIM=ik-RT_ibz_coo(ik_ibz,1)+RIM_ibz_coo(ik_ibz,1)
     !
     do ib=SC_bands(1),SC_bands(2)
       !
       deltaE=BGR_Hartree(ib,ik_ibz,1)+BGR_V_xc(ib,ik,1)+BGR_Self_Energy(ib,ik_ibz,1)
       !
       if (     l_RT_uses_E_RIM) E%E_RIM(ib,ik_RIM,1)=E_reference%E_RIM(ib,ik_RIM,1)+deltaE
       if (.not.l_RT_uses_E_RIM) E%E(ib,ik,1)=E_reference%E(ib,ik,1)+deltaE
       !
     enddo
     !
   enddo
   !
   ! Evaluation of the contribution to the minimal gap
   !
   call OCCUPATIONS_Gaps(E,I_dir=I_dir)
   !
   ib=I_dir(1)
   ik=I_dir(2)
   !
   BGR(1)=E%E_dir_gap(1)
   BGR(2)=E%E_ind_gap(1)
   BGR(3)=    BGR_Hartree(ib+1,ik,1)-    BGR_Hartree(ib,ik,1)
   BGR(4)=       BGR_V_xc(ib+1,ik,1)-       BGR_V_xc(ib,ik,1)
   BGR(5)=BGR_Self_Energy(ib+1,ik,1)-BGR_Self_Energy(ib,ik,1)
   !
 endif
 !
#if defined _TIMING
 call timing('RT BGR',OPR='stop')
#endif
 !
end subroutine RT_BGR
