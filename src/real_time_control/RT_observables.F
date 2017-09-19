!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA DS
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
 ! This subroutine compute the variation of the energy and its different components,
 ! the number of electrons injected in conduction and holes in valence,
 ! and finally the variation in entropy and the associated temperature
 ! which is compared with the temperature from the fits
 !
 use pars,           ONLY:SP,cZERO
 use units,          ONLY:HA2EV,HA2KEL
 use drivers,        ONLY:l_sc_is_libDFT,l_elel_scatt,l_elph_scatt,l_use_Hxc_collisions,l_sc_hartree
 use QP_m,           ONLY:QP_nk
 use R_lattice,      ONLY:bz_samp
 use X_m,            ONLY:P_square
 use hamiltonian,    ONLY:V_hartree_sc,Ho
 use RT_control,     ONLY:RT_do_it
 use rt_interfaces,  ONLY:RT_average_operator
 use real_time,      ONLY:RT_Tfit_occupations,RT_Efit_occupations,RT_nk,RT_Nk_ibz,RT_ibz_coo,&
&                         XC0,HARTREE0,DbGd_ibz_coo,l_RT_uses_E_DbGd,REF_V_hartree_sc,       &
&                         RT_Tfit_occupations,RT_Efit_occupations,RT_E_occupations,          &
&                         RT_H_occupations,G_lesser_reference,RT_Vnl_xc,RT_bands,            &
&                         RT_T_thermodynamics,RT_E_thermodynamics,RT_S_thermodynamics
 use electrons,      ONLY:levels,n_sp_pol,spin_occ
 use xc_functionals, ONLY:E_xc,XC_potential_driver
 use global_XC,      ONLY:WF_kind,WF_xc_functional
 use parallel_m,     ONLY:PAR_Xk_nibz,master_cpu
 use RT_interfaces,  ONLY:RT_average_operator
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none 
 !
 type(levels),  intent(in) :: en
 type(bz_samp), intent(in) :: k
 complex(SP),   intent(in) :: dG_lesser(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 real(SP),      intent(out):: on_the_fly_operators(10)
 !
 ! WorkSpace
 !
 integer     :: ib1,ib2,ik,ik_RT,ik_E,i1,i2
 real(SP)    :: Energy(2),dS_tot(3),S_e,S_h,N_carriers(2),N_err,VALUE_tmp(5),one_over_T
 complex(SP) :: G_full(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk),        &
&               G_full_h(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk),      &
&               G_full_e(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk),      &
&               dG_lesser_h(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk),   &
&               dG_lesser_e(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk),   &
&               dE_tot(3),dE_kin(3),dE_ion(3),dE_h(3),dE_xc(3),dE_hxc(3),             &
&               dE_h_sc(3),E_xc_sc(3),E_xc_ref(3),dE_xc_sc(3),dE_hxc_sc(3),           &
&               Ho_limited(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),QP_nk,n_sp_pol)
 !
 if (.not. RT_do_it('OUT') ) return
 !
#if defined _TIMING
 call timing('RT observables',OPR='start')
#endif
 !
 !
 dG_lesser_h=cZERO
 dG_lesser_e=cZERO
 !
 do ik=1,en%nk
   do ik_RT=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
     if (l_RT_uses_E_DbGd) ik_E=ik_RT-RT_ibz_coo(ik,1)+DbGd_ibz_coo(ik,1)
     do ib1=RT_bands(1),RT_bands(2)
       do ib2=RT_bands(1),RT_bands(2)
         if (l_RT_uses_E_DbGd) then
           Energy(1)=en%E_DbGd(ib1,ik_E,1)
           Energy(2)=en%E_DbGd(ib2,ik_E,1)
         else
           Energy(1)=en%E(ib1,ik,1)
           Energy(2)=en%E(ib2,ik,1)
         endif
         if ( Energy(1)<=0._SP .and. Energy(2)<=0._SP ) dG_lesser_h(ib1,ib2,ik_RT)=dG_lesser(ib1,ib2,ik_RT)
         if ( Energy(1)> 0._SP .and. Energy(2)> 0._SP ) dG_lesser_e(ib1,ib2,ik_RT)=dG_lesser(ib1,ib2,ik_RT)
       enddo
     enddo
   enddo
 enddo
 !
 G_full=dG_lesser+G_lesser_reference
 !
 G_full_h=dG_lesser_h+G_lesser_reference
 G_full_e=dG_lesser_e
 !
 dE_tot    =cZERO
 dE_kin    =cZERO
 dE_ion    =cZERO
 !
 dE_h      =cZERO
 dE_xc     =cZERO
 dE_hxc    =cZERO
 !
 E_xc_ref  =cZERO
 E_xc_sc   =cZERO
 dE_h_sc   =cZERO
 dE_xc_sc  =cZERO
 dE_hxc_sc =cZERO
 !
 ! Non-interacting energy variation
 !
 ! d_Ho = \delta\eps_{nmk} - V_Hxc[\rho_eq]
 !
 Ho_limited=Ho(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),:,:)
 call RT_average_operator(O_VALUE=dE_ion(1),O_MATRIX=Ho_limited,G=dG_lesser,  en=en,k=k,Double_Grid='ENERGY',nK_mem=k%nibz)
 call RT_average_operator(O_VALUE=dE_ion(2),O_MATRIX=Ho_limited,G=dG_lesser_h,en=en,k=k,Double_Grid='ENERGY',nK_mem=k%nibz)
 call RT_average_operator(O_VALUE=dE_ion(3),O_MATRIX=Ho_limited,G=dG_lesser_e,en=en,k=k,Double_Grid='ENERGY',nK_mem=k%nibz)
 !
 ! Non-interacting kinetc energy variation
 !
 ! dE_Kin = - i Tr { P^2 dG^<}/2  
 !
 call RT_average_operator(O_VALUE=dE_kin(1),O_MATRIX=P_square/2._SP,G=dG_lesser,  en=en,k=k,TRANS=.TRUE.,nK_mem=PAR_Xk_nibz)
 call RT_average_operator(O_VALUE=dE_kin(2),O_MATRIX=P_square/2._SP,G=dG_lesser_h,en=en,k=k,TRANS=.TRUE.,nK_mem=PAR_Xk_nibz)
 call RT_average_operator(O_VALUE=dE_kin(3),O_MATRIX=P_square/2._SP,G=dG_lesser_e,en=en,k=k,TRANS=.TRUE.,nK_mem=PAR_Xk_nibz)
 !
 ! dE_ion = dE_tot(IP) - dEkin
 !
 dE_ion = dE_ion - dE_kin
 !
 ! Non-interacting Hartree energy variation
 !
 call RT_average_operator(O_VALUE=dE_h(1),O_MATRIX=HARTREE0,G=dG_lesser,  en=en,k=k,nK_mem=k%nibz)
 call RT_average_operator(O_VALUE=dE_h(2),O_MATRIX=HARTREE0,G=dG_lesser_h,en=en,k=k,nK_mem=k%nibz)
 call RT_average_operator(O_VALUE=dE_h(3),O_MATRIX=HARTREE0,G=dG_lesser_e,en=en,k=k,nK_mem=k%nibz)
 !
 ! Non-interacting xc energy variation
 !
 call RT_average_operator(O_VALUE=dE_xc(1),O_MATRIX=XC0,G=dG_lesser,  en=en,k=k,nK_mem=k%nibz)
 call RT_average_operator(O_VALUE=dE_xc(2),O_MATRIX=XC0,G=dG_lesser_h,en=en,k=k,nK_mem=k%nibz)
 call RT_average_operator(O_VALUE=dE_xc(3),O_MATRIX=XC0,G=dG_lesser_e,en=en,k=k,nK_mem=k%nibz)
 !
 ! Sum of Hartree plus xc energy
 !
 dE_hxc=dE_h+dE_xc
 !
 dE_tot=dE_kin+dE_ion+dE_hxc
 !
 if (.not.l_use_Hxc_collisions) then
   !
   if (l_sc_hartree) then
     !
     ! Hartree potential Energy:
     !
     !  E_h[rho]=1/2 <V_h[rho]rho>= 1/2 int rho(r)rho(r')/|r-r'|
     !
     ! dE_h[rho]=E_h[rho]-E_h[rho0]
     !          =1/2*[ 2*(int rho0(r)rho(r')/|r-r'|) + (int drho(r)drho(r')/|r-r'|) ]
     ! 
     ! The first term is already computed at the IP level. Thus it only remains the second term
     ! Remember that V_hartree_sc is constructed starting from drho
     !
     call RT_average_operator(O_VALUE=dE_h_sc(1),O_REAL=V_hartree_sc/2._SP,G=dG_lesser,  en=en,k=k,nK_mem=k%nibz)
     call RT_average_operator(O_VALUE=dE_h_sc(2),O_REAL=V_hartree_sc/2._SP,G=dG_lesser_h,en=en,k=k,nK_mem=k%nibz)
     call RT_average_operator(O_VALUE=dE_h_sc(3),O_REAL=V_hartree_sc/2._SP,G=dG_lesser_e,en=en,k=k,nK_mem=k%nibz)
     !
   endif
   !
   if (l_sc_is_libDFT) then
     !
     ! Exc Energy from dE_xc[rho_n] energy density ... to be fixed, E_xc_ref is wrong
     !
     call XC_potential_driver(en,k,WF_KIND,WF_xc_functional,0,MUTE="+report +log +io_in +io_out",unMUTE=master_cpu)
     !
     call RT_average_operator(O_VALUE=E_xc_ref(1),O_REAL=E_xc,G=G_lesser_reference,en=en,k=k,nK_mem=k%nibz)
     call RT_average_operator(O_VALUE=E_xc_sc(1), O_REAL=E_xc,G=G_full            ,en=en,k=k,nK_mem=k%nibz)
     !
     dE_xc_sc = (E_xc_sc-E_xc_ref)-dE_xc
     !
   endif
   !
   ! Here I would need the Galitskii Migdal equation for the XC self energy in MBPT
   !
 else
   !
   ! Davide: This is int V_rt(r)*drho(r)
   !         - If V_rt contains only the Hartree term, this is twice the dE_h_sc
   !         - If V_rt contains also the xc-term, this is not well defined ...
   !
   call RT_average_operator(O_VALUE=dE_hxc_sc(1),O_MATRIX=RT_Vnl_xc(:,:,RT_ibz_coo(:,1),:),G=dG_lesser,  en=en,k=k,nK_mem=k%nibz)
   call RT_average_operator(O_VALUE=dE_hxc_sc(2),O_MATRIX=RT_Vnl_xc(:,:,RT_ibz_coo(:,1),:),G=dG_lesser_h,en=en,k=k,nK_mem=k%nibz)
   call RT_average_operator(O_VALUE=dE_hxc_sc(3),O_MATRIX=RT_Vnl_xc(:,:,RT_ibz_coo(:,1),:),G=dG_lesser_e,en=en,k=k,nK_mem=k%nibz)
   !
 endif
 !
 !
 ! Transfer the shadow variables
 !
 if(.not.l_use_Hxc_collisions) on_the_fly_operators(1:5)=real((/dE_kin(1),dE_ion(1),dE_h(1),  dE_xc(1),dE_tot(1)/))*HA2EV
 if(     l_use_Hxc_collisions) on_the_fly_operators(1:4)=real((/dE_kin(1),dE_ion(1),    dE_hxc(1),     dE_tot(1)/))*HA2EV
 on_the_fly_operators(6:8)=(/N_err,N_carriers(1),N_carriers(2)/)
 on_the_fly_operators(9:10)=RT_Tfit_occupations(:)*HA2KEL
 !
#if defined _TIMING
 call timing('RT observables',OPR='stop')
#endif
 !
end subroutine RT_Observables
