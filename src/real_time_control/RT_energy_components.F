!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA AM DS
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
subroutine RT_energy_components(en,k,dG_lesser,Energies,dE_IP,what)
 !
 use pars,           ONLY:SP,cZERO
 use X_m,            ONLY:P_square
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,n_sp_pol
 use hamiltonian,    ONLY:V_hartree_sc,Hzero
 use rt_interfaces,  ONLY:RT_average_operator
 use QP_m,           ONLY:QP_nk
 use units,          ONLY:HA2EV
 use drivers,        ONLY:l_sc_is_libDFT,l_use_Hxc_collisions,l_sc_hartree
 use real_time,      ONLY:RT_nk,RT_bands,RT_ibz_coo,XC0,HARTREE0, &
&                         G_lesser_reference,RT_Vnl_xc,eval_energy
 use xc_functionals, ONLY:E_xc,XC_potential_driver
 use global_XC,      ONLY:WF_kind,WF_xc_functional
 use parallel_m,     ONLY:PAR_Xk_nibz,master_cpu
 use RT_interfaces,  ONLY:RT_average_operator
 !
 implicit none 
 !
 type(levels),  intent(in) :: en
 type(bz_samp), intent(in) :: k
 complex(SP),   intent(in) :: dG_lesser(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 real(SP),      intent(out):: Energies(5),dE_IP
 character(3),  intent(in) :: what
 !
 complex(SP) :: G_full(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk),              &
&               dE_tot,dE_kin,dE_ion,dE_h,dE_xc,dE_hxc,dE_h_sc,E_xc_sc,E_xc_ref,dE_xc_sc,dE_hxc_sc
 !
 if(.not.eval_energy) return
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
 if(     trim(what)=='el') G_full=dG_lesser
 if(.not.trim(what)=='el') G_full=dG_lesser+G_lesser_reference
 !
 ! Non-interacting energy variation
 !
 ! d_Hzero = \delta\eps_{nmk} - V_Hxc[\rho_eq]
 !
 call RT_average_operator(O_VALUE=dE_ion,O_MATRIX=Hzero,G=dG_lesser,  en=en,k=k,Double_Grid='ENERGY',nK_mem=k%nibz)
 !
 ! Non-interacting kinetc energy variation
 !
 ! dE_Kin = - i Tr { P^2 dG^<}/2  
 !
 call RT_average_operator(O_VALUE=dE_kin,O_MATRIX=P_square/2._SP,G=dG_lesser,  en=en,k=k,TRANS=.TRUE.,nK_mem=PAR_Xk_nibz)
 !
 ! dE_ion = dE_tot(IP) - dEkin
 !
 dE_ion = dE_ion - dE_kin
 !
 ! Non-interacting Hartree energy variation
 !
 call RT_average_operator(O_VALUE=dE_h,O_MATRIX=HARTREE0,G=dG_lesser,  en=en,k=k,nK_mem=k%nibz)
 !
 ! Non-interacting xc energy variation
 !
 call RT_average_operator(O_VALUE=dE_xc,O_MATRIX=XC0,G=dG_lesser,  en=en,k=k,nK_mem=k%nibz)
 !
 ! Sum of Hartree plus xc energy
 !
 dE_hxc=dE_h+dE_xc
 !
 dE_IP  =real(dE_kin+dE_ion+dE_hxc,SP)
 !
 if (trim(what)=='el'.or.trim(what)=='hol') return
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
     call RT_average_operator(O_VALUE=dE_h_sc,O_REAL=V_hartree_sc/2._SP,G=dG_lesser,  en=en,k=k,nK_mem=k%nibz)
     !
   endif
   !
   if (l_sc_is_libDFT) then
     !
     ! Exc Energy from dE_xc[rho_n] energy density ... to be fixed, E_xc_ref is wrong
     !
     call XC_potential_driver(en,k,WF_KIND,WF_xc_functional,0)
     !
     call RT_average_operator(O_VALUE=E_xc_ref,O_REAL=E_xc,G=G_lesser_reference,en=en,k=k,nK_mem=k%nibz)
     call RT_average_operator(O_VALUE=E_xc_sc, O_REAL=E_xc,G=G_full            ,en=en,k=k,nK_mem=k%nibz)
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
   call RT_average_operator(O_VALUE=dE_hxc_sc,O_MATRIX=RT_Vnl_xc(:,:,RT_ibz_coo(:,1),:),G=dG_lesser,  en=en,k=k,nK_mem=k%nibz)
   !
 endif
 !
 dE_tot = cmplx(dE_IP,0._SP)+dE_h_sc+dE_xc_sc+dE_hxc_sc
 !
 dE_h   = dE_h   +dE_h_sc
 dE_xc  = dE_xc  +dE_xc_sc
 dE_hxc = dE_hxc +dE_hxc_sc
 !
 !
 ! Transfer the shadow variables
 !
 if(.not.l_use_Hxc_collisions) Energies=real((/dE_kin,dE_ion,dE_h, dE_xc, dE_tot/))*HA2EV
 if(     l_use_Hxc_collisions) Energies=real((/dE_kin,dE_ion,dE_hxc,dE_tot,cZERO/))*HA2EV
 !
end subroutine RT_energy_components
