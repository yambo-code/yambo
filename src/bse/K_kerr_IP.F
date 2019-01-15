!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
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
subroutine K_kerr_IP(iq,w)
 !
 use pars,           ONLY:SP,cZERO,cI
 use LIVE_t,         ONLY:live_timing
 use drivers,        ONLY:l_rpa_IP
 use X_m,            ONLY:Epsilon_ij,global_gauge,Dipole_Energy_treshold
 use KERR,           ONLY:DIP_projected_r90,KERR_alloc,B_Hall
 use BS_solvers,     ONLY:BSS_add_antiresonant,BSS_n_freqs,Co_factor
 use BS,             ONLY:BS_T_grp,BS_nT_grps,BS_K_coupling
 use electrons,      ONLY:E_SOC_nbands,n_sp_pol
 use R_lattice,      ONLY:bare_qpg
 use drivers,        ONLY:l_anomalous_Hall
 use parallel_m,     ONLY:PAR_BS_nT_col_grps,PP_redux_wait,&
&                         PAR_IND_T_groups,PAR_Xk_bz_index,&
&                         PAR_COM_eh_INDEX,PAR_COM_Xk_ibz_INDEX
 !
#include<memory.h>
 !
 integer,    intent(in) :: iq
 complex(SP),intent(in) :: w(BSS_n_freqs) 
 !
 ! Work Space
 !
 real(SP)          :: conjg_fac
 complex(SP)       :: E_plus_W0,res,E_plus_W(2/n_sp_pol)
 integer           :: ik_bz,iv,ic,i_sp_pol,i_T,i_T_g,ik_mem,i_pert_SOC,n_SOC
 !
 YAMBO_ALLOC(Epsilon_ij,(BSS_n_freqs,4))
 Epsilon_ij=cZERO
 !
 Epsilon_ij(:,1)=w(:)
 !
 n_SOC=1
 !
 if(E_SOC_nbands/=0.and.n_sp_pol==1) n_SOC=2
 !
 B_Hall=cZERO
 !
 if (trim(global_gauge)=='length'  ) conjg_fac=-1._SP
 if (trim(global_gauge)=='velocity') conjg_fac=+1._SP
 !
 if (l_rpa_IP) call live_timing('IP Off-diagonal absorption',PAR_BS_nT_col_grps)
 !
 do i_T_g=1,BS_nT_grps
   !    
   if (.not.PAR_IND_T_groups%element_1D(i_T_g)) cycle
   !    
   do i_T=1,BS_T_grp(i_T_g)%size    
     !    
     ik_bz = BS_T_grp(i_T_g)%table(i_T,1)
     iv    = BS_T_grp(i_T_g)%table(i_T,2)
     ic    = BS_T_grp(i_T_g)%table(i_T,3)
     i_sp_pol = BS_T_grp(i_T_g)%table(i_T,4)
     !
     ik_mem= PAR_Xk_bz_index(ik_bz)
     !
     if(.not.allocated(BS_T_grp(i_T_g)%W)) E_plus_W0=BS_T_grp(i_T_g)%E(i_T)
     if(     allocated(BS_T_grp(i_T_g)%W)) E_plus_W0=BS_T_grp(i_T_g)%E(i_T)-cI*BS_T_grp(i_T_g)%W(i_T)
     !
     if (E_SOC_nbands==0) E_plus_W=E_plus_W0
     !
     if (E_SOC_nbands/=0) E_plus_W(:)=E_plus_W0+BS_T_grp(i_T_g)%E_SOC_corr(:,i_T)
     !
     BS_T_grp(i_T_g)%dipoles_R_kerr(i_T)=conjg_fac*conjg(DIP_projected_r90(ic,iv,ik_mem,i_sp_pol))
     if (BS_K_coupling) BS_T_grp(i_T_g)%dipoles_A_kerr(i_T)=DIP_projected_r90(ic,iv,ik_mem,i_sp_pol)
     !
     if ( abs(real(E_plus_W0)) < Dipole_Energy_treshold ) cycle
     !
     ! Residuals
     res=BS_T_grp(i_T_g)%dipoles_R(i_T)*conjg(BS_T_grp(i_T_g)%dipoles_R_kerr(i_T))*BS_T_grp(i_T_g)%f_RES(i_T)
     if (allocated(BS_T_grp(i_T_g)%Z)                                     ) res=res*BS_T_grp(i_T_g)%Z(i_T)
     if (allocated(BS_T_grp(i_T_g)%W) .and. trim(global_gauge)=="velocity") res=res*E_plus_W0/conjg(E_plus_W0)
     !
     do i_pert_SOC=1,n_SOC
       Epsilon_ij(:,3)=Epsilon_ij(:,3)-  res/(w(:)-E_plus_W(i_pert_SOC))
       if (BSS_add_antiresonant.or.BS_K_coupling) then
         Epsilon_ij(:,3)=Epsilon_ij(:,3)+conjg(res)/(w(:)+conjg(E_plus_W(i_pert_SOC)))
       endif
       !
       ! Anomalous Hal term, Eq.(1.12) PRB 48, 11705 (1993)
       if(trim(global_gauge)=='length')   B_Hall(1)=B_Hall(1)+res/           bare_qpg(1,1)**2
       if(trim(global_gauge)=='velocity') B_Hall(1)=B_Hall(1)+res/E_plus_W(i_pert_SOC)**2
       !
     enddo
     !
   enddo
   !
   if (l_rpa_IP) call live_timing(steps=1)
   !
 enddo
 !
 if (l_rpa_IP) call live_timing()
 !
 ! ALL 2 ALL
 !
 call PP_redux_wait( Epsilon_ij(:,3) ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait( Epsilon_ij(:,3) ,COMM=PAR_COM_eh_INDEX%COMM )
 !
 call PP_redux_wait( B_Hall(1) ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait( B_Hall(1) ,COMM=PAR_COM_eh_INDEX%COMM )
 !
 B_Hall(1)=B_Hall(1)*Co_factor
 if (BSS_add_antiresonant.or.BS_K_coupling) B_Hall(1)=B_Hall(1)-conjg(B_Hall(1))
 !
 if(trim(global_gauge)=='length'   ) Epsilon_ij(:,3)= Epsilon_ij(:,3)*Co_factor/bare_qpg(iq,1)**2
 if(trim(global_gauge)=='velocity' ) Epsilon_ij(:,3)= Epsilon_ij(:,3)*Co_factor/     w(:)     **2
 !
 ! I add the term describing the Anomalous Hall effect which is
 ! missing in the length gauge (this should be zero for systems with a gap)
 if (l_anomalous_Hall.and.trim(global_gauge)=='length') Epsilon_ij(:,3)=Epsilon_ij(:,3)+B_Hall(1)/w(:)
 !
 ! CLEAN
 !
 if (iq==1) call KERR_alloc('DIP_projected')
 !
end subroutine
