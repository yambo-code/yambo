!
!        Copyright (C) 2000-2015 the YAMBO team
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
subroutine K_kerr_IP(w)
 !
 use pars,           ONLY:SP,pi,cZERO
 use units,          ONLY:DEG2RAD
 use memory_m,       ONLY:mem_est
 use LIVE_t,         ONLY:live_timing
 use drivers,        ONLY:l_rpa_IP
 use X_m,            ONLY:X_epsilon,DIP_projected,global_gauge
 use KERR,           ONLY:BSS_rhoq0_kerr,DIP_projected_r90,B_Hall
 use BS,             ONLY:BSS_n_freqs,BS_K_dim,BS_T_grp,BSS_eh_f,BSS_eh_table,&
&                         BSS_eh_E,BSS_eh_W,BS_K_coupling,BS_anti_res,BS_nT_grps
 use electrons,      ONLY:spin_occ,spin,E_SOC_nbands
 use R_lattice,      ONLY:d3k_factor,q_norm
 use drivers,        ONLY:l_anomalous_Hall
 use parallel_m,     ONLY:PAR_BS_nT_col_grps,PP_redux_wait,PAR_COM_eh_INDEX,PAR_IND_T_groups,&
&                         PAR_IND_Xk_ibz,PAR_IND_CON_BANDS_X,PAR_IND_VAL_BANDS_X,&
&                         PAR_Xk_bz_index,PAR_COM_Xk_ibz_INDEX
 !
 implicit none
 !
 complex(SP)       :: w(BSS_n_freqs) 
 real(SP)          :: Co,conjg_fac
 !
 ! Work Space
 !
 real(SP)          ::IP_E(2)
 complex(SP)       ::local_Z
 integer           ::ik_bz,iv,ic,i1,i_sp,i_T,i_T_g,ik_mem,i_pert_SOC,n_SOC
 !
 n_SOC=1
 if( E_SOC_nbands/=0) n_SOC=2
 !
 B_Hall=cZERO
 !
 local_Z=1._SP
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
     i_sp  = BS_T_grp(i_T_g)%table(i_T,4)
     !
     ik_mem= PAR_Xk_bz_index(ik_bz)
     !
     if (E_SOC_nbands==0) IP_E(1)=BS_T_grp(i_T_g)%E(i_T)
     if (E_SOC_nbands/=0) IP_E(:)=BS_T_grp(i_T_g)%E(i_T)+BS_T_grp(i_T_g)%E_SOC_corr(:,i_T)
     !
     if (allocated(BS_T_grp(i_T_g)%Z)) local_Z=BS_T_grp(i_T_g)%Z(i_T)
     !
     BS_T_grp(i_T_g)%dipoles_R_kerr(i_T)=conjg_fac*conjg(DIP_projected_r90(ic,iv,ik_mem,i_sp))*sqrt(BS_T_grp(i_T_g)%f(i_T))
     if (BS_K_coupling) BS_T_grp(i_T_g)%dipoles_A_kerr(i_T)=DIP_projected_r90(ic,iv,ik_mem,i_sp)*sqrt(BS_T_grp(i_T_g)%f(i_T))
     !
     if ( (abs(IP_E(1))<1.E-5 .or. (abs(IP_E(2))<1.E-5.and.E_SOC_nbands/=0))) cycle
     !
     do i_pert_SOC=1,n_SOC
       X_epsilon(6,:)=X_epsilon(6,:)-BS_T_grp(i_T_g)%dipoles_R(i_T)*conjg( BS_T_grp(i_T_g)%dipoles_R_kerr(i_T) )*&
&                       local_Z/(w(:)-IP_E(i_pert_SOC))
       !
       ! Anomalous Hal term, Eq.(1.12) PRB 48, 11705 (1993)
       if(trim(global_gauge)=='length')   B_Hall(1)=B_Hall(1)+BS_T_grp(i_T_g)%dipoles_R(i_T)* &
&                                           conjg( BS_T_grp(i_T_g)%dipoles_R_kerr(i_T) )
       if(trim(global_gauge)=='velocity') B_Hall(1)=B_Hall(1)+BS_T_grp(i_T_g)%dipoles_R(i_T)* &
&                                           conjg( BS_T_grp(i_T_g)%dipoles_R_kerr(i_T) )/real(IP_E(i_pert_SOC))**2
       !
     enddo
     !
     if (BS_anti_res) then
       !
       ! Note the plus in "+" coming from the change of sign in the occupation factor
       !
       do i_pert_SOC=1,n_SOC
         X_epsilon(6,:)=X_epsilon(6,:)+conjg(BS_T_grp(i_T_g)%dipoles_R(i_T))*BS_T_grp(i_T_g)%dipoles_R_kerr(i_T)*&
&                         local_Z/(w(:)+IP_E(i_pert_SOC))
       enddo
       !
     endif
     !
   enddo
   !
   if (l_rpa_IP) call live_timing(steps=1)
   !
 enddo
 !
 if (l_rpa_IP) call live_timing()
 !
 if (E_SOC_nbands==0) Co=real(spin_occ)/(2._SP*pi)**3*d3k_factor*4._SP*pi
 if (E_SOC_nbands/=0) Co=    1._SP     /(2._SP*pi)**3*d3k_factor*4._SP*pi
 !
 if(trim(global_gauge)=='length'   ) X_epsilon(6,:)= X_epsilon(6,:)*Co/q_norm(1)**2
 if(trim(global_gauge)=='velocity' ) X_epsilon(6,:)= X_epsilon(6,:)*Co/  w(:)   **2
 !
 ! Anomalous Hal term, Eq.(1.12) PRB 48, 11705 (1993)
 if (BS_anti_res) B_Hall(1)=B_Hall(1)-conjg(B_Hall(1))
 !
 if(trim(global_gauge)=='length'   ) B_Hall(1)=B_Hall(1)*Co/q_norm(1)**2
 if(trim(global_gauge)=='velocity' ) B_Hall(1)=B_Hall(1)*Co
 !
 ! I add the term describing the Anomalous Hall effect which is
 ! missing in the length gauge (this should be zero for systems with a gap)
 if (trim(global_gauge)=='length'.and.l_anomalous_Hall) X_epsilon(6,:)=X_epsilon(6,:)+B_Hall(1)/w(:)
 !
end subroutine
