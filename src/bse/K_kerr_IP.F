!
!        Copyright (C) 2000-2014 the YAMBO team
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
 use pars,           ONLY:SP,pi
 use units,          ONLY:DEG2RAD
 use memory_m,       ONLY:mem_est
 use LIVE_t,         ONLY:live_timing
 use drivers,        ONLY:l_rpa_IP
 use X_m,            ONLY:X_epsilon,DIP_q_dot_iR,global_gauge
 use KERR,           ONLY:BSS_rhoq0_kerr,DIP_q_dot_iR_kerr,DIP_P_symm,B_Hall
 use BS,             ONLY:BSS_n_freqs,BS_K_dim,BS_T_grp,BSS_eh_f,BSS_eh_table,&
&                         BSS_eh_E,BSS_eh_W,BS_K_coupling,BS_anti_res,BS_nT_grps
 use electrons,      ONLY:spin_occ,spin
 use R_lattice,      ONLY:d3k_factor,q_norm
 use drivers,        ONLY:l_anomalous_Hall
 use parallel_m,     ONLY:PAR_BS_nT_col_grps,PP_redux_wait,PAR_COM_eh_INDEX,PAR_IND_T_groups,&
&                         PAR_IND_Xk_ibz,PAR_IND_CON_BANDS_X,PAR_IND_VAL_BANDS_X,&
&                         PAR_Xk_bz_index,PAR_COM_Xk_ibz_INDEX
 !
 implicit none
 !
 complex(SP)       :: w(BSS_n_freqs) 
 real(SP)          :: Co
 !
 ! Work Space
 !
 complex(SP)       ::local_Z
 integer           ::ik,iv,ic,i1,i_sp,i_T,i_T_g,ik_mem
 !
 !
 B_Hall=(0._SP,0._SP)
 !
 ! eps0_xy
 !
 if (l_rpa_IP) call live_timing('IP Off-diagonal absorption',PAR_BS_nT_col_grps)
 !
 do i_T_g=1,BS_nT_grps
   !    
   if (.not.PAR_IND_T_groups%element_1D(i_T_g)) cycle
   !    
   do i_T=1,BS_T_grp(i_T_g)%size    
     !    
     ik    = BS_T_grp(i_T_g)%table(i_T,1)
     ik_mem= PAR_Xk_bz_index(ik)
     iv    = BS_T_grp(i_T_g)%table(i_T,2)
     ic    = BS_T_grp(i_T_g)%table(i_T,3)
     i_sp  = BS_T_grp(i_T_g)%table(i_T,4)
     !
     local_Z=1.
     if (allocated(BS_T_grp(i_T_g)%Z)) local_Z=BS_T_grp(i_T_g)%Z(i_T)
     !
     if (trim(global_gauge)=='length')   BS_T_grp(i_T_g)%dipoles_R_kerr(i_T)=-conjg(DIP_q_dot_iR_kerr(ic,iv,ik_mem,i_sp))
     if (trim(global_gauge)=='velocity') BS_T_grp(i_T_g)%dipoles_R_kerr(i_T)= conjg(DIP_P_symm(2,ic,iv,ik_mem,i_sp))
     BS_T_grp(i_T_g)%dipoles_R_kerr(i_T)=BS_T_grp(i_T_g)%dipoles_R_kerr(i_T)*sqrt(BS_T_grp(i_T_g)%f(i_T))
     !
     if (BS_K_coupling) then
       if (trim(global_gauge)=='length')   BS_T_grp(i_T_g)%dipoles_A_kerr(i_T)=DIP_q_dot_iR_kerr(ic,iv,ik_mem,i_sp)
       if (trim(global_gauge)=='velocity') BS_T_grp(i_T_g)%dipoles_A_kerr(i_T)=DIP_P_symm(2,ic,iv,ik_mem,i_sp)
       BS_T_grp(i_T_g)%dipoles_A_kerr(i_T)=BS_T_grp(i_T_g)%dipoles_A_kerr(i_T)*sqrt(BS_T_grp(i_T_g)%f(i_T))
     endif
     !
     if (abs(BS_T_grp(i_T_g)%E(i_T))<1.E-5) cycle
     !    
     X_epsilon(6,:)=X_epsilon(6,:)-BS_T_grp(i_T_g)%dipoles_R(i_T)*conjg( BS_T_grp(i_T_g)%dipoles_R_kerr(i_T) )*&
&                     local_Z/(w(:)-BS_T_grp(i_T_g)%E(i_T))
     !
     if (l_anomalous_Hall) then
       if(trim(global_gauge)=='length')   B_Hall(1)=B_Hall(1)+BS_T_grp(i_T_g)%dipoles_R(i_T)* &
&                                           conjg( BS_T_grp(i_T_g)%dipoles_R_kerr(i_T) )
       if(trim(global_gauge)=='velocity') B_Hall(1)=B_Hall(1)+BS_T_grp(i_T_g)%dipoles_R(i_T)* &
&                                           conjg( BS_T_grp(i_T_g)%dipoles_R_kerr(i_T) )/real(BS_T_grp(i_T_g)%E(i_T))**2
     endif
     !
     if (BS_anti_res) then
       X_epsilon(6,:)=X_epsilon(6,:)+conjg(BS_T_grp(i_T_g)%dipoles_R(i_T))*BS_T_grp(i_T_g)%dipoles_R_kerr(i_T)*&
&                       local_Z/(w(:)+BS_T_grp(i_T_g)%E(i_T))
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
 Co=real(spin_occ)/(2.*pi)**3.*d3k_factor*4.*pi
 !
 if(trim(global_gauge)=='length')   X_epsilon(6,:)= X_epsilon(6,:)*Co/q_norm(1)**2
 if(trim(global_gauge)=='velocity') X_epsilon(6,:)= X_epsilon(6,:)*Co/(real(w(:))**2+epsilon(1.))
 !
 if (BS_anti_res) B_Hall(1)=B_Hall(1)-conjg(B_Hall(1))
 !
 if(trim(global_gauge)=='length')   B_Hall(1)=B_Hall(1)*Co/q_norm(1)**2
 if(trim(global_gauge)=='velocity') B_Hall(1)=B_Hall(1)*Co
 !
 ! I add the term describing the Anomalous Hall effect which is
 ! missing in the length gauge (this should be zero for systems with a gap)
 if (trim(global_gauge)=='length'.and.l_anomalous_Hall) X_epsilon(6,:)=X_epsilon(6,:)+B_Hall(1)/w(:)
 !
end subroutine
