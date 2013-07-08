!
! Copyright (C) 2000-2011 D. Sangalli and the YAMBO team 
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
subroutine K_kerr_IP(w)
 !
 use pars,           ONLY:SP,pi
 use units,          ONLY:DEG2RAD
 use memory_m,       ONLY:mem_est
 use timing,         ONLY:live_timing
 use drivers,        ONLY:l_rpa_IP
 use X_m,            ONLY:X_epsilon,DIP_q_dot_iR
 use KERR,           ONLY:BSS_rhoq0_kerr,DIP_q_dot_iR_kerr,DIP_P_symm,B_Hall
 use fields,         ONLY:global_gauge
 use BS,             ONLY:BSS_n_freqs,BS_K_dim,BSS_rhoq0,BS_eh_f,BS_eh_table,&
&                         BS_eh_E,BS_eh_W,BS_K_coupling,BS_anti_res
 use electrons,      ONLY:spin_occ,spin
 use R_lattice,      ONLY:d3k_factor,q_norm
 use drivers,        ONLY:l_anomalous_Hall
 !
 implicit none
 !
 complex(SP)       :: w(BSS_n_freqs) 
 real(SP)          :: Co
 !
 ! Work Space
 !
 integer           ::ik,iv,ic,i1,i_sp
 !
 !
 if (BS_K_coupling.or.allocated(BS_eh_W)) then
   allocate(BSS_rhoq0_kerr(2*BS_K_dim)) 
   call mem_est("BSS_rhoq0_kerr",(/2*BS_K_dim/))
 else
   allocate(BSS_rhoq0_kerr(BS_K_dim)) 
   call mem_est("BSS_rhoq0_kerr",(/BS_K_dim/))
 endif
 !
 B_Hall=(0._SP,0._SP)
 !
 ! eps0_xy
 !
 if(l_rpa_IP) call live_timing('Eps0_off',BS_K_dim)
 !
 do i1=1,BS_K_dim
   ik  =BS_eh_table(i1,1)
   iv  =BS_eh_table(i1,2)
   ic  =BS_eh_table(i1,3)
   i_sp=spin(BS_eh_table(i1,:))
   !
   if(trim(global_gauge)=='length') BSS_rhoq0_kerr(i1)=-conjg(DIP_q_dot_iR_kerr(ic,iv,ik,i_sp))*sqrt(BS_eh_f(i1))
   if(trim(global_gauge)=='velocity') BSS_rhoq0_kerr(i1)= conjg(DIP_P_symm(2,ic,iv,ik,i_sp))*sqrt(BS_eh_f(i1))
   !
   if (BS_K_coupling) then
     if(trim(global_gauge)=='length') BSS_rhoq0_kerr(BS_K_dim+i1)=DIP_q_dot_iR_kerr(ic,iv,ik,i_sp)*sqrt(BS_eh_f(i1))
     if(trim(global_gauge)=='velocity') BSS_rhoq0_kerr(BS_K_dim+i1)=DIP_P_symm(2,ic,iv,ik,i_sp)*sqrt(BS_eh_f(i1))
   endif
   !
   if (abs(BS_eh_E(i1))<1.E-5) cycle
   X_epsilon(6,:)=X_epsilon(6,:)-BSS_rhoq0(i1)*conjg(BSS_rhoq0_kerr(i1))/(w(:)-BS_eh_E(i1))
   if (l_anomalous_Hall) then
     if(trim(global_gauge)=='length')   B_Hall(1)=B_Hall(1)+BSS_rhoq0(i1)*conjg(BSS_rhoq0_kerr(i1))
     if(trim(global_gauge)=='velocity') B_Hall(1)=B_Hall(1)+BSS_rhoq0(i1)*conjg(BSS_rhoq0_kerr(i1))/real(BS_eh_E(i1))**2
   endif
   if (BS_anti_res) then
     X_epsilon(6,:)=X_epsilon(6,:)+conjg(BSS_rhoq0(i1))*BSS_rhoq0_kerr(i1)/(w(:)+BS_eh_E(i1))
   endif
   !
   if(l_rpa_IP) call live_timing(steps=1)
   !
 enddo
 !
 if(l_rpa_IP) call live_timing()
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
