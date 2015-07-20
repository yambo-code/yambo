!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS, AM, MG
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
subroutine K_diag_compute_epsilon(iq,W,BS_E,BS_E_SOC_corr,BS_R,BS_R_kerr)
 !
 ! eps2(iw)= 1 - Sum  BS_R(K)   /(w +I eta - E)  (coupling or non-hermitian)
 !           1 - Sum |BS_R(K)|^2/(w +I eta - E)  (resonant)
 !
 use pars,           ONLY:cZERO,cONE,SP,pi
 use X_m,            ONLY:X_epsilon,X_drude_term,global_gauge,iw_ref
 use electrons,      ONLY:spin_occ,E_SOC_nbands
 use frequency,      ONLY:w_samp
 use R_lattice,      ONLY:d3k_factor,q_norm
 use parallel_m,     ONLY:PP_redux_wait,ncpu,myid,PP_indexes,PP_indexes_reset
 use LIVE_t,         ONLY:live_timing
 use BS,             ONLY:BS_H_dim,BS_K_coupling,BSS_eh_W,BS_anti_res
 use interfaces,     ONLY:PARALLEL_index
 !
 ! GPL_EXCLUDE_START
 !
#if defined _KERR
 use KERR,           ONLY:B_Hall
 use drivers,        ONLY:l_kerr,l_anomalous_Hall
 use units,          ONLY:DEG2RAD
#endif
 !
 ! GPL_EXCLUDE_END
 !
 implicit none
 !
 type(w_samp),intent(in)  :: W
 integer,     intent(in)  :: iq
 complex(SP), intent(in)  :: BS_E(BS_H_dim),BS_E_SOC_corr(2,BS_H_dim)
 complex(SP), intent(in)  :: BS_R(BS_H_dim),BS_R_kerr(BS_H_dim)
 !
 ! Work space
 !
 type(PP_indexes)     ::px
 !
 integer              :: i1,n_soc,i_pert_SOC
 logical              :: K_is_not_hermitian
 real(SP)             :: Co
 complex(SP)          :: factor_jj,EXC_E(2)
 !
 ! GPL_EXCLUDE_START
 !
#if defined _KERR
 complex(SP)          :: gauge_factor
#endif
 !
 ! GPL_EXCLUDE_END
 !
 K_is_not_hermitian=BS_K_coupling.or.allocated(BSS_eh_W)
 !
 n_SOC=1
 if(E_SOC_nbands/=0) n_SOC=2
 !
 X_epsilon(1,:)=cZERO
 X_epsilon(2,:)=cZERO
 X_epsilon(3,:)=X_epsilon(3,:)/real(ncpu,SP)
 !
 ! GPL_EXCLUDE_START
 !
#if defined _KERR
 if(l_kerr) then
   X_epsilon(5,:)=cZERO
   X_epsilon(6,:)=X_epsilon(6,:)/real(ncpu,SP)
   if(l_anomalous_Hall) then
     B_Hall(2)=cZERO
     B_Hall(1)=B_Hall(1)/real(ncpu,SP)
   endif
 endif
#endif
 !
 ! GPL_EXCLUDE_END
 !
 !
 ! If perturbative SOC energies are loaded compute the SOC energies
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/BS_H_dim/))
 !
 call live_timing('BSK     epsilon',px%n_of_elements(myid+1))
 !
 do i1=1,BS_H_dim
   !
   if (.not.px%element_1D(i1)) cycle
   !
   !
   ! GPL_EXCLUDE_START
   !
#if defined _KERR
   if (l_kerr.and.l_anomalous_Hall) then
     if (trim(global_gauge)=='length'  ) gauge_factor=cONE
     if (trim(global_gauge)=='velocity') gauge_factor=BS_E(i1)
     if (     K_is_not_hermitian) B_Hall(2)=B_Hall(2)+      BS_R_kerr(i1)          /     gauge_factor **2._SP
     if (.not.K_is_not_hermitian) B_Hall(2)=B_Hall(2)+conjg(BS_R(i1))*BS_R_kerr(i1)/real(gauge_factor)**2._SP
   endif
#endif
   !
   ! GPL_EXCLUDE_END
   !
   ! Note the use of a causal expression here needed to avoid any
   ! distinction between the resonant and antiresonant e/h Green's functions.
   !
   if (E_SOC_nbands==0) EXC_E(1)=BS_E(i1)
   if (E_SOC_nbands/=0) EXC_E(:)=BS_E(i1)+BS_E_SOC_corr(:,i1)
   !
   do i_pert_SOC=1,n_SOC
     if (     K_is_not_hermitian) X_epsilon(2,:)=X_epsilon(2,:)-BS_R(i1)/(W%p(:)-EXC_E(i_pert_SOC))
     if (.not.K_is_not_hermitian) then
       X_epsilon(2,:)=X_epsilon(2,:)-BS_R(i1)*conjg(BS_R(i1))/(W%p(:)-EXC_E(i_pert_SOC))
       if (BS_anti_res) X_epsilon(2,:)=X_epsilon(2,:)+BS_R(i1)*conjg(BS_R(i1))/(W%p(:)+EXC_E(i_pert_SOC))
     endif
     !
     ! GPL_EXCLUDE_START
     !
#if defined _KERR
     if(l_kerr) then
       if (     K_is_not_hermitian) X_epsilon(5,:)=X_epsilon(5,:)-BS_R_kerr(i1)/(W%p(:)-EXC_E(i_pert_SOC))
       if (.not.K_is_not_hermitian) then
         X_epsilon(5,:)=X_epsilon(5,:)-conjg(BS_R(i1))*BS_R_kerr(i1)/(W%p(:)-BS_E(i1))
         if(BS_anti_res) X_epsilon(5,:)=X_epsilon(5,:)+BS_R(i1)*conjg(BS_R_kerr(i1))/(W%p(:)+EXC_E(i_pert_SOC))
       endif
     endif
#endif
     !
     ! GPL_EXCLUDE_END
     !
   enddo
   !
   call live_timing(steps=1)
 enddo
 call live_timing
 call PP_redux_wait(X_epsilon)
 !
 ! GPL_EXCLUDE_START
 !
#if defined _KERR
 if(l_anomalous_Hall) call PP_redux_wait(B_Hall)
#endif
 !
 ! GPL_EXCLUDE_END
 !
 call PP_indexes_reset(px)
 !
 if (E_SOC_nbands==0) Co=real(spin_occ)/(2._SP*pi)**3*d3k_factor*4._SP*pi
 if (E_SOC_nbands/=0) Co=    1._SP     /(2._SP*pi)**3*d3k_factor*4._SP*pi
 !
 if(trim(global_gauge)=='velocity') factor_jj=X_epsilon(2,iw_ref)
 !
 if(trim(global_gauge)=='length'  ) X_epsilon(2,:)=1._SP+(X_epsilon(2,:)          +X_drude_term(:))*Co/q_norm(1)**2
 if(trim(global_gauge)=='velocity') X_epsilon(2,:)=1._SP+(X_epsilon(2,:)-factor_jj+X_drude_term(:))*Co/ W%p(:)  **2
 !
 ! GPL_EXCLUDE_START
 !
#if defined _KERR
 !
 if(l_kerr) then
   if(trim(global_gauge)=='length'  ) X_epsilon(5,:)=X_epsilon(5,:)*Co/q_norm(1)**2
   if(trim(global_gauge)=='velocity') X_epsilon(5,:)=X_epsilon(5,:)*Co/ W%p(:)  **2
   !
   if(l_anomalous_Hall) then
     if(BS_anti_res) B_Hall(2)=B_Hall(2)-conjg(B_Hall(2))
     if(trim(global_gauge)=='length'  ) B_Hall(2)=B_Hall(2)*Co/q_norm(1)**2
     if(trim(global_gauge)=='velocity') B_Hall(2)=B_Hall(2)*Co
     !
     ! I add the term describing the Anomalous Hall effect which is
     ! missing in the length gauge (this should be zero for systems with a gap)
     if (trim(global_gauge)=='length') X_epsilon(5,:)=X_epsilon(5,:)+B_Hall(2)/W%p(:)
     !
   endif
   !
 endif
 !
#endif
 !
 ! GPL_EXCLUDE_END
 !
 X_epsilon(1,:)=W%p(:)
 !
end subroutine K_diag_compute_epsilon
