!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine K_diago_compute_epsilon(iq,W,BS_E,BS_R_left,BS_R_right,BS_E_SOC_corr,BS_R_right_kerr,BS_R_PL)
 !
 ! eps2(iw)= 1 - Sum  [BS_R_left(i)*BS_R_right(i)] / [w+i*eta - E(i)]  
 !
 use pars,           ONLY:cZERO,cONE,SP,pi,rZERO
 use X_m,            ONLY:Epsilon_ii,Epsilon_ij,Joint_DOS,X_drude_term,global_gauge,iw_ref,skip_cond_sum_rule
 use electrons,      ONLY:spin_occ,nel,n_sp_pol
 use frequency,      ONLY:w_samp
 use R_lattice,      ONLY:q_norm
 use parallel_m,     ONLY:PP_redux_wait,myid,PP_indexes,PP_indexes_reset
 use LIVE_t,         ONLY:live_timing
 use BS,             ONLY:BS_H_dim,BS_pert_soc
 use BS_solvers,     ONLY:BSS_n_eig,BSS_add_antiresonant,Co_factor
 use interfaces,     ONLY:PARALLEL_index
 use R_lattice,      ONLY:nkbz
 !
 ! GPL_EXCLUDE_START
 !
#if defined _KERR
 use KERR,           ONLY:B_Hall
 use drivers,        ONLY:l_kerr,l_anomalous_Hall
#endif
 !
#if defined _PL
 use PHOTOLUM,       ONLY:PL,PL_prefactor
 use drivers,        ONLY:l_photolum
#endif
 !
 ! GPL_EXCLUDE_END
 !
 implicit none
 !
 type(w_samp),intent(in)  :: W
 integer,     intent(in)  :: iq
 complex(SP), intent(in)  :: BS_E(BS_H_dim)
 complex(SP), intent(in)  :: BS_R_left(BS_H_dim),BS_R_right(BS_H_dim)
 real(SP),    pointer     :: BS_E_SOC_corr(:,:)
 complex(SP), pointer     :: BS_R_right_kerr(:)
 real(SP),    pointer     :: BS_R_PL(:,:)
 !
 ! Work space
 !
 type(PP_indexes)     ::px
 !
 integer              :: i1,i_pert_SOC,n_SOC
 complex(SP)          :: factor_jj,EXC_E(2/n_sp_pol)
 !
 ! GPL_EXCLUDE_START
 !
#if defined _KERR
 complex(SP)          :: gauge_factor
#endif
 !
 ! GPL_EXCLUDE_END
 !
 n_SOC=1
 !
 ! GPL_EXCLUDE_START
 !
 if( BS_pert_soc.and.n_sp_pol==1) n_SOC=2
 !
 ! GPL_EXCLUDE_END
 !
 Epsilon_ii(:,2)=cZERO
 Joint_DOS(:,2) =cZERO
 !
 ! GPL_EXCLUDE_START
 !
#if defined _KERR
 if(l_kerr) then
   Epsilon_ij(:,2)=cZERO
   if(l_anomalous_Hall) B_Hall(2)=cZERO
 endif
#endif
 !
#if defined _PL
 if (l_photolum) PL(:,2) = rZERO
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
#if defined _PL
 call live_timing('BSK  EPS and PL',px%n_of_elements(myid+1))
#else
 call live_timing('BSK     epsilon',px%n_of_elements(myid+1))
#endif
 !
 do i1=1,BSS_n_eig
   !
   if (.not.px%element_1D(i1)) cycle
   !
   ! GPL_EXCLUDE_START
   !
#if defined _KERR
   if (l_kerr.and.l_anomalous_Hall) then
     if (trim(global_gauge)=='length'  ) gauge_factor=cONE
     if (trim(global_gauge)=='velocity') gauge_factor=BS_E(i1)
     B_Hall(2)=B_Hall(2)+   BS_R_left(i1)*BS_R_right_kerr(i1) / gauge_factor **2._SP
   endif
#endif
   !
   if (     BS_pert_soc) EXC_E(:)=BS_E(i1)+BS_E_SOC_corr(:,i1)
   !
   ! GPL_EXCLUDE_END
   !
   if (.not.BS_pert_soc) EXC_E(1)=BS_E(i1)
   !
   do i_pert_SOC=1,n_SOC
     !
     ! Note the use of a causal expression here needed to avoid any
     ! distinction between the resonant and antiresonant e/h Green's functions.
     !
     Epsilon_ii(:,2)=Epsilon_ii(:,2)-BS_R_left(i1)*BS_R_right(i1)/(W%p(:)-EXC_E(i_pert_SOC))
     if (BSS_add_antiresonant) Epsilon_ii(:,2)=Epsilon_ii(:,2)+&
&      conjg(BS_R_left(i1)*BS_R_right(i1))/(W%p(:)+conjg(EXC_E(i_pert_SOC)))
     !
     Joint_DOS(:,2)=Joint_DOS(:,2)-1._SP/(W%p(:)-EXC_E(i_pert_SOC))
     if (BSS_add_antiresonant) Joint_DOS(:,2)=Joint_DOS(:,2)+1._SP/(W%p(:)+conjg(EXC_E(i_pert_SOC)))
     !
     ! GPL_EXCLUDE_START
     !
#if defined _KERR
     if(l_kerr) then
       Epsilon_ij(:,2)=Epsilon_ij(:,2)-BS_R_left(i1)*BS_R_right_kerr(i1)/(W%p(:)-EXC_E(i_pert_SOC))
       if (BSS_add_antiresonant) Epsilon_ij(:,2)=Epsilon_ij(:,2)+&
&        conjg(BS_R_left(i1)*BS_R_right_kerr(i1))/(W%p(:)+conjg(EXC_E(i_pert_SOC)))
     endif
#endif
     !
#if defined _PL
     if(l_photolum) then
       PL(:,2)=PL(:,2)+PL_prefactor*BS_R_PL(1,i1)*aimag(-1._SP/(W%p(:)-EXC_E(i_pert_SOC)))/pi
       if (BSS_add_antiresonant) PL(:,2)=PL(:,2)+PL_prefactor*BS_R_PL(2,i1)*aimag(-1._SP/(W%p(:)+conjg(EXC_E(i_pert_SOC))))/pi
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
 !
 call PP_redux_wait(Epsilon_ii(:,2))
 call PP_redux_wait(Joint_DOS(:,2) )
#if defined _PL
 call PP_redux_wait(PL(:,2))
#endif
 !
 ! GPL_EXCLUDE_START
 !
#if defined _KERR
 if(l_kerr) then
   call PP_redux_wait(Epsilon_ij(:,2))
   if(l_anomalous_Hall) call PP_redux_wait(B_Hall)
 endif
#endif
 !
 ! GPL_EXCLUDE_END
 !
 call PP_indexes_reset(px)
 !
 ! GPL_EXCLUDE_START
 !
 if(trim(global_gauge)=='velocity') then
   if (.not.skip_cond_sum_rule) factor_jj=Epsilon_ii(iw_ref,2)
   if (     skip_cond_sum_rule) factor_jj=cmplx(nel*real(nkbz,SP)/spin_occ,0._SP,SP)
 endif
 !
 ! GPL_EXCLUDE_END
 !
 if(trim(global_gauge)=='length'  ) Epsilon_ii(:,2)=1._SP+(Epsilon_ii(:,2)          +X_drude_term(:))*Co_factor/q_norm(1)**2
 if(trim(global_gauge)=='velocity') Epsilon_ii(:,2)=1._SP+(Epsilon_ii(:,2)-factor_jj+X_drude_term(:))*Co_factor/ W%p(:)  **2
 !
 Joint_DOS(:,2)=Joint_DOS(:,2)*Co_factor
 !
 ! GPL_EXCLUDE_START
 !
#if defined _KERR
 !
 if(l_kerr) then
   if(trim(global_gauge)=='length'  ) Epsilon_ij(:,2)=Epsilon_ij(:,2)*Co_factor/q_norm(1)**2
   if(trim(global_gauge)=='velocity') Epsilon_ij(:,2)=Epsilon_ij(:,2)*Co_factor/ W%p(:)  **2
   !
   if(l_anomalous_Hall) then
     if(BSS_add_antiresonant) B_Hall(2)=B_Hall(2)-conjg(B_Hall(2))
     if(trim(global_gauge)=='length'  ) B_Hall(2)=B_Hall(2)*Co_factor/q_norm(1)**2
     if(trim(global_gauge)=='velocity') B_Hall(2)=B_Hall(2)*Co_factor
     !
     ! I add the term describing the Anomalous Hall effect which is
     ! missing in the length gauge (this should be zero for systems with a gap)
     if (trim(global_gauge)=='length') Epsilon_ij(:,2)=Epsilon_ij(:,2)+B_Hall(2)/W%p(:)
     !
   endif
   !
 endif
 !
#endif
 !
 ! GPL_EXCLUDE_END
 !
end subroutine K_diago_compute_epsilon
