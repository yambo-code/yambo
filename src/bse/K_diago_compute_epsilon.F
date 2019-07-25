!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use electrons,      ONLY:spin_occ,E_SOC_nbands,nel,n_sp_pol
 use frequency,      ONLY:w_samp
 use R_lattice,      ONLY:q_norm
 use parallel_m,     ONLY:PP_redux_wait,myid,PP_indexes,PP_indexes_reset
 use LIVE_t,         ONLY:live_timing
 use BS,             ONLY:BS_H_dim
 use BS_solvers,     ONLY:BSS_add_antiresonant,Co_factor
 use interfaces,     ONLY:PARALLEL_index
 use R_lattice,      ONLY:nkbz
 !
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
 !
 n_SOC=1
 !
 !
 Epsilon_ii(:,2)=cZERO
 Joint_DOS(:,2) =cZERO
 !
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
   if (E_SOC_nbands==0) EXC_E(1)=BS_E(i1)
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
     !
   enddo
   !
   call live_timing(steps=1)
 enddo
 call live_timing
 !
 call PP_redux_wait(Epsilon_ii(:,2))
 call PP_redux_wait(Joint_DOS(:,2) )
 !
 !
 call PP_indexes_reset(px)
 !
 !
 if(trim(global_gauge)=='length'  ) Epsilon_ii(:,2)=1._SP+(Epsilon_ii(:,2)          +X_drude_term(:))*Co_factor/q_norm(1)**2
 if(trim(global_gauge)=='velocity') Epsilon_ii(:,2)=1._SP+(Epsilon_ii(:,2)-factor_jj+X_drude_term(:))*Co_factor/ W%p(:)  **2
 !
 Joint_DOS(:,2)=Joint_DOS(:,2)*Co_factor
 !
 !
end subroutine K_diago_compute_epsilon
