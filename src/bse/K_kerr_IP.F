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
subroutine K_kerr_IP(w,drude_GreenF)
 !
 use pars,           ONLY:SP,pi
 use memory_m,       ONLY:mem_est
 use X_m,            ONLY:X_epsilon,DIP_q_dot_iR
 use KERR,           ONLY:BSS_rhoq0_kerr,DIP_q_dot_iR_kerr,DIP_P_symm,&
&                         chi_nn,chi_jj
 use BS,             ONLY:BSS_n_freqs,BS_K_dim,BSS_rhoq0,BS_eh_f,BS_eh_table,&
&                         BS_eh_E,BS_eh_W,BS_K_coupling
 use electrons,      ONLY:spin_occ,spin
 use R_lattice,      ONLY:d3k_factor,q_norm
 !
 implicit none
 !
 complex(SP)       :: w(BSS_n_freqs) 
 complex(SP)       :: drude_GreenF(BSS_n_freqs)          !(W_bss%n(2))
 real(SP)          :: factor
 complex(SP)       :: factor_jj
 !
 ! Work Space
 !
 integer           ::ik,iv,ic,i1,i_sp
 !
 if (BS_K_coupling.or.allocated(BS_eh_W)) then
   allocate(BSS_rhoq0_kerr(2*BS_K_dim)) 
   call mem_est("BSS_rhoq0_kerr",(/2*BS_K_dim/))
 else
   allocate(BSS_rhoq0_kerr(BS_K_dim)) 
   call mem_est("BSS_rhoq0_kerr",(/BS_K_dim/))
 endif
 !
 ! eps0_xy
 !
 do i1=1,BS_K_dim
   ik  =BS_eh_table(i1,1)
   iv  =BS_eh_table(i1,2)
   ic  =BS_eh_table(i1,3)
   i_sp=spin(BS_eh_table(i1,:))
   !
   if(chi_nn) BSS_rhoq0_kerr(i1)=-conjg(DIP_q_dot_iR_kerr(ic,iv,ik,i_sp))
   if(chi_jj) BSS_rhoq0_kerr(i1)=-conjg(DIP_P_symm(2,ic,iv,ik,i_sp))
   !
   if (BS_K_coupling) then
     if(chi_nn) BSS_rhoq0_kerr(BS_K_dim+i1)=DIP_q_dot_iR_kerr(ic,iv,ik,i_sp)
     if(chi_jj) BSS_rhoq0_kerr(BS_K_dim+i1)=DIP_P_symm(2,ic,iv,ik,i_sp)
   endif
   !
!   ! I did a check with X_os.F: the Drude term in the
!   ! off--diagonal part cannot be included in this way
!   ! Drude term
!   if (abs(BS_eh_E(i1))<1.E-5) then
!     X_epsilon(4,:)=X_epsilon(4,:)-aimag(BSS_rhoq0(i1)*conjg(BSS_rhoq0_kerr(i1)))*BS_eh_f(i1)*&
!&                                  drude_GreenF(:)
!   else
   if (abs(BS_eh_E(i1))<1.E-5) cycle
   X_epsilon(4,:)=X_epsilon(4,:)-BSS_rhoq0(i1)*conjg(BSS_rhoq0_kerr(i1))*BS_eh_f(i1)/&
&                                (w(:)-BS_eh_E(i1))
   if (BS_K_coupling) then
     X_epsilon(4,:)=X_epsilon(4,:)+BSS_rhoq0(i1)*conjg(BSS_rhoq0_kerr(i1))*BS_eh_f(i1)/&
&                                (w(:)+BS_eh_E(i1))
   endif
!   endif
 enddo
 !
 factor=real(spin_occ)/(2.*pi)**3.*d3k_factor*4.*pi
 factor_jj=0. ! X_epsilon(4,1)
 !
 if(chi_nn) X_epsilon(4,:)= X_epsilon(4,:)           *factor/q_norm(1)**2
 if(chi_jj) X_epsilon(4,:)=(X_epsilon(4,:)-factor_jj)*factor/     w(:)**2
! if(chi_nn) X_epsilon(4,:)= X_epsilon(4,:)           *factor/q_norm(1)**2 *w(:)**2
! if(chi_jj) X_epsilon(4,:)=(X_epsilon(4,:)-factor_jj)*factor
 !
end subroutine
