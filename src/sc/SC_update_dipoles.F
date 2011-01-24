!
! Copyright (C) 2000-2010 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine SC_update_dipoles(SC_R,Xen,Xk,X)
 !
 ! This routine is needed to mimic a yambo_sc calculation done in length gauge, where
 ! the [r,Sigma_cohosex] is neglected.
 !
 ! This subroutine update <i|r|j> dipoles using G_lesser_reference as
 ! Green function to construct the Hamiltonian
 ! disregarding the commutator with any nonlocal-part of the Hamiltonian except pseudo
 ! namely this subroutine removes the fictious diagonal terms present in <R>
 ! due to the rotation of an incomplete matrix
 !
 use pars,           ONLY:SP
 use com,            ONLY:warning
 use drivers,        ONLY:l_sc_run
 use electrons,      ONLY:levels,n_sp_pol
 use SC,             ONLY:SC_bands,H_rotate
 use QP_m,           ONLY:QP_nk
 use X_m,            ONLY:DIP_P,DIP_iR,X_t
 use R_lattice,      ONLY:bz_samp 
 !
 implicit none
 !
 type(levels),  intent(in) :: Xen
 type(bz_samp), intent(in) :: Xk
 complex(SP),   intent(in) :: SC_R(SC_bands,SC_bands,QP_nk,n_sp_pol)
 type(X_t),     intent(inout) :: X
 !
 ! Work Space
 !
 integer  :: ik,id,iv,ic
 real(SP) :: E_m_Ep
 !
 call warning("Optical matrix elements updated according to the new WF")
 !
 do ik=1,Xen%nk
   !
   do iv=1,SC_bands
     do ic=1,SC_bands
       E_m_Ep=Xen%E(iv,ik,1)-Xen%E(ic,ik,1)
       DIP_iR(:,ic,iv,ik,1)=DIP_P(:,ic,iv,ik,1)/E_m_Ep
       if(abs(E_m_Ep)<1.E-5) DIP_iR(:,ic,iv,ik,1)=(0._SP,0._SP)
     enddo
   enddo
   !
   do id=1,3
     DIP_P(id,1:SC_bands,1:SC_bands,ik,1) =transpose(DIP_P(id,1:SC_bands,1:SC_bands,ik,1))
     DIP_iR(id,1:SC_bands,1:SC_bands,ik,1)=transpose(DIP_iR(id,1:SC_bands,1:SC_bands,ik,1))
     call H_rotate(SC_R(1:SC_bands,1:SC_bands,ik,1),DIP_P(id,1:SC_bands,1:SC_bands,ik,1),SC_bands,-1)
     call H_rotate(SC_R(1:SC_bands,1:SC_bands,ik,1),DIP_iR(id,1:SC_bands,1:SC_bands,ik,1),SC_bands,-1)
     DIP_P(id,1:SC_bands,1:SC_bands,ik,1) =transpose(DIP_P(id,1:SC_bands,1:SC_bands,ik,1))
     DIP_iR(id,1:SC_bands,1:SC_bands,ik,1)=transpose(DIP_iR(id,1:SC_bands,1:SC_bands,ik,1))
   enddo
   !
 enddo
 !
 if(l_sc_run) call Dipole_dip_dot_field(Xen,Xk,X,X%q0)
 !
end subroutine SC_update_dipoles
