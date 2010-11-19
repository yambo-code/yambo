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
subroutine RT_update_dipoles(G_input,Xen,k,q)
 !
 ! This subroutine update <i|r|j> dipoles using G_lesser_reference as
 ! Green function to construct the Hamiltonian
 ! disregarding the commutator with any nonlocal-part of the Hamiltonian except pseudo
 ! namely this subroutine removes the fictious diagonal terms present in <R>
 ! due to the rotation of an incomplete matrix
 !
 use pars,           ONLY:SP
 use memory_m,       ONLY:mem_est
 use com,            ONLY:warning
 use electrons,      ONLY:levels,spin_occ,n_sp_pol
 use real_time,      ONLY:H_t,l_NE_with_fields
 use matrix_operate, ONLY:mat_dia_inv,DIAGO,USE_LK 
 use SC,             ONLY:SC_bands,H_rotate
 use QP_m,           ONLY:QP_nk
 use global_XC,      ONLY:Dipole_WF_xc_string,loaded_WF_xc_string
 use X_m,            ONLY:DIP_P,DIP_iR
 use fields,         ONLY:ext_field
 use R_lattice,      ONLY:bz_samp 
 !
 implicit none
 !
 type(levels),  intent(inout) :: Xen
 type(bz_samp), intent(in)    :: k,q
 complex(SP),   intent(in)    :: G_input(SC_bands,SC_bands,QP_nk)
 !
 ! Work Space
 !
 integer  :: ik,id,iv,ic
 real(SP) :: E_m_Ep
 logical  :: H_t_allocated,l_fields
 type(ext_field) :: dummy_field
 !
 H_t_allocated   =allocated(H_t)
 l_fields        =l_NE_with_fields
 l_NE_with_fields=.false.
 !
 call warning("Optical matrix elements updated according to the new WF")
 !
 if(.not.H_t_allocated) then
   allocate(H_t(SC_bands,SC_bands,QP_nk,n_sp_pol))
   call mem_est("H_t",(/size(H_t)/),(/2*SP/))         
 endif
 !
 if(associated(Xen%Eo)) deallocate(Xen%Eo)
 allocate(Xen%Eo(SC_bands,QP_nk,n_sp_pol))
 !
 call RT_Collision(G_input,dummy_field,Xen,k,q,0._SP)
 !
 do ik=1,Xen%nk
   !
   call mat_dia_inv(DIAGO,USE_LK,H_t(:,:,ik,1),E_real=Xen%Eo(:SC_bands,ik,1))
   !
   do id=1,3
     call H_rotate(H_t(:,:,ik,1),DIP_P(id,:,:,ik,1),SC_bands,1)
   enddo
   !
   do iv=1,SC_bands
     do ic=1,SC_bands
       E_m_Ep=Xen%Eo(iv,ik,1)-Xen%Eo(ic,ik,1)
       DIP_iR(:,iv,ic,ik,1)=DIP_P(:,iv,ic,ik,1)/E_m_Ep
       if(abs(E_m_Ep)<1.E-5) DIP_iR(:,iv,ic,ik,1)=(0._SP,0._SP)
     enddo
   enddo
   !
   do id=1,3
     call H_rotate(H_t(:,:,ik,1),DIP_P(id,:,:,ik,1),SC_bands,-1)
     call H_rotate(H_t(:,:,ik,1),DIP_iR(id,:,:,ik,1),SC_bands,-1)
   enddo
   !
 enddo
 !
 if(.not.H_t_allocated) then
   deallocate(H_t)
   call mem_est("H_t")
 endif
 !
 l_NE_with_fields=l_fields
 !
end subroutine RT_update_dipoles
