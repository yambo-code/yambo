!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine RT_Coulomb_relaxation(i_k,E,k,q,G_lesser)
 !  
 use pars,            ONLY:SP,cI,rZero,pi
 use electrons,       ONLY:levels,spin_occ
 use R_lattice,       ONLY:qindx_S,bz_samp
 use SC,              ONLY:RT_ibz_coo,SC_bands,RT_nk
 use real_time,       ONLY:E_E_Electron_lifetime,E_E_Hole_lifetime,&
&                          Gr_ee_matrix,l_RT_uses_E_RIM
 use QP_m,            ONLY:QP_n_states,QP_table
 use collision,       ONLY:GW_NEQ_collisions
 use parallel_m,      ONLY:PAR_QP_index,PAR_QPp_index,PAR_IND_QP,PAR_IND_QPp,PAR_Q_index,PAR_IND_Q
 !
 implicit none
 !
 type(levels),  intent(in) :: E
 type(bz_samp), intent(in) :: k,q
 integer,       intent(in) :: i_k
 complex(SP),   intent(in) :: G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 !
 ! Work Space ...
 !
 integer               :: i_qp1,i_qp2,i_p,i_kmq,i_pmq,i_n,i_m,i_mp,i_np,i_p_star,i_k_RIM,&
&                         iqbz,i_p_s,i_q,i_q_s,i_qp1_mem,i_qp2_mem,i_q_mem,i_pmq_bz,i_pmq_G0
 real(SP)              :: f_p,f_kmq,f_pmq,pre_factor,Gr_integral,&
&                         f_E_factor,f_H_factor
 !
 ! Dimensioning and Zeroing
 !==========================
 !
 E_E_Hole_lifetime(:,RT_ibz_coo(i_k,1))    =rZero
 E_E_Electron_lifetime(:,RT_ibz_coo(i_k,1))=rZero
 !
 ! Select only the m.e. of QP_table relative to kpt i_k
 !======================================================
 !
 do i_qp1=1,QP_n_states
   !
   if (QP_table(i_qp1,3)/=i_k) cycle
   !
   if (.not.PAR_IND_QP%element_1D(i_qp1)) cycle
   i_qp1_mem=PAR_QP_index(i_qp1)
   !
   i_n =QP_table(i_qp1,1)
   i_np=QP_table(i_qp1,2)
   !
   do iqbz=1,q%nbz
     !
     if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
     i_q_mem=PAR_Q_index(iqbz)
     !
     i_q   = q%sstar(iqbz,1)
     i_q_s = q%sstar(iqbz,2)
     i_kmq = k%sstar(qindx_S(i_k,iqbz,1),1)
     !
     f_kmq=-cI*G_lesser(i_np,i_np,RT_ibz_coo(i_kmq,1))/spin_occ
     !
     do i_qp2=1,QP_n_states
       !
       if (.not.PAR_IND_QPp%element_1D(i_qp2)) cycle
       i_qp2_mem=PAR_QPp_index(i_qp2)
       !
       i_m  = QP_table(i_qp2,1)
       i_mp = QP_table(i_qp2,2)
       i_p  = QP_table(i_qp2,3)
       !
       f_p=-cI*G_lesser(i_m,i_m,RT_ibz_coo(i_p,1))/spin_occ
       !
       do i_p_star=1,k%nstar(i_p)
         !
         i_p_s =k%star(i_p,i_p_star)
         !
         call RT_EE_scattering_kinematics(k,q,i_p,i_p_s,i_q,i_q_s,i_pmq_bz,i_pmq_G0)
         !
         i_pmq =k%sstar(i_pmq_bz,1)
         !
         f_pmq =-cI*G_lesser(i_mp,i_mp,RT_ibz_coo(i_pmq,1))
         !
         pre_factor=abs(4.*pi*GW_NEQ_collisions(i_qp1_mem)%v3(i_qp2_mem,i_p_s,i_q_mem))**2
         !
         Gr_integral=Gr_ee_matrix(i_qp1_mem,i_qp2_mem,i_q_mem,i_p_s)
         !
         f_E_factor=(1._SP-f_kmq)*(1._SP-f_p)*f_pmq
         f_H_factor=f_kmq*f_p*(2._SP-f_pmq)
         !
         E_E_Electron_lifetime(i_n,RT_ibz_coo(i_k,1))= E_E_Electron_lifetime(i_n,RT_ibz_coo(i_k,1))+&
&                                                   pre_factor*Gr_integral*f_E_factor
         E_E_Hole_lifetime(i_n,RT_ibz_coo(i_k,1))    = E_E_Hole_lifetime(i_n,RT_ibz_coo(i_k,1))+&
&                                                   pre_factor*Gr_integral*F_H_factor
         !
       enddo
       !
     enddo
     !
   enddo
   !
 enddo
 !
 if (.not.l_RT_uses_E_RIM) return
 !
 do i_k_RIM=RT_ibz_coo(i_k,1)+1,RT_ibz_coo(i_k,2)
   E_E_Electron_lifetime(:,i_k_RIM)= E_E_Electron_lifetime(:,RT_ibz_coo(i_k,1))
   E_E_Hole_lifetime(:,i_k_RIM)= E_E_Hole_lifetime(:,RT_ibz_coo(i_k,1))
 enddo  
 !
end subroutine RT_Coulomb_relaxation
