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
subroutine RT_G_retarded_EE_initialize(E,k,q)
 !
 use pars,           ONLY:SP,rZERO,IP,pi,tol2
 use QP_m,           ONLY:QP_n_states,QP_table
 use real_time,      ONLY:Gr_ee_matrix,Gr_kind,two_alpha
 use memory_m,       ONLY:mem_est
 use SC,             ONLY:SC_bands
 use D_lattice,      ONLY:nsym
 use R_lattice,      ONLY:bz_samp,nqbz,qindx_S,nkibz
 use parallel_m,     ONLY:PAR_nQP,PAR_nQPp,PAR_nQ,PAR_IND_Q,PAR_IND_QP,PAR_IND_QPp,PAR_Q_index,&
&                         PAR_QP_index,PAR_QPp_index
 use LIVE_t,         ONLY:live_timing
 use electrons,      ONLY:levels
 use matrix_operate, ONLY:matrix_linear_index
 !
 implicit none
 !
 type(levels)  :: E
 type(bz_samp) :: k,q
 !
 ! Work Space
 !
 integer :: i_qp1,i_qp2,i_qp1_mem,i_qp2_mem,i_n,i_k,i_m,i_p,i_np,i_kmq,i_mp,&
&           i_q,i_q_s,iqbz,iq_mem,i_p_s,i_p_star,i_pmq_bz,i_pmq,i_pmq_G0,&
&           N_2bodies,N_4bodies
 real(SP):: GAMMA_,E_center,E_nn,E_mm
 !
 real(SP),    external :: HyperbolicSecant_FT
 !
 call k_sym2sym(q,'k')
 call k_sym2sym(k,'k')
 !
 ! Gr (e-e)
 !==========
 !
 ! Dimension of the table (2/7/13). If |i>=|nk> I have that
 !
 !  Gr_{ij,lk}= Gr_{ji,lk}
 !            = Gr_{ij,kl}
 !            = Gr_{ji,kl}
 !            = Gr_{lk,ij}
 !            = Gr_{lk,ji}
 !            = Gr_{kl,ij}
 !            = Gr_{kl,ji}
 !
 ! Thus I can re-order ij and kl in globally order indexes 
 ! that runs up to N_2bodies, and re-order (ij) and (kl) in N_4bodies
 !
 N_2bodies= matrix_linear_index(1,(SC_bands(2)-SC_bands(1)+1)*nkibz)-1
 N_4bodies= matrix_linear_index(1,N_2bodies)-1
 !
 ! Allocation
 !
 allocate(Gr_ee_matrix(PAR_nQP,PAR_nQPp,PAR_nQ,nsym))
 Gr_ee_matrix=rZERO
 call live_timing('[Gret] Electron-Electron:',PAR_nQP*PAR_nQPp*PAR_nQ)
 !
 do i_qp1=1,QP_n_states
   !
   if (.not.PAR_IND_QP%element_1D(i_qp1)) cycle
   i_qp1_mem=PAR_QP_index(i_qp1)
   !
   i_n  = QP_table(i_qp1,1)
   i_np = QP_table(i_qp1,2)
   i_k  = QP_table(i_qp1,3)
   !
   do i_qp2=1,QP_n_states
     !
     if (.not.PAR_IND_QPp%element_1D(i_qp2)) cycle
     i_qp2_mem=PAR_QPp_index(i_qp2)
     !
     i_m   = QP_table(i_qp2,1)
     i_mp  = QP_table(i_qp2,2)
     i_p   = QP_table(i_qp2,3)
     !
     do iqbz=1,nqbz
       !
       if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
       iq_mem=PAR_Q_index(iqbz)
       !
       i_q   =q%sstar(iqbz,1)
       i_q_s =q%sstar(iqbz,2)
       i_kmq =k%sstar(qindx_S(i_k,iqbz,1),1)
       !
       do i_p_star=1,k%nstar(i_p)
         !
         i_p_s =k%star(i_p,i_p_star)
         !
         call RT_EE_scattering_kinematics(k,q,i_p,i_p_s,i_q,i_q_s,i_pmq_bz,i_pmq_G0)
         !
         i_pmq =k%sstar(i_pmq_bz,1)
         !
         E_nn=E%E(i_np,i_kmq,1)-E%E(i_n,i_k,1)
         E_mm=E%E(i_m,i_p,1)-E%E(i_mp,i_pmq,1)
         E_center=E_nn+E_mm
         !
         if (.not.Energy_Condition(E_nn,E_mm,E_center,E%E_ind_gap(1))) cycle
         !
         GAMMA_ = abs(E%W(i_np,i_kmq,1))+abs(E%W(i_m,i_p,1))+abs(E%W(i_mp,i_pmq,1))+abs(E%W(i_n,i_k,1))
         !
         if(trim(Gr_kind)=='QP') Gr_ee_matrix(i_qp1_mem,i_qp2_mem,iq_mem,i_p_s)=GAMMA_/(E_center**2+GAMMA_**2)
         !
         if(trim(Gr_kind)=='HS') &
&           Gr_ee_matrix(i_qp1_mem,i_qp2_mem,iq_mem,i_p_s)=HyperbolicSecant_FT(E_center,GAMMA_,2*two_alpha)
         !
         !DEBUG>
         !write (100,*) E_center*27.21, Gr_ee_matrix(i_qp1_mem,i_qp2_mem,iq_mem,i_p_s),GAMMA_*27.21
         !DEBUG<
         !
       enddo
       !
       call live_timing(steps=1)
       !
     enddo
     !
   enddo
   !
 enddo
 !
 call live_timing()
 !
 contains
   !
   logical function Energy_Condition(E1,E2,Etot,Egap)
     real(SP) :: E1,E2,Etot,Egap
     Energy_Condition=.true.
     if ( abs(E1)  <tol2     ) Energy_Condition=.false.      ! C1a
     if ( abs(E2)  <tol2     ) Energy_Condition=.false.      ! C1b
     if ( abs(Etot)<tol2     ) Energy_Condition=.false.      ! C2
     if ( abs(Etot)>2.*Egap  ) Energy_Condition=.false.      ! C3
     if (    E1/E2 >0._SP    ) Energy_Condition=.false.      ! C4a+C4b
   end function Energy_Condition 
   !
end subroutine RT_G_retarded_EE_initialize
