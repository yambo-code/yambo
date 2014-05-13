!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine RT_ELPH_total_kernel(ik,E,k,q,TIME,G_lesser)
 !  
 ! In this routine I do not perform a time step. I calculate
 ! the el-ph Kernel at time TIME using an approximate iterative scheme (light but approximated).
 !  
 ! EULER: 
 !  
 !  In the EULER scheme everything is extremely easy. G_lesser is G(TIME) and  
 !  I use the saved THETA_matrix (that in EULER is saved at TIME-RT_step) and the newly calculated
 !  DELTA matrix to get the new THETA_matrix.
 !  
 ! RK2:  
 !  
 !  In RK2 everything is much more complicated because RT_elph_kernel is called at NE_time and
 !  at NE_time+RT_step/2 and in the two cases a different strategy ***should*** be applied.
 !  
 ! Iteration: in this case I ***think*** (even if I am not completely sure) that everything is ok.
 !  
 use pars,           ONLY:SP,cZero
 use electrons,      ONLY:levels,spin_occ
 use R_lattice,      ONLY:bz_samp,qindx_S
 use SC,             ONLY:SC_bands,RT_nk
 use real_time,      ONLY:Sigma_relax,elph_gkkp_expanded,l_RT_CCA_Kernel
 use ELPH,           ONLY:ph_freqs_sq,elph_branches
 use functions,      ONLY:bose_f
 use parallel_m,     ONLY:PAR_IND_Q,PAR_Q_index,PAR_Xk_nibz,PAR_Xk_ibz_index
 use parser_m,       ONLY:parser
 !
 implicit none
 !
 type(levels),  intent(in) :: E
 type(bz_samp), intent(in) :: k,q
 integer,       intent(in) :: ik
 real(SP),      intent(in) :: TIME
 complex(SP),   intent(in) :: G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 !
 ! Work Space ...
 !
 integer               :: i_s,i_map(-1:1),il,i_gkkp
 real(SP)              :: ph_freq,N_q(2)
 !
 complex(SP)           :: Gkkp(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: S_mat_out_of_eq(2,SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: S_mat_eq(2,SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: S_mat_sum(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 !
 logical :: l_skip_ABS
 !
 ! ... band index ...
 integer               :: i_n,i_np,i_m,B_dim,i_np_m,i_n_m
 ! ... BZ index ...
 integer               :: iqbz,ik_m_q,ik_mem
 !
 B_dim=SC_bands(2)-SC_bands(1)+1
 !
 ik_mem=PAR_Xk_ibz_index(ik)
 !
 call parser('NOABS',l_skip_ABS)
 !
 i_map(-1)=1
 i_map( 0)=1
 i_map( 1)=2
 !
 S_mat_eq=cZero
 S_mat_out_of_eq=cZero
 S_mat_sum=cZero
 !
 do iqbz=1,q%nbz
   !
   if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
   !
   ik_m_q =k%sstar(qindx_S(ik,iqbz,1),1)
   !
   i_gkkp=PAR_Q_index(iqbz)
   !
   do il=elph_branches(1),elph_branches(2)
     !
     ph_freq=abs(sqrt(ph_freqs_sq(iqbz,il)))
     !
     !  This freuqency, which in principle should not be included, describes the region
     !  around q=0, which is taken into account by the integral embodied in the q_qeights
     !  Thus I do not cycle
     !  if ( (iqbz==1 .and. il<=3) ) cycle
     !
     ! N+/-
     !
     N_q    = (bose_f(ph_freq)+(/0._SP,spin_occ/))/spin_occ
     !
     ! Note the m.e. in Gkkp are stored with indexes order exchanged
     !
     Gkkp=(0._SP,0._SP)
     do i_n=SC_bands(1),SC_bands(2)
       i_n_m=i_n-SC_bands(1)+1
       Gkkp(i_n_m,i_n_m)=elph_gkkp_expanded((i_gkkp-1)*PAR_Xk_nibz+ik_mem,il,i_n,i_n)
       do i_np=SC_bands(1),SC_bands(2)
         if(i_n==i_np) cycle
         i_np_m=i_np-SC_bands(1)+1
         Gkkp(i_n_m,i_np_m)=elph_gkkp_expanded((i_gkkp-1)*PAR_Xk_nibz+ik_mem,il,i_np,i_n)
       enddo 
     enddo
     !
     if (l_RT_CCA_Kernel) then
       !
       ! Complete Collision Approximation (CCA)
       !=======================================
       !
       call RT_ELPH_CCA_Kernel(ik,ik_m_q,iqbz,il,N_q(1),E,Gkkp,l_skip_ABS)
       !
     else
       !
       ! EQUILIBRIUM Kernel
       !=====================
       !
       call RT_ELPH_eq_kernel(ik,i_map,ik_m_q,N_q,E,TIME,ph_freq,Gkkp,S_mat_eq)
       !
       ! OUT-OF-EQUILIBRIUM Kernel
       !===========================
       !
       call RT_ELPH_out_of_eq_kernel(ik,iqbz,il,i_map,ik_m_q,N_q,E,ph_freq,Gkkp,S_mat_out_of_eq)
       !
       ! SUM-UP
       !========
       !
       do i_s=-1,1,2
         do i_n=1,B_dim
           do i_np=1,B_dim
             S_mat_sum(i_n,i_np)=S_mat_sum(i_n,i_np)+&
&                                ( S_mat_out_of_eq(i_map( i_s),i_n,i_np)+conjg(S_mat_out_of_eq(i_map(-i_s),i_np,i_n))+&
&                                  S_mat_eq(i_map( i_s),i_n,i_np)+conjg(S_mat_eq(i_map(-i_s),i_np,i_n)) )
           enddo
         enddo
       enddo ! i_s
       !
     endif
     !
   enddo ! ph branch
   !
 enddo ! iqbz
 !
 ! Kernel composition
 !====================
 !
 if (.not.l_RT_CCA_Kernel) then
   !
   do i_n=SC_bands(1),SC_bands(2)
     do i_np=SC_bands(1),SC_bands(2)
       Sigma_relax(i_n,i_np,ik)=Sigma_relax(i_n,i_np,ik)+S_mat_sum(i_n-SC_bands(1)+1,i_np-SC_bands(1)+1)
     enddo 
   enddo
   !
 endif
 !
end subroutine RT_ELPH_total_kernel
