!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
subroutine RT_ELPH_total_kernel(ik,E,k,q,TIME)
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
 use pars,           ONLY:SP,cI,cZero
 use electrons,      ONLY:levels,spin_occ
 use R_lattice,      ONLY:bz_samp,qindx_S
 use SC,             ONLY:SC_bands
 use real_time,      ONLY:I1_lesser,q_weight,elph_gkkp_expanded,GKKP_matrix_index,RT_q_px,l_ELPH_CCA_kernel
 use ELPH,           ONLY:ph_freqs_sq,elph_branches
 use functions,      ONLY:bose_f
 use par_proc_m,     ONLY:pp_redux_wait
 !
 implicit none
 !
 type(levels),  intent(in) :: E
 type(bz_samp), intent(in) :: k,q
 integer,       intent(in) :: ik
 real(SP),      intent(in) :: TIME
 !
 ! Work Space ...
 !
 integer               :: i_s,i_map(-1:1),il,i_gkkp,ik_bz
 real(SP)              :: ph_freq,N_q(2)
 !
 complex(SP)           :: Gkkp(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: S_mat_out_of_eq(2,SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: S_mat_eq(2,SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: S_mat_CCA(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: S_mat_sum(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 !
 ! ... band index ...
 integer               :: i_n,i_np,i_m,B_dim,i_np_m,i_n_m
 ! ... BZ index ...
 integer               :: iqbz,ik_m_q,ik_m_qs
 !
 B_dim=SC_bands(2)-SC_bands(1)+1
 ik_bz=sum(k%nstar(:ik-1))+1
 !
 i_map(-1)=1
 i_map( 0)=1
 i_map( 1)=2
 !
 S_mat_eq=cZero
 S_mat_CCA=cZero
 S_mat_out_of_eq=cZero
 S_mat_sum=cZero
 !
 do iqbz=1,q%nbz
   !
   if (.not.RT_q_px%element_1D(iqbz)) cycle
   !
   ik_m_q =k%sstar(qindx_S(ik,iqbz,1),1)
   ik_m_qs=k%sstar(qindx_S(ik,iqbz,1),2)
   !
   i_gkkp=GKKP_matrix_index(iqbz)
   !
   do il=elph_branches(1),elph_branches(2)
     !
     ph_freq=abs(sqrt(ph_freqs_sq(iqbz,il)))
     !
     if (abs(ph_freq)<epsilon(1._SP)) cycle
     !
     ! N+/-
     !
     N_q    = (bose_f(ph_freq)+(/0.,spin_occ/))/spin_occ
     !
     ! Note the m.e. in Gkkp are stored with indexes order exchanged
     !
     Gkkp=(0._SP,0._SP)
     do i_n=SC_bands(1),SC_bands(2)
       i_n_m=i_n-SC_bands(1)+1
       Gkkp(i_n_m,i_n_m)=elph_gkkp_expanded((i_gkkp-1)*k%nbz+ik_bz,il,i_n,i_n)*sqrt(q_weight(iqbz))/sqrt(ph_freq)
       do i_np=SC_bands(1),SC_bands(2)
         i_np_m=i_np-SC_bands(1)+1
         Gkkp(i_n_m,i_np_m)=elph_gkkp_expanded((i_gkkp-1)*k%nbz+ik_bz,il,i_np,i_n)*sqrt(q_weight(iqbz))/sqrt(ph_freq)
       enddo 
     enddo
     !
     if (l_ELPH_CCA_Kernel) then
       !
       ! Complete Collision Approximation (CCA)
       !=======================================
       !
       call RT_ELPH_CCA_kernel(ik,ik_m_q,N_q,E,ph_freq,Gkkp,S_mat_CCA)
       !
       do i_n=1,B_dim
         S_mat_sum(i_n,i_n)=S_mat_CCA(i_n,i_n)
       enddo
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
 !if (ik==5) write (100,'(3f20.10)') TIME,S_mat_sum(2,2)
 !
 call pp_redux_wait(S_mat_sum(:,:))
 !
 do i_n=SC_bands(1),SC_bands(2)
   do i_np=SC_bands(1),SC_bands(2)
     !
     i_n_m=i_n-SC_bands(1)+1
     i_np_m=i_np-SC_bands(1)+1
     !
     I1_lesser(i_n,i_np,ik)=I1_lesser(i_n,i_np,ik)+S_mat_sum(i_n_m,i_np_m)
     !
   enddo 
 enddo
 !
end subroutine RT_ELPH_total_kernel
