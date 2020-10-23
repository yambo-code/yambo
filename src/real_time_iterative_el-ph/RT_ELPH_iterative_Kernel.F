!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine RT_ELPH_iterative_Kernel(ik,E,k,q,TIME)
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
 use pars,           ONLY:SP,rZERO,cZERO
 use electrons,      ONLY:levels,spin_occ
 use functions,      ONLY:bose_f
 use R_lattice,      ONLY:bz_samp,qindx_S
 use real_time,      ONLY:RT_bands
 use ELPH,           ONLY:ph_freqs_sq,elph_branches,MAX_gkkp,elph_gkkp_expanded_sq 
 use parallel_m,     ONLY:PAR_IND_Q_bz,PAR_Q_bz_index,PAR_Xk_ibz_index
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
 integer               :: i_map(-1:1),il,i_s
 real(SP)              :: ph_freq,N_q(2)
 real(SP), parameter   :: cutoff= 5.E-3
 !
 real(SP)              :: Gkkp_sq(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 complex(SP)           :: S_mat_out_of_eq(2,RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP)           :: S_mat_eq(2,RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP)           :: S_mat_sum(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 ! ... band index ...
 integer               :: i_n,i_np
 ! ... BZ index ...
 integer               :: iqbz,ik_m_q,ik_mem,iq_mem
 !
 ik_mem=PAR_Xk_ibz_index(ik)
 !
 i_map(-1)=1
 i_map( 0)=1
 i_map( 1)=2
 !
 S_mat_eq=cZERO
 S_mat_out_of_eq=cZERO
 S_mat_sum=cZERO
 !
 do iqbz=1,q%nbz
   !
   if (.not.PAR_IND_Q_bz%element_1D(iqbz)) cycle
   !
   ik_m_q =k%sstar(qindx_S(ik,iqbz,1),1)
   !
   iq_mem=PAR_Q_bz_index(iqbz)
   !
   do il=elph_branches(1),elph_branches(2)
     !
     ph_freq=abs(sqrt(ph_freqs_sq(iqbz,il)))
     !
     ! Note the m.e. in elph_gkkp_expanded_sq are stored with indexes order exchanged (see RT_ELPH_initialize)
     !
     Gkkp_sq=rZERO
     do i_n=RT_bands(1),RT_bands(2)
       !
       Gkkp_sq(i_n,i_n)=elph_gkkp_expanded_sq(i_n,i_n,il,iq_mem,ik_mem)
       if(sqrt(Gkkp_sq(i_n,i_n))<MAX_gkkp*cutoff) Gkkp_sq(i_n,i_n)=rZERO
       !
       do i_np=RT_bands(1),RT_bands(2)
         !
         if (i_n==i_np) cycle
         Gkkp_sq(i_n,i_np)=elph_gkkp_expanded_sq(i_np,i_n,il,iq_mem,ik_mem)
         if (sqrt(Gkkp_sq(i_n,i_np))<MAX_gkkp*cutoff) Gkkp_sq(i_n,i_np)=rZERO
         !
       enddo
       !
     enddo
     !
     N_q    = (bose_f(ph_freq)+(/0._SP,spin_occ/))/spin_occ
     !
     ! EQUILIBRIUM Kernel
     !=====================
     !
     call RT_ELPH_eq_kernel(ik,i_map,ik_m_q,N_q,E,TIME,ph_freq,Gkkp_sq,S_mat_eq)
     !
     ! OUT-OF-EQUILIBRIUM Kernel
     !===========================
     !
     call RT_ELPH_out_of_eq_kernel(ik,iqbz,il,i_map,ik_m_q,N_q,E,ph_freq,Gkkp_sq,S_mat_out_of_eq)
     !
     ! SUM-UP
     !========
     !
     do i_s=-1,1,2
       do i_n=RT_bands(1),RT_bands(2)
         do i_np=RT_bands(1),RT_bands(2)
           S_mat_sum(i_n,i_np)=S_mat_sum(i_n,i_np)+&
&                              ( S_mat_out_of_eq(i_map( i_s),i_n,i_np)+conjg(S_mat_out_of_eq(i_map(-i_s),i_np,i_n))+&
&                                S_mat_eq(i_map( i_s),i_n,i_np)+conjg(S_mat_eq(i_map(-i_s),i_np,i_n)) )
         enddo
       enddo
     enddo ! i_s
     !
   enddo ! ph branch
   !
 enddo ! iqbz
 !
end subroutine RT_ELPH_iterative_kernel
