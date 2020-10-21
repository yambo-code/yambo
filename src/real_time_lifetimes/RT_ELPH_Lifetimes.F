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
subroutine RT_ELPH_Lifetimes(ik,E,k)
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
 use pars,           ONLY:SP,rZERO
 use electrons,      ONLY:levels,spin_occ,n_full_bands
 use R_lattice,      ONLY:bz_samp,qindx_S,nqbz,q_sstar
 use real_time,      ONLY:RT_bands
 use RT_lifetimes,   ONLY:elph_gkkp_expanded_sq
 use ELPH,           ONLY:ph_freqs_sq,elph_branches,MAX_gkkp
 use parallel_m,     ONLY:PAR_IND_Q_bz,PAR_Q_bz_index,PAR_Xk_ibz_index
 use units,          ONLY:HA2EV

 use hamiltonian,    ONLY:B_mat_index
 use RT_occupations, ONLY:RT_el_occ,RT_life_occ,RT_ho_occ,RT_ph_occ
 use RT_lifetimes,   ONLY:RT_EP_emit_life,l_initial_equilibrium,RT_EP_abs_life,&
&                         l_RT_skip_ph_abs_lifetimes,q_weight_RT
 use real_time,      ONLY:l_RT_uses_E_FineGd,RT_levels,RT_eh_en,RTibz
 use parallel_m,     ONLY:PAR_IND_B_mat
 !
 implicit none
 !
 type(levels),  intent(in) :: E
 type(bz_samp), intent(in) :: k
 integer,       intent(in) :: ik
 !
 ! Work Space ...
 !
 real(SP)              :: ph_freq
 real(SP), parameter   :: cutoff= 5.E-3
 !
 ! ... band/k index ...
 integer               :: i_n,i_np,ik_rim,ip_rim,iqbz,ik_m_q,ik_mem,iq_mem,il,iq_ibz
 !
 ! ... Gamma components ...
 real(SP) :: P_ph_abs,P_ph_emit,DELTA_E,DELTA_W,pre_factor,&
&            f_times_abs,f_times_emit,one_m_f_times_abs,f_kmq,Hole_f_kmq,one_m_f_times_emit
 !
 ! ... G_ret ...
 real(SP), external :: RT_G_retarded_convolution
 !
 ik_mem=PAR_Xk_ibz_index(ik)
 !
 do iqbz=1,nqbz
   !
   if (.not.PAR_IND_Q_bz%element_1D(iqbz)) cycle
   !
   ik_m_q =k%sstar(qindx_S(ik,iqbz,1),1)
   iq_ibz =q_sstar(iqbz,1)
   !
   iq_mem=PAR_Q_bz_index(iqbz)
   !
   do il=elph_branches(1),elph_branches(2)
     !
     ph_freq=abs(sqrt(ph_freqs_sq(iqbz,il)))
     !
     do i_n=RT_bands(1),RT_bands(2)
       !
       do i_np=RT_bands(1),RT_bands(2)
         !
         ! Extra condition: the EP scattering cannot cross the gap
         !
         if ((i_n<=n_full_bands.and.i_np>  n_full_bands).and.(E%E_ind_gap(1)>0.1/HA2EV)) cycle
         if ((i_n> n_full_bands.and.i_np<= n_full_bands).and.(E%E_ind_gap(1)>0.1/HA2EV)) cycle
         !
         if ( .not. PAR_IND_B_mat%element_1D( B_mat_index(i_n,i_np,RT_bands) ) ) cycle
         !
         if (.not. l_RT_uses_E_FineGd) then
           !
           if(RT_levels%E(i_np,ik_m_q,1)<RT_eh_en(1) .or. RT_levels%E(i_np,ik_m_q,1)>RT_eh_en(2)) cycle
           if(RT_levels%E(i_n ,ik    ,1)<RT_eh_en(1) .or. RT_levels%E(i_n ,ik    ,1)>RT_eh_en(2)) cycle
           !
           DELTA_E=RT_levels%E(i_n,ik,1)-RT_levels%E(i_np,ik_m_q,1)
           DELTA_W=RT_levels%W(i_n,ik,1)+RT_levels%W(i_np,ik_m_q,1)
           !
         endif
         !
         pre_factor=elph_gkkp_expanded_sq(i_n,i_np,il,iq_mem,ik_mem)/real(RTibz%N_mapped(ik_m_q))
         !
         if (sqrt(elph_gkkp_expanded_sq(i_n,i_np,il,iq_mem,ik_mem))<MAX_gkkp*cutoff) cycle
         !
         do ik_rim=RTibz%k_range(ik,1),RTibz%k_range(ik,2)
           !
           ! Zeroing
           !=========
           f_times_abs       =rZERO
           f_times_emit      =rZERO
           one_m_f_times_abs =rZERO
           one_m_f_times_emit=rZERO
           !
           do ip_rim=RTibz%k_range(ik_m_q,1),RTibz%k_range(ik_m_q,2)
             !
             if (l_RT_uses_E_FineGd) then
               !
               if(RT_levels%E(i_np,ip_rim,1)<RT_eh_en(1) .or. RT_levels%E(i_np,ip_rim,1)>RT_eh_en(2)) cycle
               if(RT_levels%E(i_n ,ik_rim,1)<RT_eh_en(1) .or. RT_levels%E(i_n ,ik_rim,1)>RT_eh_en(2)) cycle
               !
               DELTA_E=RT_levels%E(i_n,ik_rim,1)-RT_levels%E(i_np,ip_rim,1)
               DELTA_W=RT_levels%W(i_n,ik_rim,1)+RT_levels%W(i_np,ip_rim,1)
               !
             endif
             !
             P_ph_abs  =RT_G_retarded_convolution(DELTA_E,+ph_freq,DELTA_W,0._SP,'EP')
             P_ph_emit =RT_G_retarded_convolution(DELTA_E,-ph_freq,DELTA_W,0._SP,'EP')
             !
             if ( abs(P_ph_abs)<1.E-7 .and. abs(P_ph_emit)<1.E-7 ) cycle
             !
             if(l_initial_equilibrium) then
               f_kmq      = RT_el_occ%N_ref(i_np,ip_rim)/spin_occ
               Hole_f_kmq = RT_ho_occ%N_ref(i_np,ip_rim)/spin_occ
             else
               f_kmq      =  RT_life_occ%dN(i_np,ip_rim)/spin_occ
               Hole_f_kmq = -RT_life_occ%dN(i_np,ip_rim)/spin_occ
             endif
             !
             f_times_abs=f_times_abs+f_kmq*P_ph_abs
             one_m_f_times_emit=one_m_f_times_emit+Hole_f_kmq*P_ph_emit
             !
             if( .not.l_RT_skip_ph_abs_lifetimes) then
               f_times_emit=f_times_emit+f_kmq*P_ph_emit
               one_m_f_times_abs=one_m_f_times_abs+Hole_f_kmq*P_ph_abs
             endif
             !
           enddo
           !
           ! PH Emission by El/Ho jumps down/up
           RT_EP_emit_life%abs(i_n,ik_rim)=RT_EP_emit_life%abs(i_n,ik_rim)+f_times_abs*pre_factor*&
&                                          (q_weight_RT(iqbz,il,1)+q_weight_RT(iqbz,il,2)*RT_ph_occ%N(il,iq_ibz))
           RT_EP_emit_life%emit(i_n,ik_rim)=RT_EP_emit_life%emit(i_n,ik_rim)+one_m_f_times_emit*pre_factor*&
&                                          (q_weight_RT(iqbz,il,1)+q_weight_RT(iqbz,il,2)*RT_ph_occ%N(il,iq_ibz))
           !
           ! T>0 or PH dyn
           !
           if (l_RT_skip_ph_abs_lifetimes) cycle
           !
           RT_EP_abs_life%emit(i_n,ik_rim)=RT_EP_abs_life%emit(i_n,ik_rim)+&
&                                          f_times_emit*pre_factor*q_weight_RT(iqbz,il,2)*RT_ph_occ%N(il,iq_ibz)
           RT_EP_abs_life%abs(i_n,ik_rim)=RT_EP_abs_life%abs(i_n,ik_rim)+&
&                                         one_m_f_times_abs*pre_factor*q_weight_RT(iqbz,il,2)*RT_ph_occ%N(il,iq_ibz)
           !
         enddo
         !
       enddo
       !
     enddo
     !
   enddo ! ph branch
   !
 enddo ! iqbz
 !
end subroutine RT_ELPH_Lifetimes
