!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine RT_ELPH_CCA_Kernel(ik,ik_m_q,i_q,i_l,E,ph_freq,Gkkp_sq)
 !  
 use pars,           ONLY:SP,rZero
 use units,          ONLY:HA2EV
 use electrons,      ONLY:levels,n_full_bands,spin_occ
 use hamiltonian,    ONLY:B_mat_index
 use RT_occupations, ONLY:RT_el_occ,RT_life_occ,RT_ho_occ
 use RT_lifetimes,   ONLY:RT_EP_emit_life,l_initial_equilibrium,RT_EP_abs_life,&
&                         l_RT_skip_ph_abs_lifetimes,q_weight_RT
 use real_time,      ONLY:l_RT_uses_E_FineGd,RT_levels,RT_eh_en,RTibz,RT_bands
 use parallel_m,     ONLY:PAR_IND_B_mat
 !
 implicit none
 !
 type(levels),  intent(in) :: E
 integer,       intent(in) :: ik,ik_m_q,i_q,i_l
 real(SP),      intent(in) :: ph_freq
 real(SP),      intent(in) :: Gkkp_sq(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 ! Work Space ...
 !
 integer  :: i_n,i_np,ik_rim,ip_rim
 real(SP) :: P_ph_abs,P_ph_emit,DELTA_E,DELTA_W,pre_factor,&
&            f_times_abs,f_times_emit,one_m_f_times_abs,f_kmq,Hole_f_kmq,one_m_f_times_emit
 real(SP), external :: RT_G_retarded_convolution
 !
 ! Kernel kind
 !============= 
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
     pre_factor=Gkkp_sq(i_np,i_n)/real(RTibz%N_mapped(ik_m_q))
     !
     if( pre_factor==0._SP ) cycle
     !
     do ik_rim=RTibz%k_range(ik,1),RTibz%k_range(ik,2)
       !
       ! Zeroing
       !=========
       f_times_abs       =rZero
       f_times_emit      =rZero
       one_m_f_times_abs =rZero
       one_m_f_times_emit=rZero
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
       RT_EP_emit_life%abs(i_n,ik_rim)=RT_EP_emit_life%abs(i_n,ik_rim)+&
&                                      f_times_abs*pre_factor*q_weight_RT(i_q,i_l,1)
       RT_EP_emit_life%emit(i_n,ik_rim)=RT_EP_emit_life%emit(i_n,ik_rim)+&
&                                       one_m_f_times_emit*pre_factor*q_weight_RT(i_q,i_l,1)
       !
       ! T>0 or PH dyn
       !
       if (l_RT_skip_ph_abs_lifetimes) cycle
       !
       RT_EP_emit_life%abs(i_n,ik_rim)=RT_EP_emit_life%abs(i_n,ik_rim)+&
&                                      f_times_abs*pre_factor*q_weight_RT(i_q,i_l,2)
       RT_EP_emit_life%emit(i_n,ik_rim)=RT_EP_emit_life%emit(i_n,ik_rim)+&
&                                       one_m_f_times_emit*pre_factor*q_weight_RT(i_q,i_l,2)
       RT_EP_abs_life%emit(i_n,ik_rim)=RT_EP_abs_life%emit(i_n,ik_rim)+&
&                                      f_times_emit*pre_factor*q_weight_RT(i_q,i_l,2)
       RT_EP_abs_life%abs(i_n,ik_rim)=RT_EP_abs_life%abs(i_n,ik_rim)+&
&                                     one_m_f_times_abs*pre_factor*q_weight_RT(i_q,i_l,2)
       !
     enddo
     !
   enddo
   !
 enddo
 !
end subroutine RT_ELPH_CCA_Kernel
