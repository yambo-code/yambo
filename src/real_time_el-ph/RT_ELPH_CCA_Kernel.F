!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine RT_ELPH_CCA_Kernel(ik,ik_m_q,i_q,i_l,N_q,E,ph_freq,Gkkp,l_skip_ABS)
 !  
 use pars,           ONLY:SP,zero_dfl,rZero
 use electrons,      ONLY:levels,n_full_bands,spin_occ
 use hamiltonian,    ONLY:B_mat_index
 use real_time,      ONLY:E_P_Hole_lifetime,E_P_Electron_lifetime,&
&                         E_P_El_abs_lifetime,E_P_Ho_abs_lifetime,&
&                         l_RT_uses_E_DbGd,DbGd_ibz_coo,RT_occupations_locked,&
&                         RT_H_occupations_reference,RT_E_occupations_reference,&
&                         RT_delta_occupations,l_initial_equilibrium,q_weight_RT,&
&                         RT_eh_en,RT_scatt_tresh,RT_ibz_coo,RT_Nk_ibz,RT_bands
 use parallel_m,     ONLY:PAR_IND_B_mat,PAR_B_mat_index,PAR_Xk_ibz_index,PAR_Q_index
 !
 implicit none
 !
 type(levels),  intent(in) :: E
 integer,       intent(in) :: ik,ik_m_q,i_q,i_l
 real(SP),      intent(in) :: ph_freq,N_q
 complex(SP),   intent(in) :: Gkkp(RT_bands(2)-RT_bands(1)+1,RT_bands(2)-RT_bands(1)+1)
 logical,       intent(in) :: l_skip_ABS
 !
 ! Work Space ...
 !
 integer  :: i_n,i_np,B_dim,i_n_shift,i_np_shift,i_b_mat_mem,ik_mem,iq_mem,ik_rim,&
&            ip_rim,i1,i2
 real(SP) :: P_ph_abs,P_ph_emit,DELTA_E,DELTA_W,pre_factor,&
&            TMP_Hole_lifetime(2),TMP_Electron_lifetime(2),f_kmq,Hole_f_kmq
 real(SP), external :: RT_G_retarded_convolution
 !
 ! Dimensions
 !============
 !
 B_dim=RT_bands(2)-RT_bands(1)+1
 ik_mem=PAR_Xk_ibz_index(ik)
 iq_mem=PAR_Q_index(i_q)
 !
 ! Kernel kind
 !============= 
 !
 do i_n=1,B_dim
   !
   i_n_shift=i_n+RT_bands(1)-1
   !
   do i_np=1,B_dim
     !
     i_np_shift=i_np+RT_bands(1)-1
     !
     ! Extra condition: the EP scattering cannot cross the gap
     !
     if (i_n_shift<=n_full_bands.and.i_np_shift>  n_full_bands) cycle
     if (i_n_shift> n_full_bands.and.i_np_shift<= n_full_bands) cycle
     !
     if ( .not. PAR_IND_B_mat%element_1D( B_mat_index(i_n_shift,i_np_shift,RT_bands) ) ) cycle
     !
     i_b_mat_mem=PAR_B_mat_index(i_n_shift,i_np_shift)
     !
     if (.not. l_RT_uses_E_DbGd) then
       !
       if(E%E(i_np_shift,ik_m_q,1)<RT_eh_en(1) .or. E%E(i_np_shift,ik_m_q,1)>RT_eh_en(2)) cycle
       if(E%E(i_n_shift ,ik    ,1)<RT_eh_en(1) .or. E%E(i_n_shift ,ik    ,1)>RT_eh_en(2)) cycle
       !
       DELTA_E=    E%E(i_n_shift,ik,1) -    E%E(i_np_shift,ik_m_q,1)
       DELTA_W=abs(E%W(i_n_shift,ik,1))+abs(E%W(i_np_shift,ik_m_q,1))
       !
     endif
     !
     pre_factor=abs(Gkkp(i_np,i_n))**2/real(RT_Nk_ibz(ik_m_q))
     !
     if( pre_factor==0._SP ) cycle
     !
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       !
       if (RT_occupations_locked(i_n_shift,ik_rim)) cycle
       !
       ! Zeroing
       !=========
       TMP_Hole_lifetime=rZero
       TMP_Electron_lifetime=rZero
       !
       do ip_rim=RT_ibz_coo(ik_m_q,1),RT_ibz_coo(ik_m_q,2)
         !
         if (RT_occupations_locked(i_np_shift,ip_rim)) cycle
         !
         if (l_RT_uses_E_DbGd) then
           !
           i1=ik_rim-RT_ibz_coo(ik,1)+DbGd_ibz_coo(ik,1)
           i2=ip_rim-RT_ibz_coo(ik_m_q,1)+DbGd_ibz_coo(ik_m_q,1)
           !
           if(E%E_DbGd(i_np_shift,i2,1)<RT_eh_en(1) .or. E%E_DbGd(i_np_shift,i2,1)>RT_eh_en(2)) cycle
           if(E%E_DbGd(i_n_shift ,i1,1)<RT_eh_en(1) .or. E%E_DbGd(i_n_shift ,i1,1)>RT_eh_en(2)) cycle
           !
           DELTA_E=    E%E_DbGd(i_n_shift,i1,1) -    E%E_DbGd(i_np_shift,i2,1)
           DELTA_W=abs(E%W_DbGd(i_n_shift,i1,1))+abs(E%W_DbGd(i_np_shift,i2,1))
           !
         endif
         !
         P_ph_abs  =RT_G_retarded_convolution(DELTA_E,+ph_freq,DELTA_W,0._SP,RT_scatt_tresh,'EP')
         P_ph_emit =RT_G_retarded_convolution(DELTA_E,-ph_freq,DELTA_W,0._SP,RT_scatt_tresh,'EP')
         !
         if ( abs(P_ph_abs)<1.E-7 .and. abs(P_ph_emit)<1.E-7 ) cycle
         !
         if(l_initial_equilibrium) then
           f_kmq      = RT_E_occupations_reference(i_np_shift,ip_rim)/spin_occ
           Hole_f_kmq = RT_H_occupations_reference(i_np_shift,ip_rim)/spin_occ
         else
           f_kmq      =  RT_delta_occupations(i_np_shift,ip_rim)/spin_occ
           Hole_f_kmq = -RT_delta_occupations(i_np_shift,ip_rim)/spin_occ
         endif
         !
         TMP_Hole_lifetime(1)=TMP_Hole_lifetime(1)+f_kmq*P_ph_abs
         TMP_Electron_lifetime(1)=TMP_Electron_lifetime(1)+Hole_f_kmq*P_ph_emit
         !
         if( abs(N_q)>zero_dfl .and. .not.l_skip_ABS) then
           TMP_Hole_lifetime(2)=TMP_Hole_lifetime(2)+f_kmq*P_ph_emit
           TMP_Electron_lifetime(2)=TMP_Electron_lifetime(2)+Hole_f_kmq*P_ph_abs
         endif
         !
       enddo
       !
       ! Emission
       E_P_Hole_lifetime(i_n_shift,ik_rim)=E_P_Hole_lifetime(i_n_shift,ik_rim)+&
&                                          TMP_Hole_lifetime(1)*pre_factor*q_weight_RT(i_q,i_l,1)
       E_P_Electron_lifetime(i_n_shift,ik_rim)=E_P_Electron_lifetime(i_n_shift,ik_rim)+&
&                                              TMP_Electron_lifetime(1)*pre_factor*q_weight_RT(i_q,i_l,1)
       !
       if(.not.abs(N_q)>zero_dfl) cycle
       !
       ! Stimulated Emission
       E_P_Hole_lifetime(i_n_shift,ik_rim)=E_P_Hole_lifetime(i_n_shift,ik_rim)+&
&                                          TMP_Hole_lifetime(1)*pre_factor*q_weight_RT(i_q,i_l,2)
       E_P_Electron_lifetime(i_n_shift,ik_rim)=E_P_Electron_lifetime(i_n_shift,ik_rim)+&
&                                              TMP_Electron_lifetime(1)*pre_factor*q_weight_RT(i_q,i_l,2)
       !
       if(l_skip_ABS) cycle
       !
       ! Absorption
       E_P_Ho_abs_lifetime(i_n_shift,ik_rim)=E_P_Ho_abs_lifetime(i_n_shift,ik_rim)+&
&                                            TMP_Hole_lifetime(2)*pre_factor*q_weight_RT(i_q,i_l,2)
       E_P_El_abs_lifetime(i_n_shift,ik_rim)=E_P_El_abs_lifetime(i_n_shift,ik_rim)+&
&                                            TMP_Electron_lifetime(2)*pre_factor*q_weight_RT(i_q,i_l,2)
       !
     enddo
     !
   enddo
   !
 enddo
 !
end subroutine RT_ELPH_CCA_Kernel
