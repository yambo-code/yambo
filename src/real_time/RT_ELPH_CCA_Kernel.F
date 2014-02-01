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
subroutine RT_ELPH_CCA_Kernel(ik,ik_m_q,i_q,i_l,Bose_f,E,Gkkp,l_skip_ABS,G_lesser)
 !  
 use pars,           ONLY:SP,cI,pi,zero_dfl,rZero
 use electrons,      ONLY:levels,spin_occ,n_full_bands
 use SC,             ONLY:SC_bands,RT_ibz_coo,RT_Nk_ibz,RT_nk,RT_occupations_locked
 use real_time,      ONLY:E_P_Hole_lifetime,E_P_Electron_lifetime,&
&                         E_P_El_abs_lifetime,E_P_Ho_abs_lifetime,&
&                         B_mat_index,l_RT_uses_E_RIM,RIM_ibz_coo
 use ELPH,           ONLY:ph_freqs_sq
 use parallel_m,     ONLY:PAR_IND_B_mat,PAR_B_mat_index,PAR_Xk_ibz_index,PAR_Q_index
 !
 implicit none
 !
 type(levels),  intent(in) :: E
 integer,       intent(in) :: ik,ik_m_q,i_q,i_l
 real(SP),      intent(in) :: Bose_f(2)
 complex(SP),   intent(in) :: Gkkp(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP),   intent(in) :: G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 logical,       intent(in) :: l_skip_ABS
 !
 ! Work Space ...
 !
 integer  :: i_n,i_np,B_dim,i_n_shift,i_np_shift,i_b_mat_mem,ik_mem,iq_mem,ik_rim,ip_rim,i1,i2
 real(SP) :: f_k_m_q,P_ph_abs,P_ph_emit,ph_freq,DELTA_E,DELTA_W,pre_factor
 real(SP) :: TMP_Hole_lifetime(2),TMP_Electron_lifetime(2)
 real(SP), external :: RT_G_retarded_convolution
 !
 ! Dimensions
 !============
 !
 B_dim=SC_bands(2)-SC_bands(1)+1
 ik_mem=PAR_Xk_ibz_index(ik)
 iq_mem=PAR_Q_index(i_q)
 !
 ! Kernel kind
 !============= 
 !
 ph_freq=abs(sqrt(ph_freqs_sq(i_q,i_l)))
 !
 do i_n=1,B_dim
   !
   i_n_shift=i_n+SC_bands(1)-1
   if(RT_occupations_locked(i_n_shift,ik)) cycle
   !
   do i_np=1,B_dim
     !
     i_np_shift=i_np+SC_bands(1)-1
     if(RT_occupations_locked(i_np_shift,ik_m_q)) cycle
     !
     ! Extra condition: the EP scattering cannot cross the gap
     ! This could be replaced by a more general condition in RT_G_retarded_convolution
     ! All other conditions have been already moved in RT_G_retarded_convolution
     !
     if (i_n_shift<=n_full_bands.and.i_np_shift>  n_full_bands) cycle
     if (i_n_shift> n_full_bands.and.i_np_shift<= n_full_bands) cycle
     !
     if ( .not. PAR_IND_B_mat%element_1D( B_mat_index(i_n_shift,i_np_shift) ) ) cycle
     i_b_mat_mem=PAR_B_mat_index(i_n_shift,i_np_shift)
     !
     DELTA_E=E%E(i_n_shift,ik,1)-E%E(i_np_shift,ik_m_q,1)
     DELTA_W=abs(E%W(i_n_shift,ik,1))+abs(E%W(i_np_shift,ik_m_q,1))
     !
     pre_factor=abs(Gkkp(i_np,i_n))**2/real(RT_Nk_ibz(ik_m_q))
     !
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       !
       ! Zeroing
       !=========
       TMP_Hole_lifetime=rZero
       TMP_Electron_lifetime=rZero
       !
       do ip_rim=RT_ibz_coo(ik_m_q,1),RT_ibz_coo(ik_m_q,2)
         !
         f_k_m_q=-cI*G_lesser(i_np_shift,i_np_shift,ip_rim)/spin_occ
         !
         if (l_RT_uses_E_RIM) then
           i1=ik_rim-RT_ibz_coo(ik,1)+RIM_ibz_coo(ik,1)
           i2=ip_rim-RT_ibz_coo(ik_m_q,1)+RIM_ibz_coo(ik_m_q,1)
           DELTA_E=E%E_RIM(i_n_shift,i1,1)-E%E_RIM(i_np_shift,i2,1)
         endif
         !
         P_ph_abs  =RT_G_retarded_convolution(DELTA_E,+ph_freq,DELTA_W,0._SP,E%E_ind_gap(1),'EP')
         P_ph_emit =RT_G_retarded_convolution(DELTA_E,-ph_freq,DELTA_W,0._SP,E%E_ind_gap(1),'EP')
         !
         if ( abs(P_ph_abs)<1.E-7 .and. abs(P_ph_emit)<1.E-7 ) cycle
         !
         TMP_Hole_lifetime(1)=TMP_Hole_lifetime(1)+f_k_m_q*P_ph_abs
         TMP_Electron_lifetime(1)=TMP_Electron_lifetime(1)+(1.-f_k_m_q)*P_ph_emit
         !
         ! Also in case of zero Bose Temp I compute these coefficients
         ! because I'll need them for the interpolation of the el-el lifetimes
         !
         if( abs(Bose_f(1))>zero_dfl .and. .not.l_skip_abs) then
           TMP_Hole_lifetime(2)=TMP_Hole_lifetime(2)+f_k_m_q*P_ph_emit
           TMP_Electron_lifetime(2)=TMP_Electron_lifetime(2)+(1.-f_k_m_q)*P_ph_abs
         endif
         !
       enddo
       !
       ! Emission
       E_P_Hole_lifetime(i_n_shift,ik_rim)=E_P_Hole_lifetime(i_n_shift,ik_rim)+&
       &                             TMP_Hole_lifetime(1)*Bose_f(2)*pre_factor
       E_P_Electron_lifetime(i_n_shift,ik_rim)=E_P_Electron_lifetime(i_n_shift,ik_rim)+&
       &                             TMP_Electron_lifetime(1)*Bose_f(2)*pre_factor
       !
       if(abs(Bose_f(1))<zero_dfl.or.l_skip_abs) cycle
       !
       ! Absorption
       E_P_Ho_abs_lifetime(i_n_shift,ik_rim)=E_P_Ho_abs_lifetime(i_n_shift,ik_rim)+&
       &                             TMP_Hole_lifetime(2)*Bose_f(1)*pre_factor
       E_P_El_abs_lifetime(i_n_shift,ik_rim)=E_P_El_abs_lifetime(i_n_shift,ik_rim)+&
       &                             TMP_Electron_lifetime(2)*Bose_f(1)*pre_factor
       !
     enddo
     !
   enddo
   !
 enddo
 !
end subroutine RT_ELPH_CCA_Kernel
