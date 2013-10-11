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
subroutine RT_ELPH_CCA_Kernel(ik,ik_m_q,i_q,i_l,Bose_f,E,Gkkp,l_skip_ABS)
 !  
 use pars,           ONLY:SP,cI,pi
 use electrons,      ONLY:levels,spin_occ,n_full_bands
 use SC,             ONLY:SC_bands,it_now,RT_ibz_coo,RT_Nk_ibz
 use real_time,      ONLY:G_lesser,MEM_index,E_P_Hole_lifetime,E_P_Electron_lifetime,&
&                         E_P_El_abs_lifetime,E_P_Ho_abs_lifetime,&
&                         B_mat_index,G_MEM_steps,l_RT_uses_E_RIM,RIM_ibz_coo
 use ELPH,           ONLY:ph_freqs_sq
 use parallel_m,     ONLY:PAR_IND_B_mat,PAR_B_mat_index,PAR_Xk_ibz_index,PAR_Q_index
 !
 implicit none
 !
 type(levels),  intent(in)  :: E
 integer,       intent(in)  :: ik,ik_m_q,i_q,i_l
 real(SP),      intent(in)  :: Bose_f(2)
 complex(SP),   intent(in)  :: Gkkp(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 logical :: l_skip_ABS
 !
 ! Work Space ...
 !
 integer  :: i_n,i_np,B_dim,i_n_shift,i_np_shift,i_b_mat_mem,ik_mem,iq_mem,ik_rim,ip_rim,i1,i2
 real(SP) :: f_k_m_q,P_ph_abs,P_ph_emit,ph_freq,DELTA_E,DELTA_W,pre_factor
 real(SP), external :: RT_G_retarded_convolution
 !
 ! Dimensions
 !============
 !
 B_dim=SC_bands(2)-SC_bands(1)+1
 pre_factor=1._SP
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
   !
   do i_np=1,B_dim
     ! 
     i_np_shift=i_np+SC_bands(1)-1
     !
     ! 16/3/2012
     !
     ! Including "by hand" the limits on the signs of DELTA I managed to get
     ! (finally) a converged el-ph dynamics (On 5/9/2013 moved in G_ret initialize).
     !
     ! Updated on 29/3/2012 with more tailored constrains. Valid only in systems
     ! with a gap.
     !
     ! n>=n_full_bands+1 => i_np>=n_full_bands+1 
     !
     ! Fermi level ----------------
     !
     ! n<=n_full_bands => i_np<=n_full_bands 
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
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       do ip_rim=RT_ibz_coo(ik_m_q,1),RT_ibz_coo(ik_m_q,2)
         !
         f_k_m_q=-cI*G_lesser(i_np_shift,i_np_shift,ip_rim,MEM_index(it_now,G_MEM_steps))/spin_occ
         !
         if (l_RT_uses_E_RIM) then
           i1=ik_rim-RT_ibz_coo(ik,1)+RIM_ibz_coo(ik,1)
           i2=ip_rim-RT_ibz_coo(ik_m_q,1)+RIM_ibz_coo(ik_m_q,1)
           DELTA_E=E%E_RIM(i_n_shift,i1,1)-E%E_RIM(i_np_shift,i2,1)
           pre_factor=1._SP/real(RT_Nk_ibz(ik_m_q))
         endif
         !
         P_ph_abs  =RT_G_retarded_convolution(DELTA_E,ph_freq,DELTA_W,E%E_ind_gap(1),+1)
         P_ph_emit =RT_G_retarded_convolution(DELTA_E,ph_freq,DELTA_W,E%E_ind_gap(1),-1)
         !
         E_P_Hole_lifetime(i_n_shift,ik_rim)=E_P_Hole_lifetime(i_n_shift,ik_rim)+&
&                                            abs(Gkkp(i_np,i_n))**2*f_k_m_q*P_ph_abs*Bose_f(2)*pre_factor
         !
         E_P_Electron_lifetime(i_n_shift,ik_rim)=E_P_Electron_lifetime(i_n_shift,ik_rim)+&
&                                                abs(Gkkp(i_np,i_n))**2*(1.-f_k_m_q)*P_ph_emit*Bose_f(2)*pre_factor
         !
         if (.not.l_skip_ABS) then
           E_P_Ho_abs_lifetime(i_n_shift,ik_rim)=E_P_Ho_abs_lifetime(i_n_shift,ik_rim)+&
&                                                abs(Gkkp(i_np,i_n))**2*f_k_m_q*P_ph_emit*Bose_f(1)*pre_factor
           E_P_El_abs_lifetime(i_n_shift,ik_rim)=E_P_El_abs_lifetime(i_n_shift,ik_rim)+&
&                                                abs(Gkkp(i_np,i_n))**2*(1.-f_k_m_q)*P_ph_abs*Bose_f(1)*pre_factor
         endif
       enddo
     enddo
     !
   enddo
   !
 enddo
 !
end subroutine RT_ELPH_CCA_Kernel
