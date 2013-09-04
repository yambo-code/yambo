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
subroutine RT_ELPH_CCA_Kernel(ik,ik_m_q,i_q,i_l,f_k,Bose_f,E,Gkkp,l_skip_ABS)
 !  
 use pars,           ONLY:SP,cI,pi
 use electrons,      ONLY:levels,spin_occ,n_full_bands
 use SC,             ONLY:SC_bands,it_now
 use real_time,      ONLY:G_lesser,MEM_index,E_P_Hole_lifetime,E_P_Electron_lifetime,&
&                         E_P_El_abs_lifetime,E_P_Ho_abs_lifetime,&
&                         Gr_ep_matrix,B_mat_index,G_MEM_steps
 use ELPH,           ONLY:ph_freqs_sq,ph_modes
 use parallel_m,     ONLY:PAR_IND_B_mat,PAR_B_mat_index,PAR_Xk_ibz_index,PAR_Q_index
 !
 implicit none
 !
 type(levels),  intent(in)  :: E
 integer,       intent(in)  :: ik,ik_m_q,i_q,i_l
 real(SP),      intent(in)  :: Bose_f(2),f_k(SC_bands(2)-SC_bands(1)+1)
 complex(SP),   intent(in)  :: Gkkp(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 logical :: l_skip_ABS
 !
 ! Work Space ...
 !
 integer  :: i_n,i_np,B_dim,i_n_shift,i_np_shift,i_b_mat_mem,ik_mem,iq_mem
 real(SP) :: DELTA,f_k_m_q,P_ph_abs,P_ph_emit,ph_freq
 !
 ! Dimensions
 !============
 !
 B_dim=SC_bands(2)-SC_bands(1)+1
 !
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
     ! (finally) a converged el-ph dynamics.
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
     f_k_m_q=-cI*G_lesser(i_np+SC_bands(1)-1,i_np+SC_bands(1)-1,ik_m_q,MEM_index(it_now,G_MEM_steps))/spin_occ
     !
     DELTA  = E%E(i_np+SC_bands(1)-1,ik_m_q,1)-E%E(i_n+SC_bands(1)-1,ik,1)
     !
     P_ph_abs =0.
     P_ph_emit=0.
     !
     if (abs(DELTA)<1.E-10) cycle
     !
     if (DELTA>1.E-10) P_ph_abs =Gr_ep_matrix(ik_mem,i_b_mat_mem,iq_mem,i_l+ph_modes)
     if (DELTA<1.E-10) P_ph_emit=Gr_ep_matrix(ik_mem,i_b_mat_mem,iq_mem,i_l)
     !
     E_P_Hole_lifetime(i_n_shift,ik)=E_P_Hole_lifetime(i_n_shift,ik)+&
&                                    abs(Gkkp(i_np,i_n))**2*f_k_m_q*P_ph_abs*Bose_f(2)
     if (.not.l_skip_ABS) &
&     E_P_Ho_abs_lifetime(i_n_shift,ik)=E_P_Ho_abs_lifetime(i_n_shift,ik)+&
&                                      abs(Gkkp(i_np,i_n))**2*f_k_m_q*P_ph_emit*Bose_f(1)
     !
     E_P_Electron_lifetime(i_n_shift,ik)=E_P_Electron_lifetime(i_n_shift,ik)+&
&                                        abs(Gkkp(i_np,i_n))**2*(1.-f_k_m_q)*P_ph_emit*Bose_f(2)
     !
     if (.not.l_skip_ABS) &
&     E_P_El_abs_lifetime(i_n_shift,ik)=E_P_El_abs_lifetime(i_n_shift,ik)+&
&                                      abs(Gkkp(i_np,i_n))**2*(1.-f_k_m_q)*P_ph_abs*Bose_f(1)
     !
   enddo
   !
 enddo
 !
end subroutine RT_ELPH_CCA_Kernel
