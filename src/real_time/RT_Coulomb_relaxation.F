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
subroutine RT_Coulomb_relaxation(ik,E,k,q)
 !  
 use pars,            ONLY:SP,rZero,cZero,pi
 use electrons,       ONLY:levels,spin_occ
 use R_lattice,       ONLY:qindx_S,qindx_C,bz_samp
 use SC,              ONLY:RT_ibz_coo,RT_occupations,SC_bands
 use real_time,       ONLY:E_E_Electron_lifetime,E_E_Hole_lifetime,RT_occupations_locked,&
&                          RT_delta_occupations,RT_occupations_reference,&
                           RT_one_minus_occupations
 use plasma,          ONLY:EH_gas
 use QP_m,            ONLY:QP_n_states,QP_table
 use collision,       ONLY:GW_NEQ_collisions
 use parallel_m,      ONLY:PAR_QP_index,PAR_IND_QP,PAR_Q_index,PAR_IND_Q,PAR_IND_Plasma,PAR_Plasma_index
 !
 implicit none
 !
 type(levels),     intent(in) :: E
 type(bz_samp),    intent(in) :: k,q
 integer,          intent(in) :: ik
 !
 ! Work Space ...
 !
 integer               :: i_qp,i_qp1,i_p,i_p_bz,ik_m_q,i_pmq,i_n,i_m,i_mp,i_np,ik_rim,&
&                         iqbz,i_p_s,i_q,i_q_s,i_qp1_mem,i_q_mem,i_pmq_bz,i1,i2,&
&                         i_J,i_J_bg,i_J_mem,ik_m_q_rim
 real(SP)              :: pre_factor,P_pl_abs,DELTA_E,DELTA_W
 !
 ! Occupations
 !
 real(SP)              :: f_p,f_pmq,df_p,df_pmq,f0_p,f0_pmq,f_kmq,df_kmq,f0_kmq,one_minus_f_kmq,one_minus_f_pmq,one_minus_f_p
 !
 ! Plasma
 !
 real(SP)              :: N_h_plasma,N_e_plasma,dN_h_plasma,dN_e_plasma,N0_h_plasma,N0_e_plasma,plasma_freq,plasma_width
 complex(SP)           :: E_plasma
 !
 real(SP), external    :: RT_G_retarded_convolution
 !
 ik_rim    =RT_ibz_coo(ik,1)
 !
#if !defined _RT_EE_RIM
 if ( all( RT_occupations_locked(SC_bands(1):SC_bands(2),ik_rim) ) ) return
#endif
 !
 q_loop: do iqbz=1,q%nbz
   !
   if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
   !
   i_q_mem=PAR_Q_index(iqbz)
   !
   i_q   = q%sstar(iqbz,1)
   i_q_s = q%sstar(iqbz,2)
   ik_m_q= k%sstar(qindx_S(ik,iqbz,1),1)
   !
   do i_J=1,EH_gas%N_poles(iqbz)
     !
     if (.not.PAR_IND_Plasma%element_1D(i_J)) cycle
     !
     i_J_mem=PAR_Plasma_index(i_J)
     !
     E_plasma   =cZero
     N_e_plasma =rZero
     N_h_plasma =rZero
     N0_e_plasma=rZero
     N0_h_plasma=rZero
     dN_e_plasma=rZero
     dN_h_plasma=rZero
     !
     ! Define plasma occupations and Energies
     !========================================
     do i_J_bg=EH_gas%grid(i_J,i_q_mem,1),EH_gas%grid(i_J,i_q_mem,2)
       !
       i_qp=EH_gas%tab(i_J_bg,i_q_mem,1)
       i_p_s=EH_gas%tab(i_J_bg,i_q_mem,2)
       !
       i_m  = QP_table(i_qp,1)
       i_mp = QP_table(i_qp,2)
       i_p  = QP_table(i_qp,3)
       !
       i_p_bz=k%s_table(i_p,i_p_s)
       !
       i_pmq_bz = qindx_C(i_p_bz,iqbz,1)
       i_pmq    = k%sstar(i_pmq_bz,1)
       !
       if (RT_occupations_locked(i_m ,RT_ibz_coo(i_p,1)))   cycle
       if (RT_occupations_locked(i_mp,RT_ibz_coo(i_pmq,1))) cycle
       !
       DELTA_E=E%E(i_m,i_p,1)-E%E(i_mp,i_pmq,1)
       DELTA_W=abs(E%W(i_m,i_p,1))+abs(E%W(i_mp,i_pmq,1))
       !
       f_p    = RT_occupations(i_m ,RT_ibz_coo(i_p,1))
       f0_p   = RT_occupations_reference(i_m ,RT_ibz_coo(i_p,1))
       df_p   = RT_delta_occupations(i_m ,RT_ibz_coo(i_p,1))
       one_minus_f_p    = RT_one_minus_occupations(i_m ,RT_ibz_coo(i_p,1))
       !
       f_pmq  = RT_occupations(i_mp ,RT_ibz_coo(i_pmq,1))
       f0_pmq = RT_occupations_reference(i_mp ,RT_ibz_coo(i_pmq,1))
       df_pmq = RT_delta_occupations(i_mp ,RT_ibz_coo(i_pmq,1))
       one_minus_f_pmq  = RT_one_minus_occupations(i_mp,RT_ibz_coo(i_pmq,1))
       !
       E_plasma=E_plasma+cmplx(delta_E,delta_W)
       !
       N_e_plasma =N_e_plasma+one_minus_f_p*f_pmq/spin_occ
       N_h_plasma =N_h_plasma+f_p*one_minus_f_pmq/spin_occ
       N0_e_plasma=N0_e_plasma+(2._SP-f0_p)*f0_pmq/spin_occ
       N0_h_plasma=N0_h_plasma+f0_p*(2._SP-f0_pmq)/spin_occ
       dN_e_plasma=dN_e_plasma-df_p*f0_pmq/spin_occ+(2._SP-f0_p)*df_pmq/spin_occ-df_p*df_pmq/spin_occ
       dN_h_plasma=dN_h_plasma+df_p*(2._SP-f0_pmq)/spin_occ-f0_p*df_pmq/spin_occ-df_p*df_pmq/spin_occ
       !
     enddo
     !
     dN_e_plasma=dN_e_plasma/real(EH_gas%grid(i_J,i_q_mem,2)-EH_gas%grid(i_J,i_q_mem,1)+1,SP)
     dN_h_plasma=dN_h_plasma/real(EH_gas%grid(i_J,i_q_mem,2)-EH_gas%grid(i_J,i_q_mem,1)+1,SP)
     N0_e_plasma=N0_e_plasma/real(EH_gas%grid(i_J,i_q_mem,2)-EH_gas%grid(i_J,i_q_mem,1)+1,SP)
     N0_h_plasma=N0_h_plasma/real(EH_gas%grid(i_J,i_q_mem,2)-EH_gas%grid(i_J,i_q_mem,1)+1,SP)
     N_e_plasma =N_e_plasma/real(EH_gas%grid(i_J,i_q_mem,2)-EH_gas%grid(i_J,i_q_mem,1)+1,SP)
     N_h_plasma =N_h_plasma/real(EH_gas%grid(i_J,i_q_mem,2)-EH_gas%grid(i_J,i_q_mem,1)+1,SP)
     E_plasma   =E_plasma/real(EH_gas%grid(i_J,i_q_mem,2)-EH_gas%grid(i_J,i_q_mem,1)+1,SP)
     !
     ! Unlike phonons, plasmons have a finite lifetime
     !
     plasma_freq=real(E_plasma)
     plasma_width=aimag(E_plasma)
     !
     do i_qp1=1,QP_n_states
       !
       ! Select only the m.e. of QP_table relative to kpt ik
       !======================================================
       if (QP_table(i_qp1,3)/=ik) cycle
       !
       if (.not.PAR_IND_QP%element_1D(i_qp1)) cycle
       i_qp1_mem=PAR_QP_index(i_qp1)
       !
       i_n =QP_table(i_qp1,1)
       i_np=QP_table(i_qp1,2)
       !
       DELTA_W=abs(E%W(i_np,ik_m_q,1))+abs(E%W(i_n,ik,1))
       !
       pre_factor=(4._SP*pi)**2._SP*GW_NEQ_collisions(i_qp1_mem)%v3r(i_J_mem,1,i_q_mem)
       !
#if defined _RT_EE_RIM
       !
       do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
         !
         if (RT_occupations_locked(i_n ,ik_rim)) cycle
         !
         do ik_m_q_rim=RT_ibz_coo(ik_m_q,1),RT_ibz_coo(ik_m_q,2)
           !
           if (RT_occupations_locked(i_np ,ik_m_q_rim)) cycle
           !
           i1=ik_rim-RT_ibz_coo(ik,1)+RIM_ibz_coo(ik,1)
           i2=ik_m_q_rim-RT_ibz_coo(ik_m_q,1)+RIM_ibz_coo(ik_m_q,1)
           DELTA_E=E%E(i_np,i2,1)-E%E(i_n,i1,1)
           !
           P_pl_abs  =RT_G_retarded_convolution(DELTA_E, plasma_freq,DELTA_W,plasma_width,E%E_ind_gap(1),'EE')/&
&                     real(RT_Nk_ibz(ik_m_q))
           !
#else
           !
           if (RT_occupations_locked(i_n ,ik)) cycle
           !
           ik_m_q_rim=RT_ibz_coo(ik_m_q,1)
           !
           if (RT_occupations_locked(i_np,ik_m_q_rim)) cycle
           !
           DELTA_E=E%E(i_np,ik_m_q,1)-E%E(i_n,ik,1)
           !
           P_pl_abs  =RT_G_retarded_convolution(DELTA_E, plasma_freq,DELTA_W,plasma_width,E%E_ind_gap(1),'EE')
           !
#endif

           !
           if(abs(P_pl_abs)<1.E-7) cycle
           !
           one_minus_f_kmq  = RT_one_minus_occupations(i_np,ik_m_q_rim)/spin_occ
           f_kmq            = RT_occupations(i_np,ik_m_q_rim)/spin_occ
           f0_kmq           = RT_occupations_reference(i_np,ik_m_q_rim)/spin_occ
           df_kmq           = RT_delta_occupations(i_np,ik_m_q_rim)/spin_occ
           !
           E_E_Hole_lifetime(i_n,ik_rim)=&
&            E_E_Hole_lifetime(i_n,ik_rim)+pre_factor*P_pl_abs*&
&            (N_h_plasma*f_kmq        +dN_h_plasma*f0_kmq          +N0_h_plasma*df_kmq)
           !
           E_E_Electron_lifetime(i_n,ik_rim)=&
&            E_E_Electron_lifetime(i_n,ik_rim)+pre_factor*P_pl_abs*&
&            (N_e_plasma*one_minus_f_kmq+dN_e_plasma*(1._SP-f0_kmq)-N0_e_plasma*df_kmq)
           !
#if defined _RT_EE_RIM
           !
         enddo
       enddo
       !
#endif
       !
     enddo
     !
   enddo
   !
 enddo q_loop
 !
end subroutine RT_Coulomb_relaxation
