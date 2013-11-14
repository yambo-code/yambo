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
subroutine RT_Coulomb_relaxation(i_k,E,k,q,G_lesser)
 !  
 use pars,            ONLY:SP,cI,rZero,cZero,pi,tol2
 use electrons,       ONLY:levels,spin_occ
 use R_lattice,       ONLY:qindx_S,bz_samp
 use SC,              ONLY:RT_ibz_coo,SC_bands,RT_nk,RT_Nk_ibz
 use real_time,       ONLY:E_E_Electron_lifetime,E_E_Hole_lifetime,&
&                          l_RT_uses_E_RIM,RIM_ibz_coo
 use plasma,          ONLY:N_plasma_poles,Plasma_tab,Plasma_tab_m1,Plasma_grid,&
&                          max_N_plasma_poles
 use QP_m,            ONLY:QP_n_states,QP_table
 use collision,       ONLY:GW_NEQ_collisions
 use parallel_m,      ONLY:PAR_QP_index,PAR_IND_QP,PAR_Q_index,&
&                          PAR_IND_Q,PAR_IND_Plasma,PAR_Plasma_index
 !
 implicit none
 !
 type(levels),  intent(in) :: E
 type(bz_samp), intent(in) :: k,q
 integer,       intent(in) :: i_k
 complex(SP),   intent(in) :: G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 !
 ! Work Space ...
 !
 integer               :: i_qp,i_qp1,i_qp2,i_p,i_kmq,i_pmq,i_n,i_m,i_mp,i_np,i_p_star,i_k_rim,i_kmq_rim,&
&                         iqbz,i_p_s,i_q,i_q_s,i_qp1_mem,i_qp2_mem,i_q_mem,i_pmq_bz,i_pmq_G0,i1,i2,&
&                         i_J,i_J_bg,i_J_mem,i_p_rim,i_pmq_rim
 real(SP)              :: f_p,f_kmq,f_pmq,pre_factor,Gr_integral,P_pl_emit,P_pl_abs,P_plasma,&
&                         f_E_factor,f_H_factor,plasma_freq,plasma_width,DELTA_E,DELTA_W,DELTA_Ep,DELTA_Wp
 !
 real(SP)              :: N_plasma(2)
 complex(SP)           :: E_plasma
 !
 real(SP)              :: E_E_Hole_st_em_lifetime(SC_bands(1):SC_bands(2),RT_nk)
 real(SP)              :: E_E_Hole_abs_lifetime(SC_bands(1):SC_bands(2),RT_nk)
 real(SP)              :: E_E_Electron_st_em_lifetime(SC_bands(1):SC_bands(2),RT_nk)
 real(SP)              :: E_E_Electron_abs_lifetime(SC_bands(1):SC_bands(2),RT_nk)
 !
 real(SP), external    :: RT_G_retarded_convolution
 !
 ! Zeroing
 !=========
 i1=RT_ibz_coo(i_k,1)
 i2=RT_ibz_coo(i_k,2)
 E_E_Hole_lifetime(:,i1:i2)    =rZero
 E_E_Electron_lifetime(:,i1:i2)=rZero
 !
 do iqbz=1,q%nbz
   !
   if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
   i_q_mem=PAR_Q_index(iqbz)
   !
   i_q   = q%sstar(iqbz,1)
   i_q_s = q%sstar(iqbz,2)
   i_kmq = k%sstar(qindx_S(i_k,iqbz,1),1)
   !
   do i_J=1,N_plasma_poles(iqbz)
     !
     if (.not.PAR_IND_Plasma%element_1D(i_J)) cycle
     i_J_mem=PAR_Plasma_index(i_J)
     !
     E_plasma=cZero
     N_plasma=rZero
     !
     ! Define plasma occupations and Energies
     !========================================
     do i_J_bg=Plasma_grid(i_J,i_q_mem,1),Plasma_grid(i_J,i_q_mem,2)
       !
       i_qp=Plasma_tab(i_J_bg,i_q_mem,1)
       i_p_s=Plasma_tab(i_J_bg,i_q_mem,2)       
       !
       i_m  = QP_table(i_qp,1)
       i_mp = QP_table(i_qp,2)
       i_p  = QP_table(i_qp,3)
       !
       call RT_EE_scattering_kinematics(k,q,i_p,i_p_s,i_q,i_q_s,i_pmq_bz,i_pmq_G0)
       !
       i_pmq =k%sstar(i_pmq_bz,1)
       !
       !! OPTION 1 <
       !!Average RIM occupations and energies<
       !DELTA_E=0._SP
       !DELTA_W=abs(E%W(i_m,i_p,1))+abs(E%W(i_mp,i_pmq,1))
       !do i_p_rim=RT_ibz_coo(i_p,1),RT_ibz_coo(i_p,2)
       !  do i_pmq_rim=RT_ibz_coo(i_pmq,1),RT_ibz_coo(i_pmq,2)
       !    i1=i_p_rim-RT_ibz_coo(i_p,1)+RIM_ibz_coo(i_p,1)
       !    i2=i_pmq_rim-RT_ibz_coo(i_pmq,1)+RIM_ibz_coo(i_pmq,1)
       !    DELTA_E=DELTA_E+E%E_RIM(i_m,i1,1)-E%E_RIM(i_mp,i2,1)
       !  enddo
       !enddo
       !DELTA_E=DELTA_E/real(RT_Nk_ibz(i_p))/real(RT_Nk_ibz(i_pmq))
       !!
       !f_p=0._SP
       !f_pmq=0._SP
       !do i_p_rim=RT_ibz_coo(i_p,1),RT_ibz_coo(i_p,2)
       !  f_p   =f_p   -cI*G_lesser(i_m,i_m,i_p_rim)
       !enddo
       !do i_pmq_rim=RT_ibz_coo(i_pmq,1),RT_ibz_coo(i_pmq,2)
       !  f_pmq =f_pmq -cI*G_lesser(i_mp,i_mp,i_pmq_rim)
       !enddo
       !f_p=  f_p  /real(RT_Nk_ibz(i_p))
       !f_pmq=f_pmq/real(RT_Nk_ibz(i_pmq))
       !! OPTION 1 <
       !
       ! OPTION 2 <
       !MAIN GRID occupations and energies
       DELTA_E=E%E(i_m,i_p,1)-E%E(i_mp,i_pmq,1)
       DELTA_W=abs(E%W(i_m,i_p,1))+abs(E%W(i_mp,i_pmq,1))
       !
       f_p   =-cI*G_lesser(i_m ,i_m ,RT_ibz_coo(i_p  ,1))
       f_pmq =-cI*G_lesser(i_mp,i_mp,RT_ibz_coo(i_pmq,1))
       ! OPTION 2 <
       !
       ! Here I could change from between Delta E>0 and Delta E<0
       ! To just E_center>0 and is=-1 / +1
       ! I'm forced to do like that with the occupations
       !
       E_plasma=E_plasma+cmplx(delta_E,delta_W)
       N_plasma(1)=N_plasma(1)+(2._SP-f_p)*f_pmq/spin_occ
       N_plasma(2)=N_plasma(2)+(2._SP-f_pmq)*f_p/spin_occ
       !
     enddo
     N_plasma=N_plasma/real(Plasma_grid(i_J,i_q_mem,2)-Plasma_grid(i_J,i_q_mem,1)+1,SP)
     E_plasma=E_plasma/real(Plasma_grid(i_J,i_q_mem,2)-Plasma_grid(i_J,i_q_mem,1)+1,SP)
     !
     ! Unlike phonons, plasmons have a finite lifetime
     plasma_freq=real(E_plasma)
     plasma_width=aimag(E_plasma)
     !
     do i_qp1=1,QP_n_states
       !
       ! Select only the m.e. of QP_table relative to kpt i_k
       !======================================================
       if (QP_table(i_qp1,3)/=i_k) cycle
       !
       if (.not.PAR_IND_QP%element_1D(i_qp1)) cycle
       i_qp1_mem=PAR_QP_index(i_qp1)
       !
       i_n =QP_table(i_qp1,1)
       i_np=QP_table(i_qp1,2)
       !
       f_kmq=-cI*G_lesser(i_np,i_np,RT_ibz_coo(i_kmq,1))/spin_occ
       !
       DELTA_E=E%E(i_np,i_kmq,1)-E%E(i_n,i_k,1)
       DELTA_W=abs(E%W(i_np,i_kmq,1))+abs(E%W(i_n,i_k,1))
       !
       pre_factor=(4.*pi)**2._SP*&
&         abs(GW_NEQ_collisions(i_qp1_mem)%v3r(i_J_mem,1,i_q_mem))/real(RT_Nk_ibz(i_kmq))
       !
       do i_k_rim=RT_ibz_coo(i_k,1),RT_ibz_coo(i_k,2)
         do i_kmq_rim=RT_ibz_coo(i_kmq,1),RT_ibz_coo(i_kmq,2)
           !
           if (l_RT_uses_E_RIM) then
             f_kmq=-cI*G_lesser(i_np,i_np,i_kmq_rim)/spin_occ
             i1=i_k_rim-RT_ibz_coo(i_k,1)+RIM_ibz_coo(i_k,1)
             i2=i_kmq_rim-RT_ibz_coo(i_kmq,1)+RIM_ibz_coo(i_kmq,1)
             DELTA_E=E%E_RIM(i_n,i1,1)-E%E_RIM(i_np,i2,1)
           endif
           !
           ! The plasma gas is not a quantistic bosonic gas, but an approximation to the eh-gas.
           ! Is the ratio beween Gamma_electrons and Gamma_holes which drives the eh-gas(nn') to
           ! the temperature of the plasma gas (J), which is indeed defined by the temperature
           ! of the eh-gas(mm'). This should ensure energy conservation.
           ! Numerical checks seems to confirm that energy is conserved, but only without RIM
           !
           P_plasma=RT_G_retarded_convolution(DELTA_E,plasma_freq,DELTA_W,plasma_width,E%E_ind_gap(1),'EE')
           !
           if(abs(P_plasma)<tol2) cycle
           !
           E_E_Hole_lifetime(i_n,i_k_rim)=E_E_Hole_lifetime(i_n,i_k_rim)+&
&                                          pre_factor*f_kmq*P_plasma*N_plasma(2)
           E_E_Electron_lifetime(i_n,i_k_rim)=E_E_Electron_lifetime(i_n,i_k_rim)+&
&                                          pre_factor*(1.-f_kmq)*P_plasma*N_plasma(1)
           !
         enddo
       enddo
       !
     enddo
     !
   enddo
   !
 enddo
 !
end subroutine RT_Coulomb_relaxation
