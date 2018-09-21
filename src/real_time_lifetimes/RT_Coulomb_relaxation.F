!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine RT_Coulomb_relaxation(ik,E,k,q)
 !  
 use pars,            ONLY:SP,rZero,cZero,pi
 use electrons,       ONLY:levels,spin_occ
 use R_lattice,       ONLY:qindx_S,qindx_C,bz_samp
 use real_time,       ONLY:E_E_Electron_lifetime,E_E_Hole_lifetime,RT_occupations_locked,&
&                          RT_life_delta_occupations,RT_E_occupations_reference,RT_H_occupations_reference,&
&                          DbGd_ibz_coo,l_RT_uses_E_DbGd,l_initial_equilibrium,DbGd_EE_scatt_table,&
&                          DbGd_EE_Nk_table,RT_eh_en,RT_ibz_coo
 use plasma,          ONLY:EH_gas
 use collision_ext,   ONLY:GW_NEQ_COLL_element,GW_NEQ_collisions
 use parallel_m,      ONLY:PAR_Q_index,PAR_IND_Q,PAR_IND_Plasma,PAR_PLASMA_index
 !
 implicit none
 !
 type(levels),     intent(in) :: E
 type(bz_samp),    intent(in) :: k,q
 integer,          intent(in) :: ik
 !
 ! Work Space ...
 !
 integer               :: i_coll,i_coll_p,i_p,i_p_bz,ik_m_q,i_pmq,i_n,i_m,i_mp,i_np,ik_rim,&
&                         iqbz,i_p_s,i_q,i_q_s,i_coll_p_mem,i_q_mem,i_pmq_bz,i1,i2,&
&                         i_J,i_J_bg,i_J_mem,ik_m_q_rim,N_poles_grouped
 real(SP)              :: pre_factor,P_pl_abs,DELTA_E,DELTA_W,TMP_Hole_lifetime,TMP_Electron_lifetime
 !
 ! Occupations
 !
 real(SP)              :: df_p,f0_p,df_pmq,f0_pmq,df_kmq,f0_kmq,Hole_f0_kmq,Hole_f0_pmq,Hole_f0_p,&
&                         Electron_occupation,Hole_occupation
 !
 ! Plasma
 !
 real(SP)              :: dN_h_plasma,dN_e_plasma,N0_h_plasma,N0_e_plasma,plasma_freq,plasma_width
 complex(SP)           :: E_plasma
 !
 real(SP), external    :: RT_G_retarded_convolution
 !
 ik_rim    =RT_ibz_coo(ik,1)
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
     i_J_mem=PAR_PLASMA_index(i_J)
     !
     E_plasma   =cZero
     N0_e_plasma=rZero
     N0_h_plasma=rZero
     dN_e_plasma=rZero
     dN_h_plasma=rZero
     !
     N_poles_grouped=0
     !
     ! Define plasma occupations and Energies
     !========================================
     do i_J_bg=EH_gas%grid(i_J,i_q_mem,1),EH_gas%grid(i_J,i_q_mem,2)
       !
       i_coll=EH_gas%tab(i_J_bg,i_q_mem,1)
       i_p_s =EH_gas%tab(i_J_bg,i_q_mem,2)
       !
       i_m  = GW_NEQ_collisions%state(i_coll,1)
       i_mp = GW_NEQ_collisions%state(i_coll,2)
       i_p  = GW_NEQ_collisions%state(i_coll,3)
       !
       i_p_bz=k%k_table(i_p,i_p_s)
       !
       i_pmq_bz = qindx_C(i_p_bz,iqbz,1)
       i_pmq    = k%sstar(i_pmq_bz,1)
       !
       if (RT_occupations_locked(i_m ,RT_ibz_coo(i_p,1)))   cycle
       if (RT_occupations_locked(i_mp,RT_ibz_coo(i_pmq,1))) cycle
       !
       if(E%E(i_mp,i_pmq,1)<RT_eh_en(1) .or. E%E(i_mp,i_pmq,1)>RT_eh_en(2)) cycle
       if(E%E(i_m ,i_p  ,1)<RT_eh_en(1) .or. E%E(i_m ,i_p  ,1)>RT_eh_en(2)) cycle
       !
       N_poles_grouped=N_poles_grouped+1
       !
       DELTA_E=    E%E(i_m,i_p,1) -    E%E(i_mp,i_pmq,1)
       DELTA_W=abs(E%W(i_m,i_p,1))+abs(E%W(i_mp,i_pmq,1))
       !
       f0_p   = RT_E_occupations_reference(i_m ,RT_ibz_coo(i_p,1))
       df_p   = RT_life_delta_occupations(i_m ,RT_ibz_coo(i_p,1))
       Hole_f0_p   = RT_H_occupations_reference(i_m ,RT_ibz_coo(i_p,1))
       !
       f0_pmq = RT_E_occupations_reference(i_mp ,RT_ibz_coo(i_pmq,1))
       df_pmq = RT_life_delta_occupations(i_mp ,RT_ibz_coo(i_pmq,1))
       Hole_f0_pmq  = RT_H_occupations_reference(i_mp ,RT_ibz_coo(i_pmq,1))
       !
       E_plasma=E_plasma+cmplx(delta_E,delta_W)
       !
       N0_e_plasma=N0_e_plasma+Hole_f0_p*f0_pmq/spin_occ
       N0_h_plasma=N0_h_plasma+f0_p*Hole_f0_pmq/spin_occ
       dN_e_plasma=dN_e_plasma-df_p*f0_pmq/spin_occ+Hole_f0_p*df_pmq/spin_occ-df_p*df_pmq/spin_occ
       dN_h_plasma=dN_h_plasma+df_p*Hole_f0_pmq/spin_occ-f0_p*df_pmq/spin_occ-df_p*df_pmq/spin_occ
       !
     enddo
     !
     if (N_poles_grouped==0) cycle
     !
     !N_poles_grouped=EH_gas%grid(i_J,i_q_mem,2)-EH_gas%grid(i_J,i_q_mem,1)+1
     !
     E_plasma   =E_plasma/real(N_poles_grouped,SP)
     !
     N0_e_plasma=N0_e_plasma/real(N_poles_grouped,SP)
     N0_h_plasma=N0_h_plasma/real(N_poles_grouped,SP)
     dN_e_plasma=dN_e_plasma/real(N_poles_grouped,SP)
     dN_h_plasma=dN_h_plasma/real(N_poles_grouped,SP)
     !
     ! Unlike phonons, plasmons have a finite lifetime
     !
     plasma_freq=real(E_plasma)
     plasma_width=aimag(E_plasma)
     !
     do i_coll_p=1,GW_NEQ_collisions%N
       !
       ! Select only the m.e. of GW_NEQ_collisions%state relative to kpt ik
       !====================================================================
       if (GW_NEQ_collisions%state(i_coll_p,3)/=ik) cycle
       !
       if (.not.GW_NEQ_collisions%PAR_IND%element_1D(i_coll_p)) cycle
       i_coll_p_mem=GW_NEQ_collisions%PAR_map(i_coll_p)
       !
       i_n =GW_NEQ_collisions%state(i_coll_p,1)
       i_np=GW_NEQ_collisions%state(i_coll_p,2)
       !
       if (.not. l_RT_uses_E_DbGd) then
         !
         if(E%E(i_np,ik_m_q,1)<RT_eh_en(1) .or. E%E(i_np,ik_m_q,1)>RT_eh_en(2)) cycle
         if(E%E(i_n ,ik    ,1)<RT_eh_en(1) .or. E%E(i_n ,ik    ,1)>RT_eh_en(2)) cycle
         !
         DELTA_E=    E%E(i_np,ik_m_q,1) -    E%E(i_n,ik,1)
         DELTA_W=abs(E%W(i_np,ik_m_q,1))+abs(E%W(i_n,ik,1))
         !
       endif
       !
       pre_factor=(4._SP*pi)**2*GW_NEQ_COLL_element(i_coll_p_mem)%v3_r(i_J_mem,1,i_q_mem)
       !
       do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
         !
         if (RT_occupations_locked(i_n ,ik_rim)) cycle
         !
         TMP_Hole_lifetime=rZero
         TMP_Electron_lifetime=rZero
         !
         do ik_m_q_rim=RT_ibz_coo(ik_m_q,1),RT_ibz_coo(ik_m_q,2)
           !
           if( .not.DbGd_EE_scatt_table(ik_m_q_rim,ik_rim) ) cycle
           !
           if (RT_occupations_locked(i_np ,ik_m_q_rim)) cycle
           !
           if (l_RT_uses_E_DbGd) then
             !
             i1=ik_rim-RT_ibz_coo(ik,1)+DbGd_ibz_coo(ik,1)
             i2=ik_m_q_rim-RT_ibz_coo(ik_m_q,1)+DbGd_ibz_coo(ik_m_q,1)
             !
             if(E%E_DbGd(i_np,i2,1)<RT_eh_en(1) .or. E%E_DbGd(i_np,i2,1)>RT_eh_en(2)) cycle
             if(E%E_DbGd(i_n ,i1,1)<RT_eh_en(1) .or. E%E_DbGd(i_n ,i1,1)>RT_eh_en(2)) cycle
             !
             DELTA_E=    E%E_DbGd(i_np,i2,1) -    E%E_DbGd(i_n,i1,1)
             DELTA_W=abs(E%W_DbGd(i_np,i2,1))+abs(E%W_DbGd(i_n,i1,1))
             !
           endif
           !
           P_pl_abs  =RT_G_retarded_convolution(DELTA_E, plasma_freq,DELTA_W,plasma_width,'EE')/&
&                     real(DbGd_EE_Nk_table(ik_rim,ik_m_q))
           !
           if(abs(P_pl_abs)<1.E-7) cycle
           !
           f0_kmq           = RT_E_occupations_reference(i_np,ik_m_q_rim)/spin_occ
           Hole_f0_kmq      = RT_H_occupations_reference(i_np,ik_m_q_rim)/spin_occ
           df_kmq           = RT_life_delta_occupations(i_np,ik_m_q_rim)/spin_occ
           !
           if(l_initial_equilibrium) then
             Hole_occupation     = N0_h_plasma*f0_kmq     
             Electron_occupation = N0_e_plasma*Hole_f0_kmq
           else
             Hole_occupation     = dN_h_plasma*f0_kmq      + N0_h_plasma*df_kmq + dN_h_plasma*df_kmq
             Electron_occupation = dN_e_plasma*Hole_f0_kmq - N0_e_plasma*df_kmq - dN_e_plasma*df_kmq
           endif
           !
           TMP_Hole_lifetime=TMP_Hole_lifetime+P_pl_abs*Hole_occupation
           TMP_Electron_lifetime=TMP_Electron_lifetime+P_pl_abs*Electron_occupation
           !
         enddo
         !
         E_E_Hole_lifetime(i_n,ik_rim)=E_E_Hole_lifetime(i_n,ik_rim)+TMP_Hole_lifetime*pre_factor
         E_E_Electron_lifetime(i_n,ik_rim)=E_E_Electron_lifetime(i_n,ik_rim)+TMP_Electron_lifetime*pre_factor
         !
       enddo
       !
     enddo
     !
   enddo
   !
 enddo q_loop
 !
end subroutine RT_Coulomb_relaxation
