!
!        Copyright (C) 2000-2023 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): FP
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
subroutine PHEL_Lifetimes(E,k,q,k_FG,E_FG)
 !
 ! This routine computes the phonon lifetimes out of equilibrium (i.e., scattering rates).
 ! 
 ! It uses a table of allowed phonon scattering transitions pre-determined 
 ! in PH_Grid_Check_Transitions.
 !
 ! The approximation route from the phononic KBE to the kernel/collision integral is:
 !   -- Time Scales Separation + QuasiParticle Approximation (TSQP)
 !
 use pars,           ONLY:SP,cI,pi,rZERO
 use units,          ONLY:HA2EV
 use electrons,      ONLY:levels,n_full_bands,spin_occ,E_fine_grid
 use R_lattice,      ONLY:bz_samp,bz_fine_grid
 use RT_occupations, ONLY:RT_ph_occ,RT_el_occ,RT_ho_occ,RT_life_occ
 use RT_lifetimes,   ONLY:l_initial_equilibrium,RT_PE_life,RT_PE_extra
 use real_time,      ONLY:l_RT_uses_E_FineGd,RT_eh_en
 use ELPH,           ONLY:PH_freqs_sq,MAX_gkkp,PH_acoustic_branch,GKKP_pe_sq,&
&                         l_GKKP_expanded_DB_exists,l_GKKP_DB_exists
 use parallel_m,     ONLY:PARs_PH_Q_ibz
 use QP_m,           ONLY:QP_G_damp,l_extended_output
 use LIVE_t,         ONLY:live_timing
 use PHEL,           ONLY:l_PH_uses_E_FineGd,phel_info
 !
 implicit none
 !
 type(levels),      intent(in) :: E
 type(bz_samp),     intent(in) :: k,q
 type(bz_fine_grid),intent(in) :: k_FG
 type(E_fine_grid) ,intent(in) :: E_FG
 !
 ! Work Space ...
 !
 real(SP)              :: ph_freq
 real(SP),   parameter :: cutoff= 5.E-3
 real(SP),   external  :: RT_G_retarded_convolution
 complex(SP),external  :: Lorentzian_FT
 !
 ! ... transition index ...
 integer               :: it
 ! ... band/branch index ...
 integer               :: il,i_n,i_np
 ! ... BZ index ...
 integer               :: iq,ikbz,ik,ik_m_q,iq_mem,ik_rim,ip_rim,iq_life
 ! ... lifetime related quantities ...
 real(SP) :: D_res,D_ares,DELTA_E,DELTA_W,pre_factor,&
&            TMP_Emit_lifetime,TMP_Abs_lifetime,N_q,f_mkq,f_nk,&
&            Hole_f_mkq,Hole_f_nk
 real (SP) :: TMP_linewidth
 !
 call live_timing('Phonon Lifetimes',phel_info%N)
 !
 ! Main loop over allowed transitions
 !====================================
 do it=1,phel_info%N
   !
   ! Indices of allowed transition
   il     = phel_info%table(it,1) ! phonon branch
   iq     = phel_info%table(it,2) ! phonon momentum (ibz)
   ikbz   = phel_info%table(it,3) ! electron momentum (bz)
   ik_m_q = phel_info%table(it,4) ! scattered elec. momentum (_PH: bz / _RT: ibz) 
   ik_rim = phel_info%table(it,5) ! fine grid elec. momentum
   ip_rim = phel_info%table(it,6) ! fine grid scattered elec. momentum
   i_n    = phel_info%table(it,7) ! electronic band
   i_np   = phel_info%table(it,8) ! electronic band
   !
   ! If running RT: use k in the ibz
   ik = k%sstar(ikbz,1)
   !
   ! Index for q-parallelised quantities
   iq_mem =PARs_PH_Q_ibz%table(iq)
   iq_life=iq_mem
   !
   if (l_GKKP_expanded_DB_exists) ph_freq=abs(sqrt(PH_freqs_sq(q%k_table(iq,1),il)))
   if (l_GKKP_DB_exists         ) ph_freq=abs(sqrt(PH_freqs_sq(iq,il)))
   !
   if (iq==1 .and. PH_acoustic_branch(il)) cycle
   !
   if (     l_initial_equilibrium) N_q = RT_ph_occ%N_ref(il,iq)
   if (.not.l_initial_equilibrium) N_q = RT_ph_occ%N(il,iq)
   !
   ! Electron-phonon matrix elements
   !====================================
   ! In Yambo we have:         GKKP_YAMBO[i_n,i_np] = <i_n ik|g(q_YAMBO l r)|i_np ik-q_YAMBO>     
   ! In my notes (FP) we have: GKKP_NOTES[i_np,i_n] = <i_n ik|g(q_YAMBO l r)|i_np ik-q_YAMBO>
   !    
   ! Therefore:                GKKP_NOTES[i_np,i_n] = GKKP_YAMBO[i_n,i_np],
   ! which is what must go into the pre_factor of the lifetimes
   !
   if (sqrt(GKKP_pe_sq%dVr(il,i_n,i_np,ikbz,iq_mem))<MAX_gkkp*cutoff) cycle
   !
   ! Prefactor
   !====================================
   ! The weight here is
   !
   !                    (2*spin_occ*pi)/(k%nbz*2* ph_freq)
   !
   ! 2 from KBE collision integral, spin_occ=2 from ph-el self-energy (polarization bubble), pi from delta functions,
   ! 1/Nk from k-sum, (2*ph_freq)^-1 to correctly normalize Gkkp_sq.
   !
   ! [NOTE]  Variable pre_factor below contains pi since it *shouldn't* come later from the hyperbolic secant FT.
   ! [NOTE2] Variable pre_factor below contains 1/(Nk_bz*N_FG_k*N_FG_k_m_q) accounting for the double grid at each bz point.
   !
   pre_factor=spin_occ*GKKP_pe_sq%dVr(il,i_n,i_np,ikbz,iq_mem)*pi/(k%nbz*k_FG%N_mapped(ik)*k_FG%N_mapped(ik_m_q)*ph_freq)
   !
   ! At equilibrium, only metals or band gaps < E_debye can give non-zero ph lifetimes
   !
   if (.not.l_initial_equilibrium) then
     !
     ! Extra condition: the EP scattering cannot cross the gap
     !
     if ((i_n<=n_full_bands.and.i_np>  n_full_bands).and.(E%E_ind_gap(1)>0.1/HA2EV)) cycle
     if ((i_n> n_full_bands.and.i_np<= n_full_bands).and.(E%E_ind_gap(1)>0.1/HA2EV)) cycle
     !
   endif
   !
   ! Double grid (no/yes)
   !====================================
   if (.not. l_RT_uses_E_FineGd .and. .not. l_PH_uses_E_FineGd) then
     !
     if(E_FG%E(i_np,ik_m_q,1)<RT_eh_en(1) .or. E_FG%E(i_np,ik_m_q,1)>RT_eh_en(2)) cycle
     if(E_FG%E(i_n ,ik    ,1)<RT_eh_en(1) .or. E_FG%E(i_n ,ik    ,1)>RT_eh_en(2)) cycle
     !
     DELTA_E=E_FG%E(i_n,ik,1)-E_FG%E(i_np,ik_m_q,1)
     DELTA_W=E_FG%W(i_n,ik,1)+E_FG%W(i_np,ik_m_q,1)
     !
   else
     ! 
     if(E_FG%E(i_np,ip_rim,1)<RT_eh_en(1) .or. E_FG%E(i_np,ip_rim,1)>RT_eh_en(2)) cycle
     if(E_FG%E(i_n ,ik_rim,1)<RT_eh_en(1) .or. E_FG%E(i_n ,ik_rim,1)>RT_eh_en(2)) cycle
     !
     DELTA_E=E_FG%E(i_n,ik_rim,1)-E_FG%E(i_np,ip_rim,1)
     DELTA_W=E_FG%W(i_n,ik_rim,1)+E_FG%W(i_np,ip_rim,1)
     !
   endif
   !
   ! Lifetimes
   !============
   ! 
   ! Lorentzian broadening / spectral convolution 

   if (l_initial_equilibrium) then
     D_ares=1./pi*aimag( Lorentzian_FT(ph_freq-cI*QP_G_damp,DELTA_E+cI*delta_W,"Ta") )
     D_res =1./pi*aimag( Lorentzian_FT(ph_freq-cI*QP_G_damp,DELTA_E+cI*delta_W,"r" ) )
   else
     D_ares=RT_G_retarded_convolution(DELTA_E,+ph_freq,DELTA_W,0._SP,'EP')
     D_res =RT_G_retarded_convolution(DELTA_E,-ph_freq,DELTA_W,0._SP,'EP')     
   endif
   !
   ! Electron and hole occupations
   !
   if(l_initial_equilibrium) then
     f_mkq      =  RT_el_occ%N_ref(i_np,ip_rim)/spin_occ
     Hole_f_mkq =  RT_ho_occ%N_ref(i_np,ip_rim)/spin_occ
     f_nk       =  RT_el_occ%N_ref(i_n,ik_rim)/spin_occ
     Hole_f_nk  =  RT_ho_occ%N_ref(i_n,ik_rim)/spin_occ
   else
     f_mkq      =  RT_life_occ%dN(i_np,ip_rim)/spin_occ
     Hole_f_mkq = -RT_life_occ%dN(i_np,ip_rim)/spin_occ
     f_nk       =  RT_life_occ%dN(i_n,ik_rim)/spin_occ
     Hole_f_nk  = -RT_life_occ%dN(i_n,ik_rim)/spin_occ
   endif 
   !
   ! Accumulation of the lifetimes
   !
   TMP_Emit_lifetime=rZERO
   TMP_Abs_lifetime=rZERO
   TMP_linewidth=rZERO
   !
   TMP_Emit_lifetime = f_mkq*Hole_f_nk*D_ares+Hole_f_mkq*f_nk*D_res
   TMP_Abs_lifetime  = f_mkq*Hole_f_nk*D_res +Hole_f_mkq*f_nk*D_ares
   ! This is Eq. (4) of [PRB 71, 064501 (2005)], i.e., retarded ph-el self-energy g^2*GG
   if (l_initial_equilibrium) TMP_linewidth      = f_mkq*Hole_f_nk*D_res-Hole_f_mkq*f_nk*D_res
   !
   ! Phonon emission (stimulated and spontaneous)
   RT_PE_life%emit(il,iq_life)=RT_PE_life%emit(il,iq_life)+TMP_Emit_lifetime*pre_factor
   ! Phonon absorption
   RT_PE_life%abs(il,iq_life) =RT_PE_life%abs(il,iq_life)+TMP_Abs_lifetime*pre_factor
   ! Linewidth at equilibrium
   if (l_initial_equilibrium) then
     RT_PE_life%eq(il,iq_life)=RT_PE_life%eq(il,iq_life)+TMP_linewidth*pre_factor
   endif
   !
   ! Interband and intraband (Drude) components of the lifetimes (eq or emit depending on calc.)
   if (l_extended_output) then
     !
     if (i_np == i_n) then
       ! Intraband
       if (l_initial_equilibrium) then
         RT_PE_extra%intraband(il,iq_life)= RT_PE_extra%intraband(il,iq_life) +TMP_linewidth*pre_factor
       else
         RT_PE_extra%intraband(il,iq_life)= RT_PE_extra%intraband(il,iq_life) +TMP_Emit_lifetime*pre_factor
       endif
       !
     else
       ! Interband
       if (l_initial_equilibrium) then
         RT_PE_extra%interband(il,iq_life)= RT_PE_extra%interband(il,iq_life) +TMP_linewidth*pre_factor
       else
         RT_PE_extra%interband(il,iq_life)= RT_PE_extra%interband(il,iq_life) +TMP_Emit_lifetime*pre_factor
       endif
       !
     endif
     !
   endif
   !   
   call live_timing( steps=1 )
   !
 enddo
 !
 call live_timing( )
 !
end subroutine PHEL_Lifetimes
