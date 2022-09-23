!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine PHEL_Self_Energy(iq,E,k,q,k_FG,E_FG)
 !
 ! This routine computes the phonon lifetimes in equilibrium (i.e., linewidths).
 ! 
 ! It uses a table of allowed phonon scattering transitions pre-determined 
 ! in PH_Grid_Check_Transitions.
 !
 ! [DEBUG] Emit and Abs lifetimes should eventually be removed (computed only 
 !         out of eq.). We keep them here for future debugging of the RT part.
 !
 use drivers,        ONLY:l_life
 use pars,           ONLY:SP,cI,pi,schlen
 use electrons,      ONLY:levels,spin_occ,E_fine_grid
 use R_lattice,      ONLY:bz_samp,bz_fine_grid
 use RT_occupations, ONLY:RT_el_occ,RT_ho_occ
 use RT_lifetimes,   ONLY:RT_PE_life
 use ELPH,           ONLY:MAX_gkkp,PH_acoustic_branch,GKKP_pe_sq
 use parallel_m,     ONLY:PARs_PH_Q_ibz
 use QP_m,           ONLY:QP_G_damp,l_extended_output
 use LIVE_t,         ONLY:live_timing
 use PHEL,           ONLY:l_PH_uses_E_FineGd,phel_info,PH_SF_W,PH_Q,PHEL_qp,PHEL_gimme_Ks,PH_energy
 use timing_m,       ONLY:timing
 use stderr,         ONLY:intc
 !
 implicit none
 !
 integer,           intent(in) :: iq
 type(levels),      intent(in) :: E
 type(bz_samp),     intent(in) :: k,q
 type(bz_fine_grid),intent(in) :: k_FG
 type(E_fine_grid) ,intent(in) :: E_FG
 !
 ! Work Space ...
 !
 real(SP)              :: ph_freq
 real(SP),   parameter :: cutoff= 5.E-3
 integer               :: n_LT_steps,n_LT_steps_current
 !
 ! ... frequencies ...
 integer               :: iw,iw_mem
 ! ... transition index ...
 integer               :: it
 ! ... band/branch index ...
 integer               :: il,i_n,i_np
 ! ... BZ index ...
 integer               :: ikbz,ik,ik_m_q,iq_mem,ik_rim,ip_rim
 ! ... lifetime related quantities ...
 real(SP)          :: D_res,D_ares,DELTA_E,DELTA_W,pre_factor,f_mkq,f_nk,Hole_f_mkq,Hole_f_nk,E_Allen,Allen_factor
 complex(SP)       :: TMP_linewidth
 character(schlen) :: sec_name
 !
 sec_name                  ='Phonon Lifetimes'
 if (.not.l_life)  sec_name='Phonon Self-Energy'
 !
 ! Live Timing setup
 !
 call live_timing(trim(sec_name)//"@ Q"//trim(intc(iq)),100)
 n_LT_steps=nint( real(phel_info%N) / 100. )
 n_LT_steps_current=0
 !
 call timing(trim(sec_name),OPR='start')
 !
 do it=1,phel_info%N
   !
   n_LT_steps_current=n_LT_steps_current+1
   !
   ! Indices of allowed transition
   il     = phel_info%table(it,1) ! phonon branch
   ik_rim = phel_info%table(it,2) ! fine grid elec. momentum
   ip_rim = phel_info%table(it,3) ! fine grid scattered elec. momentum
   i_n    = phel_info%table(it,4) ! electronic band
   i_np   = phel_info%table(it,5) ! electronic band
   ikbz   = k_FG%k_map(ik_rim)
   !
#if defined _RT
   call PHEL_gimme_Ks(k,iq,ikbz,ik,ik_m_q,.TRUE.)
#else
   call PHEL_gimme_Ks(k,iq,ikbz,ik,ik_m_q,.FALSE.)
#endif
   !
   ! Index for q-parallelised quantities
   iq_mem =PARs_PH_Q_ibz%table(iq)
   !
   ph_freq=PH_energy(q,il,iq)
   !
   if (iq==1 .and. PH_acoustic_branch(il)) then
     call Live_Timing_release( )
     cycle
   endif
   !
   ! Electron-phonon matrix elements
   !====================================
   ! In Yambo we have:         GKKP_YAMBO[i_n,i_np] = <i_n ik|g(q_YAMBO l r)|i_np ik-q_YAMBO>     
   ! In my notes (FP) we have: GKKP_NOTES[i_np,i_n] = <i_n ik|g(q_YAMBO l r)|i_np ik-q_YAMBO>
   !    
   ! Therefore:                GKKP_NOTES[i_np,i_n] = GKKP_YAMBO[i_n,i_np],
   ! which is what must go into the pre_factor of the lifetimes
   !
   if (sqrt(GKKP_pe_sq%dVr(il,i_n,i_np,ikbz,iq_mem))<MAX_gkkp*cutoff) then
     call Live_Timing_release( )
     cycle
   endif
   !
   ! Prefactor
   !====================================
   ! The weight here is
   !
   !                    (2*spin_occ*pi)/(k%nbz*2* ph_freq)
   !
   ! (i  ) 2 in the numerator since the H_ep = \sqrt(2) G_kkp c^+c Q  (the factor \sqrt(2) must be included in the paper with Yaroslav)
   ! (ii ) spin_occ from ph-el self-energy (polarization bubble 
   ! (iii) (2*ph_freq)^-1 to correctly normalize Gkkp_sq
   ! (iv ) 1/(Nk_bz*N_FG_k*N_FG_k_m_q) accounting for the double grid at each bz point.
   ! (v  ) pi since it *shouldn't* come later from the hyperbolic secant FT. In the equilibrium case 1/pi is included below
   !
   pre_factor=spin_occ*GKKP_pe_sq%dVr(il,i_n,i_np,ikbz,iq_mem)*pi/(k%nbz*k_FG%N_mapped(ik)*k_FG%N_mapped(ik_m_q)*ph_freq)
   !
   ! Double grid (no/yes)
   !====================================
   if (.not. l_PH_uses_E_FineGd) then
     !
     DELTA_E=E_FG%E(i_n,ik,1)-E_FG%E(i_np,ik_m_q,1)
     DELTA_W=E_FG%W(i_n,ik,1)+E_FG%W(i_np,ik_m_q,1)
     E_Allen=E_FG%E(i_n,ik,1)
     !
   else
     !
     DELTA_E=E_FG%E(i_n,ik_rim,1)-E_FG%E(i_np,ip_rim,1)
     DELTA_W=E_FG%W(i_n,ik_rim,1)+E_FG%W(i_np,ip_rim,1)
     E_Allen=E_FG%E(i_n,ik_rim,1)
     !
   endif
   !
   ! Electron and hole occupations
   f_mkq      =  RT_el_occ%N_ref(i_np,ip_rim)/spin_occ
   Hole_f_mkq =  RT_ho_occ%N_ref(i_np,ip_rim)/spin_occ
   f_nk       =  RT_el_occ%N_ref(i_n,ik_rim)/spin_occ
   Hole_f_nk  =  RT_ho_occ%N_ref(i_n,ik_rim)/spin_occ
   !
   if (l_life) then
     !
     ! Lorentzians
     call DELTA_functions_eval ( ph_freq-cI*QP_G_damp ) ! - is 'cause \delta (w) \propto Im [L(w)]
     !
#if defined _RT
     ! Accumulation of the lifetimes
     call COMPOSE_emit_abs( )
#endif
     !
     ! This is Eq. (4) of [PRB 71, 064501 (2005)], i.e., retarded ph-el self-energy g^2*GG
     TMP_linewidth     = (f_mkq*Hole_f_nk-Hole_f_mkq*f_nk)*D_res
     !
     ! Linewidth at equilibrium
     PHEL_qp%PI(il,iq,1)=PHEL_qp%PI(il,iq,1)+cI*TMP_linewidth*pre_factor
     !
     ! Interband and intraband (Drude) components of the lifetimes
     if (l_extended_output) call COMPOSE_extended( )
     !
   else
     !
     do iw=1,PH_SF_W(il,iq)%n_freqs
       iw_mem=(iq-1)*PH_Q(2)+iw
       TMP_linewidth       =(f_mkq-f_nk)/(PH_SF_W(il,iq)%p(iw)-DELTA_E-cI*DELTA_W)/pi
       PHEL_qp%PI(il,iq,iw)=PHEL_qp%PI(il,iq,iw)+TMP_linewidth*pre_factor
     enddo
     !
   endif
   !   
   call Live_Timing_release( )
   !
 enddo
 !
 call live_timing( )
 call timing(trim(sec_name),OPR='stop')
 !
 contains
   !
   subroutine COMPOSE_extended( )
     !
     ! This is Eq. (23) of [PRB 71, 064501 (2005)], i.e., the Allen approximation to Eq. (4)
     !
     Allen_factor = ph_freq*1./pi*aimag(1._SP/(E_Allen-cI*QP_G_damp))
     !
     PHEL_qp%allen(il,iq)= PHEL_qp%allen(il,iq) +Allen_factor*D_res*pre_factor
     !
     if (i_np == i_n) then
       ! Intraband
       PHEL_qp%intraband(il,iq)= PHEL_qp%intraband(il,iq) +TMP_linewidth*pre_factor
       !
     else
       ! Interband
       PHEL_qp%interband(il,iq)= PHEL_qp%interband(il,iq) +TMP_linewidth*pre_factor
       !
     endif
     !
   end subroutine 
   !
   subroutine COMPOSE_emit_abs( )
     !
     ! TO BE CHECKED!
     !D_ares=1./pi*aimag( Lorentzian_FT(W,DELTA_E+cI*DELTA_W,"Ta") )
     !
     ! Phonon emission (stimulated and spontaneous)
     RT_PE_life%emit(il,iq)=RT_PE_life%emit(il,iq)+(f_mkq*Hole_f_nk*D_ares+Hole_f_mkq*f_nk*D_res)*pre_factor
     !
     ! Phonon absorption
     RT_PE_life%abs(il,iq) =RT_PE_life%abs(il,iq)+(f_mkq*Hole_f_nk*D_res +Hole_f_mkq*f_nk*D_ares)*pre_factor
   end subroutine 
   !
   subroutine Live_Timing_release(  )
     if (n_LT_steps_current==n_LT_steps) then
       call live_timing( steps=1 )
       n_LT_steps_current=0
     endif
   end subroutine
   !
   subroutine DELTA_functions_eval( W )
     complex(SP) :: W
     D_res =1./pi*aimag(1._SP/(W-DELTA_E-cI*DELTA_W))
   end subroutine 
   !
end subroutine PHEL_Self_Energy
