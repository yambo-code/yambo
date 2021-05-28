!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine PHEL_Lifetimes_equilibrium(E,k,q,k_FG,E_FG)
 !
 ! This routine computes the phonon lifetimes in equilibrium (i.e., linewidths).
 ! 
 ! It uses a table of allowed phonon scattering transitions pre-determined 
 ! in PH_Grid_Check_Transitions.
 !
 ! [DEBUG] Emit and Abs lifetimes should eventually be removed (computed only 
 !         out of eq.). We keep them here for future debugging of the RT part.
 !
 use pars,           ONLY:SP,cI,pi
 use electrons,      ONLY:levels,spin_occ,E_fine_grid
 use R_lattice,      ONLY:bz_samp,bz_fine_grid
 use RT_occupations, ONLY:RT_el_occ,RT_ho_occ
 use RT_lifetimes,   ONLY:RT_PE_life
 use ELPH,           ONLY:PH_freqs_sq,MAX_gkkp,PH_acoustic_branch,GKKP_pe_sq,&
&                         l_GKKP_expanded_DB_exists,l_GKKP_DB_exists
 use parallel_m,     ONLY:PARs_PH_Q_ibz
 use QP_m,           ONLY:QP_G_damp,l_extended_output,QP_solver,QP_Sc_steps
 use LIVE_t,         ONLY:live_timing
 use PHEL,           ONLY:l_PH_uses_E_FineGd,phel_info,PH_SF_W,PH_Q,PHEL_qp
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
 complex(SP),external  :: Lorentzian_FT
 !
 ! ... frequencies ...
 integer               :: iw,iw_mem
 ! ... transition index ...
 integer               :: it
 ! ... band/branch index ...
 integer               :: il,i_n,i_np
 ! ... BZ index ...
 integer               :: iq,ikbz,ik,ik_m_q,iq_mem,ik_rim,ip_rim,iq_life
 ! ... lifetime related quantities ...
 real(SP)    :: D_res,D_ares,DELTA_E,DELTA_W,pre_factor,f_mkq,f_nk,Hole_f_mkq,Hole_f_nk,E_Allen,Allen_factor
 complex(SP) :: TMP_linewidth
 !
 call live_timing('Phonon Lifetimes [Equilibrium]',phel_info%N)
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
   ! Use k in the bz
   ik = ikbz
   !
   ! Index for q-parallelised quantities
   iq_mem =PARs_PH_Q_ibz%table(iq)
   iq_life=iq 
   !
   if (l_GKKP_expanded_DB_exists) ph_freq=abs(sqrt(PH_freqs_sq(q%k_table(iq,1),il)))
   if (l_GKKP_DB_exists         ) ph_freq=abs(sqrt(PH_freqs_sq(iq,il)))
   !
   if (iq==1 .and. PH_acoustic_branch(il)) then
     call live_timing( steps=1 )
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
     call live_timing( steps=1 )
     cycle
   endif
   !
   ! Prefactor
   !====================================
   ! The weight here is
   !
   !                    (2*spin_occ*pi)/(k%nbz*2* ph_freq)
   !
   ! 2 since FWHM=2*Im{SE^R}, spin_occ=2 from ph-el self-energy (polarization bubble), pi from delta functions,
   ! 1/Nk from k-sum, (2*ph_freq)^-1 to correctly normalize Gkkp_sq.
   !
   ! [NOTE]  Variable pre_factor below contains pi since it *shouldn't* come later from the hyperbolic secant FT.
   ! [NOTE2] Variable pre_factor below contains 1/(Nk_bz*N_FG_k*N_FG_k_m_q) accounting for the double grid at each bz point.
   !
   pre_factor=spin_occ*GKKP_pe_sq%dVr(il,i_n,i_np,ikbz,iq_mem)*pi/(k%nbz*k_FG%N_mapped(ik)*k_FG%N_mapped(ik_m_q)*ph_freq)
   !
   ! At equilibrium, only metals or band gaps < E_debye can give non-zero ph lifetimes
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
   ! Lifetimes
   !============
   !
   ! Electron and hole occupations
   f_mkq      =  RT_el_occ%N_ref(i_np,ip_rim)/spin_occ
   Hole_f_mkq =  RT_ho_occ%N_ref(i_np,ip_rim)/spin_occ
   f_nk       =  RT_el_occ%N_ref(i_n,ik_rim)/spin_occ
   Hole_f_nk  =  RT_ho_occ%N_ref(i_n,ik_rim)/spin_occ
   !
   if (trim(QP_solver)=="n") then
     !
     ! Allen Factor
     Allen_factor = ph_freq*1./pi*aimag( Lorentzian_FT(E_Allen-cI*QP_G_damp,0._SP,"r" ) )
     !
     ! Lorentzians
     call LORENTZIANs_eval ( ph_freq+cI*QP_G_damp )
     !
     ! Accumulation of the lifetimes
     call COMPOSE_emit_abs( )
     !
     ! This is Eq. (4) of [PRB 71, 064501 (2005)], i.e., retarded ph-el self-energy g^2*GG
     TMP_linewidth     = (f_mkq*Hole_f_nk-Hole_f_mkq*f_nk)*D_res
     !
     ! Linewidth at equilibrium
     PHEL_qp%PI(il,iq_life,1)=PHEL_qp%PI(il,iq_life,1)+cI*TMP_linewidth*pre_factor
     !
     ! Interband and intraband (Drude) components of the lifetimes
     if (l_extended_output) call COMPOSE_extended( )
     !
   else
     !
     do iw=1,QP_Sc_steps
       iw_mem=(iq_life-1)*PH_Q(2)+iw
       call LORENTZIANs_eval ( PH_SF_W%p(iw) )
       TMP_linewidth     = (f_mkq*Hole_f_nk-Hole_f_mkq*f_nk)*D_res
       TMP_linewidth     = (f_mkq*Hole_f_nk-Hole_f_mkq*f_nk)*Lorentzian_FT(PH_SF_W%p(iw),DELTA_E+cI*DELTA_W,"r" )/pi
       PHEL_qp%PI(il,iq_life,iw)=PHEL_qp%PI(il,iq_life,iw)+TMP_linewidth*pre_factor
     enddo
   endif
   !   
   call live_timing( steps=1 )
   !
 enddo
 !
 call live_timing( )
 !
 contains
   !
   subroutine COMPOSE_extended( )
     ! This is Eq. (23) of [PRB 71, 064501 (2005)], i.e., the Allen approximation to Eq. (4)
     PHEL_qp%approx(il,iq_life)= PHEL_qp%approx(il,iq_life) +Allen_factor*D_res*pre_factor
     !
     if (i_np == i_n) then
       ! Intraband
       PHEL_qp%intraband(il,iq_life)= PHEL_qp%intraband(il,iq_life) +TMP_linewidth*pre_factor
       !
     else
       ! Interband
       PHEL_qp%interband(il,iq_life)= PHEL_qp%interband(il,iq_life) +TMP_linewidth*pre_factor
       !
     endif
   end subroutine 
   !
   subroutine COMPOSE_emit_abs( )
     ! Phonon emission (stimulated and spontaneous)
     RT_PE_life%emit(il,iq_life)=RT_PE_life%emit(il,iq_life)+(f_mkq*Hole_f_nk*D_ares+Hole_f_mkq*f_nk*D_res)*pre_factor
     ! Phonon absorption
     RT_PE_life%abs(il,iq_life) =RT_PE_life%abs(il,iq_life)+(f_mkq*Hole_f_nk*D_res +Hole_f_mkq*f_nk*D_ares)*pre_factor
   end subroutine 
   !
   subroutine LORENTZIANs_eval( W )
     complex(SP) :: W
     D_ares=1./pi*aimag( Lorentzian_FT(W,DELTA_E+cI*DELTA_W,"Ta") )
     D_res =1./pi*aimag( Lorentzian_FT(W,DELTA_E+cI*DELTA_W,"r" ) )
   end subroutine 
   !
end subroutine PHEL_Lifetimes_equilibrium
