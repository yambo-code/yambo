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
subroutine PHEL_lifetimes(iq,E,k,q,bands,k_FG,E_FG)
 !
 ! This routine computes the phonon lifetimes. 
 ! Presently, only one approximation route from the phononic KBE to 
 ! the kernel/collision integral is available:
 !   - Time Scales Separation + QuasiParticle Approximation (TSQP)
 !
 use pars,           ONLY:SP,cI,pi,rZERO
 use units,          ONLY:HA2EV
 use electrons,      ONLY:levels,n_full_bands,spin_occ,E_fine_grid
 use hamiltonian,    ONLY:B_mat_index
 use R_lattice,      ONLY:bz_samp,qindx_X,bz_fine_grid
 use RT_occupations, ONLY:RT_ph_occ,RT_el_occ,RT_ho_occ,RT_life_occ
 use RT_lifetimes,   ONLY:l_initial_equilibrium,RT_PE_life,PE_linewidth
 use real_time,      ONLY:l_RT_uses_E_FineGd,RT_eh_en
 use ELPH,           ONLY:ph_freqs_sq,elph_branches,MAX_gkkp,PH_acoustic_branch,phel_gkkp_expanded_sq
 use parallel_m,     ONLY:PARs_PH_K_bz,PARs_PH_eh_bands,PARs_PH_Q_ibz
 use QP_m,           ONLY:QP_G_damp
 !
 implicit none
 !
 type(levels),      intent(in) :: E
 type(bz_samp),     intent(in) :: k,q
 integer,           intent(in) :: iq,bands(2)
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
 ! ... band/branch index ...
 integer               :: il,i_n,i_np
 ! ... BZ index ...
 integer               :: ikbz,ik,ik_m_q,iq_mem,ik_rim,ip_rim,iq_life
 ! ... lifetime related quantities ...
 real(SP) :: D_res,D_ares,DELTA_E,DELTA_W,pre_factor,&
&            TMP_Emit_lifetime,TMP_Abs_lifetime,TMP_eh_lifetime,N_q,f_mkq,f_nk,&
&            Hole_f_mkq,Hole_f_nk,TMP_linewidth
 !
 iq_mem =PARs_PH_Q_ibz%table(iq)
 iq_life=iq
#if defined _RT
 iq_life=iq_mem
#endif
 !
 do ikbz=1,k%nbz
   !
   if (.not.PARs_PH_K_bz%IND%element_1D(ikbz)) cycle
   !
   ! Obtain k-q in the ibz from k in the bz and q in the ibz
   ik_m_q =k%sstar(qindx_X(iq,ikbz,1),1)
   ! Obtain k in the ibz from k in the bz
   ik = k%sstar(ikbz,1)
   !
   do il=elph_branches(1),elph_branches(2)
     !
     ph_freq=abs(sqrt(ph_freqs_sq(q%k_table(iq,1),il)))
     !
     if (iq==1 .and. PH_acoustic_branch(il)) cycle
     !
     if (     l_initial_equilibrium) N_q = RT_ph_occ%N_ref(il,iq)
     if (.not.l_initial_equilibrium) N_q = RT_ph_occ%N(il,iq)
     !
     do i_n=bands(1),bands(2)
       !
       do i_np=bands(1),bands(2)
         !
         if ( .not. PARs_PH_eh_bands%IND%element_1D( B_mat_index(i_n,i_np,bands) ) ) cycle
         !
         !if (sqrt(phel_gkkp_expanded_sq(i_n,i_np,il,iq_mem,ikbz))<MAX_gkkp*cutoff) cycle
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
         if (.not. l_RT_uses_E_FineGd) then
           !
#if defined _RT
           if(E_FG%E(i_np,ik_m_q,1)<RT_eh_en(1) .or. E_FG%E(i_np,ik_m_q,1)>RT_eh_en(2)) cycle
           if(E_FG%E(i_n ,ik    ,1)<RT_eh_en(1) .or. E_FG%E(i_n ,ik    ,1)>RT_eh_en(2)) cycle
#endif
           !
           DELTA_E=E_FG%E(i_n,ik,1)-E_FG%E(i_np,ik_m_q,1)
           DELTA_W=E_FG%W(i_n,ik,1)+E_FG%W(i_np,ik_m_q,1)
           !
         endif
         !
         ! [NOTE] The weight here is
         !
         !                    (2*spin_occ*pi)/(k%nbz*2* ph_freq)
         !
         ! 2 from KBE collision integral, spin_occ=2 from ph-el self-energy (polarization bubble), pi from delta functions,
         ! 1/Nk from k-sum, (2*ph_freq)^-1 to correctly normalize Gkkp_sq.
         ! [NOTE]  Variable pre_factor below contains pi since it *shouldn't* come later from the hyperbolic secant FT.
         ! [NOTE2] Variable pre_factor below contains 1/(Nk_bz*N_FG_k) accounting for the double grid at each bz point.
         !
         pre_factor=spin_occ*phel_gkkp_expanded_sq(i_n,i_np,il,iq_mem,ikbz)*pi/(k%nbz*k_FG%N_mapped(ik)*ph_freq)
         !
         do ik_rim=k_FG%k_range(ik,1),k_FG%k_range(ik,2)
           !
           ! Zeroing
           !=========
           TMP_Emit_lifetime=rZERO
           TMP_Abs_lifetime=rZERO
           TMP_eh_lifetime=rZERO
           !DEBUG> (FP)
           TMP_linewidth=rZERO
           !DEBUG<
           !
           do ip_rim=k_FG%k_range(ik_m_q,1),k_FG%k_range(ik_m_q,2)
             !
             if (l_RT_uses_E_FineGd) then
               ! 
#if defined _RT
               if(E_FG%E(i_np,ip_rim,1)<RT_eh_en(1) .or. E_FG%E(i_np,ip_rim,1)>RT_eh_en(2)) cycle
               if(E_FG%E(i_n ,ik_rim,1)<RT_eh_en(1) .or. E_FG%E(i_n ,ik_rim,1)>RT_eh_en(2)) cycle
#endif
               !
               DELTA_E=E_FG%E(i_n,ik_rim,1)-E_FG%E(i_np,ip_rim,1)
               DELTA_W=E_FG%W(i_n,ik_rim,1)+E_FG%W(i_np,ip_rim,1)
               !
             endif
             !
#if defined _RT
             D_ares=RT_G_retarded_convolution(DELTA_E,+ph_freq,DELTA_W,0._SP,'EP')
             D_res =RT_G_retarded_convolution(DELTA_E,-ph_freq,DELTA_W,0._SP,'EP')
#else
             D_ares=1./pi*aimag( Lorentzian_FT(ph_freq-cI*QP_G_damp,DELTA_E+cI*delta_W,"Ta") )
             D_res =1./pi*aimag( Lorentzian_FT(ph_freq-cI*QP_G_damp,DELTA_E+cI*delta_W,"r" ) )
#endif
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
             TMP_Emit_lifetime=TMP_Emit_lifetime+f_nk*Hole_f_mkq*D_ares+Hole_f_nk*f_mkq*D_res
             ! 
             TMP_Abs_lifetime=TMP_Abs_lifetime+f_nk*Hole_f_mkq*D_res +Hole_f_nk*f_mkq*D_ares
             !
             !DEBUG> (FP) This is Eq. (4) of [PRB 71, 064501 (2005)], i.e., retarded ph-el self-energy g^2*GG
             TMP_linewidth=TMP_linewidth +(f_mkq-f_nk)*D_res
             !DEBUG<
           enddo ! ip_rim i.e. (mk-q)
           !
         enddo ! ik_rim i.e. (nk)
         !
         ! Phonon emission (stimulated and spontaneous)
         RT_PE_life%emit(il,iq_life)=RT_PE_life%emit(il,iq_life)+TMP_Emit_lifetime*pre_factor*(N_q+1._SP)
         ! Phonon absorption
         RT_PE_life%abs(il,iq_life) =RT_PE_life%abs(il,iq_life)+TMP_Abs_lifetime*pre_factor*N_q
         !       
         !DEBUG> (FP)
         PE_linewidth(il,iq_life)=PE_linewidth(il,iq_life) +TMP_linewidth*pre_factor
         !DEBUG<
       enddo ! i_np i.e. (mk-q)
       !
     enddo ! i_n i.e. (nk)
     !
   enddo ! ph branch
   !
 enddo ! ikbz
 !
end subroutine PHEL_lifetimes
