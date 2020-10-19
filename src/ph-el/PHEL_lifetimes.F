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
subroutine PHEL_lifetimes(iq,E,k,bands,k_FG,E_FG)
 !
 ! This routine computes the phonon lifetimes. 
 ! Presently, only one approximation route from the phononic KBE to 
 ! the kernel/collision integral is available:
 !   - Time Scales Separation + QuasiParticle Approximation (TSQP)
 !
 use pars,           ONLY:SP,rZERO,cI,pi
 use units,          ONLY:HA2EV
 use electrons,      ONLY:levels,n_full_bands,spin_occ,E_fine_grid
 use hamiltonian,    ONLY:B_mat_index
 use R_lattice,      ONLY:bz_samp,qindx_X,bz_fine_grid
 use RT_occupations, ONLY:RT_ph_occ,RT_el_occ,RT_ho_occ,RT_life_occ
 use RT_lifetimes,   ONLY:l_initial_equilibrium,RT_PE_life
 use real_time,      ONLY:l_RT_uses_E_FineGd,RT_eh_en
 use PHEL,           ONLY:phel_gkkp_expanded_sq
 use ELPH,           ONLY:ph_freqs_sq,elph_branches,MAX_gkkp
 use parallel_m,     ONLY:PAR_Q_ibz_index,PAR_Xk_ibz_index,PAR_IND_B_mat
 use QP_m,           ONLY:QP_G_damp
 !
 implicit none
 !
 type(levels),      intent(in) :: E
 type(bz_samp),     intent(in) :: k
 integer,           intent(in) :: iq,bands(2)
 type(bz_fine_grid),intent(in) :: k_FG
 type(E_fine_grid) ,intent(in) :: E_FG
 !
 ! Work Space ...
 !
 real(SP)              :: ph_freq
 !real(SP),   parameter :: cutoff= 5.E-3
 real(SP),   external  :: RT_G_retarded_convolution
 complex(SP),external  :: Lorentzian_FT
 !
 real(SP)              :: Gkkp_sq(bands(1):bands(2),bands(1):bands(2))
 ! ... band/branch index ...
 integer               :: il,i_n,i_np
 ! ... BZ index ...
 integer               :: ikbz,ik,ik_m_q,ik_mem,iq_mem,ik_rim,ip_rim
 ! ... lifetime related quantities ...
 real(SP) :: D_res,D_ares,DELTA_E,DELTA_W,pre_factor,&
&            TMP_Emit_lifetime,TMP_Abs_lifetime,TMP_eh_lifetime,N_q,f_mkq,f_nk,&
&            Hole_f_mkq,Hole_f_nk
 !
 iq_mem=PAR_Q_ibz_index(iq)
 !
 do ikbz=1,k%nbz
   !
   ! Obtain k-q in the ibz from k in the bz and q in the ibz
   ik_m_q =k%sstar(qindx_X(iq_mem,ikbz,1),1)
   ! Obtain k in the ibz from k in the bz
   ik = k%sstar(ikbz,1)
   !
   ik_mem=PAR_Xk_ibz_index(ik)
   !
   do il=elph_branches(1),elph_branches(2)
     !
     ph_freq=abs(sqrt(ph_freqs_sq(iq_mem,il)))
     !
     ! Avoiding acoustic phonons at Gamma and vanishing phonon energies
     if ( (iq_mem==1 .and. il<=3) ) cycle
     if ( abs(ph_freq)<3.E-5)     cycle
     !
     ! Time Scales separation (TS) and quasiparticle approximation (QP)
     !=======================================
     !
     if (l_initial_equilibrium) then
       N_q    = RT_ph_occ%N_ref(il,iq_mem)
     else
       N_q    = RT_ph_occ%N(il,iq_mem)
     endif
     !
     ! Kernel kind
     !=============
     !
     Gkkp_sq=rZERO
     !
     do i_n=bands(1),bands(2)
       !
       Gkkp_sq(i_n,i_n)=phel_gkkp_expanded_sq(i_n,i_n,il,iq_mem,ikbz)
       !if (sqrt(Gkkp_sq(i_n,i_n))<MAX_gkkp*cutoff) cycle
       !
       do i_np=bands(1),bands(2)
         !
         if (i_n.ne.i_np) then
           Gkkp_sq(i_n,i_np)=phel_gkkp_expanded_sq(i_np,i_n,il,iq_mem,ikbz)
           !if (sqrt(Gkkp_sq(i_n,i_np))<MAX_gkkp*cutoff) cycle
         endif 
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
           if ( .not. PAR_IND_B_mat%element_1D( B_mat_index(i_n,i_np,bands) ) ) cycle
           !
           if (.not. l_RT_uses_E_FineGd) then
             !
             if(E_FG%E(i_np,ik_m_q,1)<RT_eh_en(1) .or. E_FG%E(i_np,ik_m_q,1)>RT_eh_en(2)) cycle
             if(E_FG%E(i_n ,ik    ,1)<RT_eh_en(1) .or. E_FG%E(i_n ,ik    ,1)>RT_eh_en(2)) cycle
             !
             DELTA_E=E_FG%E(i_n,ik,1)-E_FG%E(i_np,ik_m_q,1)
             DELTA_W=E_FG%W(i_n,ik,1)+E_FG%W(i_np,ik_m_q,1)
             !
           endif
           !
         endif
         !
         ! [NOTE] The weight here is
         !
         !                    (2*spin_occ*pi)/(RT_Nk_ibz*2*ph_freq)
         !
         ! 2 from KBE collision integral, spin_occ=2 from ph-el self-energy (polarization bubble), pi from delta functions,
         ! 1/Nk from k-sum, (2*ph_freq)^-1 to correctly normalize Gkkp_sq.
         ! [NOTE] Variable pre_factor below contains pi since it *shouldn't* come later from the hyperbolic secant FT
         !
         pre_factor=spin_occ*Gkkp_sq(i_np,i_n)*pi/(real(k_FG%N_mapped(ik_m_q))*ph_freq)
         !
         if( pre_factor==0._SP ) cycle
         !
         do ik_rim=k_FG%k_range(ik,1),k_FG%k_range(ik,2)
           !
           ! Zeroing
           !=========
           TMP_Emit_lifetime=rZero
           TMP_Abs_lifetime=rZero
           TMP_eh_lifetime=rZero
           !
           do ip_rim=k_FG%k_range(ik_m_q,1),k_FG%k_range(ik_m_q,2)
             !
             if (l_RT_uses_E_FineGd) then
               !
               if(E_FG%E(i_np,ip_rim,1)<RT_eh_en(1) .or. E_FG%E(i_np,ip_rim,1)>RT_eh_en(2)) cycle
               if(E_FG%E(i_n ,ik_rim,1)<RT_eh_en(1) .or. E_FG%E(i_n ,ik_rim,1)>RT_eh_en(2)) cycle
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
             D_ares=1./pi*aimag( Lorentzian_FT(ph_freq-cI*QP_G_damp,DELTA_E,"Ta") )
             D_res =1./pi*aimag( Lorentzian_FT(ph_freq-cI*QP_G_damp,DELTA_E,"r" ) )
             !D_ares=-1./pi*aimag( Lorentzian_FT(DELTA_E+cI*QP_G_damp,-ph_freq-DELTA_W,"r") )
             !D_res =-1./pi*aimag( Lorentzian_FT(DELTA_E+cI*QP_G_damp,+ph_freq-DELTA_W,"r") )
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
             ! NB: at equilibrium this quantity is just: (f_mkq - f_nk)
             TMP_Emit_lifetime=TMP_Emit_lifetime+(Hole_f_nk*f_mkq - f_nk*Hole_f_mkq)*D_ares
             ! NB: at equilibrium this quantity is just: (f_nk - f_mkq)
             TMP_Abs_lifetime=TMP_Abs_lifetime+(f_nk*Hole_f_mkq-Hole_f_nk*f_mkq)*D_res
             !
           enddo
           !
         enddo
         !
         ! Phonon emission (stimulated and spontaneous)
         RT_PE_life%emit(il,iq_mem)=RT_PE_life%emit(il,iq_mem)+TMP_Emit_lifetime*pre_factor*(N_q+1._SP)
         ! Phonon absorption
         RT_PE_life%abs(il,iq_mem) =RT_PE_life%abs(il,iq_mem)+TMP_Abs_lifetime*pre_factor*N_q
         !
         ! [NOTE] At equilibrium at zero lattice temperature the sum of the two 
         !        (i.e., the imaginary part of the self-energy) is just:
         ! 
         !                        pre_factor*(f_mkq - f_nk)*D_ares 
         !
         !       This corresponds to 2 times a Matsubara calculation of 
         !       the self-energy Pi = g_squared*GG  (see Mahan p. 136),
         !       with the additional 2 coming from the KBE.
         !       Should this 2 factor cancel out?
         !
         !       At BoseTemp>0, the following additional term appears:
         !
         !              pre_factor*(f_mkq - f_nk)*N_q(BoseTemp)*(D_ares - D_res)
         !
       enddo ! i_np i.e. (mk-q)
       !
     enddo ! i_n i.e. (nk)
     !
   enddo ! ph branch
   !
 enddo ! ikbz
 !
end subroutine PHEL_lifetimes
