!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): FP AM
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
subroutine PH_Grid_Check_Transitions(k,q,bands,k_FG,E_FG)
 !
 ! This routine selects the indices of the PH mediated transitions. 
 !
 ! When lifetimes are calculated transitions are selected to satisfy
 !
 !   Delta_E_{n,np,k,k_m_q} = ph_E_{ql} +- PHEL_broad_factor*QP_G_damp
 !
 ! The table created here makes the code much faster as it avoids the use of all
 ! double-grid kpts in the two spheres (k and kp).
 !
 use drivers,        ONLY:l_life
 use pars,           ONLY:SP,IPL
 use units,          ONLY:HA2EV
 use electrons,      ONLY:E_fine_grid
 use hamiltonian,    ONLY:B_mat_index
 use R_lattice,      ONLY:bz_samp,bz_fine_grid
 use real_time,      ONLY:RT_eh_en
 use ELPH,           ONLY:elph_branches,PH_acoustic_branch,elph_use_q_grid,&
&                         elph_nQ
 use parallel_m,     ONLY:PARs_PH_K_bz,PARs_PH_eh_bands,PARs_PH_Q_ibz
 use QP_m,           ONLY:QP_G_damp
 use LIVE_t,         ONLY:live_timing
 use PHEL,           ONLY:phel_info,KpQ_IO_map,PH_Q,PHEL_broad_factor,&
&                         PHEL_gimme_Ks,PH_energy
 use com,            ONLY:msg
 use X_m,            ONLY:X_DbGd_nkpt_at_q,X_DbGd_nkpt_at_q_table,X_Double_Grid_setup
 !
#include<memory.h>
 !
 type(bz_samp),     intent(in)    :: k,q
 integer,           intent(in)    :: bands(2)
 type(bz_fine_grid),intent(inout) :: k_FG
 type(E_fine_grid) ,intent(in)    :: E_FG
 !
 ! Work Space ...
 !
 logical               :: l_phel_allowed,l_cycle
 real(SP)              :: ph_freq,ratio,nTrans_percent(PH_Q(2)),Radius(PH_Q(2))
 integer               :: i_loop,i_trans
 integer(IPL)          :: N_yes(PH_Q(2)),N_tot(PH_Q(2))
 ! ... band/branch index ...
 integer               :: il,i_n,i_np
 ! ... BZ index ...
 integer               :: iq,ikbz,ik,ik_m_q,ik_rim,ip_rim
 ! ... energy related quantities ...
 real(SP) :: DELTA_E,DELTA_W
 !
 call section("=","Phonon mediated scattering analysis")
 !
 ! Get nearest k-q table if using custom qpoints
 if (.not.elph_use_q_grid) then 
   YAMBO_ALLOC(KpQ_IO_map,(elph_nQ,k%nbz) )
   call PHEL_k_nearest_table(k)
 endif
 !
 ! Loop to allocate phel_table
 !=============================
 do i_loop=1,2
   !
   if (i_loop==1) then
     call live_timing('Compatibility scan',PARs_PH_Q_ibz%N_par*PARs_PH_K_bz%N_par)
   else
     if (phel_info%N==0) call error('No allowed scattering found')
     YAMBO_ALLOC(phel_info%table,(phel_info%N,6) )
     call live_timing('Compatibility fill',PARs_PH_Q_ibz%N_par*PARs_PH_K_bz%N_par)
   endif
   !
   phel_info%N = 0
   N_tot       = 0
   N_yes       = 0
   ! 
   ! Main loop over iq in the ibz
   !==============================
   do iq=PH_Q(1),PH_Q(2)
     !
     if (.not.PARs_PH_Q_ibz%IND%element_1D(iq)) cycle
     !
     call X_Double_Grid_setup(iq,k,nTrans_percent=nTrans_percent(iq),Radius=Radius(iq))
     !
     ! Inner loop over ik in the bz
     !==============================
     do ikbz=1,k%nbz
       !
       if (.not.PARs_PH_K_bz%IND%element_1D(ikbz)) cycle
       !
#if defined _RT
       call PHEL_gimme_Ks(k,iq,ikbz,ik,ik_m_q,.TRUE.)
#else
       call PHEL_gimme_Ks(k,iq,ikbz,ik,ik_m_q,.FALSE.)
#endif
       !
       ! Loop over il, i.e., phonon branches
       !====================================    
       do il=elph_branches(1),elph_branches(2)
         !
         ph_freq=PH_energy(q,il,iq)
         !
         if (iq==1 .and. PH_acoustic_branch(il)) cycle
         !
         ! Loops over i_n, i_np, i.e., electronic bands
         !==============================================
         do i_n=bands(1),bands(2)
           !
           do i_np=bands(1),bands(2)
             !
             if ( .not. PARs_PH_eh_bands%IND%element_1D( B_mat_index(i_n,i_np,bands) ) ) cycle
             !
             ! Loops over fine grid kpoints 
             !==============================    
             do i_trans=1,X_DbGd_nkpt_at_q(ikbz)
               !
               ik_rim=X_DbGd_nkpt_at_q_table(ikbz,i_trans,1)
               ip_rim=X_DbGd_nkpt_at_q_table(ikbz,i_trans,2)
               !
               call eval_delta_E_W(i_n,ik_rim,i_np,ip_rim,l_cycle)
               if (l_cycle) cycle
               !
               ! Select energy-allowed transitions
               !=================================== 
               l_phel_allowed=.TRUE.
               if (l_life) then
                 l_phel_allowed = abs(DELTA_E-ph_freq)<PHEL_broad_factor*QP_G_damp.or.&
&                                 abs(DELTA_E+ph_freq)<PHEL_broad_factor*QP_G_damp
               endif
               !
               if (l_phel_allowed) then
                 !
                 phel_info%N  = phel_info%N+1
                 N_yes(iq)    = N_yes(iq)+1
                 !
                 if (i_loop==2) phel_info%table(phel_info%N,:)= (/il,iq,ik_rim,ip_rim,i_n,i_np/)
                 !
               endif
               !
               N_tot(iq) = N_tot(iq)+1
               !
             enddo ! Fine-Grid transitions
             !
           enddo ! i_np i.e. (mk-q) 
           !
         enddo ! i_n i.e. (nk)
         !
       enddo ! il
       !
       call live_timing( steps=1 )
       !
     enddo ! ikbz
     !
     YAMBO_FREE(X_DbGd_nkpt_at_q)
     YAMBO_FREE(X_DbGd_nkpt_at_q_table)
     ! 
   enddo ! iq
   !
   call live_timing( )
   !
 enddo ! i_loop
 !
 ! Output info
 !=============
 call section("p","Transitions Report")
 call msg('r','Energy Window',PHEL_broad_factor*QP_G_damp*HA2EV,"[eV]")
 do iq=PH_Q(1),PH_Q(2)
   !
   ! Energy dictated
   if (N_tot(iq)>0) then
     ratio = float(N_yes(iq))/float(N_tot(iq))*100._SP
     call msg('r','Transitions included [Energy Constrain]  ',ratio,"[o/o]")
   endif
   !
   ! Momentum dictated
   if (nTrans_percent(iq)>0) then
     call msg('r','Transitions included [Momentum Constrain]',nTrans_percent(iq),"[o/o]")
     call msg('r','Radius over smallest radius              ',Radius(iq))
   endif
   !
 enddo
 !
 contains
   !
   subroutine eval_delta_E_W(i_n,i_k,i_m,i_p,l_flag)
     integer :: i_n,i_k,i_m,i_p
     logical :: l_flag
     l_flag=.FALSE.
#if defined _RT
     l_flag= E_FG%E(i_m,i_p,1)<RT_eh_en(1) .or. E_FG%E(i_m,i_p,1)>RT_eh_en(2).or.&
&            E_FG%E(i_n,i_k,1)<RT_eh_en(1) .or. E_FG%E(i_n,i_k ,1)>RT_eh_en(2)
#endif
     !
     DELTA_E=E_FG%E(i_n,i_k,1)-E_FG%E(i_m,i_p,1)
     DELTA_W=E_FG%W(i_n,i_k,1)+E_FG%W(i_m,i_p,1)
     !
   end subroutine
 !
end subroutine PH_Grid_Check_Transitions
