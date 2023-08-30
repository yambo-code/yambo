!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): FP AM
!
subroutine PH_Grid_Check_Transitions(iq,k,q,bands,k_FG,E_FG)
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
 use pars,           ONLY:SP,schlen
 use stderr,         ONLY:intc
 use electrons,      ONLY:E_fine_grid
 use hamiltonian,    ONLY:B_mat_index
 use R_lattice,      ONLY:bz_samp,bz_fine_grid
 use real_time,      ONLY:RT_eh_en
 use ELPH,           ONLY:elph_branches,PH_acoustic_branch
 use parallel_m,     ONLY:PARs_PH_K_bz,PARs_PH_eh_bands
 use QP_m,           ONLY:QP_G_damp
 use LIVE_t,         ONLY:live_timing
 use PHEL,           ONLY:phel_info,PHEL_broad_factor,PHEL_gimme_Ks,PH_energy,&
&                         PHEL_N_trans_yes,PHEL_N_trans_tot,PHEL_nTrans_percent,PHEL_Q_Radius
 use X_m,            ONLY:X_DbGd_nkpt_at_q,X_DbGd_nkpt_at_q_table,X_Double_Grid_setup
 use timing_m,       ONLY:timing
 !
#include<memory.h>
 !
 type(bz_samp),     intent(in)    :: k,q
 integer,           intent(in)    :: bands(2),iq
 type(bz_fine_grid),intent(inout) :: k_FG
 type(E_fine_grid) ,intent(in)    :: E_FG
 !
 ! Work Space ...
 !
 logical               :: l_phel_allowed,l_cycle
 real(SP)              :: ph_freq
 integer               :: i_loop,i_trans
 character(schlen)     :: sec_name(2)
 ! ... band/branch index ...
 integer               :: il,i_n,i_np
 ! ... BZ index ...
 integer               :: ikbz,ik,ik_m_q,ik_rim,ip_rim
 ! ... energy related quantities ...
 real(SP)              :: DELTA_E,DELTA_W
 !
 ! Loop to allocate phel_table
 !=============================
 !
 sec_name(1)='Compatibility scan'
 sec_name(2)='Compatibility fill'
 do i_loop=1,2
   sec_name(i_loop)=trim(sec_name(i_loop))//"@Q"//trim(intc(iq))
 enddo
 !
 call timing("Compatibility scan and fill",OPR='start')
 !
 do i_loop=1,2
   !
   if (i_loop==2) then
     if (phel_info%N==0) call error('No allowed scattering found')
     YAMBO_ALLOC(phel_info%table,(phel_info%N,5) )
   endif
   !
   call live_timing(trim(sec_name(i_loop)),PARs_PH_K_bz%N_par)
   !
   phel_info%N          =0
   PHEL_N_trans_yes(iq) =0
   PHEL_N_trans_tot(iq) =0
   ! 
   ! Main loop over iq in the ibz
   !==============================
   call X_Double_Grid_setup(iq,k,k_FG,nTrans_percent=PHEL_nTrans_percent(iq),Radius=PHEL_Q_Radius(iq))
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
&                               abs(DELTA_E+ph_freq)<PHEL_broad_factor*QP_G_damp
             endif
             !
             if (l_phel_allowed) then
               !
               phel_info%N          = phel_info%N+1
               PHEL_N_trans_yes(iq) = PHEL_N_trans_yes(iq)+1
               !
               if (i_loop==2) phel_info%table(phel_info%N,:)= (/il,ik_rim,ip_rim,i_n,i_np/)
               !
             endif
             !
             PHEL_N_trans_tot(iq) = PHEL_N_trans_tot(iq)+1
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
   call live_timing( )
   !
 enddo ! i_loop
 !
 call timing("Compatibility scan and fill",OPR='stop')
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
