!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine PH_Grid_Check_Transitions(k,q,bands,k_FG,E_FG)
 !
 ! Select the indices of the transitions Delta_E_{n,np,k,k_m_q} = ph_E_{ql} +- 3*QP_G_damp
 ! which actually contribute to the scattering. 
 ! This change makes the code much faster as it avoids the use of all
 ! double-grid kpts in the two spheres (k and kp).
 !
 use pars,           ONLY:SP
 use units,          ONLY:HA2EV
 use electrons,      ONLY:E_fine_grid
 use hamiltonian,    ONLY:B_mat_index
 use R_lattice,      ONLY:bz_samp,qindx_X,bz_fine_grid,nqibz
 use real_time,      ONLY:l_RT_uses_E_FineGd,RT_eh_en
 use ELPH,           ONLY:PH_freqs_sq,elph_branches,PH_acoustic_branch,&
&                         l_GKKP_expanded_DB_exists,l_GKKP_DB_exists
 use parallel_m,     ONLY:PARs_PH_K_bz,PARs_PH_eh_bands,PARs_PH_Q_ibz
 use parallel_int,   ONLY:PP_redux_wait
 use QP_m,           ONLY:QP_G_damp
 use LIVE_t,         ONLY:live_timing
 use PHEL,           ONLY:l_PH_uses_E_FineGd,phel_info
 use com,            ONLY:msg
 use stderr,         ONLY:intc
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
 logical               :: l_phel_allowed
 real(SP)              :: ph_freq,Delta_Broad,ratio
 real(SP)              :: broad_factor=3._SP
 integer               :: i_loop
 ! ... band/branch index ...
 integer               :: il,i_n,i_np
 ! ... BZ index ...
 integer               :: iq,ikbz,ik,ik_m_q,ik_rim,ip_rim,N_yes(nqibz),N_tot(nqibz)
 ! ... energy related quantities ...
 real(SP) :: DELTA_E,DELTA_W
 !
 call section("+","Phonon mediated scattering analysis")
 !
 ! Loop to allocate phel_table
 !=============================
 do i_loop=1,2
   !
   !
   if (i_loop==1) then
     call live_timing('Compatibility scan',PARs_PH_Q_ibz%N_par*PARs_PH_K_bz%N_par)
   else
     if (phel_info%N==0) call error('No allowed scattering found')
     YAMBO_ALLOC(phel_info%table,(phel_info%N,7) )
     call live_timing('Compatibility fill',PARs_PH_Q_ibz%N_par*PARs_PH_K_bz%N_par)
   endif
   !
   phel_info%N = 0
   N_tot       = 0
   N_yes       = 0
   ! 
   ! Main loop over iq in the ibz
   !==============================
   do iq=1,nqibz
     !
     if (.not.PARs_PH_Q_ibz%IND%element_1D(iq)) cycle
     !
     ! Inner loop over ik in the bz
     !==============================
     do ikbz=1,k%nbz
       !
       if (.not.PARs_PH_K_bz%IND%element_1D(ikbz)) cycle
       !
#if defined _RT
       ! Obtain k-q in the ibz from k in the bz and q in the ibz: 
       ik_m_q =k%sstar(qindx_X(iq,ikbz,1),1)
       ! Obtain k in the ibz from k in the bz: 
       ik = k%sstar(ikbz,1)
#else
       ! Obtain k-q in the bz from k in the bz and q in the ibz
       ik_m_q = qindx_X(iq,ikbz,1)
       ! Use k in the bz
       ik = ikbz
#endif
       !
       ! Loop over il, i.e., phonon branches
       !====================================    
       do il=elph_branches(1),elph_branches(2)
         !
         if (l_GKKP_expanded_DB_exists) ph_freq=abs(sqrt(PH_freqs_sq(q%k_table(iq,1),il)))
         if (l_GKKP_DB_exists         ) ph_freq=abs(sqrt(PH_freqs_sq(iq,il)))
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
             ! ... when NOT using the fine grid ...
             if (.not. l_RT_uses_E_FineGd .and. .not. l_PH_uses_E_FineGd) then
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
             ! Loops over fine grid kpoints ik_rim and ip_rim
             !================================================    
             do ik_rim=k_FG%k_range(ik,1),k_FG%k_range(ik,2)
               !
               do ip_rim=k_FG%k_range(ik_m_q,1),k_FG%k_range(ik_m_q,2)
                 !
                 ! ... when using the fine grid ...
                 if (l_RT_uses_E_FineGd .or. l_PH_uses_E_FineGd) then
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
                 ! Select energy-allowed transitions
                 !=================================== 
                 Delta_Broad = broad_factor*QP_G_damp+DELTA_W
                 l_phel_allowed = (DELTA_E >  ph_freq - Delta_Broad .and. DELTA_E <  ph_freq + Delta_Broad ) .or. &
&                                 (DELTA_E > -ph_freq - Delta_Broad .and. DELTA_E < -ph_freq + Delta_Broad )
                 !
                 if (l_phel_allowed) then
                   !
                   phel_info%N  = phel_info%N+1
                   N_yes(iq)    = N_yes(iq)+1
                   !
                   if (i_loop==2) phel_info%table(phel_info%N,:)= (/il,iq,ikbz,ik_rim,ip_rim,i_n,i_np/)
                   !
                 endif
                 !
                 N_tot(iq) = N_tot(iq)+1
                 !
               enddo ! ip_rim i.e. (mk-q)
               !
             enddo ! ik_rim i.e. (nk)
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
   enddo ! iq
   !
   call live_timing( )
   !
 enddo ! i_loop
 !
 ! Output info
 !===============
 do iq=1,nqibz
   ratio = 0._SP
   if (N_tot(iq)>0) ratio = float(N_yes(iq))/float(N_tot(iq))*100._SP
   call msg('r','DbGd@q['//trim(intc(iq))//'] k->k-q (trans %, broad eV)]',(/ratio,broad_factor*QP_G_damp*HA2EV/))
 enddo
 !
 N_yes=phel_info%N
 N_tot(1)=sum(N_tot)
 call PP_redux_wait(N_yes)
 call PP_redux_wait(N_tot)
 ratio = 0._SP
 if (N_tot(1)>0) ratio = float(N_yes(1))/float(N_tot(1))*100._SP
 call msg('nrsn','Total DbGd k->k-q (trans %, broad eV)]',(/ratio,broad_factor*QP_G_damp*HA2EV/))
 !
end subroutine PH_Grid_Check_Transitions
