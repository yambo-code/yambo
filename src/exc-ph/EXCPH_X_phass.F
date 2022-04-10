!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AC
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
! This subroutine is a simple version do not use double-grid
! neigther better integrations at gamma
! 
!> @brief Calculate X(w) including ph-assisted transitions
!
! @param[in]       wv                  frequencies range
! @param[in]       EXCPH_gkkp          read from file
! @param[in]       PH_E                read from file
! @param[in]       Bose_Temp           Bose temperature for phonons
! @param[in]       Boltz_Temp          Boltz temperature for excitons
!
! @param[out]      Epsilon_ii          Phonon-assisted dielectric constant
! @param[out]      PL_ii               Phonon-assisted luminescence
!
subroutine EXCPH_X_phass(wv,q,Epsilon_ii,PL_ii)
 !
 use frequency,     ONLY:w_samp
 use vec_operate,   ONLY:c2a
 use pars,          ONLY:SP,cZERO,rZERO,schlen
 use units,         ONLY:HA2EV,HA2CMm1
 use IO_m,          ONLY:NONE,OP_APP_CL,OP_RD_CL,DUMP,OP_RD,RD_CL_IF_END,REP,VERIFY
 use IO_int,        ONLY:io_control
 use EXCPH,         ONLY:EXCPH_states,EXCPH_sum,BS_Sat_WEIGHT,EXCPH_Renorm,BS_all_E,BS_R,BS_Sat_W_DG,&
&                        BS_Sat_WEIGHT_PH_abs,BS_E_in,EXCPH_Renorm_PH_abs,Sat_free,min_EXC_E,EXC_E_DbGd,min_EXC_E_DbGd, &
&                        BS_Sat_W_DG_PH_abs,EXCPH_Renorm_PL,EXCPH_Renorm_PL_PH_abs,Renorm_alloc,Renorm_free,DB_Q_map
 use ELPH,          ONLY:use_PH_DbGd,elph_branches,FAN_deltaE_treshold,ph_modes
 use com,           ONLY:msg
 use R_lattice,     ONLY:bz_samp,bz_samp_reset
 use D_lattice,     ONLY:nsym
 use functions,     ONLY:boltzman_f,bose_f
 use LIVE_t,        ONLY:live_timing
 use stderr,        ONLY:intc
 use parser_m,      ONLY:parser
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)  :: q
 type(w_samp),  intent(in)  :: wv
 complex(SP),   intent(out) :: Epsilon_ii(wv%n_freqs,2),PL_ii(wv%n_freqs,2)
 !
 ! Work space
 !
 complex(SP)   :: pole,W,Sat_alpha_abs,Sat_alpha_emiss
 real(SP)      :: min_E,exc_E,E_alpha,PH_E
 integer       :: i_alpha,i_beta,il,iq_bz,iw,io_err,ID_exc_gkkp,ID_gkkp,min_pos(1)
 logical       :: l_DbGd_WEIGHTs
 integer       :: iq_fg,iq_idx,iq_db
 integer       :: elph_branches_save(2)
 integer, external :: io_EXCPH_gkkp,io_ELPH
 !
 call parser('DbGdWEIGHTs',l_DbGd_WEIGHTs)
 !
 min_pos  =minloc(BS_all_E(1,:))
 min_EXC_E=minval(BS_all_E(1,:))
 !
 call msg('rs','Minimum excitonic energy : ',min_EXC_E*HA2EV,' eV, at q-point :'//intc(min_pos(1)))
 call msg('rs','FAN treshold: ',FAN_deltaE_treshold*HA2CMm1,' cm-1')
 !
 ! Load the Exciton-phonon elements and phonon energies
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),MODE=DUMP,ID=ID_exc_gkkp)
 io_err=io_EXCPH_gkkp(ID_exc_gkkp,'satellites')
 if(io_err/=0) call error('excph_gkkp databases not found!')
 !
 elph_branches_save=elph_branches
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),MODE=DUMP,ID=ID_gkkp)
 io_err=io_ELPH(ID_gkkp,'gkkp_expanded no_matrix_elements')
 if(io_err/=0) call error('gkkp_expanded databases not found!')
 elph_branches=elph_branches_save
 !
 ! The satellites
 !
 call Renorm_alloc()
 !
 Epsilon_ii            =cZERO
 PL_ii                 =cZERO
 !
 call live_timing('Satellites',q%nibz)
 !
 do iq_bz=1,q%nbz
   !
   call io_control(ACTION=RD_CL_IF_END,COM=REP,SEC=(/1+iq_bz/),MODE=DUMP,ID=ID_exc_gkkp)
   io_err=io_EXCPH_gkkp(ID_exc_gkkp,'satellites')
   !
   iq_db=DB_Q_map(iq_bz)
   call io_control(ACTION=RD_CL_IF_END,COM=REP,SEC=(/1+iq_db/),MODE=DUMP,ID=ID_gkkp)
   io_err=io_ELPH(ID_gkkp,'gkkp_expanded no_matrix_elements')
   !
   ! Add the contribution of q-point transition to the renormalization factors
   ! for all excionts i_alpha
   !
   if(use_PH_DbGd.and.l_DbGd_WEIGHTs) then
     call Eval_Sat_Weights_DbGd()
   elseif(use_PH_DbGd.and..not.l_DbGd_WEIGHTs) then
     do iq_fg=q%FGbz%k_range(iq_bz,1),q%FGbz%k_range(iq_bz,2)
       iq_idx=iq_fg-q%FGbz%k_range(iq_bz,1)+1
       BS_Sat_W_DG(:,:,:,iq_idx)=BS_Sat_WEIGHT
       BS_Sat_W_DG_PH_abs(:,:,:,iq_idx)=BS_Sat_WEIGHT_PH_abs
     enddo
   endif
   !
   call Eval_Renorm_Factor()
   !
   do iw=1,wv%n_freqs
     !
     W=wv%p(iw)
     !
     do i_alpha=EXCPH_states(1),EXCPH_states(2)
       !
       ! Satellites for the alpha exciton
       !
       do il=elph_branches(1),elph_branches(2)
         !
         do i_beta=EXCPH_sum(1),EXCPH_sum(2)
           !
           if(.not.use_PH_DbGd) then
             !      
             ! Calcualte the Satellite on the course q-grid
             !
             call Satellite_Simple(iq_bz,il,i_alpha,i_beta,W,  Sat_alpha_abs,Sat_alpha_emiss)
             !
           else
             !      
             ! Use the double-grid to average denominators
             !
             call Satellite_DbGrid(iq_bz,il,i_alpha,i_beta,W,q,Sat_alpha_abs,Sat_alpha_emiss)
             !
           endif
           !
           ! Add the satellites to the dielectric constant and luminescence
           !
           Epsilon_ii(iw,1)=Epsilon_ii(iw,1)+Sat_alpha_abs  *BS_R(i_alpha)
           PL_ii(iw,1)     =PL_ii(iw,1)     +Sat_alpha_emiss*BS_R(i_alpha)
           !
         enddo
         !
       enddo
       !
     enddo   
     !
   enddo   
   !
   call live_timing(steps=1)
   !
 enddo   
 !
 call live_timing( )
 !
 EXCPH_Renorm          = EXCPH_Renorm/real(q%nbz,SP)
 EXCPH_Renorm_PH_abs   = EXCPH_Renorm_PH_abs/real(q%nbz,SP)
 EXCPH_Renorm_PL       = EXCPH_Renorm_PL/real(q%nbz,SP)
 EXCPH_Renorm_PL_PH_abs= EXCPH_Renorm_PL_PH_abs/real(q%nbz,SP)
 !
 call msg('sr','Renormalization factors at finite temperature [Absorption]:')
 do i_alpha=EXCPH_states(1),EXCPH_states(2)
   call msg('sr','Exciton '//intc(i_alpha)//': ',(/EXCPH_Renorm(i_alpha),EXCPH_Renorm_PH_abs(i_alpha)/))
 enddo
 call msg('sr','Renormalization factors at finite temperature [Emission]:')
 do i_alpha=EXCPH_states(1),EXCPH_states(2)
   call msg('sr','Exciton '//intc(i_alpha)//': ',(/EXCPH_Renorm_PL(i_alpha),EXCPH_Renorm_PL_PH_abs(i_alpha)/))
 enddo
 !
 ! The term without satellites
 !
 min_E=min_EXC_E
 if(use_PH_DbGd) min_E=min_EXC_E_DbGd
 !
 do iw=1,wv%n_freqs
   !
   W=wv%p(iw)
   !
   do i_alpha=EXCPH_states(1),EXCPH_states(2)
     !
     pole =BS_E_in(i_alpha)
     Epsilon_ii(iw,:)=Epsilon_ii(iw,:)+(1._SP-EXCPH_Renorm(i_alpha)-EXCPH_Renorm_PH_abs(i_alpha))*BS_R(i_alpha)/(W-pole)
     !
     ! [CA] The pole of luminescence should be the pole of BS_E_in 
     !      but I think this is not consistent with the Boltzman weight, I use always Lout.
     ! [FP] Luminescence in RS (detailed balance) assumption requires Lin=Lout.
     !      Maybe put a check/logical enforcing this for PL? 
     !
     exc_E=BS_all_E(i_alpha,1)-min_E
     pole =BS_all_E(i_alpha,1)          
     PL_ii(iw,:)     =PL_ii(iw,:)     +(1._SP-EXCPH_Renorm_PL(i_alpha)-EXCPH_Renorm_PL_PH_abs(i_alpha))*BS_R(i_alpha) &
&                                      /(W-pole)*boltzman_f(exc_E)
     !
   enddo
   !
 enddo
 !
 ! Eps(w)=1._SP - X(w) in agreement with definition in src/bse/
 !
 Epsilon_ii(:,:)=1._SP-Epsilon_ii(:,:)
 PL_ii         =-PL_ii
 !
 ! Free
 !
 call Sat_free()
 call Renorm_free()
 !
 contains 
   !
   subroutine Eval_Sat_Weights_DbGd()
     use EXCPH,     ONLY:BS_Sat_E
     implicit none
     !
     real(SP) :: E_beta,bose_factor,EXC_gkkp_sq
     integer  :: iq_fg,iq_idx
     real(SP), external :: get_ph_E_DbGd
     !
     BS_Sat_W_DG       =rZERO
     BS_Sat_W_DG_PH_abs=rZERO
     !
     do i_alpha=EXCPH_states(1),EXCPH_states(2)
       !
       E_alpha =BS_E_in(i_alpha)
       !
       do i_beta=EXCPH_sum(1),EXCPH_sum(2)
         !
         do il=elph_branches(1),elph_branches(2)
           !
           ! calculate for all point of the double-grid
           ! 
           EXC_gkkp_sq=BS_Sat_WEIGHT(il,i_beta,i_alpha)*abs(BS_Sat_E(il,i_beta,i_alpha))
           ! 
           do iq_fg=q%FGbz%k_range(iq_bz,1),q%FGbz%k_range(iq_bz,2)
             !
             ph_E=get_ph_E_DbGd(il,iq_fg,iq_bz,q)
             !
             iq_idx=iq_fg-q%FGbz%k_range(iq_bz,1)+1
             !
             bose_factor=bose_f(PH_E)
             !  
             ! Excitonic energies in the double-grid
             !
             E_beta  =EXC_E_DbGd(i_beta,iq_fg)
             !
             if(ph_E<FAN_deltaE_treshold) cycle ! exclude addition phonon modes with low energy in the DbGd
             !
             if(abs(E_beta-E_alpha+ph_E)>FAN_deltaE_treshold) &
&                  BS_Sat_W_DG(il,i_beta,i_alpha,iq_idx)       =EXC_gkkp_sq/abs(E_beta-E_alpha+ph_E)**2
             !
             if(abs(E_beta-E_alpha-ph_E)>FAN_deltaE_treshold) &
&                  BS_Sat_W_DG_PH_abs(il,i_beta,i_alpha,iq_idx)=EXC_gkkp_sq/abs(E_beta-E_alpha-ph_E)**2
             ! 
           enddo
           !
         enddo  ! il
       enddo  ! i_beta
     enddo ! i_alpha
     !
   end subroutine
   !
   subroutine Eval_Renorm_Factor()
     use ELPH,          ONLY:elph_branches,PH_freqs_sq
     implicit none
     !
     integer  :: iq_idx,iq_fg,nq_around,iq_db
     real(SP) :: N_bose,weight_dbgd
     real(SP), external :: get_ph_E_DbGd
     ! 
     if(.not.use_PH_DbGd) then
       !
       do il=elph_branches(1),elph_branches(2)
         !      
         iq_db=DB_Q_map(iq_bz)
         ph_E=sqrt(abs(PH_freqs_sq(iq_db,il)))
         if(ph_E<FAN_deltaE_treshold) cycle
         !
         N_bose=bose_f(PH_E)
         !
         do i_alpha=EXCPH_states(1),EXCPH_states(2)
           do i_beta=EXCPH_sum(1),EXCPH_sum(2)
              !
              ! Phonon emission in absorption [photon absorption / phonon emission]
              !
             EXCPH_Renorm(i_alpha)           = EXCPH_Renorm(i_alpha) + BS_Sat_WEIGHT(il,i_beta,i_alpha)*(1._SP+N_bose)
             !
             ! Phonon absorption in luminescence [photon emission / phonon absorption]
             !
             EXCPH_Renorm_PL_PH_abs(i_alpha) = EXCPH_Renorm_PL_PH_abs(i_alpha) + BS_Sat_WEIGHT(il,i_beta,i_alpha)*N_bose
             !
             ! Phonon absorption in absorption [photon absorption / phonon absorption]
             !
             EXCPH_Renorm_PH_abs(i_alpha)    = EXCPH_Renorm_PH_abs(i_alpha) + BS_Sat_WEIGHT_PH_abs(il,i_beta,i_alpha)*N_bose
             !
             ! Phonon emission in luminescence [photon emission / phonon emission]
             !
             EXCPH_Renorm_PL(i_alpha)        = EXCPH_Renorm_PL(i_alpha) + BS_Sat_WEIGHT_PH_abs(il,i_beta,i_alpha)*(1._SP+N_bose)
             !
           enddo
         enddo
         !
       enddo
       !
     else
       !      
       do i_alpha=EXCPH_states(1),EXCPH_states(2)
         do i_beta=EXCPH_sum(1),EXCPH_sum(2)
           do il=elph_branches(1),elph_branches(2)
             !      
             nq_around =q%FGbz%k_range(iq_bz,2)-q%FGbz%k_range(iq_bz,1) ! Number of fine q-points around iq_bz
             weight_dbgd=1._SP/real(nq_around+1._SP,SP)                  ! Total weight of the DbGd including the iq_bz
             !
             do iq_fg=q%FGbz%k_range(iq_bz,1),q%FGbz%k_range(iq_bz,2)
               !
               ph_E=get_ph_E_DbGd(il,iq_fg,iq_bz,q)
               iq_idx=iq_fg-q%FGbz%k_range(iq_bz,1)+1
               !
               if(ph_E<FAN_deltaE_treshold) cycle
               !
               N_bose=bose_f(PH_E)
               !
               ! In the double grid case
               !
               ! BS_Sat_W_DG = Gkk^2/(E_a-E_b-w)^2
               !
               ! See above no-DG case for physical interpretation of factors
               !
               EXCPH_Renorm(i_alpha)          =EXCPH_Renorm(i_alpha)          +BS_Sat_W_DG(il,i_beta,i_alpha,iq_idx)        &
&                                                                             *(1._SP+N_bose)*weight_dbgd
               EXCPH_Renorm_PH_abs(i_alpha)   =EXCPH_Renorm_PH_abs(i_alpha)   +BS_Sat_W_DG_PH_abs(il,i_beta,i_alpha,iq_idx) &
&                                                                             *N_bose*weight_dbgd
               EXCPH_Renorm_PL(i_alpha)       =EXCPH_Renorm_PL(i_alpha)       +BS_Sat_W_DG_PH_abs(il,i_beta,i_alpha,iq_idx) &
&                                                                             *(1._SP+N_bose)*weight_dbgd
               EXCPH_Renorm_PL_PH_abs(i_alpha)=EXCPH_Renorm_PL_PH_abs(i_alpha)+BS_Sat_W_DG(il,i_beta,i_alpha,iq_idx)        &
&                                                                             *N_bose*weight_dbgd
               !
             enddo
             !
           enddo
         enddo
       enddo
       !
     endif
     !
   end subroutine
 !
end subroutine EXCPH_X_phass
