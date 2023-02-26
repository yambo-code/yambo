!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): FP AC
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
!> @brief Calculate PL(w) satellites including ph-assisted transitions
!
! @param[in]       wv                  frequencies range
! @param[in]       EXCPH_gkkp          read from file
! @param[in]       Bose_Temp           Bose temperature for phonons
! @param[in]       Boltz_Temp          Boltz temperature for excitons
!
! @param[out]      PL_ii               Satellites of phonon-assisted luminescence
!
! This is only for testing and so far only works for
! - photon emission
! - phonon emission
! - satellites only (no main peak renormalisation)
! - no double grid
! 
subroutine EXCPH_X_phass_from_Hpert(wv,q,PL_ii)
 !
 use frequency,     ONLY:w_samp
 use vec_operate,   ONLY:c2a
 use pars,          ONLY:pi,SP,cZERO,rZERO,schlen
 use units,         ONLY:HA2EV,HA2CMm1
 use IO_m,          ONLY:manage_action,NONE,OP_APP_CL,OP_RD_CL,DUMP,OP_RD,RD_CL_IF_END,REP,VERIFY
 use IO_int,        ONLY:io_control
 use EXCPH,         ONLY:EXCPH_states,EXCPH_sum,BS_all_E,BS_R,EXC_occ,EXCPH_Gkkp,&
&                        BS_E_in,Sat_free,min_EXC_E,BS_R_left,BS_Sat_E_PH_abs,BS_Sat_E
 use ELPH,          ONLY:ph_modes,elph_branches,FAN_deltaE_treshold,Q_io_map,PH_freqs_sq
 use parallel_m,    ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,  ONLY:PP_redux_wait,PARALLEL_index
 use R_lattice,     ONLY:bz_samp,bz_samp_reset,d3k_factor,bare_qpg
 use functions,     ONLY:boltzman_f,bose_f
 use LIVE_t,        ONLY:live_timing
 use stderr,        ONLY:intc
 use electrons,     ONLY:spin_occ
 use parser_m,      ONLY:parser
 use com,           ONLY:msg,of_open_close
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)  :: q
 type(w_samp),  intent(in)  :: wv
 complex(SP),   intent(out) :: PL_ii(wv%n_freqs,3)
 !
 ! Work space
 !
 complex(SP)   :: pole,W,Sat_emission,Sat_absorption,EXCPH_gaux,BS_aux
 real(SP)      :: min_E,exc_E,ph_E,bose_factor,boltzm_factor,E_alpha,E_beta
 integer       :: i_alpha,i_beta,il,iq_bz,iw,io_err,ID_exc_gkkp,ID_gkkp,min_pos(1)
 integer       :: iq_db,iq_ibz,IO_ACT
 integer       :: elph_branches_save(2)
 type(PP_indexes)::px
 complex(SP),allocatable :: BS_Sat_AMPLITUDE(:,:), BS_Sat_AMPLITUDE_PH_abs(:,:)
 real(SP),allocatable :: BS_Sat_WEIGHT_sum(:,:), BS_Sat_WEIGHT_PH_abs_sum(:,:)
 logical :: l_no_matrix_elements
 !
 integer, external :: io_EXCPH_gkkp,io_ELPH
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
 io_err=io_EXCPH_gkkp(ID_exc_gkkp)
 if(io_err/=0) call error('excph_gkkp databases not found!')
 !
 elph_branches_save=elph_branches
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),MODE=DUMP,ID=ID_gkkp)
 io_err=io_ELPH(ID_gkkp,'gkkp_expanded no_matrix_elements')
 if(io_err/=0) call error('gkkp_expanded databases not found!')
 elph_branches=elph_branches_save
 !
 ! Allocate modified workspace variables
 !
 YAMBO_ALLOC(BS_Sat_WEIGHT_sum,(ph_modes,EXCPH_sum(1):EXCPH_sum(2)))
 YAMBO_ALLOC(BS_Sat_WEIGHT_PH_abs_sum,(ph_modes,EXCPH_sum(1):EXCPH_sum(2)))
 YAMBO_ALLOC(BS_Sat_AMPLITUDE,(ph_modes,EXCPH_sum(1):EXCPH_sum(2)))
 YAMBO_ALLOC(BS_Sat_AMPLITUDE_PH_abs,(ph_modes,EXCPH_sum(1):EXCPH_sum(2)))
 !
 BS_Sat_WEIGHT_sum            =rZERO 
 BS_Sat_WEIGHT_PH_abs_sum     =rZERO 
 BS_Sat_AMPLITUDE             =cZERO 
 BS_Sat_AMPLITUDE_PH_abs      =cZERO 
 PL_ii                        =cZERO
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/wv%n_freqs/)) 
 ! 
 call live_timing('Satellites',px%n_of_elements(myid+1)*q%nbz)
 !
 do iq_bz=1,q%nbz
   !
   IO_ACT=manage_action(RD_CL_IF_END,iq_bz,1,q%nbz)
   !
   call io_control(ACTION=IO_ACT,COM=REP,SEC=(/1+iq_bz/),MODE=DUMP,ID=ID_exc_gkkp)
   io_err=io_EXCPH_gkkp(ID_exc_gkkp)
   !
   iq_db=Q_io_map(iq_bz)
   call io_control(ACTION=IO_ACT,COM=REP,SEC=(/1+iq_db/),MODE=DUMP,ID=ID_gkkp)
   io_err=io_ELPH(ID_gkkp,'gkkp_expanded no_matrix_elements')
   !
   ! Add the contribution of q-point transition to the renormalization factors
   ! for all excionts i_alpha
   !
   do iw=1,wv%n_freqs
     !
     if (.not.px%element_1D(iw)) cycle
     !
     W=wv%p(iw) 
     !
     Sat_emission                 =cZERO
     Sat_absorption               =cZERO
     !
     do il=elph_branches(1),elph_branches(2) 
       !  
       do i_beta=EXCPH_sum(1),EXCPH_sum(2)
         !
         do i_alpha=EXCPH_states(1),EXCPH_states(2)
           !
           iq_ibz=q%sstar(iq_bz,1)
           !
           iq_db=Q_io_map(iq_bz)
           ph_E=sqrt(abs(PH_freqs_sq(iq_db,il)))
           !
           if(ph_E<FAN_deltaE_treshold) cycle
           !
           bose_factor=bose_f(ph_E)
           boltzm_factor=boltzman_f(BS_all_E(i_beta,iq_ibz)-min_EXC_E)
           !
           E_alpha=BS_E_in(i_alpha)
           E_beta =BS_all_E(i_beta,iq_ibz)
           !
           BS_Sat_E       (il,i_beta,i_alpha)=E_beta-E_alpha+ph_E
           BS_Sat_E_PH_abs(il,i_beta,i_alpha)=E_beta-E_alpha-ph_E
           pole=E_beta-ph_E
           !
           EXCPH_gaux=EXCPH_Gkkp(il,i_beta,i_alpha)
           BS_aux = BS_R_left(i_alpha)
           if(l_no_matrix_elements) EXCPH_gaux=(1._SP,0._SP)
           !
           if(abs(BS_Sat_E(il,i_beta,i_alpha))>FAN_deltaE_treshold) then
             BS_Sat_AMPLITUDE(il,i_beta)=BS_Sat_AMPLITUDE(il,i_beta)+&
&                                        BS_aux*EXCPH_gaux/abs(BS_Sat_E(il,i_beta,i_alpha))
           endif
           !
           if(abs(BS_Sat_E_PH_abs(il,i_beta,i_alpha))>FAN_deltaE_treshold) then
             BS_Sat_AMPLITUDE_PH_abs(il,i_beta)=BS_Sat_AMPLITUDE_PH_abs(il,i_beta)+&
&                                               BS_aux*EXCPH_gaux/abs(BS_Sat_E_PH_abs(il,i_beta,i_alpha))
           endif
           !
         enddo ! i_alpha
         !
         BS_Sat_WEIGHT_sum(il,i_beta)        = abs(BS_Sat_AMPLITUDE(il,i_beta))**2._SP
         BS_Sat_WEIGHT_PH_abs_sum(il,i_beta) = abs(BS_Sat_AMPLITUDE_PH_abs(il,i_beta))**2._SP
         !
         ! Phonon emission in luminescence [photon emission / phonon emission]
         !
         Sat_emission=Sat_emission+&
&                     BS_Sat_WEIGHT_PH_abs_sum(il,i_beta)/(W-cmplx(pole))/pole*(1._SP+bose_factor)*boltzm_factor
         !
       enddo ! i_beta
       !
     enddo ! i_l  
     !
     PL_ii(iw,1)     =PL_ii(iw,1)+(W+2.*ph_E)**2*Sat_emission/real(q%nbz,SP)
     PL_ii(iw,3)     =PL_ii(iw,3)+(W+2.*ph_E)**2*Sat_emission/real(q%nbz,SP)
     !
     call live_timing(steps=1)
     !
   enddo ! iw   
   !
 enddo ! i_q_bz
 !
 call live_timing( )
 !
 do iw=1,wv%n_freqs
   if (.not.px%element_1D(iw)) cycle   
   W=wv%p(iw)
   PL_ii(iw,:)=-PL_ii(iw,:)*real(spin_occ,SP)/(2._SP*pi)**3*d3k_factor*4._SP*pi/bare_qpg(1,1)**2
 enddo
 !
 call PP_redux_wait(PL_ii) 
 !
 ! Free
 !
 YAMBO_FREE(BS_Sat_WEIGHT_sum)
 YAMBO_FREE(BS_Sat_WEIGHT_PH_abs_sum)
 YAMBO_FREE(BS_Sat_AMPLITUDE)
 YAMBO_FREE(BS_Sat_AMPLITUDE_PH_abs)
 !
 call PP_indexes_reset(px)
 call Sat_free()
 !
end subroutine EXCPH_X_phass_from_Hpert

