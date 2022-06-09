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
 use EXCPH,         ONLY:EXCPH_states,EXCPH_sum,BS_Sat_WEIGHT,EXCPH_Renorm,BS_all_E,BS_R,&
&                        BS_Sat_WEIGHT_PH_abs,BS_E_in,EXCPH_Renorm_PH_abs,Sat_free,min_EXC_E,min_EXC_E_DbGd, &
&                        EXCPH_Renorm_PL,EXCPH_Renorm_PL_PH_abs,Renorm_alloc,Renorm_free,l_DbGd_WEIGHTs
 use ELPH,          ONLY:use_PH_DbGd,elph_branches,FAN_deltaE_treshold,Q_io_map,PH_freqs_sq
 use parallel_m,    ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,  ONLY:PP_redux_wait,PARALLEL_index
 use com,           ONLY:msg
 use R_lattice,     ONLY:bz_samp,bz_samp_reset
 use functions,     ONLY:boltzman_f,bose_f
 use LIVE_t,        ONLY:live_timing
 use stderr,        ONLY:intc
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)  :: q
 type(w_samp),  intent(in)  :: wv
 complex(SP),   intent(out) :: Epsilon_ii(wv%n_freqs,3),PL_ii(wv%n_freqs,3)
 !
 ! Work space
 !
 complex(SP)   :: pole,W,Sat_alpha_abs,Sat_alpha_emiss
 real(SP)      :: min_E,exc_E,PH_E
 integer       :: i_alpha,i_beta,il,iq_bz,iw,io_err,ID_exc_gkkp,ID_gkkp,min_pos(1)
 integer       :: iq_db
 integer       :: elph_branches_save(2)
 type(PP_indexes)::px
 !
 real(SP), allocatable  :: max_Sat_weight(:)
 integer,  allocatable  :: max_Sat_pos(:,:)
 real(SP), allocatable  :: max_Sat_weight_PH_abs(:)
 integer,  allocatable  :: max_Sat_pos_PH_abs(:,:)
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
 ! Renormalization factors
 !
 call Renorm_alloc()
 !
 Epsilon_ii            =cZERO
 PL_ii                 =cZERO
 EXCPH_Renorm          =rZERO
 EXCPH_Renorm_PH_abs   =rZERO
 EXCPH_Renorm_PL       =rZERO
 EXCPH_Renorm_PL_PH_abs=rZERO
 !
 ! Max and min satellites Energies
 ! 
 YAMBO_ALLOC(max_Sat_weight,(EXCPH_states(1):EXCPH_states(2)))
 YAMBO_ALLOC(max_Sat_pos,(EXCPH_states(1):EXCPH_states(2),3))
 YAMBO_ALLOC(max_Sat_weight_PH_abs,(EXCPH_states(1):EXCPH_states(2)))
 YAMBO_ALLOC(max_Sat_pos_PH_abs,(EXCPH_states(1):EXCPH_states(2),3))
 !
 max_Sat_weight=rZERO
 max_Sat_pos   =0
 max_Sat_weight_PH_abs=rZERO
 max_Sat_pos_PH_abs   =0
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/wv%n_freqs/)) 
 ! 
 call live_timing('Satellites',px%n_of_elements(myid+1)*q%nbz)
 !
 do iq_bz=1,q%nbz
   !
   call io_control(ACTION=RD_CL_IF_END,COM=REP,SEC=(/1+iq_bz/),MODE=DUMP,ID=ID_exc_gkkp)
   io_err=io_EXCPH_gkkp(ID_exc_gkkp)
   !
   iq_db=Q_io_map(iq_bz)
   call io_control(ACTION=RD_CL_IF_END,COM=REP,SEC=(/1+iq_db/),MODE=DUMP,ID=ID_gkkp)
   io_err=io_ELPH(ID_gkkp,'gkkp_expanded no_matrix_elements')
   !
   ! Add the contribution of q-point transition to the renormalization factors
   ! for all excionts i_alpha
   !
   call EXCPH_eval_sat(iq_bz,q)
   !
   if(use_PH_DbGd.and.l_DbGd_WEIGHTs)  call EXCPH_ave_sat_DbGrid(iq_bz,q)
   !
   call Find_Max_Sat()
   !
   call EXCPH_eval_renorm(iq_bz,q,EXCPH_Renorm,EXCPH_Renorm_PH_abs,EXCPH_Renorm_PL,EXCPH_Renorm_PL_PH_abs)
   !
   do iw=1,wv%n_freqs
     !
     if (.not.px%element_1D(iw)) cycle
     !
     W=wv%p(iw)
     !
     do i_alpha=EXCPH_states(1),EXCPH_states(2)
       !
       ! Satellites for the alpha exciton
       !
       do il=elph_branches(1),elph_branches(2)
         !
         iq_db=Q_io_map(iq_bz)
         ph_E=sqrt(abs(PH_freqs_sq(iq_db,il)))
         !
         do i_beta=EXCPH_sum(1),EXCPH_sum(2)
           !
           if(.not.use_PH_DbGd) then
             !      
             ! Calcualte the Satellite on the course q-grid
             !
             call Satellite_Simple(iq_bz,q,il,i_alpha,i_beta,W,Sat_alpha_abs,Sat_alpha_emiss)
             !
           else
             !      
             ! Use the double-grid to average denominators
             !
             call Satellite_DbGrid(iq_bz,q,il,i_alpha,i_beta,W,Sat_alpha_abs,Sat_alpha_emiss)
             !
           endif
           !
           ! Add the satellites to the dielectric constant and luminescence
           !
           Epsilon_ii(iw,1)=Epsilon_ii(iw,1)+Sat_alpha_abs*BS_R(i_alpha)/real(q%nbz,SP)
           Epsilon_ii(iw,3)=Epsilon_ii(iw,3)+Sat_alpha_abs*BS_R(i_alpha)/real(q%nbz,SP)
           !
           PL_ii(iw,1)     =PL_ii(iw,1)+(W+2.*ph_E)**2*Sat_alpha_emiss*BS_R(i_alpha)/real(q%nbz,SP)
           PL_ii(iw,3)     =PL_ii(iw,3)+(W+2.*ph_E)**2*Sat_alpha_emiss*BS_R(i_alpha)/real(q%nbz,SP)
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
 enddo   
 !
 call live_timing( )
 !
 call Sat_report()
 !
 call msg('sr','Renormalization factors at finite temperature [Absorption]:')
 call msg('sr','        [ph-emission]   [ph-absorption]')
 do i_alpha=EXCPH_states(1),EXCPH_states(2)
   call msg('sr','Exciton '//trim(intc(i_alpha))//': ',(/EXCPH_Renorm(i_alpha),EXCPH_Renorm_PH_abs(i_alpha)/))
 enddo
 call msg('sr','Renormalization factors at finite temperature [Emission]:')
 call msg('sr','        [ph-emission]   [ph-absorption]')
 do i_alpha=EXCPH_states(1),EXCPH_states(2)
   call msg('sr','Exciton '//trim(intc(i_alpha))//': ',(/EXCPH_Renorm_PL(i_alpha),EXCPH_Renorm_PL_PH_abs(i_alpha)/))
 enddo
 !
 ! The term without satellites
 !
 min_E=min_EXC_E
 if(use_PH_DbGd) min_E=min_EXC_E_DbGd
 !
 call live_timing('Epsilon',wv%n_freqs)
 do iw=1,wv%n_freqs
   !
   if (.not.px%element_1D(iw)) cycle   
   !
   W=wv%p(iw)
   !
   do i_alpha=EXCPH_states(1),EXCPH_states(2)
     !
     pole =BS_E_in(i_alpha)
     Epsilon_ii(iw,1:2)=Epsilon_ii(iw,1:2)+(1._SP-EXCPH_Renorm(i_alpha)-EXCPH_Renorm_PH_abs(i_alpha))*BS_R(i_alpha)/(W-pole)
     !
     ! [CA] The pole of luminescence should be the pole of BS_E_in 
     !      but I think this is not consistent with the Boltzman weight, I use always Lout.
     ! [FP] Luminescence in RS (detailed balance) assumption requires Lin=Lout.
     !      Maybe put a check/logical enforcing this for PL? 
     !
     exc_E=BS_E_in(i_alpha)-min_E
     pole =BS_E_in(i_alpha)          
     PL_ii(iw,1:2)     =PL_ii(iw,1:2)  + W**3*(1._SP-EXCPH_Renorm_PL(i_alpha)-EXCPH_Renorm_PL_PH_abs(i_alpha))*BS_R(i_alpha) &
&                                      /(W-pole)*boltzman_f(exc_E)
     !
   enddo
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing( )
 !
 !
 ! Eps(w)=1._SP - X(w) in agreement with definition in src/bse/
 !
 !
 do iw=1,wv%n_freqs
   if (.not.px%element_1D(iw)) cycle   
   W=wv%p(iw)
   Epsilon_ii(iw,:)=1._SP-Epsilon_ii(iw,:)
   PL_ii(iw,:)=-W*PL_ii(iw,:)
 enddo
 !
 call PP_redux_wait(Epsilon_ii)
 call PP_redux_wait(PL_ii) 
 !
 ! Free
 !
 call PP_indexes_reset(px)
 call Sat_free()
 call Renorm_free()
 !
 contains
 !
 subroutine Find_Max_Sat()
   implicit none
   !
   ! Work Space
   !
   integer :: i1
   !      
   ! Find the maximum weight of the satellites
   ! 
   do i1=EXCPH_states(1),EXCPH_states(2)
     if(max_Sat_weight(i1)<maxval(abs(BS_Sat_WEIGHT(:,:,i1)))) then
        max_Sat_weight(i1)=maxval(abs(BS_Sat_WEIGHT(:,:,i1)))
        max_Sat_pos(i1,1) =iq_bz
        max_Sat_pos(i1,2:3) =maxloc(abs(BS_Sat_WEIGHT(:,:,i1)))
     endif
     if(max_Sat_weight_PH_abs(i1)<maxval(abs(BS_Sat_WEIGHT_PH_abs(:,:,i1)))) then
        max_Sat_weight_PH_abs(i1)=maxval(abs(BS_Sat_WEIGHT_PH_abs(:,:,i1)))
        max_Sat_pos_PH_abs(i1,1) =iq_bz
        max_Sat_pos_PH_abs(i1,2:3) =maxloc(abs(BS_Sat_WEIGHT_PH_abs(:,:,i1)))
      endif
    enddo
    !
 end subroutine Find_Max_Sat
 !
 subroutine Sat_report()
   use stderr,        ONLY:intc
   use units,         ONLY:HA2EV
   use ELPH,          ONLY:ph_freqs_sq
   implicit none
   !
   ! work space
   !
   character(schlen) :: message
   real(SP)          :: ph_E
   integer           :: iq_bz,i1,iq_ibz
   !
   call msg('sr',' Max satellite weight: ')
   do i1=EXCPH_states(1),EXCPH_states(2)
     iq_bz  = max_Sat_pos(i1,1) 
     iq_db  =Q_io_map(iq_bz)
     iq_ibz =q%sstar(iq_bz,1)
     ph_E=sqrt(abs(ph_freqs_sq(iq_db,max_Sat_pos(i1,2))))
     write(message,*) 'Exciton '//trim(intc(i1))//' max(W_sat)=',max_Sat_weight(i1)
     call msg('sr',message)
     write(message,*) '   for iq = '//intc(max_Sat_pos(i1,1))//' il = '//intc(max_Sat_pos(i1,2))//'  ib = ' &
&                                     //intc(max_Sat_pos(i1,3)),' w(iq,il) = ',ph_E*HA2EV*1000._SP,'meV'
     call msg('sr',message)
     write(message,*) '   E(in) = ',real(BS_E_in(i1))*HA2EV, ' E(out)= ',BS_all_E(max_Sat_pos(i1,3),iq_ibz)*HA2EV
     call msg('sr',message)
   enddo
   !
   call msg('sr',' Max satellite weight (phonon absorption): ')
   do i1=EXCPH_states(1),EXCPH_states(2)
      iq_bz = max_Sat_pos_PH_abs(i1,1) !  q%k_table(max_Sat_pos(i1,1), 1)
      iq_db =Q_io_map(iq_bz)
      iq_ibz=q%sstar(iq_bz,1)
      ph_E=sqrt(abs(ph_freqs_sq(iq_db,max_Sat_pos_PH_abs(i1,2))))
      write(message,*) 'Exciton '//trim(intc(i1))//' max(W_sat)=',max_Sat_weight_PH_abs(i1)
      call msg('sr',message)
      write(message,*) '   for iq = '//intc(max_Sat_pos_PH_abs(i1,1))//' il = '//intc(max_Sat_pos_PH_abs(i1,2))//'  ib = ' &
&                                     //intc(max_Sat_pos_PH_abs(i1,3)),' w(iq,il) = ',ph_E*HA2EV*1000._SP,'meV'
     call msg('sr',message)
     write(message,*) '   E(in) = ',real(BS_E_in(i1))*HA2EV, ' E(out)= ',BS_all_E(max_Sat_pos(i1,3),iq_ibz)*HA2EV
     call msg('sr',message)
     enddo
    !
 end subroutine Sat_report
 !
end subroutine EXCPH_X_phass

