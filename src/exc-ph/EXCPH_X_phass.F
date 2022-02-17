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
 use units,         ONLY:HA2EV
 use IO_m,          ONLY:NONE,OP_APP_CL,OP_RD_CL,DUMP,OP_RD,RD_CL_IF_END,REP,VERIFY
 use IO_int,        ONLY:io_control
 use EXCPH,         ONLY:EXCPH_states,EXCPH_sum,BS_Sat_WEIGHT,EXCPH_Renorm,BS_all_E,BS_R,ID_EXC,ID_EXC_obj,&
&                        BS_Sat_WEIGHT_PH_abs,BS_E_in,EXCPH_Renorm_PH_abs,Sat_free,min_EXC_E,EXC_E_DbGd,min_EXC_E_DbGd
 use ELPH,          ONLY:use_PH_DbGd,E_kpq_sh_fact,elph_branches
 use interpolate,   ONLY:INTERPOLATE_is_serial,INTERPOLATION_coefficients,INTERP_obj,INTERP_shell_factor,&
&                        INTERPOLATE_is_quiet,INTERPOLATION_BZ
 use com,           ONLY:msg
 use R_lattice,     ONLY:bz_samp,bz_samp_reset
 use functions,     ONLY:boltzman_f,bose_f
 use LIVE_t,        ONLY:live_timing
 use stderr,        ONLY:intc
 !
#include<memory.h>
 !
 type(w_samp),  intent(in)  :: wv
 type(bz_samp), intent(in)  :: q
 complex(SP),   intent(out) :: Epsilon_ii(wv%n_freqs,2),PL_ii(wv%n_freqs,2)
 !
 ! Work space
 !
 complex(SP)   :: pole,W,Sat_alpha_abs,Sat_alpha_emiss
 real(SP)      :: min_E,exc_E,E_alpha,bose_factor,PH_E,v(3)
 type(bz_samp) :: q_DbGd
 integer       :: i_alpha,i_beta,il,iq,iw,io_err,ID_exc_gkkp,ID_gkkp,min_pos(1)
 integer       :: ID_E_kpq,ID_E_kpq_obj,ID_DG,min_pos_E_DbGd(1)
 integer, external :: io_EXCPH_gkkp,io_ELPH
 character(schlen) :: q_point
 !
 min_pos  =minloc(BS_all_E(1,:))
 min_EXC_E=minval(BS_all_E(1,:))
 call msg('rs','Minimum excitonic energy : ',min_EXC_E*HA2EV,' eV, at q-point :'//intc(min_pos(1)))
 !
 ! Load the Exciton-phonon elements and phonon energies
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=DUMP,ID=ID_exc_gkkp)
 io_err=io_EXCPH_gkkp(ID_exc_gkkp,'satellites')
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=DUMP,ID=ID_gkkp)
 io_err=io_ELPH(ID_gkkp,'gkkp_expanded no_matrix_elements')
 !
 YAMBO_ALLOC(EXCPH_Renorm,(EXCPH_states(2)))
 YAMBO_ALLOC(EXCPH_Renorm_PH_abs,(EXCPH_states(2)))
 !
 if(use_PH_DbGd) then
   !      
   call msg('sr','EXC_E(k+q) calculated using a smooth Fourier interpolation')
   INTERPOLATE_is_serial=.TRUE.
   INTERPOLATE_is_quiet =.TRUE.
   INTERP_shell_factor  =E_kpq_sh_fact
   call INTERPOLATION_BZ_setup(q)
   INTERP_obj(ID_E_kpq_obj)%what="EXC_E(k+q)"
   call INTERPOLATION_coefficients(R1D=BS_all_E,k=q,ID=ID_EXC,ID_obj=ID_EXC_obj)
   !
   call bz_samp_reset(q_DbGd)
   q_DbGd%nibz=q%FGbz%N
   YAMBO_ALLOC(q_DbGd%pt,(q_DbGd%nibz,3))
   q_DbGd%pt=q%FGbz%pt
   !
   YAMBO_ALLOC(EXC_E_DbGd,(EXCPH_states(2),q%FGbz%N))
   call INTERPOLATION_BZ(K=q_DbGd,NK=q%FGbz%N,ID=ID_E_kpq,R1D=EXC_E_DbGd)
   !
   min_EXC_E_DbGd=minval(EXC_E_DbGd(1,:))
   min_pos_E_DbGd=minloc(EXC_E_DbGd(1,:))
   call c2a(v_in=q_DbGd%pt(min_pos_E_DbGd,:),v_out=v,mode="ki2a")
   call msg('rs','Minimum excitonic energy with DbGrid: ',min_EXC_E_DbGd*HA2EV,' eV')
   write(q_point,'(3f12.6,a)') v(:),' [rlu] '
   call msg('rs','Minimum q-point in the DbGrid : '//q_point)
   !
 endif
 !
 ! The satellites
 !
 Epsilon_ii         =cZERO
 PL_ii              =cZERO
 EXCPH_Renorm       =rZERO
 EXCPH_Renorm_PH_abs=rZERO
 !
 call live_timing('Satellites',q%nibz)
 !
 do iq=1,q%nibz
   !
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1+iq/),MODE=DUMP,ID=ID_exc_gkkp)
   io_err=io_EXCPH_gkkp(ID_exc_gkkp,'satellites')
   !
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1+iq/),MODE=DUMP,ID=ID_gkkp)
   io_err=io_ELPH(ID_gkkp,'gkkp_expanded no_matrix_elements')
   !
   ! Add the contribution of q-point transition to the renormalization factors
   ! for all excionts i_alpha
   !
   call Eval_Renorm_Factor(iq)
   !
   do iw=1,wv%n_freqs
     !
     W=wv%p(iw)
     !
     do i_alpha=EXCPH_states(1),EXCPH_states(2)
       !
       ! Satellites for the alpha exciton
       !
       Sat_alpha_abs  =cZERO
       Sat_alpha_emiss=cZERO
       !
       do il=elph_branches(1),elph_branches(2)
         !
         do i_beta=EXCPH_sum(1),EXCPH_sum(2)
           !
           if(use_PH_DbGd) then
             call Satellite_DbGrid(iq,il,i_alpha,i_beta,W,Sat_alpha_abs,Sat_alpha_emiss)
           else
             call Satellite_Simple(iq,il,i_alpha,i_beta,W,Sat_alpha_abs,Sat_alpha_emiss)
           endif
           !
         enddo
         !
       enddo
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
   call live_timing(steps=1)
   !
 enddo   
 !
 call live_timing( )
 !
 EXCPH_Renorm       = EXCPH_Renorm/real(q%nbz,SP)
 EXCPH_Renorm_PH_abs= EXCPH_Renorm_PH_abs/real(q%nbz,SP)
 !
 call msg('sr','Renormalization factors at finite temperature:')
 do i_alpha=EXCPH_states(1),EXCPH_states(2)
   call msg('sr','Exciton '//intc(i_alpha)//': ',(/EXCPH_Renorm(i_alpha),EXCPH_Renorm_PH_abs(i_alpha)/))
 enddo
 !
 ! The term without satellites
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
     ! The pole of luminescence should be the pole of BS_E_in 
     ! but I think this is not consistent with the Boltzman weight, I use always Lout. Point to be discussed
     !
     exc_E=BS_all_E(i_alpha,1)-min_EXC_E
     pole =BS_all_E(i_alpha,1)          
     PL_ii(iw,:)     =PL_ii(iw,:)     +(1._SP-EXCPH_Renorm(i_alpha)-EXCPH_Renorm_PH_abs(i_alpha))*BS_R(i_alpha) &
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
 YAMBO_FREE(EXCPH_Renorm)
 YAMBO_FREE(EXCPH_Renorm_PH_abs)
 !
 contains 
   !
   subroutine Eval_Renorm_Factor(iq_in)
     use ELPH,          ONLY:FAN_deltaE_treshold,PH_freqs_sq
     implicit none
     integer, intent(in) :: iq_in
     !
     do il=elph_branches(1),elph_branches(2)
       !
       ph_E=sqrt(PH_freqs_sq(iq_in,il))
       bose_factor=bose_f(PH_E)
       if(ph_E<FAN_deltaE_treshold) cycle
       !
       do i_beta=EXCPH_sum(1),EXCPH_sum(2)
         forall(i_alpha=EXCPH_states(1):EXCPH_states(2))
           EXCPH_Renorm(i_alpha)        = EXCPH_Renorm(i_alpha)        + BS_Sat_WEIGHT(il,i_beta,i_alpha)*(1._SP+bose_factor)
           EXCPH_Renorm_PH_abs(i_alpha) = EXCPH_Renorm_PH_abs(i_alpha) + BS_Sat_WEIGHT_PH_abs(il,i_beta,i_alpha)*bose_factor
         end forall
       enddo
       !
     enddo
     !
   end subroutine
 !
end subroutine EXCPH_X_phass
