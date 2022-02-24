!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM FP AC
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
!> @brief Calculate exciton non-radiative life-time induced by phonon-scattering
!!
subroutine EXCPH_lifetime(E,k,q,X,wv)
 !
 use units,          ONLY:HA2EV,HA2CMm1
 use pars,           ONLY:SP,cZERO,rZERO,schlen,pi
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use parser_m,       ONLY:parser
 use com,            ONLY:msg,of_open_close
 use X_m,            ONLY:X_t
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:manage_action,OP_RD,REP,RD_CL_IF_END,DUMP
 use EXCPH,          ONLY:EXCPH_sum,EXCPH_states,BS_E,BS_all_E,BS_E_in, &
&                         alloc_EXC,free_EXC,EXC_LifeTime,min_EXC_E,EXCPH_Gkkp_sq 
 use ELPH,           ONLY:PH_freqs,use_PH_DbGd,elph_branches,FAN_deltaE_treshold
 use frequency,      ONLY:w_samp
 use stderr,         ONLY:intc
 !
#include<memory.h>
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 type(X_t)     ::X
 type(w_samp)  :: wv 
 !
 ! Work space
 !
 integer :: ID_exc_gkkp,ID_gkkp,io_err
 integer, external :: io_EXCPH_gkkp,io_ELPH
 integer :: iq_bz,i_alpha,i_beta,il
 integer :: min_pos(1)
 !
 call section('*','EXCPH-LifTime')
 !
 ! Load excitonic energies 
 !
 call EXCPH_load_L(1,X,'check','Lout')
 !
 ! Allocations 
 !
 call alloc_EXC(q)
 !
 ! Load excitonic energies 
 !
 call EXCPH_load_L(1,X,'eigenvalues','Lin')
 BS_E_in(:)=BS_E(:) ! L_in can be different from L_out 
 !
 do iq_bz=1,q%nibz
   call EXCPH_load_L(iq_bz,X,'eigenvalues','Lout')
   BS_all_E(:,iq_bz)=real(BS_E(:)) ! I suppose the BSE eigenvalues are real
 enddo
 !
 ! Load the Exciton-phonon elements and phonon energies
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),MODE=DUMP,ID=ID_exc_gkkp)
 io_err=io_EXCPH_gkkp(ID_exc_gkkp,'gkkp')
 if(io_err/=0) call error('excph_gkkp databases not found!')
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),MODE=DUMP,ID=ID_gkkp)
 io_err=io_ELPH(ID_gkkp,'gkkp_expanded no_matrix_elements')
 if(io_err/=0) call error('gkkp_expanded databases not found!')
 !
 ! Info on excitons
 ! 
 min_pos  =minloc(BS_all_E(1,:))
 min_EXC_E=minval(BS_all_E(1,:))
 !
 call msg('rs','Minimum excitonic energy : ',min_EXC_E*HA2EV,' eV, at q-point :'//intc(min_pos(1)))
 call msg('rs','FAN treshold: ',FAN_deltaE_treshold*HA2CMm1,' cm-1')
 call msg('rs','Self-energy broadening : ',wv%dr(1)*HA2EV/1000._SP,'meV')
 !
 ! Turn on double-grid if presents
 call EXCPH_double_grid_setup(q)
 !
 ! Calculate the life-times
 !
 YAMBO_ALLOC(EXC_LifeTime,(EXCPH_states(1):EXCPH_states(2)))
 EXC_LifeTime=rZERO
 !
 do iq_bz=1,q%nbz
   !
   call io_control(ACTION=RD_CL_IF_END,COM=REP,SEC=(/1+iq_bz/),MODE=DUMP,ID=ID_exc_gkkp)
   io_err=io_EXCPH_gkkp(ID_exc_gkkp,'gkkp')
   !
   call io_control(ACTION=RD_CL_IF_END,COM=REP,SEC=(/1+iq_bz/),MODE=DUMP,ID=ID_gkkp)
   io_err=io_ELPH(ID_gkkp,'gkkp_expanded no_matrix_elements')
   !
   do i_alpha=EXCPH_states(1),EXCPH_states(2)
     !
     do i_beta=EXCPH_sum(1),EXCPH_sum(2)
       !
       do il=elph_branches(1),elph_branches(2)
         !
         if(use_PH_DbGd) then
           EXC_LifeTime(i_alpha)=EXC_LifeTime(i_alpha)+EXCPH_Gkkp_sq(il,i_beta,i_alpha)*DbGrid_pole()
         else
           EXC_LifeTime(i_alpha)=EXC_LifeTime(i_alpha)+EXCPH_Gkkp_sq(il,i_beta,i_alpha)*Simple_pole()
         endif
         !
       enddo
       !
     enddo
     !
   enddo
   !
 enddo
 !
 EXC_LifeTime=EXC_LifeTime*2._SP*pi/real(q%nbz)
 !
 ! Output
 !
 call msg('rs','Exciton lifetimes:')
 do i_alpha=EXCPH_states(1),EXCPH_states(2)
   call msg('rs','Exciton '//intc(i_alpha)//' lifetime: ',EXC_LifeTime(i_alpha)*HA2EV/1000._SP,'meV')
 enddo
 !
 ! Deallocations
 !
 YAMBO_FREE(EXC_LifeTime)
 call free_EXC()
 !
 contains
  !
  function Simple_pole()
    use EXCPH,          ONLY:min_EXC_E
    use ELPH,           ONLY:FAN_deltaE_treshold,PH_freqs_sq
    use functions,      ONLY:bose_f,boltzman_f,Lorentzian_func
    implicit none
    !
    real(SP) :: Simple_pole
    real(SP) :: E_alpha,E_beta
    real(SP) :: ph_E,N_bose,F_boltz
    !
    Simple_pole=rZERO
    !
    E_alpha=real(BS_E_in(i_alpha),SP)
    E_beta =BS_all_E(i_beta,iq_bz)
    !
    ph_E=sqrt(abs(PH_freqs_sq(iq_bz,il)))
    if(ph_E<FAN_deltaE_treshold) return
    !
    N_bose  =bose_f(ph_E)
    !
    F_boltz=boltzman_f(E_beta-min_EXC_E)  ! Bose function? Boltzman function?
    !
    Simple_pole=(1._SP+N_bose+F_boltz)*Lorentzian_func(E_alpha-E_beta-ph_E,wv%dr(1)) &
&                    +(N_bose-F_boltz)*Lorentzian_func(E_alpha-E_beta+ph_E,wv%dr(1))
    !
  end function Simple_Pole
  !
  function DbGrid_pole()
    use EXCPH,          ONLY:min_EXC_E_DbGd,EXC_E_DbGd
    use ELPH,           ONLY:FAN_deltaE_treshold,PH_freqs_sq
    use functions,      ONLY:bose_f,boltzman_f,Lorentzian_func
    implicit none
    !
    real(SP) :: DbGrid_pole
    real(SP) :: E_alpha,E_beta
    real(SP) :: ph_E,N_bose,F_boltz
    integer  :: nq_around,iq_fg
    real(SP) :: weight_dbgd
    real(SP), external :: get_ph_E_DbGd
    !
    DbGrid_pole=rZERO
    !
    E_alpha=real(BS_E_in(i_alpha),SP)
    !
    nq_around  =q%FGbz%k_range(iq_bz,2)-q%FGbz%k_range(iq_bz,1) 
    weight_dbgd=1._SP/real(nq_around+1._SP,SP)                  
    !
    do iq_fg=q%FGbz%k_range(iq_bz,1),q%FGbz%k_range(iq_bz,2)
      !
      ph_E=get_ph_E_DbGd(il,iq_fg,iq_bz,q)
      if(ph_E<FAN_deltaE_treshold) cycle
      !
      N_bose  =bose_f(ph_E)
      !
      E_beta=EXC_E_DbGd(i_beta,iq_fg)
      F_boltz=boltzman_f(E_beta-min_EXC_E_DbGd)  ! Bose function? Boltzman function?
      !
      DbGrid_pole=DbGrid_pole+(1._SP+N_bose+F_boltz)*Lorentzian_func(E_alpha-E_beta-ph_E,wv%dr(1)) &
&                                  +(N_bose-F_boltz)*Lorentzian_func(E_alpha-E_beta+ph_E,wv%dr(1))
      !
    enddo
    !
    DbGrid_pole=DbGrid_pole*weight_dbgd
    !
  end function DbGrid_pole
  !
end subroutine
