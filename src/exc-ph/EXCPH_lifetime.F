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
subroutine EXCPH_lifetime(k,q,X)
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
 use EXCPH,          ONLY:EXCPH_sum,EXCPH_states,BS_E,BS_all_E,BS_E_in,L_damping,n_exc_in, &
&                         EXC_LifeTime,min_EXC_E,EXCPH_Gkkp_sq,DB_Q_map,l_no_matrix_elements, &
&                         RESIDUALS_and_EIGENVALUES_alloc,RESIDUALS_and_EIGENVALUES_free,alphaQ 
 use ELPH,           ONLY:use_PH_DbGd,elph_branches,FAN_deltaE_treshold,Q_io_map
 use vec_operate,    ONLY:degeneration_finder,iku_v_norm,iku_v_2D_norm
 use frequency,      ONLY:w_samp
 use stderr,         ONLY:intc
 use parallel_m,    ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,  ONLY:PP_redux_wait,PARALLEL_index
 use LIVE_t,        ONLY:live_timing
 !
#include<memory.h>
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 type(X_t)     ::X
 !
 ! Work space
 !
 integer  :: ID_exc_gkkp,ID_gkkp,io_err,IO_ACT
 integer, external :: io_EXCPH_gkkp,io_ELPH
 integer  :: iq,iq_s,iq_bz,i_alpha,j_alpha,i_beta,il,i_star,iq_db,igrp
 integer  :: elph_branches_save(2)
 integer  :: min_pos(1),first_exc_grp(n_exc_in),n_of_exc_grp(n_exc_in),n_deg_grp
 logical  :: l_keep_deg,l_extend_output
 real(SP) :: ExcPhM,EXC_life_average,EXC_energy_average,Pole
 real(SP) , allocatable :: EXC_life_of_q(:,:),EXC_pole_of_q(:,:)
 type(PP_indexes)::px
 !
 call parser('NoMatrxEl',l_no_matrix_elements)
 call parser('KeepDeg',  l_keep_deg)
 call parser('ExtendOutput', l_extend_output)
 !
 call section('*','EXCPH-LifeTime')
 !
 call k_build_up_BZ_tables(k)
 call k_build_up_BZ_tables(q)
 !
 call k_ibz2bz(q,"i",.false.)
 !
 ! Load excitonic energies 
 !
 call EXCPH_load_L(1,X,'check','Lout')
 !
 ! Allocations 
 !
 call RESIDUALS_and_EIGENVALUES_alloc(q)
 if (l_extend_output) then
   allocate(EXC_life_of_q(EXCPH_states(1):EXCPH_states(2),q%nbz))
   allocate(EXC_pole_of_q(EXCPH_states(1):EXCPH_states(2),q%nbz))
   EXC_life_of_q=rZERO
   EXC_pole_of_q=rZERO
 endif
 !
 ! Load excitonic energies 
 !
 call EXCPH_load_L(1,X,'eigenvalues','Lin')
 BS_E_in(:)=BS_E(EXCPH_states(1):EXCPH_states(2)) ! L_in can be different from L_out 
 !
 do iq=1,q%nibz
   call EXCPH_load_L(iq,X,'eigenvalues','Lout')
   BS_all_E(:,iq)=real(BS_E(:)) ! I suppose the BSE eigenvalues are real
   !write(*,*) iq,BS_all_E(:,iq)
 enddo
 !
 if(alphaQ/=rZERO) then
   !      
   call msg('rs','Excitonic bands 2D stretching :',alphaQ)
   !
   do iq=1,q%nibz
     BS_all_E(:,iq)=BS_all_E(:,iq)+alphaQ*iku_v_2D_norm(q%pt(iq,:3))
   enddo
   !
 endif
 !
 ! Here we find the degenerations of the first n_exc_in values of BS_all_E at gamma
 ! and we set their energies to exactly the same value (average). This is to
 ! improve the quality of the interpolation at gamma in the case of double grid 
 ! (try to avoid unphysical splitting of degenerate states) 
 !
 call degeneration_finder(BS_all_E(EXCPH_states(1):EXCPH_states(2),1),n_exc_in,first_exc_grp,&
 &        n_of_exc_grp,n_deg_grp,0.001/HA2EV,Include_single_values=.true.)
 !
 do igrp=1,n_deg_grp
   j_alpha=first_exc_grp(igrp)+EXCPH_states(1)-1
   i_alpha=first_exc_grp(igrp)+n_of_exc_grp(igrp)+EXCPH_states(1)-2
   EXC_energy_average=sum(BS_all_E(j_alpha:i_alpha,1))/real(n_of_exc_grp(igrp),SP)
   BS_all_E(j_alpha:i_alpha,1)=EXC_energy_average
 enddo
 !
 !
 ! Load the Exciton-phonon elements and phonon energies
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),MODE=DUMP,ID=ID_exc_gkkp)
 io_err=io_EXCPH_gkkp(ID_exc_gkkp)
 if(io_err/=0) call error('excph_gkkp databases not found!')
 !
 elph_branches_save = elph_branches
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),MODE=DUMP,ID=ID_gkkp)
 io_err=io_ELPH(ID_gkkp,'gkkp_expanded no_matrix_elements')
 elph_branches = elph_branches_save
 if(io_err/=0) call error('gkkp_expanded databases not found!')
 !
 ! Info on excitons
 ! 
 min_pos  =minloc(BS_all_E(1,:))
 min_EXC_E=minval(BS_all_E(1,:))
 !
 call msg('rs','Minimum excitonic energy : ',min_EXC_E*HA2EV,' eV, at q-point :'//intc(min_pos(1)))
 call msg('rs','FAN treshold: ',FAN_deltaE_treshold*HA2CMm1,' cm-1')
 call msg('rs','Self-energy broadening : ',L_damping*HA2EV*1000._SP,'meV')
 !
 ! Turn on double-grid if presents
 call EXCPH_double_grid_setup(q)
 !
 ! Calculate the life-times
 !
 YAMBO_ALLOC(EXC_LifeTime,(EXCPH_states(1):EXCPH_states(2)))
 EXC_LifeTime=rZERO
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/q%nbz/))
 !
 call live_timing('Lifetimes',px%n_of_elements(myid+1))
 !
 do iq=1,q%nibz
 do i_star=1,q%nstar(iq)
   !
   iq_s=q%star(iq,i_star)
   iq_bz = q%k_table(iq, iq_s)
   !
   if (.not.px%element_1D(iq_bz)) cycle
   !
   IO_ACT=manage_action(RD_CL_IF_END,iq_bz,1,q%nbz,PAR_INDEX=px)
   !
   call io_control(ACTION=IO_ACT,COM=REP,SEC=(/1+iq_bz/),MODE=DUMP,ID=ID_exc_gkkp)
   io_err=io_EXCPH_gkkp(ID_exc_gkkp)
   if(io_err/=0) call error('excph_gkkp databases not found!')
   !
   !iq_db=DB_Q_map(iq_bz)
   iq_db=Q_io_map(iq_bz)
   call io_control(ACTION=IO_ACT,COM=REP,SEC=(/1+iq_db/),MODE=DUMP,ID=ID_gkkp)
   io_err=io_ELPH(ID_gkkp,'gkkp_expanded no_matrix_elements')
   if(io_err/=0) call error('gkkp_expanded databases not found!')
   !
   do i_alpha=EXCPH_states(1),EXCPH_states(2)
     !
     do i_beta=EXCPH_sum(1),EXCPH_sum(2)
       !
       do il=elph_branches(1),elph_branches(2)
         !
         ExcPhM=EXCPH_Gkkp_sq(il,i_beta,i_alpha)
         if(l_no_matrix_elements) ExcPhM=1._SP 
         !
         if(.not.use_PH_DbGd) Pole=Simple_pole()
         if(use_PH_DbGd)      Pole=DbGrid_pole()
         !
         EXC_LifeTime(i_alpha)=EXC_LifeTime(i_alpha)+ExcPhM*Pole
         if (l_extend_output) then
           EXC_life_of_q(i_alpha,iq_bz)=EXC_life_of_q(i_alpha,iq_bz)+ExcPhM*Pole
           EXC_pole_of_q(i_alpha,iq_bz)=EXC_pole_of_q(i_alpha,iq_bz)+Pole
         endif
         ! DS Debug <
         ! write(100+i_alpha,*) i_alpha,EXC_LifeTime(i_alpha)
         ! DS Debug >
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
 enddo
 !
 call live_timing( )
 !
 call PP_redux_wait(EXC_LifeTime)
 if (l_extend_output) then
   call PP_redux_wait(EXC_life_of_q)
   call PP_redux_wait(EXC_pole_of_q)
 endif
 !
 call PP_indexes_reset(px)
 !
 ! Perform average
 !
 call degeneration_finder(real(BS_E_in(EXCPH_states(1):EXCPH_states(2))),n_exc_in,first_exc_grp, &
 &        n_of_exc_grp,n_deg_grp,0.001/HA2EV,Include_single_values=.true.)
 !
 do igrp=1,n_deg_grp
   j_alpha=first_exc_grp(igrp)+EXCPH_states(1)-1
   i_alpha=first_exc_grp(igrp)+n_of_exc_grp(igrp)+EXCPH_states(1)-2
   EXC_life_average=sum(EXC_LifeTime(j_alpha:i_alpha))/real(n_of_exc_grp(igrp),SP)
   EXC_LifeTime(j_alpha:i_alpha)=EXC_life_average
 enddo
 !
 EXC_LifeTime=EXC_LifeTime*2._SP*pi/real(q%nbz,SP)
 !if (l_extend_output) EXC_life_of_q=EXC_life_of_q*2._SP*pi/real(q%nbz,SP)
 !
 ! Print output and write on file
 !
 call print_and_write()
 !
 if (l_extend_output) then
   call print_LifeTimes_of_q("L")
   call print_LifeTimes_of_q("P")
 endif
 !
 ! Deallocations
 !
 YAMBO_FREE(EXC_LifeTime)
 call RESIDUALS_and_EIGENVALUES_free()
 if (l_extend_output) then
   deallocate(EXC_life_of_q)
 endif
 !
 contains
  !
  function Simple_pole()
    use EXCPH,          ONLY:min_EXC_E,DB_Q_map
    use ELPH,           ONLY:FAN_deltaE_treshold,PH_freqs_sq,Q_io_map
    use functions,      ONLY:bose_f,boltzman_f,Lorentzian_func
    use units,          ONLY:HA2EV
    implicit none
    !
    real(SP) :: Simple_pole
    real(SP) :: E_alpha,E_beta
    real(SP) :: ph_E,N_bose,F_boltz
    integer  :: iq_db
    !
    Simple_pole=rZERO
    !
    E_alpha=real(BS_E_in(i_alpha),SP)
    E_beta =BS_all_E(i_beta,iq)
    !
    iq_db=Q_io_map(iq_bz)
    ph_E=sqrt(abs(PH_freqs_sq(iq_db,il)))
    !
    if(ph_E<FAN_deltaE_treshold) return
    !
    ! Remove degenerate states E(alpha,iq_in)=E(beta,iq_in) [currently iq_in=1]
    !
    if(iq_db==1.and.abs(E_alpha-E_beta)<FAN_deltaE_treshold .and. .not.l_keep_deg) return
    ! even if keeping deg states, still remove the one where i_alpha=i_beta
    if(iq_db==1.and.i_alpha==i_beta .and. l_keep_deg) return
    ! [FP] This removes resonances, I think it is not correct
    !if(abs(E_alpha-E_beta-ph_E)<FAN_deltaE_treshold.or.abs(E_alpha-E_beta+ph_E)<FAN_deltaE_treshold) return
    !
    N_bose  =bose_f(ph_E)
    !
    F_boltz=boltzman_f(E_beta-min_EXC_E)  ! Bose function? Boltzman function?
    !
    Simple_pole=(1._SP+N_bose+F_boltz)*Lorentzian_func(E_alpha-E_beta-ph_E,L_damping) &
&                    +(N_bose-F_boltz)*Lorentzian_func(E_alpha-E_beta+ph_E,L_damping)
    !
    ! DEBUG <
    ! write(200+i_alpha,*) E_alpha*HA2EV,E_beta*HA2EV,ph_E*HA2EV
    ! DEBUG >
    !
  end function Simple_Pole
  !
  function DbGrid_pole()
    use EXCPH,          ONLY:min_EXC_E_DbGd,EXC_E_DbGd
    use ELPH,           ONLY:FAN_deltaE_treshold,PH_freqs,PH_freqs_sq,Q_io_map
    use functions,      ONLY:bose_f,boltzman_f,Lorentzian_func
    implicit none
    !
    real(SP) :: DbGrid_pole
    real(SP) :: E_alpha,E_beta
    real(SP) :: ph_E,N_bose,F_boltz
    integer  :: nq_around,iq_fg,iE_fg
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
      !
      if(iq_fg==q%FGbz%k_range(iq_bz,1)) then
        !
        ! Here we get original points for coarse grids
        ! (partially counters inaccuracies in the interpolation in the exc.
        ! case)
        !
        iq_db=Q_io_map(iq_bz) ! This mapping is because sometimes the phonon q_bz differs from the exciton one in yambo
        ph_E=sqrt(abs(PH_freqs_sq(iq_db,il)))
        !
        E_beta=BS_all_E(i_beta,iq)  ! Getting this in the IBZ for the equivalent iq to iq_bz
        !
        ! If there is the double-grid I read the gamma phonons from there
        ! because they include the LO-TO splitting and the acustic sum rule
        !
        if(iq_bz==1) ph_E=PH_freqs%FG%E(il,1,1)
        !
      else
        iE_fg=q%FGbz%E_map(iq_fg)                ! Map w(q) from BZ to IBZ
        ph_E =PH_freqs%FG%E(il,iE_fg,1)          ! w(q_db) phonon energy in the D-Grid
        !
        E_beta=EXC_E_DbGd(i_beta,iq_fg)          ! E(q_bz) exciton energy in the D-Grid
      endif
      !
      if(ph_E<FAN_deltaE_treshold) cycle
      !
      ! Remove degenerate states E(alpha,iq_in)=E(beta,iq_fg(iq_in)) [currently iq_in=1]
      !
      if(iq==1.and.abs(E_alpha-E_beta)<FAN_deltaE_treshold .and. .not.l_keep_deg) return
      ! even if keeping deg states, still remove the one where i_alpha=i_beta
      if(iq==1.and.i_alpha==i_beta .and. l_keep_deg) return
      ! [FP] This removes resonances, I think it is not correct
      !if(abs(E_alpha-E_beta-ph_E)<FAN_deltaE_treshold.or.abs(E_alpha-E_beta+ph_E)<FAN_deltaE_treshold) return
      !
      N_bose  =bose_f(ph_E)
      F_boltz=boltzman_f(E_beta-min_EXC_E_DbGd)  ! Bose function? Boltzman function?
      !
      DbGrid_pole=DbGrid_pole+(1._SP+N_bose+F_boltz)*Lorentzian_func(E_alpha-E_beta-ph_E,L_damping) &
&                                  +(N_bose-F_boltz)*Lorentzian_func(E_alpha-E_beta+ph_E,L_damping)
      !
    enddo
    !
    DbGrid_pole=DbGrid_pole*weight_dbgd
    !
  end function DbGrid_pole
  !
  subroutine print_and_write()
    use pars,           ONLY:schlen
    use OUTPUT,         ONLY:OUTPUT_driver
    use stderr,         ONLY:real2ch,intc,log2ch
    !
    implicit none
    !
    character(schlen) :: what='QP'
    !
    call OUTPUT_driver(what,action="reset")
    call OUTPUT_driver(what,action="open")
    !
    call OUTPUT_driver(what,ORDER=(/"Eo"/))
    call OUTPUT_driver(what,ORDER=(/"Width [me"/))
    !
    call msg('rs','Exciton lifetimes:')
    !
    call msg('o '//what,'#')    
    call msg('o '//what,'# === Exciton LifeTimes === ')    
    call msg('o '//what,'#')    
    call msg('o '//what,'# Exciton Sum                  : '//trim(intc(EXCPH_sum(1)))//' - '//trim(intc(EXCPH_sum(2))))    
    call msg('o '//what,'# Exciton States               : '//trim(intc(EXCPH_states(1)))//' - '//trim(intc(EXCPH_states(2))))    
    call msg('o '//what,'# Self-Energy broadening       :'//trim(real2ch(L_damping*HA2EV*1000._SP))//' meV')    
    call msg('o '//what,'# FAN treshold                 :'//trim(real2ch(FAN_deltaE_treshold*HA2EV*1000._SP))//' meV')    
    call msg('o '//what,'# PH Doble-Grid                : '//log2ch(use_PH_DbGd))    
    !
    do i_alpha=EXCPH_states(1),EXCPH_states(2)
      !
      call OUTPUT_driver(what,TITLES=(/"Band   "/),I_VALUES=(/i_alpha/))
      call OUTPUT_driver(what,TITLES=(/"E"/),R_VALUES=(/real(BS_E_in(i_alpha))/),UNIT="eV")
      call OUTPUT_driver(what,TITLES=(/"Width"/),R_VALUES=(/EXC_LifeTime(i_alpha)/),UNIT="meV")
      call OUTPUT_driver(what,action="write")
      !
      call msg('rs','Exciton ['//trim(intc(i_alpha))//'] Lifetime: ',EXC_LifeTime(i_alpha)*HA2EV*1000._SP,'meV')
      !
    enddo  
    !
    call OUTPUT_driver(what,action="close")
    !
  end subroutine 
  !
  subroutine print_LifeTimes_of_q(MODE)
    ! MODE="L"[ife]:
    !   - print q-map of lifetimes for each exc_in state for q-space plot
    !     We print l(q)/sum_q l(q)
    ! MODE="P"[ole]:
    !   - Print value of the Pole [F_occ*delta(...)] for each exc_in state for q-space plot
    !     We print sum_{beta,mu} Pole_{beta,mu}(q)
    !
    use vec_operate,   ONLY:iku_v_norm
    use com,           ONLY:of_open_close
    !use YPPm,          ONLY:K_transform
    !
    implicit none
    !
    character(1) :: MODE
    character(schlen), allocatable :: headings(:)
    character(schlen)              :: file_name
    real(SP), allocatable          :: values(:)
    real(SP)                       :: tmp_q(3)
    !
    allocate(headings(n_exc_in+4))
    YAMBO_ALLOC(values,(n_exc_in+4))
    values=rZERO
    !
    headings(1)=" |q| (a.u.) "
    do i_alpha=EXCPH_states(1),EXCPH_states(2)
      headings(i_alpha+1)=' e'//intc(i_alpha)
      !
      if (MODE=="L") then
        EXC_life_of_q(i_alpha,:)=EXC_life_of_q(i_alpha,:)/SUM(EXC_life_of_q(i_alpha,:))
      endif
      !
    enddo
    headings(n_exc_in+2:n_exc_in+4)=&
&           (/"q_x (rlu)","q_y (rlu)","q_z (rlu)"/)
    !
    if (MODE=="L") file_name="LifeTimes_of_q"
    if (MODE=="P") file_name="Resonance_of_q"
    !
    call of_open_close(trim(file_name),'ot')
    call msg('o '//trim(file_name),"#",headings,INDENT=0,USE_TABS=.true.)
    call msg('o '//trim(file_name),"#")
    !
    do iq=1,q%nibz
    do i_star=1,q%nstar(iq)
      !
      iq_s=q%star(iq,i_star)
      iq_bz = q%k_table(iq, iq_s)
      !
      if (iq_bz>1) values(1)=iku_v_norm(q%ptbz(iq_bz,:3))
      !
      tmp_q = q%ptbz(iq_bz,:3)
      !
      !call K_transform(tmp_q,'iku')
      if (MODE=="L") values(1+1:n_exc_in+1)=EXC_life_of_q(:,iq_bz)
      if (MODE=="P") values(1+1:n_exc_in+1)=EXC_pole_of_q(:,iq_bz)
      values(n_exc_in+2:n_exc_in+4)=tmp_q
      call msg('o '//trim(file_name),' ',values,INDENT=0,USE_TABS=.true.)
      !
    enddo
    enddo
    call of_open_close(trim(file_name))
    !
  end subroutine print_LifeTimes_of_q
  !
end subroutine
