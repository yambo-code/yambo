!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine QP_driver(X,Xen,Xk,en,k,q,Xw)
 !
 use pars,          ONLY:SP,IP,pi,cONE,cZERO
 use units,         ONLY:HA2EV
 use memory_m,      ONLY:mem_est
 use drivers,       ONLY:l_life,l_ppa,l_elel_corr,l_elph_corr,l_cohsex,Finite_Tel,l_elphoton_corr
 use electrons,     ONLY:levels,n_sp_pol,spin
 use stderr,        ONLY:real2ch
 use R_lattice,     ONLY:bz_samp
 use frequency,     ONLY:w_samp
 use com,           ONLY:msg,warning
 use X_m,           ONLY:X_t
 use parser_m,      ONLY:parser
 use QP_m,          ONLY:QP_t,QP_dSc,QP_dSc_steps,QP_Vxc,QP_Sc,QP_solver,QP_table,&
&                        QP_Vnl_xc,QP_n_states,QP_nb,QP_G_zoom_er,QP_G_Zoom_treshold,&
&                        QP_nk,QP_state,QP_dSc_test,QP_reset,use_GreenF_Zoom,&
&                        QP_Sc_steps,QP_G_er,QP_G_dr,QP_SET_temperature_pre_factor,&
&                        GWo_iterations,COHSEX_use_empties,On_Mass_Shell_approx,use_GreenF_to_eval_QP,&
&                        GF_is_causal,QP_G_amplitude_integral,l_extended_output,&
&                        l_GW_terminator,GW_terminator_Kind,l_QP_Expand,QP_G_info
#if defined _ELPH
 use ELPH,          ONLY:QP_DW,QP_OMS_Fan
#endif
 use IO_m,          ONLY:io_control,OP_RD_CL,VERIFY,REP,NONE,IO_NO_BINDING_ERROR
 use global_XC,     ONLY:QP_DB_kind,SE_COHSEX,SE_GoWo_PPA,SE_GoWo,SE_GWo_PPA,SE_GWo,SE_POLARON,SE_GW_QED
 use interfaces,    ONLY:eval_G_minus_G
 use parallel_m,    ONLY:PAR_COM_WORLD
 implicit none
 !
 type(levels) ::en,Xen
 type(bz_samp)::Xk,k,q
 type(X_t)    ::X
 type(w_samp) ::Xw
 !
 ! Work Space
 !
 type(QP_t)        ::qp
 integer           ::i1,io_QP_and_GF_err,io_G_err,ID_QP,ID_G,i_spin
 integer, external ::io_QP_and_GF
 !
 ! Driver Logicals
 !=================
 !
 call parser('NewtDchk',   QP_dSc_test)
 call parser('OnMassShell',On_Mass_Shell_approx)
 call parser('QPExpand'   ,l_QP_Expand)
 call parser('UseEbands',  COHSEX_use_empties)
 ! GPL_EXCLUDE_START
 call parser('GreenF2QP',  use_GreenF_to_eval_QP)
 use_GreenF_Zoom=QP_G_Zoom_treshold>0._SP
 ! GPL_EXCLUDE_END
 call parser('ExtendOut',  l_extended_output)
 !
 if (On_Mass_Shell_approx) QP_dSc_test=.FALSE.
 !
 l_GW_terminator=GW_terminator_Kind/='none'
 !
 ! S_x/Vxc are always need if not l_life or el. correlations is
 ! skipped
 !==============================================================
 if (.not.allocated(QP_Vnl_xc).and..not.allocated(QP_Vxc).and.l_elel_corr.and..not.l_life) return
 !
 if (.not.l_elel_corr.and..not.l_elph_corr.and..not.l_elphoton_corr) then
   call warning('e-p, e-e and e-photon correlation switched off')
   return
 endif
 !
 ! Head message
 !==============
 call QP_reset(qp)
 !
 ! Setup Temperature pre-factor
 !==============================
 call QP_SET_temperature_pre_factor( )
 !
 qp%n_descs=1
 if (trim(QP_solver)=='n') then
   write (qp%description(1),'(a)') ' GW solver              : Newton'
   call section('*','Dyson equation: Newton solver')
 else if (trim(QP_solver)=='s') then
   !
   ! When using secant no SC available
   !
   GWo_iterations=0
   !
   write (qp%description(1),'(a)') ' GW solver              : Secant'
   call section('*','Dyson equation: non perturbative QP_secant method')
   !
 else if (trim(QP_solver)=='g') then
   write (qp%description(1),'(a)') ' GW solver              : Full Green`s function'
   call section('*','Dyson equation: full Green`s function')
   call msg('r', '[Green] Sc steps                                         :',QP_Sc_steps)
   call msg('r', '[Green] Sc energy range (centered in the bare value) [ev]:',QP_G_er*HA2EV)
   call msg('rn','[Green] Sc damping range                             [ev]:',QP_G_dr*HA2EV)
   !
   GF_is_causal=Finite_Tel.or.l_elph_corr 
   !
 else if (.not.l_life) then
   return
   !
 endif
 qp%n_descs=2
 if (l_ppa) then
   qp%description(2)=' PPA imaginary pt   [ev]:'//trim(real2ch(X%ppaE*HA2EV))
   !
   QP_DB_kind=SE_GoWo_PPA
   !
   ! GPL_EXCLUDE_START
   !
   if ( GWo_iterations > 0 ) QP_DB_kind=SE_GWo_PPA
   !
   ! GPL_EXCLUDE_END
   !
 else if (l_cohsex) then
   !
   ! in COHSEX no SC possible
   !
   GWo_iterations=0
   !
   qp%description(2)=' == COHSEX GW =='
   !
   QP_DB_kind=SE_COHSEX
   !
 else
   !
   if (l_elel_corr)     qp%description(2)=' == Real Axis GW =='
   if (l_elph_corr)     qp%description(2)=' == El-Ph Self-Energy =='
   if (l_elphoton_corr) qp%description(2)=' == Lamb Shift =='
   !
   ! Here I am not considering the case where ELPH + GW is used.
   ! For this case I need to create a new global KIND.
   !
   QP_DB_kind=SE_GoWo
   !
   if (l_elph_corr) QP_DB_kind=SE_POLARON 
   !
   if (l_elphoton_corr) QP_DB_kind=SE_GW_QED
   !
   ! GPL_EXCLUDE_START
   !
   if ( GWo_iterations > 0 ) QP_DB_kind=SE_GWo_PPA
   !
   ! GPL_EXCLUDE_END
   !
 endif
 !
 ! Basic defs
 !============
 !
 call QP_state_table_setup(en)
 !
 ! Here I copy several informations to the qp_t type.
 ! This is because I want the qp to be completely independent of the run
 ! in order to be possibly read as QP correction.
 !
 qp%nk=QP_nk
 qp%nb=QP_nb
 qp%n_states=QP_n_states
 !
 ! In lifetimes calculations  the X db may be not
 ! present. So I need to define some variables that 
 ! must be correctly written in the QP_description(s)
 !
 if (l_life) then
   call X_pre_setup(Xen,X)
   if (X%ng_db==0) X%ng_db=X%ng
 endif
 !
 if (l_ppa.and.l_GW_terminator) X%ng=eval_G_minus_G(X%ng,0,COMM=PAR_COM_WORLD)
 !
 ! Local Allocations
 !===================
 !
 call local_alloc()
 !
 call QP_descriptions(qp,X,Xw,.FALSE.)
 !
 do i1=1,QP_n_states
   !
   i_spin=spin(QP_table(i1,:))
   !
   ! To perform the SC GWo the bare interaction are needed. Those are
   ! stored in the %E array, or in %Eo array if an initial QP correction
   ! has been added already
   !
   qp%E_bare(i1)=en%E(QP_table(i1,1),QP_table(i1,3),i_spin)
   if (associated(en%Eo)) qp%E_bare(i1)=en%Eo(QP_table(i1,1),QP_table(i1,3),i_spin)
   !
   qp%k(QP_table(i1,3),:)=k%pt(QP_table(i1,3),:)
   !
 enddo
 !
 if (.not.l_life) then
   !
   ! QP I/O
   !========
   !
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID_QP)
   io_QP_and_GF_err=io_QP_and_GF('QP',qp,ID_QP)
   !
   if (io_QP_and_GF_err==0.and..not.trim(QP_solver)=='g') then
     call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2,3/),ID=ID_QP)
     io_QP_and_GF_err=io_QP_and_GF('QP',qp,ID_QP)
     call QP_report_and_write(k,qp,en,0)
     call local_free()
     return
   endif
   !
   ! Green Functions I/O
   !=====================
   !
   if (trim(QP_solver)=='g') then
     !
     call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID_G)
     io_G_err=io_QP_and_GF('G',qp,ID_G)
     !
     if (io_G_err==0.or.io_G_err==IO_NO_BINDING_ERROR) then
       !
       if (use_GreenF_to_eval_QP) use_GreenF_to_eval_QP=(.not.io_QP_and_GF_err==0).and.(io_G_err==0)
       if (use_GreenF_Zoom)       use_GreenF_Zoom=io_G_err==IO_NO_BINDING_ERROR.and..not.use_GreenF_to_eval_QP
       !
       call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2,3/),ID=ID_G)
       io_G_err=io_QP_and_GF('G',qp,ID_G)
       !
       if (use_GreenF_to_eval_QP) then
         call QP_Green_Function(qp,en,0) 
         call QP_report_and_write(k,qp,en,-1)
         call local_free()
         return
       endif
       !
       if (use_GreenF_Zoom) then
         allocate(QP_G_zoom_er(QP_n_states,2))
         call mem_est("QP_G_zoom_er",(/size(QP_G_zoom_er)/),(/SP/))
       else
         QP_G_Zoom_treshold=0._SP
       endif
       call QP_Green_Function(qp,en,0) 
       if (.not.use_GreenF_Zoom) then
         call QP_report_and_write(k,qp,en,0)
         call local_free()
         return
       endif
       !
     else
       QP_G_Zoom_treshold=0._SP
     endif
     !
   endif
   !
 endif
 !
 ! Updated descriptions (needed in case of Zoomed GFs)
 !=====================================================
 !
 call QP_descriptions(qp,X,Xw,.TRUE.)
 !
 call mem_est("QP_Sc",(/size(QP_Sc)/))
 !
 ! Solvers 
 !=========
 !
 if (trim(QP_solver)=='n') then
   !
   ! NEWTON
   !--------
   call QP_newton(X,Xen,Xk,en,k,q,qp,Xw)
   !
 endif
 !
 if (trim(QP_solver)=='s') then
   !
   ! SECANT
   !--------
   call QP_secant_driver(X,Xen,Xk,en,k,q,qp,Xw)
   !
 endif
 !
 ! GREEN`s FUNCTIONS
 !-------------------
 !
 if (trim(QP_solver)=='g') then
   !
   if (l_elel_corr.and.(.not.l_ppa.and..not.l_cohsex)) call QP_real_axis(X,Xen,Xk,en,k,q,qp,Xw,0) 
   !
# if defined _ELPH 
   !
   if (l_elph_corr)                                    call ELPH_Sigma_c(en,k,q,qp)
   !
#endif
   !
   call QP_Green_Function(qp,en,-1)
   !
   call QP_report_and_write(k,qp,en,-1)
   call local_free()
   return
   !
 endif
 !
 ! LIFETIMES
 !
 if (l_life) then
   !
   if (l_elel_corr)     call QP_real_axis(X,Xen,Xk,en,k,q,qp,Xw,0) 
   !
#if defined _QED
   !
   ! Radiative Lifetimes (Using <p>)
   !=================================
   !
   if (l_elphoton_corr) call QED_Radiative_Lifetimes(en,k,q,qp,X)
   !
#endif
   !
   qp%Z=cONE
   !
   call QP_report_and_write(k,qp,en,-1)
   call local_free()
   return
   !
 endif
 !
 ! Reporting
 !
 call QP_report_and_write(k,qp,en,-1)
 call local_free()
 !
 contains
   !
   subroutine local_alloc()
     !
     allocate(qp%Z(qp%n_states),qp%E(qp%n_states),&
&             qp%E_bare(qp%n_states),&
&             qp%k(qp%nk,3),qp%table(qp%n_states,3+n_sp_pol-1))
     !
     call mem_est("qp_Z qp_E qp_E_bare qp_K qp_table",&
&        (/QP_n_states,QP_n_states,QP_n_states,3*qp%nk,QP_n_states*(3+n_sp_pol-1)/),&
&        (/2*SP,       2*SP,       SP,         SP,           IP/))
     qp%table=QP_table
     qp%E=cZERO
     qp%k=0._SP
     !
     ! Sc energy steps. 2/3 If Newton/Secant. QP_Sc_steps 
     ! the full Green`s function is requested.
     !
     if (.not.l_life)                 QP_dSc_steps=2
     if (.not.l_life.and.QP_dSc_test) QP_dSc_steps=3
     !
     ! Cohsex is static
     !
     if (l_cohsex)                    QP_dSc_steps=1
     !
     if (trim(QP_solver)/='g')        QP_Sc_steps=QP_dSc_steps
     !
     allocate(QP_Sc(QP_n_states,QP_Sc_steps))
     call mem_est("QP_Sc",(/size(QP_Sc)/))
     !
#if defined _ELPH
     if (l_elph_corr) then
       allocate(QP_OMS_Fan(QP_n_states))
       call mem_est("QP_OMS_Fan",(/size(QP_OMS_Fan)/))
       allocate(QP_DW(QP_n_states))
       call mem_est("QP_DW",(/size(QP_DW)/))
     endif
#endif
     !
     if (trim(QP_solver)=='g') then
       allocate(qp%GreenF(QP_n_states,QP_Sc_steps))
       call mem_est("qp_GreenF",(/size(qp%GreenF)/))
       allocate(qp%S_total(QP_n_states,QP_Sc_steps))
       call mem_est("qp_S_total",(/size(qp%S_total)/))
       allocate(qp%GreenF_W(QP_n_states,QP_Sc_steps))
       call mem_est("qp_GreenF_W",(/size(qp%GreenF_W)/))
       qp%GreenF_n_steps=QP_Sc_steps
       allocate(QP_G_amplitude_integral(QP_n_states))
       call mem_est("QP_G_amplitude_integral",(/size(QP_G_amplitude_integral)/))
       allocate(QP_G_info(QP_n_states,3,2))
       call mem_est("QP_G_info",(/size(QP_G_info)/))
     else if (.not.l_life) then
       allocate(QP_dSc(qp%n_states,QP_dSc_steps-1))
       call mem_est("QP_dSc",(/size(QP_dSc)/))
     endif
     !
   end subroutine
   !
   subroutine local_free()
     deallocate(qp%Z,qp%E,qp%E_bare,qp%k,qp%table,QP_table,QP_state)
#if defined _ELPH
     if (allocated(QP_DW))          deallocate(QP_DW)
     if (allocated(QP_OMS_Fan))     deallocate(QP_OMS_Fan)
#endif
     if (allocated(QP_Sc))          deallocate(QP_Sc)
     if (allocated(QP_dSc))         deallocate(QP_dSc)
     if (allocated(QP_Vnl_xc))      deallocate(QP_Vnl_xc,QP_Vxc)
     if (associated(qp%GreenF)) then
       deallocate(qp%GreenF)
       nullify(qp%GreenF)
     endif
     if (associated(qp%GreenF_W)) then
       deallocate(qp%GreenF_W)
       nullify(qp%GreenF_W)
     endif
     if (associated(qp%S_total))  then
       deallocate(qp%S_total)
       nullify(qp%S_total)
     endif
     if (allocated(QP_G_info)) deallocate(QP_G_info)
     if (allocated(QP_G_amplitude_integral)) deallocate(QP_G_amplitude_integral)
     if (allocated(QP_G_zoom_er))  deallocate(QP_G_zoom_er)
     call mem_est("QP_G_amplitude_integral")
     call mem_est("QP_Sc QP_dSc qp_GreenF QP_Vnl_xc QP_Vxc qp_GreenF_W QP_G_zoom_er")
     call mem_est("qp_Z qp_E qp_E_bare qp_K qp_table QP_table QP_state qp_S_total")
     call mem_est("QP_DW QP_OMS_Fan")
   end subroutine
   !
end subroutine
