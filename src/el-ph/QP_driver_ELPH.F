!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
! 
subroutine QP_driver_ELPH(X,Xen,Xk,en,k,q,Xw,Dip)
 !
 use pars,          ONLY:SP,pi,cONE,cZERO
 use units,         ONLY:HA2EV
 use drivers,       ONLY:l_life,l_ppa,l_mpa,l_elel_corr,l_elph_corr,l_cohsex,&
&                        Finite_Tel,l_elphoton_corr,l_rim_w
 use electrons,     ONLY:levels,n_sp_pol,spin
 use R_lattice,     ONLY:bz_samp,RIM_W_is_diagonal,RIM_ng,RIM_W_ng
 use DIPOLES,       ONLY:DIPOLE_t
 use frequency,     ONLY:w_samp
 use com,           ONLY:msg
 use X_m,           ONLY:X_t
 use QP_m,          ONLY:QP_t,QP_dSc,QP_dSc_steps,QP_Vxc,QP_Sc,QP_solver,QP_table,QP_n_G_bands,&
&                        QP_Vnl_xc,QP_n_states,QP_nb,QP_G_zoom_er,QP_G_Zoom_treshold,&
&                        QP_nk,QP_state,QP_dSc_test,QP_reset,use_GreenF_Zoom,l_QP_symmetrize,&
&                        QP_Sc_steps,QP_G_er,QP_G_dr,QP_SET_temperature_pre_factor,&
&                        GF_is_causal,QP_G_amplitude_integral,use_GreenF_to_eval_QP,&
&                        l_GW_terminator,QP_G_info
 use parser_m,      ONLY:parser
 use ELPH,          ONLY:QP_DW,QP_OMS_Fan
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:OP_RD_CL,VERIFY,REP,NONE,IO_NO_BINDING_ERROR
 use global_XC,     ONLY:QP_DB_kind,QP_SE_COHSEX,QP_SE_GoWo_PPA,QP_SE_GoWo,&
&                        QP_SE_POLARON,QP_SE_GW_QED
 use interfaces,    ONLY:eval_G_minus_G
 use parallel_m,    ONLY:PAR_COM_WORLD,PAR_IND_WF_linear
 use stderr,        ONLY:intc
 use descriptors,   ONLY:IO_desc,IO_desc_reset,IO_desc_add
 !
#include<y_memory.h>
 !
 type(levels)  ::en,Xen
 type(bz_samp) ::Xk,k,q
 type(X_t)     ::X
 type(w_samp)  ::Xw
 type(DIPOLE_t)::Dip
 !
 ! Work Space
 !
 type(QP_t)        ::qp
 integer           ::i1,io_QP_and_GF_err,io_G_err,ID_QP,ID_G,i_spin
 integer           ::lw_bnd
 integer, external ::io_QP_and_GF
 real(SP),external ::G2E
 !
 if (l_elph_corr) QP_DB_kind=QP_SE_POLARON 
 !
 if (trim(QP_solver)=='g') then
   !
   call local_alloc()
   !
   if (l_elph_corr) call ELPH_Sigma_c(en,k,q,qp)
   !
   call QP_Green_Function(qp,en,-1)
   !
   call QP_report_and_write_ELPG(k,qp,en,-1)
   call local_free()
   return
   !
 endif
 !
 contains
   !
   subroutine local_alloc()
     !
     YAMBO_ALLOC(qp%Z,(qp%n_states))
     YAMBO_ALLOC(qp%E,(qp%n_states))
     YAMBO_ALLOC(qp%E_bare,(qp%n_states))
     YAMBO_ALLOC(qp%k,(qp%nk,3))
     YAMBO_ALLOC(qp%table,(qp%n_states,3+n_sp_pol-1))
     !
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
     YAMBO_ALLOC(QP_Sc,(QP_n_states,QP_Sc_steps))
     !
     if (l_elph_corr) then
       YAMBO_ALLOC(QP_OMS_Fan,(QP_n_states))
       YAMBO_ALLOC(QP_DW,(QP_n_states))
     endif
     !
     if (trim(QP_solver)=='g') then
       YAMBO_ALLOC(qp%GreenF,(QP_n_states,QP_Sc_steps))
       YAMBO_ALLOC(qp%S_total,(QP_n_states,QP_Sc_steps))
       YAMBO_ALLOC(qp%GreenF_W,(QP_n_states,QP_Sc_steps))
       qp%GreenF_n_steps=QP_Sc_steps
       YAMBO_ALLOC(QP_G_amplitude_integral,(QP_n_states))
       YAMBO_ALLOC(QP_G_info,(QP_n_states,3,2))
     else if (.not.l_life.and.QP_dSc_steps>1) then
       YAMBO_ALLOC(QP_dSc,(qp%n_states,QP_dSc_steps-1))
     endif
     !
   end subroutine
   !
   subroutine local_free()
     YAMBO_FREE(PAR_IND_WF_linear%element_2D)
     YAMBO_FREE(qp%Z)
     YAMBO_FREE(qp%E)
     YAMBO_FREE(qp%E_bare)
     YAMBO_FREE(qp%k)
     YAMBO_FREE(qp%table)
     YAMBO_FREE(QP_table)
     YAMBO_FREE(QP_state)
#if defined _ELPH
     YAMBO_FREE(QP_DW)
     YAMBO_FREE(QP_OMS_Fan)
#endif
     YAMBO_FREE(QP_Sc)
     YAMBO_FREE(QP_dSc)
     YAMBO_FREE(QP_Vnl_xc)
     YAMBO_FREE(QP_Vxc)
     YAMBO_FREE(qp%GreenF)
     YAMBO_FREE(qp%GreenF_W)
     YAMBO_FREE(qp%S_total)
     YAMBO_FREE(QP_G_info)
     YAMBO_FREE(QP_G_amplitude_integral)
     YAMBO_FREE(QP_G_zoom_er)
   end subroutine
   !
end subroutine
!
subroutine check_qp_states(QP_table,QP_state,qp,en,Xen)
  !
  ! Check if qp-indexes are continous for SC-GW
  !  
  use QP_m,          ONLY:QP_t
  use electrons,     ONLY:n_sp_pol,levels
#include<y_memory.h>
  !
  !
  type(QP_t), intent(in)      :: qp
  type(levels),intent(inout)  :: en,Xen
  logical,    intent(in)      :: QP_state(qp%nb,qp%nk)
  integer,    intent(in)      :: QP_table(qp%n_states,3+n_sp_pol-1)
  !
  ! Work space
  !
  integer :: lw_bnd
  !
  lw_bnd=minval(QP_table(:,1))
  !
  if(any(QP_state(lw_bnd:qp%nb,:).eqv..FALSE.).or.qp%nk/=en%nk) &
      & call error("Not continous QP-indexes not suppoerted in self-consistent GW")
  !
  if(.not.allocated(en%Eo)) then
    YAMBO_ALLOC(en%Eo,(en%nb,en%nk,n_sp_pol))
    en%Eo=en%E            
  endif
  !
end subroutine check_qp_states
