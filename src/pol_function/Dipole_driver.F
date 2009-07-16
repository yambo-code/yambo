!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine Dipole_driver(Xen,Xk,X,field_dir)
 !
 use drivers,        ONLY:l_optics
 use pars,           ONLY:SP
 use com,            ONLY:warning
#if defined _RAS
 use ras_module,     ONLY:transloc, lras
#endif
#if defined _SC || defined _RT || defined _MAGNETIC
 use SC,             ONLY:using_SC_basis,SC_bands,local_V,load_SC_components
 use global_XC,      ONLY:WF_kind,WF_xc_functional,MORE_NONE,setup_global_XC,WF_perturbation
#endif
 use drivers,        ONLY:l_col_cut
 use electrons,      ONLY:levels,n_sp_pol
 use par_proc_m,     ONLY:pp_redux_wait
 use D_lattice,      ONLY:nsym,i_time_rev,dl_sop,sop_inv
 use R_lattice,      ONLY:bz_samp,q0_def_norm,q_norm,bare_qpg
 use X_m,            ONLY:DIP_iq_dot_r,X_alloc,X_t,DIP_iR_or_P,long_path,Dipole_bands_ordered,&
&                         Vnl_commutator_warning,Eval_P_and_P2_only,P_square
 use IO_m,           ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP
 use wave_func,      ONLY:wf_ng,wf_load,WF_free
 use vec_operate,    ONLY:v_norm
 implicit none
 !
 type(bz_samp), intent(inout) :: Xk
 type(levels),  intent(inout) :: Xen
 type(X_t),     intent(inout) :: X
 real(SP),      intent(inout) :: field_dir(3)
 !
 ! Work Space
 !
 integer                   :: ik,i1,ic,iv,is,i_spin
 real(SP)                  :: field_dir_rot(3)
 logical                   :: use_trans_gauge
 !
 !I/O
 !
 integer :: ID,io_err
 integer, external :: ioOSTNTS
 !
 if (allocated(DIP_iq_dot_r)) return
 !
#if defined _SC || defined _RT || defined _MAGNETIC
 ! Check first the kind of the wavefunctions 
 ! 
 call load_SC_components('check_bands',n_bands=X%ib(2),&
&                        kind=WF_kind,xc_functional=WF_xc_functional,perturbation=WF_perturbation)
 call setup_global_XC('WF',WF_kind,MORE_NONE,WF_xc_functional,WF_perturbation)
#endif
 !
 ! Transitions ordering
 !
 Dipole_bands_ordered=.true.
#if defined _SC || defined _RT || defined _MAGNETIC
 Dipole_bands_ordered=.not.using_SC_basis.or.(using_SC_basis.and.local_V)
 if (Eval_P_and_P2_only) Dipole_bands_ordered=.FALSE.
#else
 Eval_P_and_P2_only=.false.
#endif
 !
 ! Check first if ostnts DB is already done
 !
 X%ngostnts=wf_ng
 !
 ! GPL_EXCLUDE_START RAS_IGNORE
 !
 ! If I am trying to go from a transverse to a long. gauge
 ! calculation X%long_gauge will be false here even if a long_path is
 ! provided
 !
 if (.not.X%long_gauge) X%long_gauge=trim(long_path)/='none'
 !
 ! GPL_EXCLUDE_END RAS_IGNORE
 !
 ! Swith off I/O in case gradient is calculated
 if (Eval_P_and_P2_only) call IO_mute("io_in io_out")
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=ioOSTNTS(X,Xen,ID)
 !
#if defined _SC || defined _RT || defined _MAGNETIC
 if (Eval_P_and_P2_only) call IO_mute(" ")
#endif
 !
 if (io_err.ne.0) then
   !
   ! OptGrad allocation...
   !
   ! When using internal SC potentials oscilators must be rotated
   ! in the new basis. So all transitions are needed
   !
   if (Dipole_bands_ordered) then
     call X_alloc('OptGrad',(/3,X%ib(2),Xen%nbm,Xk%nibz/))
   else
     call X_alloc('OptGrad',(/3,X%ib(2),X%ib(2),Xk%nibz/))
#if defined _SC || defined _RT || defined _MAGNETIC
     if (Eval_P_and_P2_only) then
       call X_alloc('P_square',(/X%ib(2),X%ib(2),Xk%nibz/))
       P_square=(0.,0.)
     endif
#endif
   endif
   DIP_iR_or_P=(0.,0.)
   !
#if !defined _MANYK
   call wf_load(0,1,X%ib,(/1,Xk%nibz/),space='G',title='-Oscillators/G space')
#endif
   !
   use_trans_gauge = .true.
   !
   ! GPL_EXCLUDE_START RAS_IGNORE
   !
   ! Attempt calculation within longitudinal gauge if requested
   !
   if(trim(long_path)/='none') then
     !
     ! Use longitudinal gauge, supporting fallback to transverse gauge
     !
     call Dipole_longitudinal(Xen,Xk,X,field_dir,use_trans_gauge)
     !
     if(use_trans_gauge) then
       call warning('Reverting to transverse gauge')
       X%long_gauge=.false.
     else
       X%long_gauge=.true.
     endif
   endif
   !
   ! Fallback to transverse
   !
   ! GPL_EXCLUDE_END RAS_IGNORE
   !
   if (use_trans_gauge) call Dipole_transverse(Xen,Xk,X)
   !
   ! Free any loaded WFs
   !
   call WF_free()
#if defined _SC || defined _RT || defined _MAGNETIC
   !
   ! Swith off I/O in case gradient is calculated
   !
   if (Eval_P_and_P2_only) call IO_mute("io_in io_out")
   !
#endif
   !
   ! Write the DIP_iR_or_P to its database (Only in the case of "R")
   !
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
   io_err=ioOSTNTS(X,Xen,ID)
   !
 end if
 !
#if defined _SC || defined _RT || defined _MAGNETIC
 if (Eval_P_and_P2_only) then
   call IO_mute(" ")
   return
 endif
#endif
 !
 ! The field direction and the gamma-point norm must be renormalized here in case the
 ! oscillator strengths have been calculated using the long. gauge.
 ! In this case q0_def_norm is not the default one but corresponds
 ! to the norm of the longitudinal shift.
 !
 field_dir=field_dir*q0_def_norm/v_norm(field_dir)
 q_norm(1)=q0_def_norm
 if (.not.l_col_cut) bare_qpg(1,1)=q0_def_norm
 !
 ! Warn about missing [Vnl,r] commutator
 !
 if (.not.X%Vnl_included.and..not.Vnl_commutator_warning.and.l_optics) then
   call warning(' Missing non-local pseudopoential contribution')
   Vnl_commutator_warning=.TRUE.
 endif
 !
 ! Calculate the q-dependent oscillators (OptOsc)
 !
 call X_alloc('OptOsc',(/X%ib(2),Xen%nbm,Xk%nbz/))
 DIP_iq_dot_r = (0.0_SP,0.0_SP)
 !
 do i1 = 1,Xk%nbz
   !
   ik = Xk%sstar(i1,1)
   is = sop_inv(Xk%sstar(i1,2))
   field_dir_rot = matmul( dl_sop(:,:,is), field_dir )
   !
   do iv = X%ib(1), Xen%nbm
     !
     do ic = Xen%nbf+1, X%ib(2)
     !
#if defined _RAS
       if(lras) then
         if(.not.transloc(ic,iv,ik)) cycle
       endif
#endif
       !
       ! GPL_INCLUDE_START SPIN_IGNORE
       ! i_spin=1
       ! GPL_INCLUDE_END SPIN_IGNORE
       ! 
       ! GPL_EXCLUDE_START SPIN_IGNORE
       !
       do i_spin=1,n_sp_pol
         ! 
         ! GPL_EXCLUDE_END SPIN_IGNORE
         !
         ! DIP_iq_dot_r = i q . < v k | r | c k >
         !
         DIP_iq_dot_r(ic,iv,i1,i_spin) = & 
&          dot_product( field_dir_rot, DIP_iR_or_P(:,ic,iv,ik,i_spin) )
         if ( is > nsym/(i_time_rev+1) ) DIP_iq_dot_r(ic,iv,i1,i_spin) = &
&          dot_product( DIP_iR_or_P(:,ic,iv,ik,i_spin), field_dir_rot ) 
         !
         ! 
         ! GPL_EXCLUDE_START SPIN_IGNORE
         !
       enddo
       ! 
       ! GPL_EXCLUDE_END SPIN_IGNORE
       !
     enddo
     DIP_iq_dot_r(iv,iv,i1,:) = (1.0_SP,0.0_SP)
   enddo
 enddo
 call pp_redux_wait()
 !
 ! Clean up
 !
 call X_alloc('OptGrad') ! Used in O_transverse/O_longitudinal/ioOSTNTS
 !
end subroutine
