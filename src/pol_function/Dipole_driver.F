!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
 use drivers,        ONLY:l_optics,l_real_time,l_sc_run
 use pars,           ONLY:SP,cI
 use com,            ONLY:warning,error
#if defined _RAS
 use ras_module,     ONLY:transloc, lras
#endif
 use drivers,        ONLY:l_col_cut
 use electrons,      ONLY:levels,n_sp_pol
 use par_proc_m,     ONLY:pp_redux_wait
 use D_lattice,      ONLY:nsym,i_time_rev,dl_sop,sop_inv
 use R_lattice,      ONLY:bz_samp,q0_def_norm,q_norm,bare_qpg
 use fields,         ONLY:grid_path,global_gauge
 use X_m,            ONLY:DIP_q_dot_iR,X_alloc,X_t,DIP_iR,Dipole_bands_ordered,&
&                         Vnl_commutator_warning,Dipole_uses_shifted_grids,DIP_P
 use IO_m,           ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP
 use wave_func,      ONLY:wf_ng,WF_load,WF_free
 use vec_operate,    ONLY:v_norm
#if defined _SC      
 use X_m,            ONLY:P_square
#endif
#if defined _DISTRIBUTED
 use WF_distribute,  ONLY:WF_states_setup
#endif
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
 real(SP)                  :: field_dir_rot(3),E_m_Ep
 logical                   :: use_trans_gauge
 !
 !I/O
 !
 integer :: ID,io_err
 integer, external :: io_DIPOLES
 !
#if !defined _RAS
 if (allocated(DIP_q_dot_iR)) return
#endif
 !
 ! Transitions ordering
 !
 Dipole_bands_ordered=.true.
 !
 ! NOTE that in case we will enter here during a SC run to update the screened interaction
 ! all transitions must be considered in order to rotate <P>.
 !
 ! in real-time simulations P and P^2 are ALWAYS calculated => no band ordering
 !
#if defined _SC 
 !
 if(l_sc_run.or.l_real_time) Dipole_bands_ordered=.FALSE.
 !
#endif
 !
 Dipole_uses_shifted_grids=trim(grid_path)/='none'
 !
#if defined _SC 
 !
 ! GAUGE switch
 !==============
 !
 ! velocity imposed in _SC (imposed if non-local potentials)
 !
! if(trim(gauge_type)/='velocity'.and.trim(gauge_type)/='length'.and.&
 !
#endif
 !
 ! Check first if ostnts DB is already done
 !
 X%ngostnts=wf_ng
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=io_DIPOLES(X,Xen,ID)
 !
 if (io_err.ne.0) then
   !
   ! OptGrad allocation...
   !
   ! When using internal SC potentials oscilators must be rotated
   ! in the new basis. So all transitions are needed
   !
   if (Dipole_bands_ordered) then
     call X_alloc('DIP_iR',(/3,X%ib(2),Xen%nbm,Xk%nibz/))
   else
     call X_alloc('DIP_iR',(/3,X%ib(2),X%ib(2),Xk%nibz/))
   endif
   DIP_iR=(0._SP,0._SP)
   !
#if defined _RT
   call X_alloc('DIP_P'   ,(/3,X%ib(2),X%ib(2),Xk%nibz/))
   call X_alloc('P_square',(/X%ib(2),X%ib(2),Xk%nibz/))
   DIP_P=(0._SP,0._SP)
   P_square=(0._SP,0._SP)
#endif
   !   
#if defined _DISTRIBUTED
   !
   if(trim(grid_path)=='none'.and.Dipole_bands_ordered) then
     !
     ! Valence bands are loaded by all processors
     !
     call WF_states_setup(k_to_load=(/1,Xk%nibz/),b_to_load=(/1,Xen%nbm/),par_loop=.FALSE.)
     !
     ! Parallelization on conductions bands and k-points
     !
     call WF_states_setup(k_to_load=(/1,Xk%nibz/),b_to_load=(/Xen%nbf+1,X%ib(2)/))
     !
     ! Report
     !
     call WF_states_setup(b_to_load=(/1,X%ib(2)/),report=.true.)
     !
   endif
   !
#endif
   !
#if !defined _MANYK
   call WF_load(0,1,X%ib,(/1,Xk%nibz/),space='G',title='-Oscillators/G space')
#endif
   !
   use_trans_gauge = .true.
   !
   ! GPL_EXCLUDE_START RAS_IGNORE
   !
   ! Attempt calculation using shifted grids if requested
   !
   if(Dipole_uses_shifted_grids) then
     !
     ! Use shifted grids, supporting fallback to transverse gauge
     !
     call Dipole_shifted_grids(Xen,Xk,X,field_dir,use_trans_gauge)
     !
     if (use_trans_gauge) call warning('Reverting to transverse gauge')
     if (use_trans_gauge) Dipole_uses_shifted_grids=.FALSE.
     !
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
   !
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
   io_err=io_DIPOLES(X,Xen,ID)
   !
 end if
 !
#if defined _RT  
 if (l_real_time) return
#endif
 !
 ! The field direction and the gamma-point norm must be renormalized here in case the
 ! oscillator strengths have been calculated using the shifted grids.
 ! In this case q0_def_norm is not the default one but corresponds
 ! to the norm of the grid shift.
 !
 field_dir=field_dir*q0_def_norm/v_norm(field_dir)
 q_norm(1)=q0_def_norm
 if (.not.l_col_cut) bare_qpg(1,1)=q0_def_norm
 !
 ! Warn about missing [Vnl,r] commutator
 !
 if (.not.X%Vnl_included.and..not.Vnl_commutator_warning.and.l_optics) then
   call warning(' Missing non-local pseudopotential contribution')
   Vnl_commutator_warning=.TRUE.
 endif
 !
 ! Calculate the q-dependent oscillators
 !
 call X_alloc('DIP_q_dot_iR',(/X%ib(2),Xen%nbm,Xk%nbz/))
 DIP_q_dot_iR = (0.0_SP,0.0_SP)
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
         DIP_q_dot_iR(ic,iv,i1,i_spin) = dot_product( field_dir_rot, DIP_iR(:,ic,iv,ik,i_spin) )
         if ( is > nsym/(i_time_rev+1) ) DIP_q_dot_iR(ic,iv,i1,i_spin) = dot_product( DIP_iR(:,ic,iv,ik,i_spin), field_dir_rot ) 
         !
         ! GPL_EXCLUDE_START SPIN_IGNORE
         !
       enddo
       ! 
       ! GPL_EXCLUDE_END SPIN_IGNORE
       !
     enddo
     DIP_q_dot_iR(iv,iv,i1,:) = (1.0_SP,0.0_SP)
   enddo
   !
 enddo
 call pp_redux_wait()
 !
 ! Clean up
 !
 call X_alloc('DIP_iR') 
 !
end subroutine
