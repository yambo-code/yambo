!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine X_O_strengths_cut(Xen, Xk, X, field_dir)
!====================================
 use pars
 use com,                     ONLY : msg, warning
 use timing,                  ONLY : live_timing
 use electrons,               ONLY : levels, n_spin, n_spinor, n_sp_pol
 use par_proc_m,              ONLY : pp_redux_wait, pp_indexes, myid, &
&                                    master_node, pp_indexes_reset
 use par_indexes_m,           ONLY : par_indexes
 use D_lattice,               ONLY : nsym, i_time_rev, alat, dl_sop, sop_inv
 use R_lattice,               ONLY : g_vec, bz_samp, q0_def_norm,q_norm,bare_qpg
 use X_m,                     ONLY : X_alloc, X_t, long_path
 use IO_m,                    ONLY : io_control, OP_RD_CL, OP_WR_CL, VERIFY, REP
 use memory_m,                ONLY : mem_est
 use wave_func,               ONLY : wf, wf_ng, wf_state, wf_load, WF_free
 use optcut,                  ONLY : Xcut_q0grad, Xcut_rhoq0, ng_limits, setup_optcut, pscut
 use drivers,                 ONLY : l_col_cut
#if defined _RAS
 use ras_module,              ONLY : transloc, lras
#endif
  use vec_operate,    ONLY:v_norm
 implicit none
 type(bz_samp),  intent(in)       :: Xk
 type(levels),   intent(in)       :: Xen
 type(X_t)                        :: X
 real(SP),          intent(inout) :: field_dir(3)
!ws
 integer                          :: io_db, io_err, i_spin
 integer                          :: ik, i1, icfft, ivfft, ic, iv, is

 real(SP)                         :: omega, field_dir_rot(3)
 type(pp_indexes)                 :: px
 complex(SP)                      :: PS(3), spinor_avg(3)
 real(SP), parameter          :: fac = 0.70710678118654752440_SP
 character(schlen)                :: sch ! DEBUG
 logical                          :: use_trans_gauge
!functions
 integer,  external               :: ioOSTNTS_cut

 call section('=','Optical oscillators with cut off')

 if (allocated(Xcut_rhoq0)) return
 
 X%ngostnts = wf_ng
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=io_db)
 io_err = ioOSTNTS_cut(X, Xen, io_db)

 if (io_err.ne.0) then
   !
   call pp_indexes_reset(px)
   !
   ! WF loading (needed in both cases)
   !
#if !defined _BIGSYS
   call wf_load(0, 1, X%ib, (/1,Xk%nibz/), space='G', title='-Oscillators/G space')
#endif
   !
   ! Set up data needed for cut off routines
   !
   call nG_limits( Xk%nibz )
   !
   ! OptGrad cut allocation
   !
   allocate(Xcut_q0grad(3, X%ib(2), Xen%nbm, Xk%nibz, n_spin))
   call mem_est('Xcut_q0grad', (/product( (/3, X%ib(2), Xen%nbm, Xk%nibz, n_spin/) )/) )
   Xcut_q0grad = (0.,0.)

   use_trans_gauge = .true.
   if(trim(long_path)/='none') then
     !
     ! Use longitudinal gauge, supporting fallback to transverse gauge
     !
     call X_O_longitudinal_cut(Xen,Xk,X,field_dir, use_trans_gauge)
     !
     if(use_trans_gauge) call warning('Reverting to transverse gauge')
   endif
   !
   if(use_trans_gauge) call X_O_transverse_cut(Xen,Xk,X)

   !
   ! Free any loaded WFs
   !
   call WF_free()
   !
   if (master_node) call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=io_db)
   io_err=ioOSTNTS_cut(X,Xen,io_db)
 endif
 !
 ! The field direction and the gamma-point norm must be renormalized here in case the
 ! oscillator strengths have been calculated using the long. gauge.
 ! In this case q0_def_norm is not the default one but corresponds
 ! to the norm of the longitudinal shift.
 !

! THIS MUST BE CHECKED FOR LONG GAUGE + CUT

 field_dir=field_dir*q0_def_norm/v_norm(field_dir)
 q_norm(1)=q0_def_norm
 if (.not.l_col_cut) bare_qpg(1,1)=q0_def_norm
 !
 ! Calculate the q-dependent oscillators (OptOscCut)
 !
 allocate(Xcut_rhoq0(X%ib(2),Xen%nbm,Xk%nbz,n_spin))
 call mem_est('Xcut_rhoq0',(/ product( (/X%ib(2),Xen%nbm,Xk%nbz,n_spin/) ) /) )
 Xcut_rhoq0=(0.0_SP,0.0_SP)

 do i1=1,Xk%nbz
   !
   ik  = Xk%sstar(i1, 1)
   is  = sop_inv(Xk%sstar(i1, 2) )
   field_dir_rot = matmul( dl_sop(:,:,is), field_dir )
   !
   do iv = X%ib(1), Xen%nbm
     do ic = Xen%nbf+1, X%ib(2)
     !
#if defined _RAS
       if(lras) then
         if(.not.transloc(ic,iv,ik)) cycle
       endif
#endif
       do i_spin = 1, n_sp_pol
         !
         ! X_rhoq0 = i q . < v k | r | c k >
         !
         Xcut_rhoq0(ic,iv,i1,i_spin) = &
&          dot_product( field_dir_rot, Xcut_q0grad(:,ic,iv,ik,i_spin) ) 
         if ( is > nsym/(i_time_rev+1) ) Xcut_rhoq0(ic,iv,i1,i_spin) = &
&          dot_product( Xcut_q0grad(:,ic,iv,ik,i_spin), field_dir_rot ) 
       enddo
     enddo
     Xcut_rhoq0(iv,iv,i1,:)=(1.0_SP,0.0_SP)
   enddo
 enddo
 call pp_redux_wait()
 !
 ! Clean up
 !
 deallocate(Xcut_q0grad)
 call mem_est('Xcut_q0grad')
 return

end subroutine X_O_strengths_cut
