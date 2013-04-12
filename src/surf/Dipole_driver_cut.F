!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
 subroutine Dipole_driver_cut(Xen, Xk, X, field_dir)
!====================================
 use pars
 use com,                     ONLY : msg, warning
 use timing,                  ONLY : live_timing
 use electrons,               ONLY : levels, n_spin, n_spinor, n_sp_pol
 use parallel_m,              ONLY : pp_redux_wait, pp_indexes, myid, &
&                                    master_cpu, pp_indexes_reset
 use D_lattice,               ONLY : nsym, i_time_rev, alat, dl_sop, sop_inv
 use R_lattice,               ONLY : g_vec, bz_samp, q0_def_norm,q_norm,bare_qpg
 use X_m,                     ONLY : X_alloc, X_t, Dipole_bands_ordered
 use fields,                  ONLY : grid_path
 use IO_m,                    ONLY : io_control, OP_RD_CL, OP_WR_CL, VERIFY, REP
 use memory_m,                ONLY : mem_est
 use wave_func,               ONLY : wf, wf_ng, wf_state, WF_load, WF_free
 use optcut,                  ONLY : DIP_iR_cut, DIP_q_dot_iR_cut, ng_limits, setup_optcut, pscut
 use drivers,                 ONLY : l_col_cut
#if defined _SURF
 use ras_module,              ONLY : transloc, lras
#endif
  use vec_operate,    ONLY:v_norm
#if defined _DISTRIBUTED
 use WF_distribute,  ONLY:WF_states_setup
#endif
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
 character(schlen)                :: sch 
 logical                          :: use_trans_gauge
!functions
 integer,  external               :: io_DIPOLES_cut

 call section('=','Optical oscillators with cut off')

 if (allocated(DIP_q_dot_iR_cut)) return
 Dipole_bands_ordered=.true.
 
 X%ngostnts = wf_ng
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=io_db)
 io_err = io_DIPOLES_cut(X, Xen, io_db)

 if (io_err.ne.0) then
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
   ! WF loading (needed in both cases)
   !
   call WF_load(0, 1, X%ib, (/1,Xk%nibz/), space='G', title='-Oscillators/G space')
   !
   ! Set up data needed for cut off routines
   !
   call nG_limits( Xk%nibz )
   !
   ! OptGrad cut allocation
   !
   allocate(DIP_iR_cut(3, X%ib(2), Xen%nbm, Xk%nibz, n_spin))
   call mem_est('DIP_iR_cut', (/product( (/3, X%ib(2), Xen%nbm, Xk%nibz, n_spin/) )/) )
   DIP_iR_cut = (0.,0.)
   !
   ! Cutoff with shifted grids has been removed because it is too unstable.
   !
   if(trim(grid_path)/='none') call warning('Cutoff with shifted grids not supported! Reverting to transverse gauge')
   !
   call Dipole_transverse_cut(Xen,Xk,X)
   !
   ! Free any loaded WFs
   !
   call WF_free()
   !
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=io_db)
   io_err=io_DIPOLES_cut(X,Xen,io_db)
 endif
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
 ! Calculate the q-dependent oscillators (OptOscCut)
 !
 allocate(DIP_q_dot_iR_cut(X%ib(2),Xen%nbm,Xk%nbz,n_spin))
 call mem_est('DIP_q_dot_iR_cut',(/ product( (/X%ib(2),Xen%nbm,Xk%nbz,n_spin/) ) /) )
 DIP_q_dot_iR_cut=(0.0_SP,0.0_SP)

 do i1=1,Xk%nbz
   !
   ik  = Xk%sstar(i1, 1)
   is  = sop_inv(Xk%sstar(i1, 2) )
   field_dir_rot = matmul( dl_sop(:,:,is), field_dir )
   !
   do iv = X%ib(1), Xen%nbm
     do ic = Xen%nbf+1, X%ib(2)
     !
#if defined _SURF
       if(lras) then
         if(.not.transloc(ic,iv,ik)) cycle
       endif
#endif
       do i_spin = 1, n_sp_pol
         !
         ! DIP_iq_dot_r = i q . < v k | r | c k >
         !
         DIP_q_dot_iR_cut(ic,iv,i1,i_spin) = &
&          dot_product( field_dir_rot, DIP_iR_cut(:,ic,iv,ik,i_spin) ) 
         if ( is > nsym/(i_time_rev+1) ) DIP_q_dot_iR_cut(ic,iv,i1,i_spin) = &
&          dot_product( DIP_iR_cut(:,ic,iv,ik,i_spin), field_dir_rot ) 
       enddo
     enddo
     DIP_q_dot_iR_cut(iv,iv,i1,:)=(1.0_SP,0.0_SP)
   enddo
 enddo
 call pp_redux_wait()
 !
 ! Clean up
 !
 deallocate(DIP_iR_cut)
 call mem_est('DIP_iR_cut')
 return

end subroutine Dipole_driver_cut
