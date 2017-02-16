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
subroutine X_O_strengths(Xen,Xk,X,field_dir)
 !
 use pars,           ONLY:SP
 use com,            ONLY:warning
 use drivers,        ONLY:l_col_cut
 use electrons,      ONLY:levels,n_sp_pol
 use par_proc_m,     ONLY:pp_redux_wait
 use D_lattice,      ONLY:nsym,i_time_rev,dl_sop,sop_inv
 use R_lattice,      ONLY:bz_samp,q0_def_norm,q_norm,bare_qpg
 use X_m,            ONLY:X_rhoq0,X_alloc,X_t,X_q0grad,long_path,P_bands_ordered
 use IO_m,           ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP
 use memory_m,       ONLY:mem_est
 use wave_func,      ONLY:wf_ng,wf_load,WF_free
 use vec_operate,    ONLY:v_norm
 implicit none
 !
 type(bz_samp), intent(inout) :: Xk
 type(levels),  intent(inout) :: Xen
 type(X_t),  intent(inout) :: X
 real(SP),   intent(inout) :: field_dir(3)
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
 if (allocated(X_rhoq0)) return
 !
 !
 ! Transitions ordering
 !
 P_bands_ordered=.true.
 !
 ! Check first if ostnts DB is already done
 !
 X%ngostnts=wf_ng
 !
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=ioOSTNTS(X,Xen,ID)
 !
 if (io_err.ne.0) then
   !
   ! OptGrad allocation...
   !
   ! When using internal SC potentials oscilators must be rotated
   ! in the new basis. So all transitions are needed
   !
   if (P_bands_ordered) then
     call X_alloc('OptGrad',(/3,X%ib(2),Xen%nbm,Xk%nibz/))
   else
     call X_alloc('OptGrad',(/3,X%ib(2),X%ib(2),Xk%nibz/))
   endif
   X_q0grad=(0.,0.)
   !
   call wf_load(0,1,X%ib,(/1,Xk%nibz/),space='G',title='-Oscillators/G space')
   !
   use_trans_gauge = .true.
   !
   !
   if (use_trans_gauge) call X_O_transverse(Xen,Xk,X)
   !
   ! Free any loaded WFs
   !
   call WF_free()
   !
   ! Write the q0grad to its database
   !
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
   io_err=ioOSTNTS(X,Xen,ID)
   !
 end if
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
 ! Calculate the q-dependent oscillators (OptOsc)
 !
 call X_alloc('OptOsc',(/X%ib(2),Xen%nbm,Xk%nbz/))
 X_rhoq0 = (0.0_SP,0.0_SP)
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
       do i_spin = 1, n_sp_pol
         !
         ! X_rhoq0 = i q . < v k | r | c k >
         !
         X_rhoq0(ic,iv,i1,i_spin) = & 
&          dot_product( field_dir_rot, X_q0grad(:,ic,iv,ik,i_spin) )
         if ( is > nsym/(i_time_rev+1) ) X_rhoq0(ic,iv,i1,i_spin) = &
&          dot_product( X_q0grad(:,ic,iv,ik,i_spin), field_dir_rot ) 
         !
       enddo
     enddo
     X_rhoq0(iv,iv,i1,:) = (1.0_SP,0.0_SP)
   enddo
 enddo
 call pp_redux_wait()
 !
 ! Clean up
 !
 call X_alloc('OptGrad') ! Used in O_transverse/O_longitudinal/ioOSTNTS
 !
end subroutine
