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
 subroutine X_O_strengths(Xen,Xk,X,field_dir)
 !
#if defined PJ_GKS
 use pars,           ONLY:SP,lchlen
 use com,            ONLY:warning,inpath
 use xc_functionals, ONLY:GS_xc_FUNCTIONAL
 use DFT,            ONLY:lgks,igks,auxpath
#else
 use pars,           ONLY:SP
 use com,            ONLY:warning
#endif
 use drivers,        ONLY:l_col_cut
 use electrons,      ONLY:levels,n_sp_pol
 use par_proc_m,     ONLY:pp_redux_wait,master_node
 use D_lattice,      ONLY:nsym,i_time_rev,dl_sop,sop_inv
 use R_lattice,      ONLY:bz_samp,q0_def_norm,q_norm,bare_qpg
 use X_m,            ONLY:X_rhoq0,X_alloc,X_t,X_q0grad,long_path
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
#if defined PJ_GKS
 character(lchlen) :: save_inpath
 integer :: io_rld, nb_aux
 type(bz_samp) :: k
 type(levels)  :: en
 integer, external:: ioDB1_reload
#endif
 !
 !I/O
 !
 integer :: ID,io_err
 integer, external :: ioOSTNTS
 !
 if (allocated(X_rhoq0)) return
 !
 ! Check first if ostnts DB is already done
 !
 X%ngostnts=wf_ng
 !
 ! If I am trying to go from a transverse to a long. gauge
 ! calculation X%long_gauge will be false here even if a long_path is
 ! provided
 !
 if (.not.X%long_gauge) X%long_gauge=trim(long_path)/='none'
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=ioOSTNTS(X,Xen,ID)
 !
 if (io_err.ne.0) then
   !
   ! OptGrad allocation
   !
#if  defined PJ_GKS
   if (GS_xc_FUNCTIONAL > 400) then
     lgks = .true.
     igks = GS_xc_FUNCTIONAL
     auxpath = 'AUX/' 
     io_rld = ioDB1_reload(en,k,trim(auxpath))
     nb_aux = en%nb
     allocate(Xen%Eo(Xen%nb,Xen%nk,n_sp_pol))
     call mem_est("Eo",(/size(Xen%Eo)/),(/SP/)) 
          Xen%Eo=en%E
     call X_alloc('OptGrad',(/3,nb_aux,nb_aux,Xk%nibz/))
   else           
    lgks = .false.
     call X_alloc('OptGrad',(/3,X%ib(2),Xen%nbm,Xk%nibz/))
   endif
#else
   call X_alloc('OptGrad',(/3,X%ib(2),Xen%nbm,Xk%nibz/))
#endif
   X_q0grad=(0.,0.)
   !
   ! Wavefunctions need to be read in both cases
   !
#if  defined PJ_GKS
   if (lgks) then
     save_inpath = inpath
     inpath = auxpath
     call wf_load(0,1,(/1,nb_aux/),(/1,Xk%nibz/),space='G',&
&                 title='-Oscillators (AUX)/G space')
     inpath = save_inpath
   else
     call wf_load(0,1,X%ib,(/1,Xk%nibz/),space='G',title='-Oscillators/G space')
   end if
#else
   call wf_load(0,1,X%ib,(/1,Xk%nibz/),space='G',title='-Oscillators/G space')
#endif
   !
   ! Attempt calculation within longitudinal gauge if requested
   !
   use_trans_gauge = .true.
   if(trim(long_path)/='none') then
     !
     ! Use longitudinal gauge, supporting fallback to transverse gauge
     !
     call X_O_longitudinal(Xen,Xk,X,field_dir,use_trans_gauge)
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
   if (use_trans_gauge) call X_O_transverse(Xen,Xk,X)
   !
   ! Free any loaded WFs
   !
   call WF_free()
   !
#if defined PJ_GKS
   ! 
   ! When using xc non-local approximation Vnl add the contribution [r,Vnl]
   ! 
   if (lgks) then
     deallocate(Xen%Eo)      
     call X_O_gks(Xen,Xk,X)
     io_rld = ioDB1_reload(en,k,trim(inpath))
   end if
   !
#endif
   ! Write the q0grad to its database
   !
   if (master_node) call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
   io_err=ioOSTNTS(X,Xen,ID)
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
   ik = Xk%sstar(i1,1)
   is = sop_inv(Xk%sstar(i1,2))
   field_dir_rot = matmul( dl_sop(:,:,is), field_dir )
   do iv = X%ib(1), Xen%nbm
     do ic = Xen%nbf+1, X%ib(2)
       do i_spin = 1, n_sp_pol
         !
         ! X_rhoq0 = i q . < v k | r | c k >
         !
         X_rhoq0(ic,iv,i1,i_spin) = & 
&          dot_product( field_dir_rot, X_q0grad(:,ic,iv,ik,i_spin) )
         if ( is > nsym/(i_time_rev+1) ) X_rhoq0(ic,iv,i1,i_spin) = &
&          dot_product( X_q0grad(:,ic,iv,ik,i_spin), field_dir_rot ) 
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
