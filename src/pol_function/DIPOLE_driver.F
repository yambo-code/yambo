!
!        Copyright (C) 2000-2015 the YAMBO team
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
subroutine Dipole_driver(Xen,Xk,X,field_dir)
 !
 use drivers,        ONLY:l_optics,l_sc_run
 use LIVE_t,         ONLY:is_section_time_short
 use interfaces,     ONLY:WF_load
 use pars,           ONLY:SP
 use com,            ONLY:warning,grid_path
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp,k_map
 use X_m,            ONLY:X_alloc,X_t,DIP_iR,Dipole_bands_ordered,Dipole_covariant,&
&                         DIP_cov,l_rotate_dipoles,Vnl_commutator_warning,         &
&                         Dipole_uses_shifted_grids,DIP_P,DIP_S
 use IO_m,           ONLY:io_control,OP_RD_CL,VERIFY,REP,OP_WR_CL,OP_APP_WR_CL,OP_RD,RD_CL_IF_END,&
&                         IO_and_Messaging_switch,io_DIP
 use wave_func,      ONLY:wf_ng,WF
 use parallel_m,     ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_nibz,master_cpu,PAR_COM_k_for_P,ncpu,&
&                         PAR_COM_q_for_Xo
#if defined _SC || defined _KERR
 use X_m,            ONLY:DIP_P
#endif
#if defined _KERR
 use X_m,            ONLY:global_gauge
#endif
 !
 implicit none
 !
 type(bz_samp), intent(inout) :: Xk
 type(levels),  intent(inout) :: Xen
 type(X_t),     intent(inout) :: X
 real(SP),      intent(inout) :: field_dir(3)
 !
 ! Work Space
 !
 integer           :: ik,bands_to_load(2)
 logical           :: use_trans_gauge
 !
 ! I/O
 !
 integer           :: ID,io_err(3)
 integer, external :: io_DIPOLES,io_Overlaps
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
 !
 !
 Dipole_uses_shifted_grids=trim(grid_path)/='none'
 !
 ! Check first if ostnts DB is already done
 !
 X%ngostnts=wf_ng
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
 io_err(1)=io_DIPOLES(X,Xen,ID) 
 !
 if (io_err(1)==0) then
   !
   do ik=1,Xk%nibz
     !
     if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle 
     !
     call io_control(ACTION=RD_CL_IF_END,COM=REP,SEC=(/1+ik/),ID=ID)
     io_err(1)=io_DIPOLES(X,Xen,ID)
     !
   enddo
   !
 endif
 !
 io_err(2:3)=0
 !
 !
 if (any(io_err/=0)) then
   !
   ! I/O privilegies: temporarly switch it on
   !
   call IO_and_Messaging_switch("+io_out")
   !
   ! OptGrad allocation...
   !
   ! When using internal SC potentials oscilators must be rotated
   ! in the new basis. So all transitions are needed
   !
   if (Dipole_bands_ordered) then
     call X_alloc('DIP_iR',(/3,X%ib(2),Xen%nbm,PAR_DIPk_nibz/))
   else
     call X_alloc('DIP_iR',(/3,X%ib(2),X%ib(2),PAR_DIPk_nibz/))
   endif
   DIP_iR=(0._SP,0._SP)
   !
#if defined _KERR
   if (trim(global_gauge)=='velocity') then
     call X_alloc('DIP_P'   ,(/3,X%ib(2),X%ib(2),PAR_DIPk_nibz/))
     DIP_P=(0._SP,0._SP)
   endif
#endif
   !
   bands_to_load=X%ib
   !
   !   
   ! WF load 
   !
   ! Here I avoid the deallocation of states_to_load as I ASSUME that AFTER this routine 
   ! there will be other WF_load calls using the present WF distribution. 
   !
   call WF_load(WF,0,1,bands_to_load,(/1,Xk%nibz/),space='G',title='-Oscillators/G space',&
&               force_WFo=(l_sc_run.or.l_rotate_dipoles),keep_states_to_load=.TRUE.)
   !
   use_trans_gauge = .true.
   !
   !
   if (use_trans_gauge)  call DIPOLE_transverse(Xen,Xk,X)
   !
   !
   ! Free any loaded WFs
   !
   call WF_free(WF)
   !
   ! If the calculation of the dipoles is very short the I/O is switched off
   !
   if (is_section_time_short(COMM=PAR_COM_q_for_Xo%COMM)) then
     io_DIP=.FALSE.
     call warning('DIPOLEs I/O switched-off to prevent MPI communication problems')
   endif
   !
   if (master_cpu) then
     call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),ID=ID)
     io_err(1)=io_DIPOLES(X,Xen,ID)
     !
     !
   endif
   !
   do ik=1,Xk%nibz
     !
     if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle 
     if (.not. (PAR_COM_k_for_P%CPU_id==0.or.PAR_COM_k_for_P%n_CPU==ncpu) ) cycle
     ! 
     call io_control(ACTION=OP_APP_WR_CL,COM=REP,SEC=(/1+ik/),ID=ID)
     io_err(1)=io_DIPOLES(X,Xen,ID)
     !
   enddo
   !
   !
   !
 end if
 !
 ! I/O privilegies: RESTORE to previous values
 !
 call IO_and_Messaging_switch("RESTORE")
 !
 !
 ! Warn about missing [Vnl,r] commutator
 !
 if (.not.X%Vnl_included.and..not.Vnl_commutator_warning.and.l_optics) then
   call warning(' Missing non-local pseudopotential contribution')
   Vnl_commutator_warning=.TRUE.
 endif
 !
 !
 call DIPOLE_dip_dot_field(Xen,Xk,X,field_dir)
 !
 ! Clean up
 !
 call X_alloc('DIP_iR') 
 !
 !
end subroutine
