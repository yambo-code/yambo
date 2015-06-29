!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
 ! Presently there exist two gauges: (i ) the length which uses <r>
 !                                   (ii) the velocity which uses <p>
 !
 ! For each of the two gauges there exist three approaches of computing the dipoles
 ! 1) transverse approach   --> directly computes <p> and then <r>=<p>/DeltaE
 ! 2) shifted kpts approach --> computes q*<nk|r|mk> as <nk|r|mk+q> with small q (phases not defined)
 !                              and then <p> as <r>*DeltaE
 ! 3) covariant approach    --> computes <nk|r|mk> in reciprocal space as <nk|partial_k|mk>
 !                              takes into account the phases. symmetries not yet implemented 
 !
 use drivers,        ONLY:l_optics
 use LIVE_t,         ONLY:is_section_time_short
 use pars,           ONLY:SP,cZERO
 use com,            ONLY:warning,grid_path
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp
 use X_m,            ONLY:X_alloc,X_t,DIP_iR,use_covariant_approach,&
&                         Vnl_commutator_warning,use_direct_approach,&
&                         use_shifted_grids_approach,DIP_P,force_p_direct_approach
 use IO_m,           ONLY:io_control,OP_RD_CL,VERIFY,REP,OP_WR_CL,OP_APP_WR_CL,OP_RD,RD_CL_IF_END,&
&                         IO_and_Messaging_switch,io_DIP
 use wave_func,      ONLY:wf_ng
 use parallel_m,     ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_nibz,master_cpu,PAR_COM_k_for_P,ncpu,&
&                         PAR_COM_q_for_Xo
 use parser_m,       ONLY:parser
#if defined _SC
 use com,            ONLY:secnm
 use drivers,        ONLY:l_real_time,l_sc_run
 use X_m,            ONLY:P_square
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
 integer           :: ik,i_sp_pol
 logical           :: use_dipole_transverse
 !
 ! I/O
 !
 integer           :: ID,io_err
 integer, external :: io_DIPOLES
#if defined _SC
 logical           :: l_evaluating_em1s 
#endif
 !
 ! Setup logicals and other variables
 !====================================
 !
 call parser('DipoleCov',use_covariant_approach)
 call parser('PDirect'  ,force_p_direct_approach)
 !
 use_shifted_grids_approach = trim(grid_path)/='none'
 use_direct_approach        = (.not.use_covariant_approach).and.(.not.use_shifted_grids_approach)
 !
 X%ngostnts=wf_ng
 !
#if defined _SC 
 l_evaluating_em1s=index(secnm,"Dielectric")/=0
#endif
 !
 ! Check if Dipoles DBs exist and are ok
 !=======================================
 call Dipoles_IO('read ')
 !
 ! In case dipoles were not found/ok then I need to compute them
 !==============================================================
 if (io_err/=0) then
   !
   ! I/O privilegies: temporarly switch it on
   !
   call IO_and_Messaging_switch("+io_out")
   !
   ! OptGrad allocation...
   !
   call Dipoles_alloc()
   !
   ! Here I avoid the deallocation of states_to_load as I ASSUME that AFTER this routine 
   ! there will be other WF_load calls using the present WF distribution. 
   !
   ! GPL_EXCLUDE_START 
   !
   ! Using shifted grids; supporting fallback to direct approach
   !
   if (use_shifted_grids_approach) call DIPOLE_shifted_grids(Xen,Xk,X)
   !
   ! Using covariant derivatives; supporting fallback to direct approach
   !
   if (use_covariant_approach)     call DIPOLE_build_covariants(Xen,Xk,X)
   !
   ! GPL_EXCLUDE_END 
   !
   ! Using the direct approach, i.e. computing <P>
   !
   force_p_direct_approach=force_p_direct_approach.and.(.not.use_direct_approach)
   use_dipole_transverse  =use_direct_approach  .or.force_p_direct_approach
#if defined _SC
   use_dipole_transverse  =use_dipole_transverse.or.l_sc_run.or.l_real_time
#endif
   if (use_dipole_transverse)      call DIPOLE_transverse(Xen,Xk,X)
   !
#if defined _SC
   ! if(.not.l_sc_run.and.l_rotate_dipoles) call DIPOLE_rotate(SC_R,Xen,Xk,X)
#endif
   !
   ! If the calculation of the dipoles is very short the I/O is switched off
   !
#if !defined _YPP_RT
   if (is_section_time_short(COMM=PAR_COM_q_for_Xo%COMM)) then
     io_DIP=.FALSE.
     call warning('DIPOLEs I/O switched-off to prevent MPI communication problems')
   endif
#endif
   !
   call Dipoles_IO('write')
   !
   ! I/O privilegies: RESTORE to previous values
   !
   call IO_and_Messaging_switch("RESTORE")
   !
 endif
 !
#if defined _SC
 if ((l_sc_run.or.l_real_time).and..not.l_evaluating_em1s) return
#endif
 !
 ! Warn about missing [Vnl,r] commutator
 !
 if (.not.X%Vnl_included.and..not.Vnl_commutator_warning.and.l_optics.and.use_dipole_transverse) then
   call warning(' Missing non-local pseudopotential contribution')
   Vnl_commutator_warning=.TRUE.
 endif
 !
 ! Finally project the dipoles along q
 !=====================================
 call DIPOLE_project_along_q(Xen,Xk,X,field_dir)
 !
 ! Clean up
 !
 call X_alloc('DIP_iR') 
 call X_alloc('DIP_P')
#if defined _SC
 call X_alloc('P_square') 
#endif
 !
 contains
   !
   subroutine Dipoles_IO(read_or_write)
     !
     character(5), intent(in) :: read_or_write
     !
     integer                  :: IO_ACTION
     logical                  :: do_io,reading,writing
     !
     reading=trim(read_or_write)=="read"
     writing=trim(read_or_write)=="write"
     !
     do_io  =master_cpu.or.reading
     !
     if(do_io) then
       if(reading) IO_ACTION=OP_RD_CL
       if(writing) IO_ACTION=OP_WR_CL
       call io_control(ACTION=IO_ACTION,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
       io_err=io_DIPOLES(X,Xen,ID)
       !
     endif
     !
     if (io_err==0) then
       !
       if(reading) IO_ACTION=OP_RD_CL
       if(writing) IO_ACTION=OP_APP_WR_CL
       !
       do ik=1,Xk%nibz
         if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle
         if (.not. (PAR_COM_k_for_P%CPU_id==0.or.PAR_COM_k_for_P%n_CPU==ncpu) .and. writing) cycle
         do i_sp_pol=1,n_sp_pol
           call io_control(ACTION=IO_ACTION,COM=REP,SEC=(/1+ik+(i_sp_pol-1)*Xk%nibz/),ID=ID)
           io_err=io_DIPOLES(X,Xen,ID)
        enddo
       enddo
     endif
     !
   end subroutine Dipoles_IO
   !
   !
   subroutine Dipoles_alloc()
     !
     call X_alloc('DIP_iR',(/3,X%ib(2),X%ib(2),PAR_DIPk_nibz/))
     call X_alloc('DIP_P' ,(/3,X%ib(2),X%ib(2),PAR_DIPk_nibz/))
     DIP_iR=cZERO
     DIP_P=cZERO
     !
#if defined _SC
     if (l_sc_run.or.l_real_time) then
       call X_alloc('P_square',(/X%ib(2),X%ib(2),PAR_DIPk_nibz/))
       P_square=cZERO
     endif
#endif
     !
   end subroutine Dipoles_alloc
   !
end subroutine
