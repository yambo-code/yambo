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
 ! Presently there exist two gauges: (i ) the length   which uses <r>            
 !                                   (ii) the velocity which uses <v>=<p-[x,Vnl]> 
 !
 ! DIP_iR=i<r>
 ! DIP_P = <v>   (should be called DIP_V ... )
 !
 ! For each of the two gauges there exist three approaches of computing the dipoles
 ! 1) G space approach      --> directly computes <v> and then <r>=<v>/DeltaE (called transverse approach as well)
 ! 2) real space approach   --> directly computes <r> and then <v>=<r>*DeltaE (only for non periodic directions)
 ! 2) shifted kpts approach --> computes q*<nk|r|mk> as <nk|r|mk+q> with small q (phases not defined)
 !                              and then <v> as <r>*DeltaE
 ! 3) covariant approach    --> computes <nk|r|mk> in reciprocal space as <nk|partial_k|mk>
 !                              takes into account the phases. symmetries not yet implemented 
 !                              and then <v> as <r>*DeltaE
 !
 use drivers,        ONLY:l_optics
 use LIVE_t,         ONLY:is_section_time_short
 use pars,           ONLY:SP,cZERO
 use com,            ONLY:warning,grid_path
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp
 use X_m,            ONLY:X_alloc,X_t,DIP_iR,use_covariant_approach,use_real_space_approach,&
&                         Vnl_commutator_warning,use_g_space_approach,Dipole_approach,&
&                         use_shifted_grids_approach,DIP_P,force_v_g_space_approach
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
 logical           :: use_dipole_transverse,idir_not_done(3)
 !
 ! I/O
 !
 integer           :: ID,io_err
 integer, external :: io_DIPOLES
#if defined _SC
 logical           :: l_evaluating_em1s 
#endif
 !
 ! Setup logicals 
 !================
 !
 use_g_space_approach        = trim(Dipole_approach)=='G-space v' 
 use_real_space_approach     = trim(Dipole_approach)=='R-space x'
 use_covariant_approach      = trim(Dipole_approach)=='Covariant'
 use_shifted_grids_approach  = trim(Dipole_approach)=='Shifted grids'
 !
 call parser('PDirect'  ,force_v_g_space_approach)
 !
#if defined _SC
 force_v_g_space_approach=force_v_g_space_approach.or.l_sc_run.or.l_real_time
#endif
 force_v_g_space_approach=force_v_g_space_approach.and.(.not.use_g_space_approach)
 use_dipole_transverse   =force_v_g_space_approach.or.use_g_space_approach
 idir_not_done           =.true.
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
   ! Allocation
   !
   call Dipoles_alloc()
   !
   ! GPL_EXCLUDE_START 
   !
   if (use_shifted_grids_approach) call DIPOLE_shifted_grids(Xen,Xk,X)
   !
   if (use_covariant_approach)     call DIPOLE_build_covariants(Xen,Xk,X)
   !
   if (use_real_space_approach)    call DIPOLE_x_real_space(Xen,Xk,X,idir_not_done)
   !
   ! GPL_EXCLUDE_END 
   !
   if (use_dipole_transverse)      call DIPOLE_transverse(Xen,Xk,X)
   !
   ! If the calculation of the dipoles is very short the I/O is switched off
   if (is_section_time_short(COMM=PAR_COM_q_for_Xo%COMM)) then
     io_DIP=.FALSE.
     call warning('DIPOLEs I/O switched-off to prevent MPI communication problems')
   endif
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
