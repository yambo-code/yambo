!
!        Copyright (C) 2000-2017 the YAMBO team
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
 ! PARALLEL structure: The dipoles are based on a "derived" structure. 
 !                     K-points are driven by these communicators and indexes:
 !
 ! PAR_IND_DIPk_ibz
 ! PAR_DIPk_nibz
 ! PAR_COM_DIPOLES_k_subgroup
 !
 ! while the general world where the calculations lives is
 !
 ! PAR_COM_DIPOLES
 !
 ! being a derived structure it must be mapped in the calling routine.
 !
 use drivers,        ONLY:l_optics
#if defined _PL
 use drivers,        ONLY:l_photolum
#endif
 use pars,           ONLY:SP,cZERO
 use com,            ONLY:warning
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp
 use X_m,            ONLY:X_alloc,X_t,DIP_iR,use_covariant_approach,use_real_space_approach,&
&                         Vnl_commutator_warning,use_g_space_approach,Dipole_approach,&
&                         use_shifted_grids_approach,DIP_P,force_v_g_space_approach,Dipole_Energy_treshold,&
&                         l_X_terminator
 use IO_m,           ONLY:io_control,OP_RD_CL,VERIFY,REP,OP_WR_CL,OP_APP_WR_CL,OP_RD,&
&                         IO_and_Messaging_switch
 use wave_func,      ONLY:wf_ng
 use parallel_m,     ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_nibz,&
&                         PP_redux_wait,PAR_COM_PLASMA_INDEX,PAR_COM_Q_INDEX,l_par_RT
 use parser_m,       ONLY:parser
#if defined _RT || defined _SC
 use drivers,        ONLY:l_real_time,l_sc_run
 use com,            ONLY:secnm
#endif
#if defined _RT
 use X_m,            ONLY:DIP_spin
 use electrons,      ONLY:n_spin
#endif
#if defined _SC || defined _RT
 use X_m,            ONLY:P_square
#endif
#if defined _QED
 use drivers,        ONLY:l_elphoton_corr
#endif
#if defined _TIMING
 use timing_m,       ONLY:timing
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
 logical           :: use_dipole_transverse,Dipole_bands_ordered,idir_not_done(3)
#if defined _SC || defined  _RT
 logical           :: l_evaluating_em1s
#endif
 !
 ! I/O
 !
 integer           :: ID,io_err
 integer, external :: io_DIPOLES
 !
#if defined _TIMING
 call timing('Dipoles',OPR='start')
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
 if(.not.any((/use_g_space_approach,use_real_space_approach,use_covariant_approach,use_shifted_grids_approach/))) then
   call warning(' Dipoles approach defined in input not recognised. Reverting to G-space v')
   use_g_space_approach = .true.
   Dipole_approach='G-space v'
 endif
 !
#if defined _SC || defined _RT
 force_v_g_space_approach=force_v_g_space_approach.or.l_sc_run.or.l_real_time
#endif
 force_v_g_space_approach=force_v_g_space_approach.and.(.not.use_g_space_approach)
 use_dipole_transverse   =force_v_g_space_approach.or.       use_g_space_approach
 idir_not_done           =.true.
 !
 X%ngostnts=wf_ng
 !
 !
 ! NOTE: 1 - that in case we will enter here during a SC run to update the
 !           screened interaction all transitions must be considered in order to rotate <P>.
 !       2 - when using internal SC potentials oscilators must be rotated
 !           in the new basis. So all transitions are needed.
 !       3 - In real-time simulations P and P^2 are ALWAYS calculated => no band ordering
 !
 Dipole_bands_ordered=.true.
 !
#if defined _SC  || defined _RT
 l_evaluating_em1s=index(secnm,"Dielectric")/=0
 if ((l_sc_run.or.l_real_time).and..not.l_evaluating_em1s) Dipole_bands_ordered=.false.
#endif
#if defined _QED 
 if (l_elphoton_corr) Dipole_bands_ordered=.false.
#endif
 !
 ! Set up band limits
 !
 if (Dipole_bands_ordered.or.Dipole_Energy_treshold<0._SP) then
   X%ib_lim(1)=Xen%nbm
   X%ib_lim(2)=Xen%nbf+1
   if (l_X_terminator) X%ib_lim(2)=X%ib(1)
 else
   X%ib_lim(1)=X%ib(2)
   X%ib_lim(2)=X%ib(1)
 endif
 !
 ! Check if Dipoles DBs exist and are ok
 !=======================================
 io_err=-1
 call DIPOLES_IO('read ')
 !
 ! In case dipoles were not found/ok then I need to compute them
 !==============================================================
 if (io_err/=0) then
   !
   ! I/O privilegies: temporarly switch it on
   !
   call IO_and_Messaging_switch("SAVE")
   call IO_and_Messaging_switch("+io_out",CONDITION=.TRUE.)
   !
   ! Allocation
   !
   call DIPOLES_alloc()
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
#if defined _RT
   if (l_real_time.and.n_spin>1)   call DIPOLE_magnetization(Xk,X)
#endif
   !
   call DIPOLES_ppredux_and_symmetrize()
   !
   ! If the calculation of the dipoles is very short the I/O is switched off
   !
   call DIPOLES_IO('write')
   !
   ! I/O privilegies: RESTORE to previous values
   !
   call IO_and_Messaging_switch("RESTORE")
   !
 endif
 !
#if defined _TIMING
 call timing('Dipoles',OPR='stop')
#endif
 !
#if defined _SC || defined _RT
 if ((l_sc_run.or.l_real_time).and..not.l_evaluating_em1s) return
#endif
#if defined _QED
 if (l_elphoton_corr) return
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
 call DIPOLE_project_along_q(Xk,X,field_dir)
 !
 ! Clean up
 !
 call X_alloc('DIP_iR') 
#if defined _PL
 if (.not.l_photolum) call X_alloc('DIP_P')
#else
 call X_alloc('DIP_P')
#endif
#if defined _RT
 if (l_real_time.and.n_spin>1) call X_alloc('DIP_spin')
#endif
#if defined _SC || defined _RT
 call X_alloc('P_square') 
#endif
 !
 contains
   !
   subroutine DIPOLES_IO(read_or_write)
     !
     use parallel_m, ONLY:master_cpu,PAR_COM_DIPOLES,PAR_COM_DIPOLES_k_subgroup,ncpu
     use com,        ONLY:msg
     use IO_m,       ONLY:io_DIP
     !
     character(5), intent(in) :: read_or_write
     !
     integer                  :: IO_ACTION
     logical                  :: write_header,read_header,reading,writing,l_vnl(1)
     !
     if (.not.io_DIP) return 
     !
     reading=trim(read_or_write)=="read"
     writing=trim(read_or_write)=="write"
     !
     ID=0
     write_header  =(     master_cpu.and.writing)
     read_header   =(.not.master_cpu.and.writing).or.reading
     !
     ! AM March 2016. The next lines are in the nasty case where only a fraction of all CPU's compute the
     ! dipoles. In this case only those cpu's know the right value of X%Vnl_included that is defined in the
     ! calculation process. If, in the same run, all cpu's must access the dipoles the other CPU's will have
     ! the wrong value of X%Vnl_included and the code will get stuck.
     !
     if (reading.and.PAR_COM_DIPOLES%n_CPU==ncpu)then
       l_vnl=X%Vnl_included
       call PP_redux_wait(l_vnl)
       X%Vnl_included=l_vnl(1)
     endif
     !
     if(write_header) then
       call msg('s','[DIP] Writing dipoles header ')
       call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
       io_err=io_DIPOLES(X,ID)
     endif
     !
     call PP_redux_wait(COMM=PAR_COM_DIPOLES%COMM)
     !
     if(read_header) then
       call msg('s','[DIP] Checking dipoles header ')
       call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
       io_err=io_DIPOLES(X,ID)
     endif
     !
     ! In case io_err is /=0 all CPU have to sincronize before starting
     ! to recompute the dipoles and write a new header
     !
     call PP_redux_wait(COMM=PAR_COM_DIPOLES%COMM)
     !
     if (io_err==0) then
       !
       if (reading) IO_ACTION=OP_RD_CL
       if (writing) IO_ACTION=OP_APP_WR_CL
       !
       do ik=1,Xk%nibz
         if (.not.PAR_IND_DIPk_ibz%element_1D(ik)                   ) cycle
         if ((writing.and.PAR_COM_DIPOLES_k_subgroup%CPU_id==0).or.reading) then
           do i_sp_pol=1,n_sp_pol
             call io_control(ACTION=IO_ACTION,COM=REP,SEC=(/1+ik+(i_sp_pol-1)*Xk%nibz/),ID=ID)
             io_err=io_DIPOLES(X,ID)
          enddo
         endif
       enddo
       !
     endif
     !
   end subroutine DIPOLES_IO
   !
   subroutine DIPOLES_alloc()
     !
     call X_alloc('DIP_iR',(/3,X%ib_lim(2),X%ib(2),X%ib(1),X%ib_lim(1),PAR_DIPk_nibz/))
     call X_alloc('DIP_P' ,(/3,X%ib_lim(2),X%ib(2),X%ib(1),X%ib_lim(1),PAR_DIPk_nibz/))
     DIP_iR=cZERO
     DIP_P=cZERO
     !
#if defined _RT
     if (l_real_time.and.n_spin>1) then
       call X_alloc('DIP_spin',(/3,X%ib_lim(2),X%ib(2),X%ib(1),X%ib_lim(1),PAR_DIPk_nibz/))
       DIP_spin=cZERO
     endif
#endif

     !
#if defined _SC || defined _RT
     if (l_sc_run.or.l_real_time) then
       call X_alloc('P_square',(/X%ib_lim(2),X%ib(2),X%ib(1),X%ib_lim(1),PAR_DIPk_nibz/))
       P_square=cZERO
     endif
#endif
     !
   end subroutine DIPOLES_alloc
   !
   subroutine DIPOLES_ppredux_and_symmetrize()
     !
     use parallel_m,  ONLY:PAR_DIPk_ibz_index,PAR_COM_DIPOLES_k_subgroup
     !
     integer  :: ic,iv,i_sp_pol,ik,ik_mem
     !
     do ik=1,Xk%nibz
       !
       if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle
       !
       ik_mem=PAR_DIPk_ibz_index(ik)
       ! 
       do i_sp_pol=1,n_sp_pol
         !
         call PP_redux_wait(DIP_iR(:,:,:,ik_mem,i_sp_pol),COMM=PAR_COM_DIPOLES_k_subgroup%COMM)
         call PP_redux_wait( DIP_P(:,:,:,ik_mem,i_sp_pol),COMM=PAR_COM_DIPOLES_k_subgroup%COMM)
         !
#if defined _RT
         !
         if (l_real_time.and.n_spin>1) then
           call PP_redux_wait(DIP_spin(:,:,:,ik_mem,i_sp_pol),COMM=PAR_COM_DIPOLES_k_subgroup%COMM)
         endif
         !
#endif
         !
#if defined _SC || defined _RT
         !
         if (l_sc_run.or.l_real_time) then
           !
           call PP_redux_wait(P_square(:,:,ik_mem,i_sp_pol),COMM=PAR_COM_DIPOLES_k_subgroup%COMM)
           !
         endif
#endif
         !
         if (l_par_RT) then
           !
#if defined _RT
           DIP_iR(:,:,:,ik_mem,:)= DIP_iR(:,:,:,ik_mem,:)/PAR_COM_PLASMA_INDEX%n_CPU/PAR_COM_Q_INDEX%n_CPU
           DIP_P(:,:,:,ik_mem,:)= DIP_P(:,:,:,ik_mem,:)/PAR_COM_PLASMA_INDEX%n_CPU/PAR_COM_Q_INDEX%n_CPU
           if (l_real_time.and.n_spin>1) then
             DIP_spin(:,:,:,ik_mem,:)= DIP_spin(:,:,:,ik_mem,:)/PAR_COM_PLASMA_INDEX%n_CPU/PAR_COM_Q_INDEX%n_CPU
           endif
#endif
#if defined _SC || defined _RT
           if (l_sc_run.or.l_real_time) then
             P_square(:,:,ik_mem,:)= P_square(:,:,ik_mem,:)/PAR_COM_PLASMA_INDEX%n_CPU/PAR_COM_Q_INDEX%n_CPU
           endif
#endif
           !
         endif
         !
       enddo
       !
       ! Symmetrization 
       !================
       ! Impose P(/iR) to be Hermitian (/anti-Hermitian)
       !
       do i_sp_pol=1,n_sp_pol
         do iv=X%ib(1),X%ib_lim(1)
           if(iv>=X%ib_lim(2)) then
             DIP_iR(:,iv,iv,ik_mem,i_sp_pol)=cmplx(0.,aimag(DIP_iR(:,iv,iv,ik_mem,i_sp_pol)),SP)
             DIP_P (:,iv,iv,ik_mem,i_sp_pol)=cmplx( real(DIP_P(:,iv,iv,ik_mem,i_sp_pol)),0. ,SP)
#if defined _RT
             if (l_real_time.and.n_spin>1) then
               DIP_spin(:,iv,iv,ik_mem,i_sp_pol)=cmplx( real(DIP_spin(:,iv,iv,ik_mem,i_sp_pol)),0. ,SP)
             endif
#endif
#if defined _SC || defined _RT
             if (l_sc_run.or.l_real_time) then
               P_square(iv,iv,ik_mem,i_sp_pol)=cmplx(real(P_square(iv,iv,ik_mem,i_sp_pol)),0. ,SP)
             endif
#endif
           endif
           do ic=iv+1,X%ib(2)
             if( iv>=X%ib_lim(2) .and. ic<=X%ib_lim(1) ) then
               DIP_iR(:,iv,ic,ik_mem,i_sp_pol) = -conjg(DIP_iR(:,ic,iv,ik_mem,i_sp_pol))
               DIP_P (:,iv,ic,ik_mem,i_sp_pol) =  conjg(DIP_P (:,ic,iv,ik_mem,i_sp_pol))
#if defined _RT
               if (l_real_time.and.n_spin>1) then
                 DIP_spin(:,iv,ic,ik_mem,i_sp_pol)= conjg(DIP_spin(:,ic,iv,ik_mem,i_sp_pol))
               endif
#endif
#if defined _SC || defined _RT
               if (l_sc_run.or.l_real_time) then
                 P_square(iv,ic,ik_mem,i_sp_pol) =  conjg(P_square(ic,iv,ik_mem,i_sp_pol))
               endif
#endif
             endif
           enddo
         enddo
       enddo
       !
     enddo
     !
   end subroutine DIPOLES_ppredux_and_symmetrize
   !
end subroutine
