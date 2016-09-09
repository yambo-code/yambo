!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine DIPOLE_driver(Xen,Xk,Xq,Dip)
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
 ! PAR_COM_DIPk_ibz_A2A
 !
 ! while the general world where the calculations lives is
 !
 ! PAR_COM_DIPk_ibz_INDEX
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
 use interfaces,     ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use DIPOLES,        ONLY:DIP_alloc,DIPOLE_t,DIP_iR,use_covariant_approach,use_real_space_approach,&
&                         Vnl_commutator_warning,use_g_space_approach,&
&                         use_shifted_grids_approach,DIP_P,force_v_g_space_approach
 use X_m,            ONLY:l_X_terminator,X_terminator_Kind
 use IO_m,           ONLY:IO_and_Messaging_switch
 use wave_func,      ONLY:wf_ng
 use parallel_m,     ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_nibz,PAR_COM_DIPk_ibz_INDEX,&
&                         PP_redux_wait,PAR_IND_CON_BANDS_DIP,PAR_IND_VAL_BANDS_DIP
 use parser_m,       ONLY:parser
#if defined _RT
 use DIPOLES,        ONLY:DIP_spin
 use electrons,      ONLY:n_spin
#endif
#if defined _SC
 use com,            ONLY:secnm
 use drivers,        ONLY:l_real_time,l_sc_run
 use DIPOLES,        ONLY:P_square
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
 type(bz_samp), intent(inout) :: Xk,Xq
 type(levels),  intent(inout) :: Xen
 type(DIPOLE_t),intent(inout) :: Dip
 !
 ! Work Space
 !
 integer           :: ik,i_sp_pol,io_err
 logical           :: use_dipole_transverse,Dipole_bands_ordered,idir_not_done(3)
#if defined _SC
 logical           :: l_evaluating_em1s
#endif
 !
#if defined _TIMING
 call timing('Dipoles',OPR='start')
#endif
 !
 ! Setup logicals 
 !================
 !
 use_g_space_approach        = trim(Dip%approach)=='G-space v'
 use_real_space_approach     = trim(Dip%approach)=='R-space x'
 use_covariant_approach      = trim(Dip%approach)=='Covariant'
 use_shifted_grids_approach  = trim(Dip%approach)=='Shifted grids'
 !
 call parser('PDirect'  ,force_v_g_space_approach)
 !
 if(.not.any((/use_g_space_approach,use_real_space_approach,use_covariant_approach,use_shifted_grids_approach/))) then
   call warning(' Dipoles approach defined in input not recognised. Reverting to G-space v')
   use_g_space_approach = .true.
   Dip%approach='G-space v'
 endif
 !
#if defined _SC
 force_v_g_space_approach=force_v_g_space_approach.or.l_sc_run.or.l_real_time
#endif
 force_v_g_space_approach=force_v_g_space_approach.and.(.not.use_g_space_approach)
 use_dipole_transverse   =force_v_g_space_approach.or.       use_g_space_approach
 idir_not_done           =.true.
 !
 DIP%ngostnts=wf_ng
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
#if defined _SC 
 l_evaluating_em1s=index(secnm,"Dielectric")/=0
 if ((l_sc_run.or.l_real_time).and..not.l_evaluating_em1s) Dipole_bands_ordered=.false.
#endif
#if defined _QED 
 if (l_elphoton_corr) Dipole_bands_ordered=.false.
#endif
 !
 ! Set up band limits
 !
 if (Dipole_bands_ordered.or.Dip%Energy_treshold<0._SP) then
   Dip%ib_lim(1)=Xen%nbm
   Dip%ib_lim(2)=Xen%nbf+1
   if (l_X_terminator) Dip%ib_lim(2)=Dip%ib(1)
 else
   Dip%ib_lim(1)=Dip%ib(2)
   Dip%ib_lim(2)=Dip%ib(1)
 endif
 !
 ! Check if Dipoles DBs exist and are ok
 !=======================================
 io_err=-1
 call DIPOLE_IO(Xk,Dip,'check',io_err)
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
   ! Parallelization
   !==================
   call PARALLEL_global_indexes(Xen,Xk,Xq,"DIPOLES",Dip=Dip)
   !
   ! WF distribution
   !=================
   call PARALLEL_WF_distribute(K_index=PAR_IND_DIPk_ibz,&
&                              B_index=PAR_IND_CON_BANDS_DIP,&
&                              Bp_index=PAR_IND_VAL_BANDS_DIP,&
&                              CLEAN_UP=.TRUE.)
   !
   call PARALLEL_WF_index(COMM=PAR_COM_DIPk_ibz_INDEX)
   !
   ! Allocation
   !
   call DIPOLES_alloc()
   !
   ! GPL_EXCLUDE_START 
   !
   if (use_shifted_grids_approach) call DIPOLE_shifted_grids(Xen,Xk,Dip)
   !
   if (use_covariant_approach)     call DIPOLE_build_covariants(Xen,Xk,Dip)
   !
   if (use_real_space_approach)    call DIPOLE_x_real_space(Xen,Xk,Dip,idir_not_done)
   !
   ! GPL_EXCLUDE_END 
   !
   if (use_dipole_transverse)      call DIPOLE_transverse(Xen,Xk,Dip)
   !
#if defined _RT
   if (l_real_time.and.n_spin>1)   call DIPOLE_magnetization(Xen,Xk,Dip)
#endif
   !
   call DIPOLES_ppredux_and_symmetrize()
   !
   ! If the calculation of the dipoles is very short the I/O is switched off
   !
   call DIPOLE_IO(Xk,Dip,'write',io_err)
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
 ! Warn about missing [Vnl,r] commutator
 !
 if (.not.Dip%Vnl_included.and..not.Vnl_commutator_warning.and.l_optics.and.use_dipole_transverse) then
   call warning(' Missing non-local pseudopotential contribution')
   Vnl_commutator_warning=.TRUE.
 endif
 !
 ! Clean up
 !
 call DIP_alloc('DIP_iR')
 call DIP_alloc('DIP_P')
 call DIP_alloc('DIP_spin')
 call DIP_alloc('P_square') 
 !
 contains
   !
   subroutine DIPOLES_alloc()
     !
     call DIP_alloc('DIP_iR',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_DIPk_nibz/))
     call DIP_alloc('DIP_P' ,(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_DIPk_nibz/))
     DIP_iR=cZERO
     DIP_P=cZERO
     !
#if defined _RT
     if (l_real_time.and.n_spin>1) then
       call DIP_alloc('DIP_spin',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_DIPk_nibz/))
       DIP_spin=cZERO
     endif
#endif

     !
#if defined _SC
     if (l_sc_run.or.l_real_time) then
       call DIP_alloc('P_square',(/Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_DIPk_nibz/))
       P_square=cZERO
     endif
#endif
     !
   end subroutine DIPOLES_alloc
   !
   subroutine DIPOLES_ppredux_and_symmetrize()
    !
    use parallel_m,  ONLY:PAR_DIPk_ibz_index,PAR_COM_DIPk_ibz_A2A
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
        call PP_redux_wait(DIP_iR(:,:,:,ik_mem,i_sp_pol),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
        call PP_redux_wait( DIP_P(:,:,:,ik_mem,i_sp_pol),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
        !
#if defined _RT
        !
        if (l_real_time.and.n_spin>1) then
          call PP_redux_wait(DIP_spin(:,:,:,ik_mem,i_sp_pol),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
        endif
        !
#endif
        !
#if defined _SC
        !
        if (l_sc_run.or.l_real_time) then
          !
          call PP_redux_wait(P_square(:,:,ik_mem,i_sp_pol),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
          !
        endif
#endif
        !
      enddo
      !
      ! Symmetrization 
      !================
      ! Impose P(/iR) to be Hermitian (/anti-Hermitian)
      !
      do i_sp_pol=1,n_sp_pol
        do iv=Dip%ib(1),Dip%ib_lim(1)
          if(iv>=Dip%ib_lim(2)) then
            DIP_iR(:,iv,iv,ik_mem,i_sp_pol)=cmplx(0.,aimag(DIP_iR(:,iv,iv,ik_mem,i_sp_pol)),SP)
            DIP_P (:,iv,iv,ik_mem,i_sp_pol)=cmplx( real(DIP_P(:,iv,iv,ik_mem,i_sp_pol)),0. ,SP)
#if defined _RT
            if (l_real_time.and.n_spin>1) then
              DIP_spin(:,iv,iv,ik_mem,i_sp_pol)=cmplx( real(DIP_spin(:,iv,iv,ik_mem,i_sp_pol)),0. ,SP)
            endif
#endif
#if defined _SC
            if (l_sc_run.or.l_real_time) &
&             P_square(iv,iv,ik_mem,i_sp_pol)=cmplx(real(P_square(iv,iv,ik_mem,i_sp_pol)),0. ,SP)
#endif
          endif
          do ic=iv+1,Dip%ib(2)
            if( iv>=Dip%ib_lim(2) .and. ic<=Dip%ib_lim(1) ) then
              DIP_iR(:,iv,ic,ik_mem,i_sp_pol) = -conjg(DIP_iR(:,ic,iv,ik_mem,i_sp_pol))
              DIP_P (:,iv,ic,ik_mem,i_sp_pol) =  conjg(DIP_P (:,ic,iv,ik_mem,i_sp_pol))
#if defined _RT
              if (l_real_time.and.n_spin>1) then
                DIP_spin(:,iv,ic,ik_mem,i_sp_pol)= conjg(DIP_spin(:,ic,iv,ik_mem,i_sp_pol))
              endif
#endif
#if defined _SC
              if (l_sc_run.or.l_real_time) &
&               P_square(iv,ic,ik_mem,i_sp_pol) =  conjg(P_square(ic,iv,ik_mem,i_sp_pol))
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
