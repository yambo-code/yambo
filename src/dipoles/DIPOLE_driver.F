!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine DIPOLE_driver(Xen,Xk,Xq,X,Dip)
 !
 ! Presently there exist two gauges: (i ) the length   which uses <r>            
 !                                   (ii) the velocity which uses <v>=<p-[x,Vnl]> 
 !
 ! DIP_iR=i<r>
 ! DIP_P = <v>   (should be called DIP_V ... )
 !
 ! For each of the two gauges there exist four approaches of computing the dipoles
 ! 1) G space approach      --> directly computes <v> and then <r>=<v>/DeltaE (called transverse approach as well)
 ! 2) real space approach   --> directly computes <r> and then <v>=<r>*DeltaE (only for non periodic directions)
 ! 3) shifted kpts approach --> computes q*<nk|r|mk> as <nk|r|mk+q> with small q (phases not defined)
 !                              and then <v> as <r>*DeltaE
 ! 4) covariant approach    --> computes <nk|r|mk> in reciprocal space as <nk|partial_k|mk>
 !                              takes into account the phases. symmetries not yet implemented 
 !                              and then <v> as <r>*DeltaE
 !
 use drivers,        ONLY:l_optics,l_chi,l_em1s,l_ppa,l_cohsex,l_bss,l_bse,l_em1d,&
&                         l_gw0,l_elel_corr,l_life
#if defined _PL
 use drivers,        ONLY:l_photolum
#endif
#if defined _NL
 use drivers,        ONLY:l_nl_optics
#endif
 use BS,             ONLY:BS_bands
 use pars,           ONLY:SP,cZERO
 use com,            ONLY:warning
 use electrons,      ONLY:levels,n_sp_pol
 use X_m,            ONLY:X_t
 use IO_m,           ONLY:io_DIP
 use R_lattice,      ONLY:bz_samp
 use interfaces,     ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use DIPOLES,        ONLY:DIP_alloc,DIPOLE_t,DIP_iR,use_covariant_approach,use_real_space_approach,&
&                         Vnl_commutator_warning,use_g_space_approach,&
&                         use_shifted_grids_approach,DIP_P,DIP_S,eval_OVERLAPS
 use IO_m,           ONLY:IO_and_Messaging_switch
 use parallel_m,     ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_nibz,PAR_COM_DIPk_ibz_INDEX,&
&                         PP_redux_wait,PAR_IND_CON_BANDS_DIP,PAR_IND_VAL_BANDS_DIP,&
&                         mpi_comm_world
 use parser_m,       ONLY:parser
#if defined _RT || defined _SC || defined _NL
 use drivers,        ONLY:l_real_time,l_sc_run,l_nl_optics
 use com,            ONLY:secnm
#endif
#if defined _RT
 use DIPOLES,        ONLY:DIP_spin,DIP_orbital,DIP_P_spinor
 use electrons,      ONLY:n_spinor
#endif
#if defined _NL
 use nl_optics,      ONLY:l_use_DIPOLES
#endif
#if defined _SC || defined _RT
 use DIPOLES,        ONLY:P_square
 use electrons,      ONLY:n_spin
#endif
#if defined _QED
 use drivers,        ONLY:l_elphoton_corr
#endif
 use timing_m,       ONLY:timing
 !
 implicit none
 !
 type(bz_samp), intent(inout) :: Xk,Xq
 type(levels),  intent(inout) :: Xen
 type(X_t),     intent(in)    :: X
 type(DIPOLE_t),intent(inout) :: Dip
 !
 ! Work Space
 !
 integer           :: ik,i_sp_pol,io_err,ib(2)
 logical           :: l_sta_X,l_dyn_X,use_dipole_transverse,idir_not_done(3),&
&                     l_real_time_magn_dipoles
 !
 call timing('Dipoles',OPR='start')
 !
 call section('*','Dipoles')
 !
 ! DS 2017/08/22 : Case with io_DIP=.false. to be fixed
 if (.not.io_DIP) then
   call warning(' Cannot use DIP_P on the flight at the moment. Setting io_DIP=.true.')
   io_DIP=.true.
 endif
 !
 l_real_time_magn_dipoles=.false.
#if defined _SC  || defined _RT
 l_real_time_magn_dipoles=l_real_time.and.n_spin>1
#endif
 !
#if defined _NL
 if (l_nl_optics) then
   call parser('UseDipoles',l_use_DIPOLES)
   if(.not.l_use_DIPOLES) then
     Dip%approach="Covariant"
     Dip%ib(1)   =1
   endif
 endif
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
 call parser('PDirect'  ,Dip%force_v_g_space)
 !
 if(.not.any((/use_g_space_approach,use_real_space_approach,use_covariant_approach,use_shifted_grids_approach/))) then
   call warning(' Dipoles approach defined in input not recognised. Reverting to G-space v')
   use_g_space_approach = .true.
   Dip%approach='G-space v'
 endif
 !
#if defined _SC || defined _RT
 Dip%force_v_g_space=Dip%force_v_g_space.or.l_sc_run.or.l_real_time.or.l_nl_optics
#endif
 Dip%force_v_g_space     =Dip%force_v_g_space.and.(.not.use_g_space_approach)
 use_dipole_transverse   =Dip%force_v_g_space.or.       use_g_space_approach
 idir_not_done           =.true.
 !
 Dip%computed='R P'
#if defined _SC
 if (l_sc_run) Dip%computed='R P P2'
#endif
#if defined _RT
 l_real_time_magn_dipoles=l_real_time.and.n_spin>1
 if (l_real_time_magn_dipoles) Dip%computed='R P P2 Magn'
#endif
 !
 if(use_covariant_approach)  eval_OVERLAPS=.true.
 !
 ! Dipole dimensions to be fixed
 !
 call DIPOLE_dimensions(Xen,Dip,Dip%ib,(/0._SP,0._SP,0._SP/))
 !
 ! Check if Dipoles DBs exist and are ok
 !=======================================
 io_err=-1
 call DIPOLE_IO(Xk,Xen,Dip,'check',io_err,'DIP')
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
   if (use_shifted_grids_approach) call DIPOLE_shifted_grids(Xen,Xk,Dip)
   !
   if (use_covariant_approach)     call DIPOLE_build_covariants(Xen,Xk,Dip)
   !
   if (use_real_space_approach)    call DIPOLE_x_real_space(Xen,Xk,Dip,idir_not_done)
   !
   if (use_dipole_transverse)      call DIPOLE_transverse(Xen,Xk,Dip,l_real_time_magn_dipoles)
   !
   ! GPL_EXCLUDE_START 
   !
#if defined _RT
   if (l_real_time_magn_dipoles)   call DIPOLE_orbital_magnetization(Xen,Xk,Dip)
   !
   if (l_real_time_magn_dipoles)   call DIPOLE_spin_magnetization(Xen,Xk,Dip)
#endif
   !
   ! GPL_EXCLUDE_END
   !
   call DIPOLES_ppredux_and_symmetrize()
   !
   ! If the calculation of the dipoles is very short the I/O is switched off
   !
   call DIPOLE_IO(Xk,Xen,Dip,'write',io_err,'DIP')
   !
   ! I/O privilegies: RESTORE to previous values
   !
   call IO_and_Messaging_switch("RESTORE")
   !
 endif
 !
 call timing('Dipoles',OPR='stop')
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
 if (.not.io_DIP) return 
 !
 call DIP_alloc('DIP_iR')
 call DIP_alloc('DIP_P')
 if (use_covariant_approach) call DIP_alloc('DIP_S')
#if defined _RT
 if (l_real_time_magn_dipoles) then
   call DIP_alloc('DIP_orbital')
   call DIP_alloc('DIP_spin')
   if(n_spinor==2) call DIP_alloc('DIP_P_spinor')
 endif
#endif
#if defined _SC || defined _RT
 if(l_real_time.or.l_sc_run) call DIP_alloc('P_square') 
#endif
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
     ! GPL_EXCLUDE_START 
     !
#if defined _RT
     if (l_real_time_magn_dipoles) then
       call DIP_alloc('DIP_orbital',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_DIPk_nibz,2/))
       DIP_orbital=cZERO
       call DIP_alloc('DIP_spin',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_DIPk_nibz/))
       DIP_spin=cZERO
       if (n_spinor==2) then
         call DIP_alloc('DIP_P_spinor',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_DIPk_nibz/))
         DIP_P_spinor=cZERO
       endif
      endif
#endif
     !
#if defined _SC || defined _RT
     if (l_sc_run.or.l_real_time.or.l_nl_optics) then
       call DIP_alloc('P_square',(/Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_DIPk_nibz/))
       P_square=cZERO
     endif
#endif
     if(use_covariant_approach) then
       call DIP_alloc('DIP_S',(/Dip%ib(2),Dip%ib(2),6,Xk%nbz/))
       DIP_S=cZERO
     endif
     !
     ! GPL_EXCLUDE_END 
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
        ! GPL_EXCLUDE_START 
        !
#if defined _RT
        !
        if (l_real_time_magn_dipoles) then
          call PP_redux_wait(DIP_orbital(:,:,:,ik_mem,i_sp_pol,2),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
          call PP_redux_wait(DIP_spin(:,:,:,ik_mem,i_sp_pol),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
          if (n_spinor==2) &
&           call PP_redux_wait(DIP_P_spinor(:,:,:,:,:,ik_mem),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
        endif
        !
#endif
        !
#if defined _SC || defined _RT
        !
        if (l_sc_run.or.l_real_time) then
          !
          call PP_redux_wait(P_square(:,:,ik_mem,i_sp_pol),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
          !
        endif
#endif
        !
        ! GPL_EXCLUDE_END 
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
            !
            ! GPL_EXCLUDE_START 
            !
#if defined _RT
            if (l_real_time_magn_dipoles) then
              DIP_orbital(:,iv,iv,ik_mem,i_sp_pol,:)=cmplx( real(DIP_orbital(:,iv,iv,ik_mem,i_sp_pol,:)),0. ,SP)
              DIP_spin(:,iv,iv,ik_mem,i_sp_pol)=cmplx( real(DIP_spin(:,iv,iv,ik_mem,i_sp_pol)),0. ,SP)
              if(n_spinor==2) &
&               DIP_P_spinor(:,:,:,iv,iv,ik_mem)=cmplx( real(DIP_P_spinor(:,:,:,iv,iv,ik_mem)),0. ,SP)
            endif
#endif
#if defined _SC || defined _RT
            if (l_sc_run.or.l_real_time) &
&             P_square(iv,iv,ik_mem,i_sp_pol)=cmplx(real(P_square(iv,iv,ik_mem,i_sp_pol)),0. ,SP)
#endif
            ! GPL_EXCLUDE_END  
            !
          endif
          do ic=iv+1,Dip%ib(2)
            if( iv>=Dip%ib_lim(2) .and. ic<=Dip%ib_lim(1) ) then
              DIP_iR(:,iv,ic,ik_mem,i_sp_pol) = -conjg(DIP_iR(:,ic,iv,ik_mem,i_sp_pol))
              DIP_P (:,iv,ic,ik_mem,i_sp_pol) =  conjg(DIP_P (:,ic,iv,ik_mem,i_sp_pol))
              !
              ! GPL_EXCLUDE_START 
              !
#if defined _RT
              if (l_real_time_magn_dipoles) then
                DIP_orbital(:,iv,ic,ik_mem,i_sp_pol,:)= conjg(DIP_orbital(:,ic,iv,ik_mem,i_sp_pol,:))
                DIP_spin(:,iv,ic,ik_mem,i_sp_pol)= conjg(DIP_spin(:,ic,iv,ik_mem,i_sp_pol))
                if (n_spinor==2) &
&                 DIP_P_spinor(:,:,:,iv,ic,ik_mem)= conjg(DIP_P_spinor(:,:,:,ic,iv,ik_mem))
              endif
#endif
#if defined _SC || defined _RT
              if (l_sc_run.or.l_real_time) &
&               P_square(iv,ic,ik_mem,i_sp_pol) =  conjg(P_square(ic,iv,ik_mem,i_sp_pol))
#endif
              !
              ! GPL_EXCLUDE_END 
              !
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
