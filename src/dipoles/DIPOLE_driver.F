!
!        Copyright (C) 2000-2021 the YAMBO team
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
 ! DIP_P = <P>
 ! DIP_v = <v> = <P-[x,Vnl]>
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
 use drivers,        ONLY:l_optics,l_real_time,l_sc_run,l_nl_optics
 use pars,           ONLY:SP,cZERO,schlen
 use electrons,      ONLY:levels,n_sp_pol
 use IO_m,           ONLY:io_DIP
 use R_lattice,      ONLY:bz_samp
 use DIPOLES,        ONLY:DIP_alloc,DIPOLE_t,DIP_iR,use_covariant_approach,use_real_space_approach,&
&                         Vnl_commutator_warning,use_g_space_approach,&
&                         use_shifted_grids_approach,DIP_P,DIP_v,DIP_S,eval_OVERLAPS,&
&                         compute_P2_dipoles,compute_Orb_dipoles,compute_Spin_dipoles
 use IO_int,         ONLY:IO_and_Messaging_switch
 use parallel_m,     ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_nibz,PAR_COM_DIPk_ibz_A2A,&
&                         PAR_IND_CON_BANDS_DIP,PAR_IND_VAL_BANDS_DIP
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use parser_m,       ONLY:parser
 use DIPOLES,        ONLY:DIP_spin,DIP_orbital,DIP_P_spinor
 use electrons,      ONLY:n_spin,n_spinor
#if defined _NL
 use nl_optics,      ONLY:l_use_DIPOLES
#endif
#if defined _SC || defined _RT
 use DIPOLES,        ONLY:P_square
#endif
 use BS,             ONLY:BSE_prop,l_BS_magnons,l_BS_dichroism,l_BS_photolum
#if defined _SLEPC && !defined _NL
 use BS_solvers,     ONLY:BSS_mode
#endif
 !
 use timing_m,       ONLY:timing
 !
 implicit none
 !
 type(bz_samp), intent(inout) :: Xk,Xq
 type(levels),  intent(inout) :: Xen
 type(DIPOLE_t),intent(inout) :: Dip
 !
 ! Work Space
 !
 integer           :: ik,io_err
 character(schlen) :: msg
 logical           :: l_warning,use_dipole_transverse,idir_not_done(3)
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
 l_BS_magnons= (index(BSE_prop,'magn')/=0)
 l_BS_dichroism= (index(BSE_prop,'dich')/=0)
 l_BS_photolum= (index(BSE_prop,'photolum')/=0)
 if (l_BS_dichroism.or.l_BS_magnons .or. l_BS_photolum) Dip%bands_ordered=.false.
#if defined _SLEPC && !defined _NL
 if ( index(BSS_mode,'s')/=0 )  Dip%bands_ordered=.false.
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
 use_g_space_approach        = trim(Dip%approach)=='G-space v'
 use_real_space_approach     = trim(Dip%approach)=='R-space x'
 use_covariant_approach      = trim(Dip%approach)=='Covariant'
 use_shifted_grids_approach  = trim(Dip%approach)=='Shifted grids'
 !
 compute_P2_dipoles          = index(Dip%computed,"P2")  /=0
 compute_Spin_dipoles        = index(Dip%computed,"Spin")/=0
 compute_Orb_dipoles         = index(Dip%computed,"Orb") /=0
#if defined _SC
 compute_P2_dipoles          = compute_P2_dipoles .or. l_sc_run
#endif
#if defined _RT
 compute_P2_dipoles          = compute_P2_dipoles   .or. l_real_time
 compute_Spin_dipoles        = compute_Spin_dipoles .or. (l_real_time .and. n_spin>1)
 compute_Orb_dipoles         = compute_Orb_dipoles  .or. (l_real_time .and. n_spin>1)
#endif
 compute_Spin_dipoles        = compute_Spin_dipoles .or. (l_BS_magnons .and. n_spin>1)
 !
 if(.not.any((/use_g_space_approach,use_real_space_approach,use_covariant_approach,use_shifted_grids_approach/))) then
   call warning(' Dipoles approach defined in input not recognised. Reverting to G-space v')
   use_g_space_approach = .true.
   Dip%approach='G-space v'
 endif
 !
 call parser('PDirect'  ,Dip%force_v_g_space)
#if defined _SC || defined _RT || defined _NL
 Dip%force_v_g_space=Dip%force_v_g_space.or.l_sc_run.or.l_real_time.or.l_nl_optics
#endif
 Dip%force_v_g_space     =Dip%force_v_g_space.or.l_BS_photolum
 Dip%force_v_g_space     =Dip%force_v_g_space.and.(.not.use_g_space_approach)
 use_dipole_transverse   =.true.
 idir_not_done           =.true.
 !
 Dip%computed='R V P'
 if (compute_P2_dipoles)   Dip%computed=trim(Dip%computed)//' P2'
 if (compute_Spin_dipoles) Dip%computed=trim(Dip%computed)//' Spin'
 if (compute_Orb_dipoles)  Dip%computed=trim(Dip%computed)//' Orb'
 !
 if (use_covariant_approach)  eval_OVERLAPS=.true.
 !
 ! Dipole dimensions to be fixed
 !
 call DIPOLE_dimensions(Xen,Dip,Dip%ib,(/0._SP,0._SP,0._SP/))
 !
 ! Parallelization
 !==================
 call PARALLEL_global_indexes(Xen,Xk,Xq,"DIPOLES",Dip=Dip)
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
   ! WF distribution
   !=================
   call PARALLEL_WF_distribute(K_index=PAR_IND_DIPk_ibz,&
&                              B_index=PAR_IND_CON_BANDS_DIP,&
&                              Bp_index=PAR_IND_VAL_BANDS_DIP,&
&                              CLEAN_UP=.TRUE.)
   !
   call PARALLEL_WF_index(COMM=PAR_COM_DIPk_ibz_A2A)
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
   if (use_dipole_transverse)      call DIPOLE_transverse(Xen,Xk,Dip)
   !
   if (Compute_Spin_dipoles)       call DIPOLE_spin_magnetization(Xen,Xk,Dip)
   !
   call DIPOLES_ppredux_and_symmetrize()
   !
#if defined _RT
   if (compute_Orb_dipoles)        call DIPOLE_orbital_magnetization(Xen,Xk,Dip)
#else
   if (l_BS_dichroism)             call DIPOLE_orb_magn_forCD(Xen,Xk,Dip)
#endif
   !
   call DIPOLES_ppredux_and_symmetrize_orbt()
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
 l_warning=(.not.Vnl_commutator_warning).and.l_optics
 !
 ! Warn about missing [Sigma,r] commutator
 !
 if (      Dip%Vnl_included .and.l_warning.and.(use_g_space_approach.or.Dip%force_v_g_space)) then
   msg=' [r,Vnl^pseudo] included in'
   if (.not.use_g_space_approach) msg=trim(msg)//' velocity dipoles.'
   if (     use_g_space_approach) msg=trim(msg)//' position and velocity dipoles.'
   call warning(trim(msg))
   call warning(' In case H contains other non local terms, these are neglected ')
   Vnl_commutator_warning=.TRUE.
 endif
 !
 ! Warn about missing [Vnl,r] commutator
 !
 if ((.not.Dip%Vnl_included).and.l_warning) then
   msg=' [r,Vnl^pseudo] not included in'
   if (.not.use_g_space_approach) msg=trim(msg)//' velocity dipoles'
   if (     use_g_space_approach) msg=trim(msg)//' position and velocity dipoles'
   call warning(trim(msg))
   call warning(' In case H contains other non local terms, also these are neglected ')
   Vnl_commutator_warning=.TRUE.
 endif
 !
 ! Clean up
 !
 if (.not.io_DIP) return
 !
 call DIP_alloc('DIP_iR')
 call DIP_alloc('DIP_P')
 call DIP_alloc('DIP_v')
 if (use_covariant_approach) call DIP_alloc('DIP_S')
 if (compute_P2_dipoles)     call DIP_alloc('P_square') 
 if (compute_Orb_dipoles)    call DIP_alloc('DIP_orbital')
 if (compute_Spin_dipoles)   call DIP_alloc('DIP_spin')
#if defined _RT
 if (compute_Spin_dipoles.and.n_spinor==2) call DIP_alloc('DIP_P_spinor')
#endif
 !
 if(l_BS_dichroism) call DIP_alloc('DIP_orbital')
 !
 contains
   !
   subroutine DIPOLES_alloc()
     !
     call DIP_alloc('DIP_iR',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_DIPk_nibz/))
     call DIP_alloc('DIP_P' ,(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_DIPk_nibz/))
     call DIP_alloc('DIP_v' ,(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_DIPk_nibz/))
     DIP_iR=cZERO
     DIP_P=cZERO
     DIP_v=cZERO
     !
     if (compute_Orb_dipoles) then
       call DIP_alloc('DIP_orbital',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_DIPk_nibz,2/))
       DIP_orbital=cZERO
     endif
     if (compute_Spin_dipoles) then
       call DIP_alloc('DIP_spin',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_DIPk_nibz/))
       DIP_spin=cZERO
     endif
#if defined _RT
     if (compute_Spin_dipoles.and.n_spinor==2) then
       call DIP_alloc('DIP_P_spinor',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_DIPk_nibz/))
       DIP_P_spinor=cZERO
     endif
#endif
     !
#if defined _SC || defined _RT || defined _NL
     if (compute_P2_dipoles) then
       call DIP_alloc('P_square',(/Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_DIPk_nibz/))
       P_square=cZERO
     endif
#endif
     !
     if (l_BS_dichroism) then
       call DIP_alloc('DIP_orbital',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_DIPk_nibz,1/))
       DIP_orbital=cZERO
     endif
     !
     if(use_covariant_approach) then
       call DIP_alloc('DIP_S',(/Dip%ib(2),Dip%ib(2),6,Xk%nbz/))
       DIP_S=cZERO
     endif
     !
   end subroutine DIPOLES_alloc
   !
   subroutine DIPOLES_ppredux_and_symmetrize()
    !
    use parallel_m,  ONLY:PAR_DIPk_ibz_index,PAR_COM_DIPk_ibz_A2A
    !
    integer  :: ic,iv,i_sp_pol,i_sp_pol_inv,i_sp_pol_tmp,ik,ik_mem
    !
    i_sp_pol_inv=1
    !
    do i_sp_pol=1,n_sp_pol
      !
      if(i_sp_pol==1 .and. n_sp_pol==2) i_sp_pol_inv=2
      if(i_sp_pol==2 .and. n_sp_pol==2) i_sp_pol_inv=1
      !
      do ik=1,Xk%nibz
        !
        if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle
        !
        ik_mem=PAR_DIPk_ibz_index(ik)
        !
        call PP_redux_wait(DIP_iR(:,:,:,ik_mem,i_sp_pol),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
        call PP_redux_wait( DIP_P(:,:,:,ik_mem,i_sp_pol),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
        call PP_redux_wait( DIP_v(:,:,:,ik_mem,i_sp_pol),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
        !
        if (compute_Spin_dipoles.and.i_sp_pol==1) then
          do i_sp_pol_tmp=1,n_sp_pol
            call PP_redux_wait(DIP_spin(:,:,:,ik_mem,i_sp_pol_tmp,1),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
          enddo
        endif
#if defined _RT
        if (compute_Spin_dipoles.and.n_spinor==2) &
        & call PP_redux_wait(DIP_P_spinor(:,:,:,:,:,ik_mem),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
#endif
#if defined _SC || defined _RT || defined _NL
        if (compute_P2_dipoles) call PP_redux_wait(P_square(:,:,ik_mem,i_sp_pol),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
#endif
        !
        ! Symmetrization 
        !================
        ! Impose P(/iR) to be Hermitian (/anti-Hermitian)
        !
        do iv=Dip%ib(1),Dip%ib_lim(1)
          if(iv>=Dip%ib_lim(2)) then
            DIP_iR(:,iv,iv,ik_mem,i_sp_pol)=cmplx(0.,aimag(DIP_iR(:,iv,iv,ik_mem,i_sp_pol)),SP)
            DIP_P (:,iv,iv,ik_mem,i_sp_pol)=cmplx( real(DIP_P(:,iv,iv,ik_mem,i_sp_pol)),0. ,SP)
            DIP_v (:,iv,iv,ik_mem,i_sp_pol)=cmplx( real(DIP_v(:,iv,iv,ik_mem,i_sp_pol)),0. ,SP)
            !
            ! Warning: if n_sp_pol==2 DIP_spin(iv,iv) is not the expectation value but <iv up |S | iv dn>
            if (compute_Spin_dipoles.and.(.not.n_sp_pol==2) ) &
            & DIP_spin(:,iv,iv,ik_mem,i_sp_pol,1)=cmplx( real(DIP_spin(:,iv,iv,ik_mem,i_sp_pol,1)),0. ,SP)
#if defined _RT
            if (compute_Spin_dipoles.and.n_spinor==2) &
            & DIP_P_spinor(:,:,:,iv,iv,ik_mem)=cmplx( real(DIP_P_spinor(:,:,:,iv,iv,ik_mem)),0. ,SP)
#endif
#if defined _SC || defined _RT
            if (compute_P2_dipoles) then
              P_square(iv,iv,ik_mem,i_sp_pol)=cmplx(real(P_square(iv,iv,ik_mem,i_sp_pol)),0. ,SP)
            endif
#endif
            !
          endif
          !
          do ic=iv+1,Dip%ib(2)
            if( iv>=Dip%ib_lim(2) .and. ic<=Dip%ib_lim(1) ) then
              DIP_iR(:,iv,ic,ik_mem,i_sp_pol) = -conjg(DIP_iR(:,ic,iv,ik_mem,i_sp_pol))
              DIP_P (:,iv,ic,ik_mem,i_sp_pol) =  conjg(DIP_P (:,ic,iv,ik_mem,i_sp_pol))
              DIP_v (:,iv,ic,ik_mem,i_sp_pol) =  conjg(DIP_v (:,ic,iv,ik_mem,i_sp_pol))
              if (compute_Spin_dipoles) then
                DIP_spin(1,iv,ic,ik_mem,i_sp_pol_inv,1)= conjg(DIP_spin(1,ic,iv,ik_mem,i_sp_pol,1))
                DIP_spin(2,iv,ic,ik_mem,i_sp_pol_inv,1)= conjg(DIP_spin(2,ic,iv,ik_mem,i_sp_pol,1))
                DIP_spin(3,iv,ic,ik_mem,i_sp_pol,1)    = conjg(DIP_spin(3,ic,iv,ik_mem,i_sp_pol,1))
              endif
#if defined _RT
              if (compute_Spin_dipoles.and.n_spinor==2) &
              & DIP_P_spinor(:,:,:,iv,ic,ik_mem)= conjg(DIP_P_spinor(:,:,:,ic,iv,ik_mem))
#endif
#if defined _SC || defined _RT
              if (compute_P2_dipoles) &
&               P_square(iv,ic,ik_mem,i_sp_pol) =  conjg(P_square(ic,iv,ik_mem,i_sp_pol))
#endif
              !
            endif
          enddo
        enddo
        !
      enddo
    enddo
    !
   end subroutine DIPOLES_ppredux_and_symmetrize
   !
   subroutine DIPOLES_ppredux_and_symmetrize_orbt()
    !
    use parallel_m,  ONLY:PAR_DIPk_ibz_index,PAR_COM_DIPk_ibz_A2A
    !
    integer  :: ic,iv,i_sp_pol,ik,ik_mem,i_dips,n_dips
    !
    n_dips=0
    if (compute_Orb_dipoles) n_dips=2
    if (l_BS_dichroism)      n_dips=1
    !
    do i_dips=1,n_dips
    do i_sp_pol=1,n_sp_pol
      !
      do ik=1,Xk%nibz
        !
        if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle
        !
        ik_mem=PAR_DIPk_ibz_index(ik)
        !
        call PP_redux_wait(DIP_orbital(:,:,:,ik_mem,i_sp_pol,i_dips),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
        !
        ! Symmetrization 
        !================
        do iv=Dip%ib(1),Dip%ib_lim(1)
          if(.not.(iv>=Dip%ib_lim(2)) ) cycle
          DIP_orbital(:,iv,iv,ik_mem,i_sp_pol,i_dips)=cmplx( real(DIP_orbital(:,iv,iv,ik_mem,i_sp_pol,i_dips)),0. ,SP)
          !
          do ic=iv+1,Dip%ib(2)
            if( .not.(ic<=Dip%ib_lim(1)) ) cycle
            DIP_orbital(:,iv,ic,ik_mem,i_sp_pol,i_dips)= conjg(DIP_orbital(:,ic,iv,ik_mem,i_sp_pol,i_dips))
          enddo
        enddo
        !
      enddo
    enddo
    enddo
    !
   end subroutine DIPOLES_ppredux_and_symmetrize_orbt
   !
end subroutine
