!
! License-Identifier: GPL
!
! Copyright (C) 2014 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
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
 ! Note that <P> is ALWAYS calculated using the g_space approach
 !
 use drivers,        ONLY:l_optics
 use pars,           ONLY:SP,schlen
 use electrons,      ONLY:levels,n_sp_pol
 use IO_m,           ONLY:io_DIP
 use R_lattice,      ONLY:bz_samp
 use DIPOLES,        ONLY:DIPOLE_t,DIP_iR,x_space_obs,g_space_obs,covariant_obs,shifted_grids_obs,&
&                         Vnl_commutator_warning,DIP_P,DIP_v,DIP_spin,DIP_orbital,P_square
 use ALLOC,          ONLY:DIPOLE_ALLOC_global
 use IO_int,         ONLY:IO_and_Messaging_switch
 use parallel_m,     ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_nibz,PAR_COM_DIPk_ibz_A2A,&
&                         PAR_IND_CON_BANDS_DIP,PAR_IND_VAL_BANDS_DIP
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use DIPOLES,        ONLY:DIP_P_spinor
 use stderr,         ONLY:STRING_match
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
 logical           :: l_warning,direction_is_todo(3)
 !
 call section('*','Dipoles')
 !===========================
 call timing('Dipoles',OPR='start')
 !
 ! Setup 
 !=======
 direction_is_todo=.TRUE.
 call DIPOLE_setup(Dip)
 !
 ! Dipole dimensions to be fixed
 !===============================
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
   call DIPOLE_ALLOC_global(Dip,PAR_DIPk_nibz)
   !
   ! <R>, <V>, <P>, <P_spinor>, <P^2> S_overlap
   !
   if (len_trim(shifted_grids_obs)>0)              call DIPOLE_shifted_grids(Xen,Xk,Dip)
   !
   if (len_trim(covariant_obs)>0)                  call DIPOLE_covariant(Xen,Xk,Dip,direction_is_todo)
   !
   if (len_trim(x_space_obs)>0)                    call DIPOLE_x_real_space(Xen,Xk,Dip,direction_is_todo)
   !
   if (len_trim(g_space_obs)>0)                    call DIPOLE_g_space(Xen,Xk,Dip)
   !
   if (STRING_match(Dip%computed,"M_spin"))        call DIPOLE_spin_magnetization(Xen,Xk,Dip)
   !
   call DIPOLES_ppredux_and_symmetrize()
   !
   if (STRING_match(Dip%computed,"M_orb"))         call DIPOLE_orbital_magnetization(Xen,Xk,Dip)
   !
   if (STRING_match(Dip%computed,"M_CD_orb"))      call DIPOLE_orb_magn_forCD(Xen,Xk,Dip)
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
 if (      Dip%Vnl_included .and.l_warning.and.(len_trim(g_space_obs)>0.or.Dip%force_v_g_space)) then
   msg=' [r,Vnl^pseudo] included in'
   if (.not.len_trim(g_space_obs)>0) msg=trim(msg)//' velocity dipoles.'
   if (     len_trim(g_space_obs)>0) msg=trim(msg)//' position and velocity dipoles.'
   call warning(trim(msg))
   call warning(' In case H contains other non local terms, these are neglected ')
   Vnl_commutator_warning=.TRUE.
 endif
 !
 ! Warn about missing [Vnl,r] commutator
 !
 if ((.not.Dip%Vnl_included).and.l_warning) then
   msg=' [r,Vnl^pseudo] not included in'
   if (.not.len_trim(g_space_obs)>0) msg=trim(msg)//' velocity dipoles'
   if (     len_trim(g_space_obs)>0) msg=trim(msg)//' position and velocity dipoles'
   call warning(trim(msg))
   call warning(' In case H contains other non local terms, also these are neglected ')
   Vnl_commutator_warning=.TRUE.
 endif
 !
 ! Clean up
 !
 if (.not.io_DIP) return
 !
 call DIPOLE_ALLOC_global()
 !
 contains
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
        if (STRING_match(Dip%computed,"M_spin").and.i_sp_pol==1) then
          do i_sp_pol_tmp=1,n_sp_pol
            call PP_redux_wait(DIP_spin(:,:,:,ik_mem,i_sp_pol_tmp,1),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
          enddo
        endif
        if (STRING_match(Dip%computed,"P_spinor")) then
          call PP_redux_wait(DIP_P_spinor(:,:,:,:,:,ik_mem),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
        endif
        if (STRING_match(Dip%computed,"P2")) then
          call PP_redux_wait(P_square(:,:,ik_mem,i_sp_pol),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
        endif
        !
        ! Symmetrization 
        !================
        ! Impose P(/iR) to be Hermitian (/anti-Hermitian)
        !
        do iv=Dip%ib(1),Dip%ib_lim(1)
          if(iv>=Dip%ib_lim(2)) then
            DIP_iR(:,iv,iv,ik_mem,i_sp_pol)=cmplx(0.,aimag(DIP_iR(:,iv,iv,ik_mem,i_sp_pol)),SP)
            DIP_P (:,iv,iv,ik_mem,i_sp_pol)=cmplx( real(DIP_P(:,iv,iv,ik_mem,i_sp_pol), SP),0. ,SP)
            DIP_v (:,iv,iv,ik_mem,i_sp_pol)=cmplx( real(DIP_v(:,iv,iv,ik_mem,i_sp_pol), SP),0. ,SP)
            !
            ! Warning: if n_sp_pol==2 DIP_spin(iv,iv) is not the expectation value but <iv up |S | iv dn>
            if (STRING_match(Dip%computed,"M_spin").and.(.not.n_sp_pol==2)) then
              DIP_spin(:,iv,iv,ik_mem,i_sp_pol,1)=cmplx( real(DIP_spin(:,iv,iv,ik_mem,i_sp_pol,1)),0. ,SP)
            endif
            if (STRING_match(Dip%computed,"P_spinor")) then
              DIP_P_spinor(:,:,:,iv,iv,ik_mem)=cmplx( real(DIP_P_spinor(:,:,:,iv,iv,ik_mem)),0. ,SP)
            endif
            if (STRING_match(Dip%computed,"P2")) then
              P_square(iv,iv,ik_mem,i_sp_pol)=cmplx(real(P_square(iv,iv,ik_mem,i_sp_pol)),0. ,SP)
            endif
            !
          endif
          !
          do ic=iv+1,Dip%ib(2)
            if( iv>=Dip%ib_lim(2) .and. ic<=Dip%ib_lim(1) ) then
              DIP_iR(:,iv,ic,ik_mem,i_sp_pol) = -conjg(DIP_iR(:,ic,iv,ik_mem,i_sp_pol))
              DIP_P (:,iv,ic,ik_mem,i_sp_pol) =  conjg(DIP_P (:,ic,iv,ik_mem,i_sp_pol))
              DIP_v (:,iv,ic,ik_mem,i_sp_pol) =  conjg(DIP_v (:,ic,iv,ik_mem,i_sp_pol))
              if (STRING_match(Dip%computed,"M_spin")) then
                DIP_spin(1,iv,ic,ik_mem,i_sp_pol_inv,1)= conjg(DIP_spin(1,ic,iv,ik_mem,i_sp_pol,1))
                DIP_spin(2,iv,ic,ik_mem,i_sp_pol_inv,1)= conjg(DIP_spin(2,ic,iv,ik_mem,i_sp_pol,1))
                DIP_spin(3,iv,ic,ik_mem,i_sp_pol,1)    = conjg(DIP_spin(3,ic,iv,ik_mem,i_sp_pol,1))
              endif
              if (STRING_match(Dip%computed,"P_spinor")) then
                DIP_P_spinor(:,:,:,iv,ic,ik_mem)= conjg(DIP_P_spinor(:,:,:,ic,iv,ik_mem))
              endif
              if (STRING_match(Dip%computed,"P2")) then
                P_square(iv,ic,ik_mem,i_sp_pol) =  conjg(P_square(ic,iv,ik_mem,i_sp_pol))
              endif
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
    if (STRING_match(Dip%computed,"M_orb").and.STRING_match(Dip%computed,"M_it")) then
      n_dips=2
    else if (STRING_match(Dip%computed,"M_orb").or.STRING_match(Dip%computed,"M_CD_orb")) then
      n_dips=1
    endif
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
