!
! Copyright (C) 2012 WanT Group
! Adapted for p2y by DS
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!**********************************************************
SUBROUTINE atmproj_to_internal( filein, fileqp, fileham, filespace, filewan, do_orthoovp )
   !**********************************************************
   !
   ! Convert the datafile written by the projwfc program (QE suite) to
   ! the internal representation.
   !
   ! 3 files are creates: fileham, filespace, filewan
   !
   use pars,                ONLY:cZERO
   use timing_m,            ONLY:timing
   use com,                 ONLY:msg,error,file_exists
   !
   USE util_module,         ONLY : mat_is_herm, mat_mul
   USE parser_base_module,  ONLY : change_case

   !USE mp
   !USE log_module,          ONLY : log_push, log_pop
   !USE converters_module,   ONLY : cart2cry, cry2cart
   !USE grids_module,        ONLY : grids_get_rgrid
   !USE lattice_module,      ONLY : lattice_read_ext, lattice_init
   !USE symmetry_module,     ONLY : symmetry_read_ext, nsym, symmetry_deallocate
   !USE ions_module,         ONLY : ions_read_ext, ions_init, ions_deallocate
   !USE files_module,        ONLY : file_exist, dir_exist
   !
   USE symmetrize_kgrid_module,    ONLY : symmetrize_kgrid
   USE lattice_module,             ONLY : avec, bvec, alat
   USE io_module,                  ONLY : dft_unit, pseudo_dir
   USE util_module
   USE parser_module
   !
   IMPLICIT NONE

   LOGICAL, PARAMETER :: binary = .TRUE.
   
   !
   ! input variables
   !
   CHARACTER(*), INTENT(IN) :: filein
   CHARACTER(*), OPTIONAL, INTENT(IN) :: fileham, filespace, filewan
   CHARACTER(*), OPTIONAL, INTENT(IN) :: fileqp
   LOGICAL,      OPTIONAL, INTENT(IN) :: do_orthoovp
   
   !
   ! local variables
   !
   CHARACTER(19)     :: subname="atmproj_to_internal"
   INTEGER           :: iunit, ounit
   LOGICAL           :: do_orthoovp_
   LOGICAL           :: lhave_fileqp
   INTEGER           :: atmproj_nbnd_, atmproj_nbndmin_
   INTEGER           :: atmproj_ndim_
   !
   CHARACTER(nstrx)  :: attr, energy_units
   CHARACTER(nstrx)  :: filetype_
   LOGICAL           :: write_ham, write_space, write_loc
   REAL(dbl)         :: norm, efermi, nelec
   LOGICAL           :: spin_noncollinear, lexist
   REAL(dbl)         :: proj_wgt
   INTEGER           :: dimwann, natomwfc, nkpts, nspin, nbnd
   INTEGER           :: nkpts_all, nspin_
   INTEGER           :: nk(3), shift(3), nrtot, nr(3)
   INTEGER           :: i, j, ir, ik, ikeq, ib, ib_, isp
   INTEGER           :: ierr
   !
   INTEGER,        ALLOCATABLE :: ivr(:,:), itmp(:)
   INTEGER,        ALLOCATABLE :: kpteq_map(:), kpteq_symm(:)
   REAL(dbl),      ALLOCATABLE :: vkpt_cry(:,:), vkpt(:,:), wk(:), wr(:), vr(:,:)
   REAL(dbl),      ALLOCATABLE :: vkpt_all(:,:), wk_all(:)
   REAL(dbl),      ALLOCATABLE :: eig(:,:,:)
   REAL(dbl),      ALLOCATABLE :: rtmp(:,:)
   COMPLEX(dbl),   ALLOCATABLE :: rham(:,:,:,:), kham(:,:,:)
   COMPLEX(dbl),   ALLOCATABLE :: proj(:,:,:,:)
   COMPLEX(dbl),   ALLOCATABLE :: kovp(:,:,:,:), rovp(:,:,:,:)
   COMPLEX(dbl),   ALLOCATABLE :: kovp_all(:,:,:,:)
   COMPLEX(dbl),   ALLOCATABLE :: cu_tmp(:,:,:), eamp_tmp(:,:)
   !
   COMPLEX(dbl),   ALLOCATABLE :: zaux(:,:), ztmp(:,:)
   COMPLEX(dbl),   ALLOCATABLE :: kovp_sq(:,:)
   REAL(dbl),      ALLOCATABLE :: w(:)
   !
   LOGICAL          :: lpara
#if defined __SHIFT_TEST
   !Luis 3: Changes related to Sohrab's shifting scheme 
   !PA   = A^dagger * A 
   !I_PA = inv(PA)
   COMPLEX(dbl),   ALLOCATABLE :: A(:,:),PA(:,:), IPA(:,:), kham_aux(:,:), E(:,:)
   INTEGER           :: shifting_scheme = 2
#endif
  
#if defined __WRITE_ASCIIHAM
   CHARACTER(100)    :: kham_file
   INTEGER           :: iw,jw
#endif
   !
!------------------------------
! main body
!------------------------------
!
   CALL timing( subname, OPR='start' )
   !CALL log_push( subname )


   !
   ! re-initialize all the times
   !
   CALL atmproj_tools_init( filein, ierr )
   IF ( ierr/=0 ) CALL errore(subname,'initializing atmproj',10)

   !
   ! search for units indipendently of io_module
   !
   CALL iotk_free_unit( iunit )
   CALL iotk_free_unit( ounit )

   !
   ! what files are to be written
   !
   write_ham = .FALSE.
   write_space = .FALSE.
   write_loc = .FALSE.
   IF ( PRESENT(fileham) )    write_ham = .TRUE.
   IF ( PRESENT(filespace) )  write_space = .TRUE.
   IF ( PRESENT(filewan) )    write_loc = .TRUE.
   !
   lhave_fileqp=.FALSE.
   IF ( PRESENT(fileqp) ) THEN
       IF ( file_exists(fileqp) ) THEN
           lhave_fileqp=.TRUE.
       ELSE
           CALL errore(subname,"file: "//TRIM(fileqp)//" does not exist",10)
       ENDIF
   ENDIF

   !
   ! orthogonalization controlled by input
   ! NOTE: states are non-orthogonal by default
   !
   do_orthoovp_ = .FALSE.
   IF ( PRESENT( do_orthoovp ) ) do_orthoovp_ = do_orthoovp
   

!
!---------------------------------
! read data from filein (by projwfc, QE suite)
!---------------------------------
!
 
   !
   ! get DFT data
   ! qexml and qexsd are supported.
   ! note that the parallelism inside *_read_ext routines may be harmful
   ! and needs to be handled
   !
   lpara = .TRUE.
   !
   IF ( ionode .AND. TRIM(file_fmt)=="qexml") THEN
      CALL qexml_openfile( file_data, "read", IERR=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"opening qexml "//TRIM(file_data), ABS(ierr) )
   ENDIF
   !
   IF ( ionode .AND. TRIM(file_fmt)=="qexsd") THEN
      CALL qexsd_openfile( file_data, "read", IERR=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"opening qexsd "//TRIM(file_data), ABS(ierr) )
      CALL qexsd_open_output(ierr)
      IF ( ierr/=0) CALL errore(subname,'opening Output section in dftdata file',1)
   ENDIF
   !
   call error(' lattice subroutines not imported') 
   !CALL lattice_read_ext( trim(file_fmt), LPARA=lpara )
   !CALL lattice_init( )
   !
   call error(' ions subroutines not imported') 
   !CALL ions_read_ext( trim(file_fmt), LPARA=lpara )
   !CALL ions_init()
   !
   call error(' symmetry subroutines not imported') 
   !CALL symmetry_read_ext( trim(file_fmt), LPARA=lpara )
   !
   IF ( ionode .AND. TRIM(file_fmt)=="qexml") THEN
      CALL qexml_closefile( "read", IERR=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"closing qexml "//TRIM(file_data), ABS(ierr) )
   ENDIF
   !
   IF ( ionode .AND. TRIM(file_fmt)=="qexsd") THEN
      CALL qexsd_close_output(ierr)
      IF ( ierr/=0) CALL errore(subname,'closing Output section in dftdata file',1)
      CALL qexsd_closefile( "read", IERR=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"closing qexsd "//TRIM(file_data), ABS(ierr) )
   ENDIF

   !
   ! read pseudos
   !
   !IF ( ionode ) THEN
   !   lexist = dir_exist(pseudo_dir)
   !ENDIF
   !CALL mp_bcast( lexist, ionode_id )
   lexist=.true.
   !
   pseudo_dir=TRIM(savedir)
   !
   CALL readpp()

   !
   ! reading dimensions
   ! and small data
   !
   CALL atmproj_read_ext( filein, nbnd, nkpts, nspin, natomwfc, &
                          nelec, efermi, energy_units, IERR=ierr)
   IF ( ierr/=0 ) CALL errore(subname, "reading dimensions I", ABS(ierr))

   dimwann = natomwfc
   !
   atmproj_nbnd_ = nbnd
   IF ( atmproj_nbnd > 0 ) atmproj_nbnd_ = MIN(atmproj_nbnd, nbnd)
   !
   atmproj_nbndmin_ = MIN( MAX(1,atmproj_nbndmin), atmproj_nbnd_)
   atmproj_ndim_ = atmproj_nbnd_-atmproj_nbndmin_+1

   !
   ! nspin_ definition
   ! non-collinear case
   !
   IF ( nspin == 4 ) THEN
       nspin_ = 1
       spin_noncollinear = .true.
   ELSE
       spin_noncollinear = .false.
       nspin_ = nspin
   ENDIF

   !
   ! quick report
   !
   IF (ionode) THEN
       !
       call msg('s',' Dimensions found in atomic_proj file: ')
       call msg('s'
       call msg('s','   nbnd     :  ',nbnd)
       call msg('s','   nkpts    :  ',nkpts)
       call msg('s','   nspin    :  ',nspin)
       call msg('s','   natomwfc :  ',natomwfc)
       call msg('s','   nelec    :  ',nelec)
       call msg('s','   efermi   :  ',efermi )
       call msg('s','   en-units :  '//TRIM(energy_units))
       !IF (lhave_fileqp) &
       !   WRITE(stdout,"(2x, '   fileQP   :  ',a )") TRIM(fileqp)
       !
       !
       call msg('s',' ATMPROJ conversion to be done using: ')
       call msg('s','   atmproj_nbnd :  ',atmproj_nbnd_
       call msg('s','atmproj_nbndmin :  ',atmproj_nbndmin_
       call msg('s','   atmproj_thr  :  ',atmproj_thr
       call msg('s','   atmproj_sh   :  ',atmproj_sh
       !call msg('s','   do_orthoovp  :  '//TRIM(log2char(do_orthoovp_)))
       !
   ENDIF

   !
   ! allocations
   !
   ALLOCATE( vkpt(3,nkpts), wk(nkpts), STAT=ierr )
   IF (ierr/=0) CALL errore(subname, 'allocating vkpt, wk', ABS(ierr))
   ALLOCATE( vkpt_cry(3, nkpts), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'allocating vkpt_cry', ABS(ierr) )
   !
   ALLOCATE( eig(nbnd,nkpts,nspin_), STAT=ierr )
   IF (ierr/=0) CALL errore(subname, 'allocating eig', ABS(ierr))
   ALLOCATE( proj(natomwfc,nbnd,nkpts,nspin_), STAT=ierr )
   IF (ierr/=0) CALL errore(subname, 'allocating proj', ABS(ierr))

   !
   ! read-in massive data
   !
   IF ( do_orthoovp_ ) THEN
       !
       ALLOCATE( kovp(1,1,1,1), STAT=ierr )
       IF (ierr/=0) CALL errore(subname, 'allocating kovp I', ABS(ierr))
       !
       CALL atmproj_read_ext( filein, VKPT=vkpt, WK=wk, EIG=eig, PROJ=proj, IERR=ierr )
       !
   ELSE
       !
       ALLOCATE( kovp(natomwfc,natomwfc,nkpts,nspin), STAT=ierr )
       IF (ierr/=0) CALL errore(subname, 'allocating kovp II', ABS(ierr))
       !
       ! reading  proj(i,b)  = < phi^at_i | evc_b >
       !
       CALL atmproj_read_ext( filein, VKPT=vkpt, WK=wk, EIG=eig, PROJ=proj, KOVP=kovp, IERR=ierr )
       !
   ENDIF
   !
   IF ( ierr/=0 ) CALL errore(subname, "reading data II", ABS(ierr))

   !
   ! units (first we convert to bohr^-1, 
   ! then we want vkpt to be re-written in crystal units)
   !
   vkpt = vkpt * TPI / alat
   !
   vkpt_cry = vkpt
   call error(' Need to translate cart2cry in yambo syntax')
   !CALL cart2cry( vkpt_cry, bvec ) 
   !
   CALL get_monkpack( nk, shift, nkpts, vkpt_cry, 'CRYSTAL', bvec, ierr)
   !
   ! if kpts are not MP, try to symmetrize the grid
   !
   IF ( ierr/= 0 ) THEN
       !
       CALL symmetrize_kgrid( nkpts, vkpt, bvec, nkpts_all )
       !
       ALLOCATE( vkpt_all(3,nkpts_all), wk_all(nkpts_all) )
       ALLOCATE( kpteq_map(nkpts_all) )
       ALLOCATE( kpteq_symm(nkpts_all) )
       !
       wk_all(1:nkpts_all)=1.0d0/REAL(nkpts_all,dbl)
       !
       CALL symmetrize_kgrid( nkpts, vkpt, bvec, nkpts_all, vkpt_all, kpteq_map, kpteq_symm )
       !
       CALL get_monkpack( nk, shift, nkpts_all, vkpt_all, 'CARTESIAN', bvec, ierr)
       IF ( ierr/=0 ) CALL errore(subname,'kpt grid not Monkhorst-Pack',ABS(ierr))
       !
   ELSE
       !
       nkpts_all = nkpts
       ALLOCATE( vkpt_all(3,nkpts_all), wk_all(nkpts_all) )
       ALLOCATE( kpteq_map(nkpts_all) )
       ALLOCATE( kpteq_symm(nkpts_all) )
       !
       vkpt_all(:,1:nkpts)  = vkpt(:,1:nkpts)
       wk_all(1:nkpts)      = wk(1:nkpts)
       !
       DO ik = 1, nkpts
          kpteq_map(ik)=ik
          kpteq_symm(ik)=1
       ENDDO
       !
   ENDIF

   !
   ! normalize the k-weights to 1.0
   !
   norm   = SUM( wk )
   wk(:)  = wk(:) / norm
   norm   = SUM( wk_all )
   wk_all(:) = wk_all(:) / norm

   !
   ! further alloc
   !
   IF ( .not. do_orthoovp_ ) THEN
       !
       ALLOCATE( kovp_all(natomwfc,natomwfc,nkpts_all,nspin), STAT=ierr )
       IF (ierr/=0) CALL errore(subname, 'allocating kovp_all III', ABS(ierr))
       !
   ENDIF

   !
   ! kpts and real-space lattice vectors
   !
   nr(1:3) = nk(1:3)
   !
   ! get the grid dimension
   !
   call error(' grids_gst_rgrid not yet imported into yambo')
   !CALL grids_get_rgrid(nr, NRTOT=nrtot )
   !
   ALLOCATE( ivr(3, nrtot), vr(3, nrtot), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'allocating ivr, vr', ABS(ierr) )
   ALLOCATE( wr(nrtot), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'allocating wr', ABS(ierr) )
   !
   !CALL grids_get_rgrid(nr, WR=wr, IVR=ivr )
   !
   vr(:,:) = REAL( ivr, dbl)
   call error(' Need to translate cry2cart in yambo syntax')
   !CALL cry2cart( vr, avec)
 
   

   !
   ! efermi and eigs are converted to eV's
   !
   CALL change_case( energy_units, 'lower' )
   !
   SELECT CASE( ADJUSTL(TRIM(energy_units)) )
   CASE ( "ha", "hartree", "au" )
      !
      efermi = efermi * TWO * RYD 
      eig    = eig    * TWO * RYD
      !
   CASE ( "ry", "ryd", "rydberg" )
      !
      efermi = efermi * RYD 
      eig    = eig    * RYD
      !
   CASE ( "ev", "electronvolt" )
      !
      ! do nothing
   CASE DEFAULT
      CALL errore( subname, 'unknown units for efermi: '//TRIM(energy_units), 72)
   END SELECT


   !
   ! overwrite EIG re-reading them from fileqp if necessary
   ! eV units are assumed
   !
   IF ( lhave_fileqp ) THEN
       !
       CALL read_fileqp( fileqp, SIZE(eig,1), SIZE(eig,2), SIZE(eig,3), eig)
       efermi = 0.0
       !
   ENDIF

   !
   ! shifting
   !
   ! apply the energy shift,
   ! meant to set the zero of the energy scale (where we may have
   ! spurious 0 eigenvalues) far from any physical energy region of interest
   !
   eig    = eig  -efermi


   ! 
   ! build the Hamiltonian in real space
   ! 
   IF ( write_ham ) THEN

       ALLOCATE( rham(dimwann, dimwann, nrtot, nspin_), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'allocating rham', ABS(ierr) )
       ALLOCATE( kham(dimwann, dimwann, nkpts_all), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'allocating kham', ABS(ierr) )
       !
       IF ( .NOT. do_orthoovp_ ) THEN
           ALLOCATE( rovp(natomwfc,natomwfc,nrtot,nspin), STAT=ierr )
           IF (ierr/=0) CALL errore(subname, 'allocating rovp', ABS(ierr))
       ENDIF
       
#if defined __SHIFT_TEST
       IF ( shifting_scheme == 2 ) THEN
           call msg('s',' ATMPROJ uses the experimental shifting: ')
           !PA = A'*A
           ALLOCATE( A(natomwfc, atmproj_ndim_), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname, 'allocating Space-A Projector', ABS(ierr))
           ALLOCATE( PA(atmproj_ndim_, atmproj_ndim_), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname, 'allocating Space-A Projector', ABS(ierr))
           ALLOCATE( IPA(atmproj_ndim_, atmproj_ndim_), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname, 'allocating inv(P_A)', ABS(ierr))
           ALLOCATE( E(atmproj_ndim_, atmproj_ndim_), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname, 'allocating E', ABS(ierr))
           ALLOCATE( kham_aux(atmproj_ndim_, natomwfc), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname, 'allocating kham_aux', ABS(ierr))
       ENDIF
#endif
          
       DO isp = 1, nspin_

           IF ( TRIM(spin_component) == "up"   .AND. isp == 2 ) CYCLE
           IF ( TRIM(spin_component) == "down" .AND. isp == 1 ) CYCLE
           IF ( TRIM(spin_component) == "dw"   .AND. isp == 1 ) CYCLE

           !
           ! build kham
           !
           kpt_loop:&
           DO ik = 1, nkpts_all
               !
               ikeq = kpteq_map(ik)
               !
               kham(:,:,ik) = ZERO
               !
               ALLOCATE( ztmp(natomwfc,nbnd), STAT=ierr)
               IF ( ierr/=0 ) CALL errore(subname,'allocating ztmp', ABS(ierr) )

               !
               ! ztmp(i, b) = < phi^at_i | evc_b >
               !
               IF ( kpteq_symm(ik) <= nsym ) THEN
                   ! no time-reversal involved
                   ztmp(1:natomwfc,1:nbnd) = proj(1:natomwfc,1:nbnd,ikeq,isp) 
               ELSE
                   ztmp(1:natomwfc,1:nbnd) = CONJG( proj(1:natomwfc,1:nbnd,ikeq,isp) )
               ENDIF
               !
               CALL atmproj_rotate_proj( natomwfc, nbnd, kpteq_symm(ik), vkpt_cry(:,ikeq), ztmp)
       
#if defined __SHIFT_TEST
               !
               IF ( shifting_scheme == 2 ) THEN
                   !temporary solution. It assumes the columns of proj are in
                   !ascending order of projectability
                   A  = proj(1:natomwfc,atmproj_nbndmin_:atmproj_nbnd_,ik,isp) 
                   PA = ZERO
                   IPA= ZERO
                   !PA = A' * A
                   CALL mat_mul( PA, A, 'C', A, 'N', atmproj_ndim_, atmproj_ndim_, natomwfc)
                   CALL mat_inv( atmproj_ndim_, PA, IPA)
               
                   E  = CZERO 
                   kham_aux = CZERO
                   !TO DO, 
                   !are the eigs in ascending order. where?
                   !bands.x, nscf.x can change the order of the eigenvalues
                   DO ib  = atmproj_nbndmin_, atmproj_nbnd_
                      ib_ = ib -atmproj_nbndmin_+1
                      E(ib_,ib_) = eig(ib,ik,isp) 
                      !E(ib,ib) = eig(ib,ik,isp) - atmproj_sh 
                   ENDDO
                   !HKS_aux = (E - kappa*IPA)*A'
                   CALL mat_mul( kham_aux, E -atmproj_sh*IPA, 'N', A, 'C', atmproj_ndim_, natomwfc, atmproj_ndim_)
                   !CALL mat_mul( kham_aux, E, 'N', A, 'C', atmproj_nbnd_, natomwfc, atmproj_nbnd_)
                   !HKS = A*HKS_aux = A*(E - kappa*IPA)*A'
                   CALL mat_mul( kham(:,:,ik), A, 'N', kham_aux, 'N', natomwfc, natomwfc, atmproj_ndim_)
               
               ELSE
#endif
                  
               !
               ibnd_loop:&
               DO ib = atmproj_nbndmin_, atmproj_nbnd_
                   !
                   ! filtering
                   ! Note: - This is one way of doing the filtering.
                   !       It filters within the atmproj_nbnd bands.
                   !       Useful if used with atmproj_bnd == Inf (i.e. QE's nbnd)
                   !       so that the filter is controlled only by atmproj_thr
                   !
                   !       - Another way is to set atmproj_thr<=0 
                   !       and then the filtering is controlled only by atmproj_nbnd
                   IF ( atmproj_thr > 0.0d0 ) THEN
                       !
                       proj_wgt = REAL( DOT_PRODUCT( ztmp(:,ib), ztmp(:,ib) ) )
                       IF ( proj_wgt < atmproj_thr ) CYCLE ibnd_loop
                       !
                   ENDIF
                   !
                   !
                   DO j = 1, dimwann
                   DO i = 1, dimwann
                       !
                       kham(i,j,ik) = kham(i,j,ik) &
                                        + ztmp(i,ib) * (eig(ib,ikeq,isp)-atmproj_sh) * CONJG( ztmp(j,ib) )
                       !
                   ENDDO
                   ENDDO
                   !
               ENDDO ibnd_loop

#if defined __SHIFT_TEST
               ENDIF
#endif
               !
               IF ( .NOT. mat_is_herm( dimwann, kham(:,:,ik), TOLL=EPS_m8 ) ) &
                   CALL errore(subname,'kham not hermitian',10)
               !
               DEALLOCATE( ztmp, STAT=ierr)
               IF ( ierr/=0 ) CALL errore(subname,'deallocating ztmp', ABS(ierr) )


               !
               ! overlaps
               ! projections are read orthogonal, if non-orthogonality
               ! is required, we multiply by S^1/2
               !
               IF ( .NOT. do_orthoovp_ ) THEN
                   !
                   ALLOCATE( zaux(dimwann,dimwann), ztmp(dimwann,dimwann), STAT=ierr )
                   IF ( ierr/=0 ) CALL errore(subname, 'allocating raux-rtmp', ABS(ierr) )
                   ALLOCATE( w(dimwann), kovp_sq(dimwann,dimwann), STAT=ierr )
                   IF ( ierr/=0 ) CALL errore(subname, 'allocating w, kovp_sq', ABS(ierr) )

                   !
                   ! kpt symmetrization needs to be implemented
                   ! when non-orthogonal orbitals are used
                   IF ( nkpts /= nkpts_all ) CALL errore(subname,"kpt symmetrization and OVPs not implemented",10)

!
! The following implementation is still tentative, so the above error is kept
! for the moment
!

                   !
                   !
                   ! ztmp(i, j) = < phi^at_i | phi^at_j >
                   !
                   IF ( kpteq_symm(ik) <= nsym ) THEN
                      ! no time-reversal involved
                      kovp_all(1:natomwfc,1:natomwfc,ik,isp) = kovp(1:natomwfc,1:natomwfc,ikeq,isp) 
                   ELSE
                      kovp_all(1:natomwfc,1:natomwfc,ik,isp) = CONJG( kovp(1:natomwfc,1:natomwfc,ikeq,isp) )
                   ENDIF
                   !
                   CALL atmproj_rotate_ovp( natomwfc, kpteq_symm(ik), vkpt_cry(:,ikeq), kovp_all(:,:,ik,isp))
!
! end of tentative implementation
!
       
                   !
                   ! diagonalize ovp
                   !
                   CALL mat_hdiag( zaux, w(:), kovp_all(:,:,ik,isp), dimwann)
                   !              
                   DO i = 1, dimwann
                       !
                       IF ( w(i) <= ZERO ) CALL errore(subname,'unexpected eig < = 0 ',i)
                       w(i) = SQRT( w(i) )
                       !
                   ENDDO
                   !
                   DO j = 1, dimwann
                   DO i = 1, dimwann
                       !
                       ztmp(i,j) = zaux(i,j) * w(j)
                       !
                   ENDDO
                   ENDDO
                   !
                   CALL mat_mul( kovp_sq, ztmp, 'N', zaux, 'C', dimwann, dimwann, dimwann)
                   !
                   IF ( .NOT. mat_is_herm( dimwann, kovp_sq, TOLL=EPS_m8 ) ) &
                       CALL errore(subname,'kovp_sq not hermitean',10)

                   !
                   ! apply the basis change to the Hamiltonian
                   ! multiply kovp_sq (S^1/2) to the right and the left of kham
                   !
                   CALL mat_mul( zaux, kovp_sq,      'N', kham(:,:,ik), 'N', dimwann, dimwann, dimwann)
                   CALL mat_mul( kham(:,:,ik), zaux, 'N', kovp_sq,      'N', dimwann, dimwann, dimwann)
                   !
                   !
                   DEALLOCATE( zaux, ztmp, STAT=ierr)
                   IF ( ierr/=0 ) CALL errore(subname,'deallocating zaux, ztmp',ABS(ierr))
                   DEALLOCATE( w, kovp_sq, STAT=ierr)
                   IF ( ierr/=0 ) CALL errore(subname,'deallocating w, kovp_sq',ABS(ierr))
                   !
               ENDIF
               
               !
               ! fermi energy is taken into accout
               ! The energy shift is performed on the final matrix
               ! and not on the DFT eigenvalues            
               ! as     eig(:,:,:) = eig(:,:,:) -efermi
               ! because the atomic basis is typically very large 
               ! and would require a lot of bands to be described
               !
               IF ( .NOT. do_orthoovp_ ) THEN
                   !
                   DO j = 1, dimwann
                   DO i = 1, dimwann
                       kham(i,j,ik) = kham(i,j,ik) + atmproj_sh * kovp_all(i,j,ik,isp)
                   ENDDO
                   ENDDO
                   !
               ELSE
                   !
                   DO i = 1, dimwann
                       kham(i,i,ik) = kham(i,i,ik) + atmproj_sh
                   ENDDO
                   !
               ENDIF
               !
           ENDDO kpt_loop

#if defined __WRITE_ASCIIHAM
           ! 
           if (isp == 1 .and. nspin_==1) kham_file = "kham.txt"
           if (isp == 1 .and. nspin_==2) kham_file = "kham_up.txt"
           if (isp == 2) kham_file = "kham_down.txt"

           IF (isp ==1) THEN !
              OPEN (unit = 14, file = "k.txt")
              DO ik =1, nkpts
                 WRITE(14,"(3f20.13)") vkpt_cry(1,ik), vkpt_cry(2,ik), vkpt_cry(3,ik)
              ENDDO
              CLOSE(14)

              OPEN (unit = 14, file = "wk.txt")
              DO ik =1, nkpts
                 WRITE(14,"(f20.13)") wk(ik)
              ENDDO
              CLOSE(14)

              OPEN (unit = 14, file = "kovp.txt")
              DO ik =1, nkpts
                  DO iw=1,dimwann
                     DO jw=1,dimwann
                        WRITE(14,"(2f20.13)") real(kovp(iw,jw,ik,isp)),aimag(kovp(iw,jw,ik,isp))
                     ENDDO
                  ENDDO
              ENDDO
              CLOSE(14)
           ENDIF

           OPEN (unit = 14, file = trim(kham_file))
           DO ik =1, nkpts
               DO iw=1,dimwann
                  DO jw=1,dimwann
                     WRITE(14,"(2f20.13)") real(kham(iw,jw,ik)),aimag(kham(iw,jw,ik))
                  ENDDO
               ENDDO
           ENDDO
           CLOSE(14)
#endif


           ! 
           ! convert to real space
           !
           DO ir = 1, nrtot
               !
               CALL compute_rham( dimwann, vr(:,ir), rham(:,:,ir,isp), &
                                  nkpts_all, vkpt_all, wk_all, kham )
               !
               IF ( .NOT. do_orthoovp_ ) THEN
                   !
                    CALL compute_rham( dimwann, vr(:,ir), rovp(:,:,ir,isp), &
                                       nkpts_all, vkpt_all, wk_all, kovp_all(:,:,:,isp) )
                   !
               ENDIF
               !
           ENDDO
           !
       ENDDO 
 
#if defined __SHIFT_TEST
       !
       IF ( shifting_scheme .EQ. 2 ) THEN
           DEALLOCATE( A )
           DEALLOCATE( PA )
           DEALLOCATE( IPA )
           DEALLOCATE( E )
           DEALLOCATE( kham_aux )
       ENDIF
#endif

       ! 
       DEALLOCATE( kham, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating kham', ABS(ierr) )
       !
   ENDIF
   !
   IF ( ALLOCATED( kovp ) ) THEN
       DEALLOCATE( kovp, STAT=ierr)
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating kovp', ABS(ierr) )
   ENDIF
   !
   IF ( ALLOCATED( kovp_all ) ) THEN
       DEALLOCATE( kovp_all, STAT=ierr)
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating kovp_all', ABS(ierr) )
   ENDIF
   !
   DEALLOCATE( vkpt_all, kpteq_map, kpteq_symm, STAT=ierr)
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating vkpt_all, kpteq_map, kpteq_symm', ABS(ierr) )


!
!---------------------------------
! write to fileout (internal fmt)
!---------------------------------
!
   IF ( ionode .AND. write_ham ) THEN
       !
       CALL iotk_open_write( ounit, FILE=TRIM(fileham), BINARY=binary )
       CALL iotk_write_begin( ounit, "HAMILTONIAN" )
       !
       !
       CALL iotk_write_attr( attr,"dimwann",dimwann,FIRST=.TRUE.)
       CALL iotk_write_attr( attr,"nkpts",nkpts)
       CALL iotk_write_attr( attr,"nspin",nspin)
       CALL iotk_write_attr( attr,"spin_component",TRIM(spin_component))
       CALL iotk_write_attr( attr,"nk",nk)
       CALL iotk_write_attr( attr,"shift",shift)
       CALL iotk_write_attr( attr,"nrtot",nrtot)
       CALL iotk_write_attr( attr,"nr",nr)
       CALL iotk_write_attr( attr,"have_overlap", .NOT. do_orthoovp_ )
       CALL iotk_write_attr( attr,"fermi_energy", 0.0_dbl )
       CALL iotk_write_empty( ounit,"DATA",ATTR=attr)
       !
       CALL iotk_write_attr( attr,"units","bohr",FIRST=.TRUE.)
       CALL iotk_write_dat( ounit,"DIRECT_LATTICE", avec, ATTR=attr, COLUMNS=3)
       !
       CALL iotk_write_attr( attr,"units","bohr^-1",FIRST=.TRUE.)
       CALL iotk_write_dat( ounit,"RECIPROCAL_LATTICE", bvec, ATTR=attr, COLUMNS=3)
       !
       CALL iotk_write_attr( attr,"units","crystal",FIRST=.TRUE.)
       CALL iotk_write_dat( ounit,"VKPT", vkpt_cry, ATTR=attr, COLUMNS=3)
       CALL iotk_write_dat( ounit,"WK", wk)
       !
       CALL iotk_write_dat( ounit,"IVR", ivr, ATTR=attr, COLUMNS=3)
       CALL iotk_write_dat( ounit,"WR", wr)
       !
       !
       spin_loop: & 
       DO isp = 1, nspin
          !
          IF ( TRIM(spin_component) == "up"   .AND. isp == 2 ) CYCLE
          IF ( TRIM(spin_component) == "down" .AND. isp == 1 ) CYCLE
          IF ( TRIM(spin_component) == "dw"   .AND. isp == 1 ) CYCLE
          !
          IF ( TRIM(spin_component) == "all" .AND. nspin == 2 ) THEN
              !
              CALL iotk_write_begin( ounit, "SPIN"//TRIM(iotk_index(isp)) )
              !
          ENDIF
          !
          !
          CALL iotk_write_begin( ounit,"RHAM")
          !
          DO ir = 1, nrtot
              !
              CALL iotk_write_dat( ounit,"VR"//TRIM(iotk_index(ir)), rham(:,:, ir, isp) )
              !
              IF ( .NOT. do_orthoovp_ ) THEN
                  CALL iotk_write_dat( ounit,"OVERLAP"//TRIM(iotk_index(ir)), &
                                       rovp( :, :, ir, isp) )
              ENDIF
              !
              !
          ENDDO
          !
          CALL iotk_write_end( ounit,"RHAM")
          !
          IF ( nspin == 2 .AND. TRIM(spin_component) == "all" ) THEN
              !
              CALL iotk_write_end( ounit, "SPIN"//TRIM(iotk_index(isp)) )
              !
          ENDIF
          !
       ENDDO spin_loop
       !
       CALL iotk_write_end( ounit, "HAMILTONIAN" )
       CALL iotk_close_write( ounit )
       !
   ENDIF

   IF ( ionode .AND. write_space ) THEN
       !
       CALL iotk_open_write( ounit, FILE=TRIM(filespace), BINARY=binary )
       !
       !
       CALL iotk_write_begin( ounit, "WINDOWS" )
       !
       CALL iotk_write_attr( attr,"nbnd",atmproj_ndim_,FIRST=.TRUE.)
       CALL iotk_write_attr( attr,"nkpts",nkpts)
       CALL iotk_write_attr( attr,"nspin",nspin)
       CALL iotk_write_attr( attr,"spin_component",TRIM(spin_component))
       CALL iotk_write_attr( attr,"efermi", 0.0_dbl )
       CALL iotk_write_attr( attr,"dimwinx", atmproj_ndim_ )
       CALL iotk_write_empty( ounit,"DATA",ATTR=attr)
       !
       ALLOCATE( itmp(nkpts), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'allocating itmp', ABS(ierr))
       ALLOCATE( eamp_tmp(atmproj_ndim_,dimwann), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'allocating eamp_tmp', ABS(ierr))
       !
       itmp(:) = atmproj_ndim_
       CALL iotk_write_dat( ounit, "DIMWIN", itmp, COLUMNS=8 )
       itmp(:) = 1
       CALL iotk_write_dat( ounit, "IMIN", itmp, COLUMNS=8 )
       itmp(:) = atmproj_ndim_
       CALL iotk_write_dat( ounit, "IMAX", itmp, COLUMNS=8 )
       !
       DO isp = 1, nspin
           !
           IF ( nspin == 2 ) THEN
               CALL iotk_write_begin( ounit, "SPIN"//TRIM(iotk_index(isp)) )
           ENDIF
           !
           CALL iotk_write_dat( ounit, "EIG", eig(atmproj_nbndmin_:atmproj_nbnd_,:,isp), COLUMNS=4)
           !
           IF ( nspin == 2 ) THEN
               CALL iotk_write_end( ounit, "SPIN"//TRIM(iotk_index(isp)) )
           ENDIF
           !
       ENDDO
       !
       CALL iotk_write_end( ounit, "WINDOWS" )
       !
       !
       CALL iotk_write_begin( ounit, "SUBSPACE" )
       !
       CALL iotk_write_attr( attr,"dimwinx",atmproj_ndim_,FIRST=.TRUE.)
       CALL iotk_write_attr( attr,"nkpts",nkpts)
       CALL iotk_write_attr( attr,"dimwann", dimwann)
       CALL iotk_write_empty( ounit,"DATA",ATTR=attr)
       !
       itmp(:) = atmproj_ndim_
       CALL iotk_write_dat( ounit, "DIMWIN", itmp, COLUMNS=8 )
       !
       DO isp = 1, nspin
           !
           IF ( nspin == 2 ) THEN
               CALL iotk_write_begin( ounit, "SPIN"//TRIM(iotk_index(isp)) )
           ENDIF
           !
           DO ik = 1, nkpts
               !
               eamp_tmp(1:atmproj_ndim_, 1:dimwann) = &
                        CONJG(TRANSPOSE(proj(1:dimwann,atmproj_nbndmin_:atmproj_nbnd_,ik,isp) )) 
               !
               DO ib = atmproj_nbndmin_, atmproj_nbnd_
                   ib_ = ib -atmproj_nbndmin_+1
                   proj_wgt = REAL( DOT_PRODUCT( proj(:,ib,ik,isp ), proj(:,ib,ik,isp ) ) )
                   IF ( proj_wgt < atmproj_thr ) eamp_tmp( ib_, :) = 0.0d0
               ENDDO
               !
               CALL iotk_write_dat( ounit, "EAMP"//TRIM(iotk_index(ik)), eamp_tmp )
               !
           ENDDO
           !
           IF ( nspin == 2 ) THEN
               CALL iotk_write_end( ounit, "SPIN"//TRIM(iotk_index(isp)) )
           ENDIF
           !
       ENDDO
       !
       CALL iotk_write_end( ounit, "SUBSPACE" )
       !
       !
       CALL iotk_close_write( ounit )
       !
       DEALLOCATE( itmp, eamp_tmp, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating itmp, eamp', ABS(ierr))
       !
   ENDIF


   IF ( ionode .AND. write_loc ) THEN
       !
       CALL iotk_open_write( ounit, FILE=TRIM(filewan), BINARY=binary )
       !
       CALL iotk_write_begin( ounit, "WANNIER_LOCALIZATION" )
       !
       CALL iotk_write_attr( attr,"dimwann",dimwann,FIRST=.TRUE.)
       CALL iotk_write_attr( attr,"nkpts",nkpts)
       CALL iotk_write_empty( ounit,"DATA",ATTR=attr)
       !
       CALL iotk_write_attr( attr,"Omega_I",0.0d0,FIRST=.TRUE.)
       CALL iotk_write_attr( attr,"Omega_D",0.0d0)
       CALL iotk_write_attr( attr,"Omega_OD",0.0d0)
       CALL iotk_write_attr( attr,"Omega_tot",0.0d0)
       CALL iotk_write_empty( ounit,"SPREADS",ATTR=attr)
       !
       ALLOCATE( rtmp(3,dimwann), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'allocating rtmp', ABS(ierr))
       ALLOCATE( cu_tmp(dimwann,dimwann,nkpts), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'allocating cu_tmp', ABS(ierr))
       !
       cu_tmp(:,:,:) = 0.0d0
       !
       DO ik = 1, nkpts
       DO i  = 1, dimwann
           cu_tmp(i,i,ik) = 1.0d0
       ENDDO
       ENDDO
       !
       rtmp(:,:)=0.0d0
       !
       CALL iotk_write_dat(ounit,"CU",cu_tmp)
       CALL iotk_write_dat(ounit,"RAVE",rtmp,COLUMNS=3)
       CALL iotk_write_dat(ounit,"RAVE2",rtmp(1,:))
       CALL iotk_write_dat(ounit,"R2AVE",rtmp(1,:))
       !
       DEALLOCATE( rtmp, cu_tmp, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating rtmp, cu_tmp', ABS(ierr))
       !
       CALL iotk_write_end( ounit, "WANNIER_LOCALIZATION" )
       !
       CALL iotk_close_write( ounit )
       !
   ENDIF

!
! local cleaning
!
   call error(' ions subroutines not imported') 
   !CALL ions_deallocate()
   call error(' symmetry subroutines not imported') 
   !CALL symmetry_deallocate()

   DEALLOCATE( proj, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating proj', ABS(ierr) )
   !
   DEALLOCATE( vkpt, wk, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating vkpt, wk', ABS(ierr) )
   DEALLOCATE( vkpt_cry, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating vkpt_cry', ABS(ierr) )
   !
   DEALLOCATE( ivr, wr, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating ivr, wr', ABS(ierr) )
   DEALLOCATE( vr, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating vr', ABS(ierr) )
   !
   DEALLOCATE( eig, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating eig', ABS(ierr) )
   !
   IF( ALLOCATED( rham ) ) THEN
       DEALLOCATE( rham, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating rham', ABS(ierr) )
   ENDIF
   IF( ALLOCATED( rovp ) ) THEN
       DEALLOCATE( rovp, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating rovp', ABS(ierr) )
   ENDIF
   

   !CALL log_pop( subname )
   CALL timing( subname, OPR='stop' )
   !
   RETURN
   !
END SUBROUTINE atmproj_to_internal

