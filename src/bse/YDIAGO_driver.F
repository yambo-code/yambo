!
! License-Identifier: GPL
!
! Copyright (C) 2024 The Yambo Team
!
! Authors (see AUTHORS file for details): NM
!
subroutine YDIAGO_driver(i_BS_mat, BS_energies, BS_VR, &
    & neigs_this_cpu, neig_shift, neigs_range, eigvals_range, &
    & BS_VL, BS_overlap, solver_type, elpasolver)
  !
  ! Here We diagonalize any BSE hamilitioan
  !
  !      | (K_r)     (cI*K_c)    |  
  !  K = |                       |
  !      | (-cI*K_c^*)  (-K_r^*) |
  !
  !  This total number of eigenvectors found can be obtained by
  !  by calling size function of eigvals i.e size(BS_energies)
  !
  !  Note : Ydiago always returns normalized right eigenvectors 
  !  and the left eigenvectors are choosen such that overlap is a an identity matrix.
  !  This implies that left eigenvectors maynot be normalized in some cases.
  use pars,           ONLY:cI,cONE, SP
  use BS,             ONLY:BS_K_dim,BS_H_dim,BS_blk,n_BS_blks,BS_K_coupling,&
    &                        BS_res_ares_n_mat,l_BS_ares_from_res
  use BS_solvers,     ONLY:BSS_eh_E,BSS_eh_W,BSS_perturbative_width
  use  gpu_m,         ONLY:have_cuda
  use parallel_m,     ONLY:MPI_COMM_WORLD,par_distribute_set
  use openmp,         ONLY:n_threads_K
  use LIVE_t,         ONLY:live_timing
  use com,            ONLY:msg
  use ydiago_interface
  !
  implicit none
  !
  integer, intent(in)      :: i_BS_mat
  complex(SP), allocatable :: BS_energies(:)
  ! Energy values. these will be allocated internally.(realloced if already allocated)
  ! Each cpu gets full set of eigenvalues and size(BS_energies) gives 
  ! total number of eigenvectors found (will be same on all processes).
  complex(SP), allocatable, target :: BS_VR(:,:) 
  ! Right eigenvectors. these will be allocated internally (realloced if allocated)
  ! This process store eigenvectors from [neig_shift+1, neigs_this_cpu+neig_shift]
  integer, intent(out)     :: neigs_this_cpu  
  integer, intent(out)     :: neig_shift
  !
  ! optional arguments  
  integer, optional, target          :: neigs_range(2)
  ! index range of eigen values (input)
  real(SP), optional, target         :: eigvals_range(2)
  ! value range of eigenvalues (input)
  complex(SP), allocatable, optional, target :: BS_VL(:,:) 
  ! Left eigenvectors. these will be allocated internally (input) !(realloced if already allocated)
  character, optional                :: solver_type   ! if 'e' uses elpa, 's' scalapack
  ! SOlver to use Elpa or scalapack (input)
  integer,  optional                 :: elpasolver
  ! 1 or 2. put 2 as default and give an option to user (input)
  complex(SP),pointer,optional       :: BS_overlap(:,:) ! overlap matrix
  !
  ! FIX ME : Move to a better place (modules)
  integer, parameter                 :: blacs_block_size = 64
  ! blacs block size
  ! 64 is good default but give a variable so advanced expert users can play with it
  !
  ! Local variables
  type(c_ptr)                        :: mpicxt, diago_mat, evecs
  integer                            :: nProcs, my_rank, neig_tmp
  integer(YDIAGO_INT)                :: SL_H_dim, ProcX, ProcY, elpa_nthreads, mpi_comm
  integer(ERROR_INT)                 :: error_diago ! diagonalization error code
  integer                            :: ierr !! mpi error code 
  integer(YDIAGO_INT)                :: neig_found, i_c, i_r, blacs_blk_size 
  integer(YDIAGO_LL_INT)             :: nelements
  ! optional local vars with their defaults
  character                          :: solver_type_aux   = 's'
  integer(YDIAGO_INT)                :: elpa_solver_aux   = 2
  type(c_ptr)                        :: neigs_range_tmp   = c_null_ptr
  type(c_ptr)                        :: eigvals_range_tmp = c_null_ptr
  type(c_ptr)                        :: evecs_left        = c_null_ptr
  logical                            :: compute_left_eigs = .false.
  logical                            :: run_gpu = .false.
  complex(YDIAGO_CMPLX), target, allocatable :: eig_vals(:)
  !
  integer(YDIAGO_INT), target        :: neigs_range_y(2)
  real(YDIAGO_FLOAT), target         :: eigvals_range_y(2)
  !
  ! Gpu support via ELPA.
  type(c_ptr)                             :: gpu_str = c_null_ptr
  character(kind=c_char, len=20), target  :: gpu_device_elpa
  ! NM : gpu_device_elpa must be atleat 20 characters long 

  integer                            :: evec_fac = 1
  ! evec_fac = 2 if bse_solver function was used to diagonalize else 1

  if(PRESENT(neigs_range)) then
    if(neigs_range(1)>0) then
      neig_tmp=neigs_range(2)-neigs_range(1)+1
      neigs_range_y(1) = neigs_range(1) 
      neigs_range_y(2) = neigs_range(2) 
      ! DS: I need to divide by 2 the nnumber of requested eigenv in the
      ! coupling case, otherwise dimensions do not match
      if (BS_K_coupling) neigs_range_y(2) = neigs_range_y(1) + neig_tmp/2-1
      neigs_range_tmp = c_loc(neigs_range_y)
    end if
  end if
  !!
  if(present(eigvals_range)) then
    if(eigvals_range(1)>0._SP) then
      eigvals_range_y(1) = eigvals_range(1) 
      eigvals_range_y(2) = eigvals_range(2) 
      eigvals_range_tmp = c_loc(eigvals_range_y)
    end if
  end if
  !!
  if(present(BS_VL)) compute_left_eigs = .true.
  if(present(solver_type)) solver_type_aux = solver_type
  if(present(elpasolver)) elpa_solver_aux = elpasolver
  !
#if !defined _ELPA
  ! K_diago_driver does these checks. Here we just proceed
  ! if defined _ELPA is only needed for compilation purposes
  if (solver_type_aux .eq. 'e') call error("elpa solver requested but elpa not available")
#endif
  !
  run_gpu = have_cuda ! Turn this flag to run the diagonalization on gpu.
  ! Note that even when compiled with gpu support, if this is set to false,
  ! then the diagonalization will run on cpu.
  !
if (run_gpu) then
  ! Set the gpu flag
  call set_elpa_gpu_str(gpu_device_elpa)
  if (gpu_device_elpa(1:3) /= c_char_"not") then 
    gpu_str = c_loc(gpu_device_elpa)
    ! NM: ELPA GPU for now does not support solver type =2, 
    ! in this case we force the solver to be type=1 (i.e standard QR)
    ! In future, if it works, we need to remove this.
    ! See https://github.com/marekandreas/elpa/blob/master/src/elpa2/elpa2_template.F90#L1742
    if (elpa_solver_aux .eq. 2) then 
      call msg("s", 'Warning : ELPA Solver 2 is not GPU Ported. Switching to QR Solver.') 
      elpa_solver_aux = 1
    endif
  else 
    call msg("s", 'Warning : Ydiago not compiled with GPU. using CPU version')
    gpu_str = c_null_ptr
  endif 
endif

#if defined _OPENMP && !defined _GPU
  elpa_nthreads = int(n_threads_K,kind=YDIAGO_INT)
#else 
  elpa_nthreads = 1 
#endif
  ! Start the function
  !
  if(     BS_K_coupling) SL_H_dim=BS_H_dim
  if(.not.BS_K_coupling) SL_H_dim=BS_K_dim(i_BS_mat)
  !
  ! Allocate the 2D block cyclic matrix
  !
  ! First create a blacs grid
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nProcs, ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierr)

  ProcX = int(sqrt(real(nProcs)),kind=YDIAGO_INT)
  ProcY = ProcX
  ! Maximize the number of cpus participating
  if ( ProcX * (ProcX + 1) .le. nProcs) ProcY = ProcX + 1
  if ( ProcX * (ProcX + 2) .le. nProcs) ProcY = ProcX + 2
  ! Geev block size must be smaller than mat dim and >=8
  ! Start with the default 64
  blacs_blk_size = blacs_block_size
  ! if not try 32
  if (blacs_blk_size .gt. SL_H_dim/ProcY) blacs_blk_size = blacs_blk_size/2
  ! if not try 16
  if (blacs_blk_size .gt. SL_H_dim/ProcY) blacs_blk_size = blacs_blk_size/2
  ! Last try.
  if (blacs_blk_size .gt. SL_H_dim/ProcY) then
    ! force the solver to be scalapack
    ! If block size is <8, geev solver will fail.
    solver_type_aux = 's'
    blacs_blk_size = 8 !SL_H_dim
    ProcX = 1
    ProcY = 1
  endif
  
  call msg("s","BLACS grid",(/ProcX,ProcY/))

  mpi_comm = MPI_COMM_WORLD

  mpicxt = BLACScxtInit_Fortran('R', mpi_comm, ProcX, ProcY)
  if (.not. c_associated(mpicxt)) then
    call error("Failed to initiate BLACS context")
  end if

  diago_mat = init_D_Matrix(SL_H_dim, SL_H_dim, blacs_blk_size, blacs_blk_size, mpicxt)
  if (.not. c_associated(diago_mat)) then
    call error("Failed to initiate block cyclic BSE matrix")
  end if
  !
  ! Fill the block cyclic matrix
  call live_timing('Filling BSE Matrix',1)
  call K_fill_block_cyclic(i_BS_mat, diago_mat)
  call live_timing()
  ! Now we create a matix for eigenvectors
  !
  allocate(eig_vals(SL_H_dim))

  evecs = init_D_Matrix(SL_H_dim, SL_H_dim, blacs_blk_size, blacs_blk_size, mpicxt)
  if (.not. c_associated(evecs)) then
    call error("Failed to initiate eigenvectors for block cyclic BSE matrix")
  end if
  !
  ! Allocate left eigenvectors only incase requested
  if (compute_left_eigs) then 
    !
    if (.not. l_BS_ares_from_res .and. BS_K_coupling) then
      !
      evecs_left = init_D_Matrix(SL_H_dim, SL_H_dim, blacs_blk_size, blacs_blk_size, mpicxt)
      !
      if (.not. c_associated(evecs_left)) then
        call error("Failed to initiate left eigenvectors for block cyclic BSE matrix")
      end if
      !
    endif
    !
  endif
  !
  if (solver_type_aux .eq. 'e')   call section('=','ELPA Diagonalization')
  if (solver_type_aux .eq. 's')   call section('=','Scalapack Diagonalization')
  !
  ! Call the solvers
  ! 
  evec_fac = 1          ! This is important set to 1
  !
  call live_timing('BSK diagonalize',1)
  !
  if (.not. BS_K_coupling) then
    !
    neig_found = SL_H_dim ! This will be modified by scalapack functions
    if (solver_type_aux .eq. 'e' .and. present(neigs_range)) then
      ! DS: Just for the elpa case I need to set neig_found in input
      !     This is at least what was in the coding before
      if (neigs_range(2) > 0) neig_found = neigs_range(2)
    endif
    !
    ! ===========  TDA Case ===============
    if (solver_type_aux .eq. 'e') then
#if defined _ELPA
      error_diago = Heev_Elpa(diago_mat, c_loc(eig_vals), evecs, neig_found, elpa_solver_aux, gpu_str, elpa_nthreads)
#endif
    else
      error_diago = Heev(diago_mat, 'L', neigs_range_tmp, eigvals_range_tmp, c_loc(eig_vals), evecs, neig_found)
    endif
    !
  else if (l_BS_ares_from_res.and.BS_K_coupling) then
    !
    ! =========  Non-TDA when Anti-res = -conj(res) ==========
    !
    !       // https://doi.org/10.1016/j.laa.2015.09.036
    !     Eigen values come in pair i.e (-lambda, lambda).
    !         Right eigenvectors         Left eigenvectors
    !           +ve     -ve               +ve       -ve
    !     X = [ X_1, conj(X_2) ]    Y = [  X_1, -conj(X_2)]
    !          [X_2, conj(X_1) ]        [ -X_2,  conj(X_1)],
    ! Note that the overlap matrix is identity in this case, so we donot construct
    !
    ! Elpa always gives full spectrum
    neig_found = SL_H_dim/2 ! Only +ve eigenvalues computes and rest are retreived
    if (solver_type_aux .eq. 's' .and. present(neigs_range)) then
    endif
    !
    if (solver_type_aux .eq. 'e') then
#if defined _ELPA
      error_diago = BSE_Solver_Elpa(diago_mat, c_loc(eig_vals), evecs, elpa_solver_aux, gpu_str, elpa_nthreads) 
#endif
    else 
      error_diago = BSE_Solver(diago_mat, neigs_range_tmp, eigvals_range_tmp, c_loc(eig_vals), evecs, neig_found)
    endif
    !
    ! Incase of Chosleky failure (due to not being +ve definite, fall back to Geev solver)
    ! -50 is the error code when CHOLESKY factorization fails. see Ydiago/src/common/error.h
    if (error_diago == -50) then
      !
      call msg("s", 'Warning : Cholesky decomposition failed. &
        & Switching to slow scalapack solver. This solver is not-GPU supported.')
      !
      if (compute_left_eigs .and. .not. c_associated(evecs_left)) then 
        ! initiate left eigenvectors in case not
        evecs_left = init_D_Matrix(SL_H_dim, SL_H_dim, blacs_blk_size, blacs_blk_size, mpicxt)
        if (.not. c_associated(evecs_left)) then
          call error("Failed to initiate left eigenvectors for block cyclic BSE matrix")
        end if
        !
      endif
      !
      neig_found = SL_H_dim
      !
      if (blacs_blk_size .lt. 8 ) call error("Too small block size. User solver: o")
      ! NM : FIX ME : In this case, simply switch to lapack solver.
      error_diago = Geev(diago_mat, c_loc(eig_vals), evecs_left, evecs)
      !
      evec_fac = 1
    else 
      evec_fac = 2
    endif
    !
    !
  else 
    ! 
    ! ========== General solver =================
    neig_found = SL_H_dim
    !
    if (blacs_blk_size .lt. 8 ) call error("Too small block size. User solver: o")
    ! NM : FIX ME : In this case, simply switch to lapack solver.
    error_diago = Geev(diago_mat, c_loc(eig_vals), evecs_left, evecs)
    ! On output diago_mat has overlap i.e VL^H @ VR
    !
  endif

  if (error_diago /= 0) call error("Diagonalization failed")
  call live_timing()

  ! Free the space of the distributed matrix
  call free_D_Matrix(diago_mat)

  if(allocated(BS_energies)) deallocate(BS_energies)
  ! 
  allocate(BS_energies(neig_found*evec_fac))
  !
  BS_energies(1:neig_found*evec_fac:evec_fac) = eig_vals(1:neig_found)
  
  if (evec_fac == 2) then 
    ! set -ve values in case of special solver
    BS_energies(2:neig_found*evec_fac:2) = -eig_vals(1:neig_found)
  endif
  !
  deallocate(eig_vals)
  !
  !call section('=','Folding eigenvectors')
  ! Now retreive the right eigen_vectors
  !
  ! Compute number of right eigenvectors residing in this cpu
  ! neigs_this_cpu, neig_shift
  call par_distribute_set(neig_found, neigs_this_cpu, neig_shift)

  ! Set the Overlap to null() as this is always identity in case Ydiago solver is used
  if (present(BS_overlap)) BS_overlap => null()

  nelements = neigs_this_cpu*SL_H_dim
  !
  if(allocated(BS_VR)) deallocate(BS_VR)
  !
  allocate(BS_VR(SL_H_dim, evec_fac*neigs_this_cpu))
  !
  ! retreive right eigenvectors
  ! initiate get queue
  call live_timing('Retrieving Right eigenvectors',1)
  error_diago = initiateGetQueue(evecs, nelements)
  if (error_diago /= 0) call error("Failed to initiate GetQueue")
  !
  ! request the right eigenvectors
  do i_c = 0, neigs_this_cpu-1
    do i_r = 1, SL_H_dim
      error_diago = dmatget_fortran(evecs, i_r, i_c + neig_shift + 1, c_loc(BS_VR(i_r, evec_fac*i_c + 1)) )
      if (error_diago /= 0) call error("Failed to retreive eigenvectors from distributed matrix")
    enddo
  enddo
  !
  ! finalize the queue. The eigenvectors buffer gets magically filled after this call !
  error_diago = ProcessGetQueue(evecs)
  if (error_diago /= 0) call error("Failed to Process GetQueue")
  ! we got the data ! Free right eigen-vectors.
  call free_D_Matrix(evecs)
  call live_timing()
  !
  if (evec_fac == 2) then
    ! set the right eigenvectors for -ve eigenvalues
    BS_VR(1:SL_H_dim/2, 2:2*neigs_this_cpu:2)  = conjg(BS_VR(SL_H_dim/2 + 1:SL_H_dim, 1:2*neigs_this_cpu:2))
    BS_VR(SL_H_dim/2 + 1:SL_H_dim, 2:2*neigs_this_cpu:2) = conjg(BS_VR(1:SL_H_dim/2, 1:2*neigs_this_cpu:2))
  endif
  !
  ! Set the left eigen vectors in case requested
  if (compute_left_eigs) then 
    !
    if(allocated(BS_VL)) deallocate(BS_VL)
    allocate(BS_VL(SL_H_dim, evec_fac*neigs_this_cpu))
    !
    if (evec_fac  == 2) then 
      ! left eigen-vectors for +ve eigenvalues
      BS_VL(1:SL_H_dim/2, 1:2*neigs_this_cpu:2)            =  BS_VR(1:SL_H_dim/2, 1:2*neigs_this_cpu:2)
      BS_VL(SL_H_dim/2 + 1:SL_H_dim, 1:2*neigs_this_cpu:2) = -BS_VR(SL_H_dim/2 + 1:SL_H_dim, 1:2*neigs_this_cpu:2)
      ! left eigen-vectors for -ve eigenvalues
      BS_VL(1:SL_H_dim/2, 2:2*neigs_this_cpu:2)  = CONJG(BS_VL(SL_H_dim/2 + 1:SL_H_dim, 1:2*neigs_this_cpu:2))
      BS_VL(SL_H_dim/2 + 1:SL_H_dim, 2:2*neigs_this_cpu:2) = CONJG(BS_VL(1:SL_H_dim/2, 1:2*neigs_this_cpu:2))
    else 
      ! set the Queue for the left eigen vectors
      call live_timing('Retrieving Left eigenvectors',1)
      error_diago = initiateGetQueue(evecs_left, nelements)
      if (error_diago /= 0) call error("Failed to initiate left eigvec GetQueue")
      !
      ! request the left eigenvectors
      do i_c = 0, neigs_this_cpu-1
        do i_r = 1, SL_H_dim
          error_diago = dmatget_fortran(evecs_left, i_r, i_c + neig_shift + 1, c_loc(BS_VL(i_r, evec_fac*i_c + 1)) )
          if (error_diago /= 0) call error("Failed to retreive left eigenvectors from distributed matrix")
        enddo
      enddo
      ! finalize the queue
      error_diago = ProcessGetQueue(evecs_left)
      if (error_diago /= 0) call error("Failed to Process left eigen vectors GetQueue")
      call live_timing()
    endif
    !
  endif
  !
  ! Free remaining resources
  call free_D_Matrix(evecs_left)
  ! Free blacs context
  call BLACScxtFree(mpicxt)
  !
  neigs_this_cpu = evec_fac*neigs_this_cpu
  neig_shift     = evec_fac*neig_shift
  !
end subroutine YDIAGO_driver 
