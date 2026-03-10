!
! License-Identifier: GPL
! Copyright (C) 2006 The Yambo Team
!
subroutine K_fill_block_cyclic(i_BS_mat, diago_mat)
  !
  ! Here I fill the kernel (coupling not yet included) in a 
  ! 2D block-cyclic matrix to be used for diagonalization and/or invertion
  !
  !      | (K_r)     (cI*K_c)    |  
  !  K = |                       |
  !      | (-cI*K_c^*)  (-K_r^*) |
  !
  use pars,           ONLY:cI,cONE
  use BS,             ONLY:BS_K_dim,BS_H_dim,BS_blk,n_BS_blks,BS_K_coupling,&
  &                        BS_res_ares_n_mat,l_BS_ares_from_res
  use BS_solvers,     ONLY:BSS_eh_E,BSS_eh_W,BSS_perturbative_width, &
  &                        run_inversion,run_Haydock
  use  gpu_m,         ONLY:have_cuda
  !
  !use, intrinsic :: iso_c_binding, only: c_ptr
  use ydiago_interface
  !
  implicit none
  !
  integer, intent(in)               :: i_BS_mat
  type(c_ptr), intent(in)           :: diago_mat
  integer                           :: i_c,i_r,i_Tk,i_Tp,i_B,H_shift(2)
  complex(YDIAGO_CMPLX)             :: Mij,Mij_star
  integer(YDIAGO_INT)               :: H_pos(2),SL_K_dim(2),SL_H_dim
  integer(ERROR_INT)                :: error_diago ! diagonalization error code
  integer                           :: ierr !! mpi error code 
  integer(kind=YDIAGO_LL_INT)       :: nelements 
  ! Aux variables 
  
  if(     BS_K_coupling) SL_H_dim=BS_H_dim
  if(.not.BS_K_coupling) SL_H_dim=BS_K_dim(i_BS_mat)
  SL_K_dim=BS_K_dim
  !
  ! First compute number of elements that each cpu is going to fill 
  nelements = 0 ! counter 
  !
  do i_B=1,n_BS_blks
    !
    i_Tk  =BS_blk(i_B)%iT_k
    i_Tp  =BS_blk(i_B)%iT_p
    !
    if (i_BS_mat/=BS_blk(i_B)%ira_k .and. BS_res_ares_n_mat==2) cycle
    !
    H_shift=0
    !
    if(BS_blk(i_B)%mode=="C") H_shift(2)=BS_K_dim(1)
    if(BS_blk(i_B)%mode=="A") H_shift(:)=BS_K_dim(1)
    !
    do i_r=1,BS_blk(i_B)%size(1)
      !
      H_pos(1)=BS_blk(i_B)%coordinate(1)+i_r-1
      !
      do i_c=1,BS_blk(i_B)%size(2)
        !
        H_pos(2)=BS_blk(i_B)%coordinate(2)+i_c-1
        !
        if (H_pos(1)+H_shift(1)>H_pos(2)+H_shift(2)) cycle
        !
        if (l_BS_ares_from_res.and.H_pos(1)>H_pos(2)) cycle
        !
        select case(BS_blk(i_B)%mode)
        !
        case("R")
          nelements = nelements + 2
          if (l_BS_ares_from_res.and.BS_K_coupling) then
            nelements = nelements + 2
          endif
        !
        case("C")
          nelements = nelements + 2 
          if (l_BS_ares_from_res) then
            nelements = nelements + 2
          endif
        !
        case("A")
          if(BS_res_ares_n_mat==1) then
            nelements = nelements + 2
          else
            nelements = nelements + 2
          endif
        !
        case("Q")
          nelements = nelements + 2 
        end select
      enddo
    enddo
  enddo
  !
  ! Now Fill the 2D block cyclic matrix
  ! initiate the set Queue
  error_diago = initiateSetQueue(diago_mat, nelements)
  if (error_diago /= 0) call error("Initiating set Queue failed")
  !
  ! // fill the elements
  do i_B=1,n_BS_blks
    !
    i_Tk  =BS_blk(i_B)%iT_k
    i_Tp  =BS_blk(i_B)%iT_p
    !
    if (i_BS_mat/=BS_blk(i_B)%ira_k .and. BS_res_ares_n_mat==2) cycle
    !
    H_shift=0
    if(BS_blk(i_B)%mode=="C") H_shift(2)=BS_K_dim(1)
    if(BS_blk(i_B)%mode=="A") H_shift(:)=BS_K_dim(1)
    !
    do i_r=1,BS_blk(i_B)%size(1)
      !
      H_pos(1)=BS_blk(i_B)%coordinate(1)+i_r-1
      !
      do i_c=1,BS_blk(i_B)%size(2)
        !
        H_pos(2)=BS_blk(i_B)%coordinate(2)+i_c-1
        !
        ! Then the upper triangle of each block and direct symmetrization
        !
        if (H_pos(1)+H_shift(1)>H_pos(2)+H_shift(2)) cycle
        if (l_BS_ares_from_res.and.H_pos(1)>H_pos(2)) cycle
        !
        Mij     =      BS_blk(i_B)%mat(i_r,i_c)
        Mij_star= real(BS_blk(i_B)%mat(i_r,i_c))-cI*aimag(BS_blk(i_B)%mat(i_r,i_c))
        !
        ! Add energies to the diagonal
        !
        if(H_pos(1)+H_shift(1)==H_pos(2)+H_shift(2)) then
          Mij     =real(Mij)     +BSS_eh_E(H_pos(1)+H_shift(1))*cONE
          Mij_star=real(Mij_star)+BSS_eh_E(H_pos(1)+H_shift(1))*cONE
          if (allocated(BSS_eh_W).and..not.BSS_perturbative_width) then
            Mij     =Mij            +cI*BSS_eh_W(H_pos(1)+H_shift(1))
            Mij_star=Mij_star       -cI*BSS_eh_W(H_pos(1)+H_shift(1))
          endif
        endif
        !
        select case(BS_blk(i_B)%mode)
        case("R")
          !
          error_diago = DMatSet_Fortran(diago_mat, H_pos(1), H_pos(2),       Mij  )
          if (error_diago /= 0) call error("Setting 2D block cyclic matrix failed")
          !
          ! The resonant block is hermitial
          error_diago = DMatSet_Fortran(diago_mat, H_pos(2), H_pos(1),   Mij_star )
          if (error_diago /= 0) call error("Setting 2D block cyclic matrix failed")
          !
          !
          if (l_BS_ares_from_res.and.BS_K_coupling) then
            !
            ! The anti-resonant block is A=-R*
            !
            error_diago = DMatSet_Fortran(diago_mat, H_pos(1)+SL_K_dim(1), H_pos(2)+SL_K_dim(1), -Mij_star)
            if (error_diago /= 0) call error("Setting 2D block cyclic matrix failed")
            !
            ! The anti-resonant block is hermitian
            ! 
            error_diago = DMatSet_Fortran(diago_mat, H_pos(2)+SL_K_dim(1), H_pos(1)+SL_K_dim(1), -Mij )
            if (error_diago /= 0) call error("Setting 2D block cyclic matrix failed")
            !
          endif
        case("C")
          !
          error_diago = DMatSet_Fortran(diago_mat, H_pos(1), H_pos(2)+SL_K_dim(1),  Mij  )
          if (error_diago /= 0) call error("Setting 2D block cyclic matrix failed")
          !
          ! Anti-coupling from coupling: the whole BSE matrix is Pseudo-HErmitian 
          ! 
          error_diago = DMatSet_Fortran(diago_mat, H_pos(2)+SL_K_dim(1), H_pos(1), -Mij_star  )
          if (error_diago /= 0) call error("Setting 2D block cyclic matrix failed")
          !
          
          if (l_BS_ares_from_res) then
            ! The coupling block and the anti-coupling block are symmetric
            !
            error_diago = DMatSet_Fortran(diago_mat, H_pos(2), H_pos(1)+SL_K_dim(1), Mij)
            if (error_diago /= 0) call error("Setting 2D block cyclic matrix failed")
            !
            error_diago = DMatSet_Fortran(diago_mat, H_pos(1)+SL_K_dim(1), H_pos(2), -Mij_star)
            if (error_diago /= 0) call error("Setting 2D block cyclic matrix failed")
            !
          endif
        case("A")
          ! The anti-resonant block is hermitial
          !          
          if(BS_res_ares_n_mat==1) then
            error_diago = DMatSet_Fortran(diago_mat, H_pos(1)+SL_K_dim(1), H_pos(2)+SL_K_dim(1), Mij  )
            if (error_diago /= 0) call error("Setting 2D block cyclic matrix failed")
            !
            error_diago = DMatSet_Fortran(diago_mat, H_pos(2)+SL_K_dim(1), H_pos(1)+SL_K_dim(1),  Mij_star  )
            if (error_diago /= 0) call error("Setting 2D block cyclic matrix failed")
            !
          else
            !
            error_diago = DMatSet_Fortran(diago_mat, H_pos(1), H_pos(2),       Mij  )
            if (error_diago /= 0) call error("Setting 2D block cyclic matrix failed")
            !
            error_diago = DMatSet_Fortran(diago_mat, H_pos(2), H_pos(1),  Mij_star  )
            if (error_diago /= 0) call error("Setting 2D block cyclic matrix failed")
            !
          endif
        case("Q")
          !
          error_diago = DMatSet_Fortran(diago_mat, H_pos(1)+SL_K_dim(1), H_pos(2), Mij  )
          if (error_diago /= 0) call error("Setting 2D block cyclic matrix failed")
          !
          ! Coupling from anti-coupling: the whole BSE matrix is Pseudo-HErmitian
          !
          error_diago = DMatSet_Fortran(diago_mat, H_pos(2), H_pos(1)+SL_K_dim(1), -Mij_star  )
          if (error_diago /= 0) call error("Setting 2D block cyclic matrix failed")
          !
        end select
        !
      enddo
      !
    enddo
    !
  enddo
  !
  ! Free the blocks immediately if not required.
  if ( (.not.(run_Haydock.or.run_inversion)) ) call BS_blks_free(i_BS_mat)
  !
  ! Finalize the setQueue
  error_diago = ProcessSetQueue(diago_mat)
  if (error_diago /= 0) call error("Failed to fill BSE 2D block cyclic matrix")
  ! 
end subroutine K_fill_block_cyclic 
