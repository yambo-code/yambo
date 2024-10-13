!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): MB, AF
!
subroutine MPA_ALLOC_parallel(MPA_E_par,MPA_R_par,NG,NW,mode)
  !
  use pars,          ONLY:SP,DP,cZERO,cONE,cI
  use matrix,        ONLY:MATRIX_reset,PAR_matrix
  use parallel_int,  ONLY:PARALLEL_live_message
  use cuda_m,        ONLY:have_cuda
  !
#include <memory.h>
  !
  type(PAR_matrix) :: MPA_E_par,MPA_R_par
  integer          :: NG,NW
  character(*)     :: mode
  !
  ! Work Space
  !
  integer :: MPA_rows(2),MPA_cols(2)
  logical :: l_CPU_ONLY
  !
  ! Reset...
  !
  call MATRIX_reset(MPA_E_par)
  call MATRIX_reset(MPA_R_par)
  !
  ! Type of allocation....
  !
  !l_XUP      = index(mode,"XUP")>0
  l_CPU_ONLY = index(mode,"CPU_ONLY")>0
  !
  ! Initialization
  !
  call MATRIX_init( "MPA", MPA_E_par, NG, NW )  
  call MATRIX_init( "MPA", MPA_R_par, NG, NW )
  !endif
  !
  MPA_rows = MPA_E_par%rows
  MPA_cols = MPA_E_par%cols
  !
  YAMBO_ALLOC(MPA_E_par%blc,(MPA_rows(1):MPA_rows(2),MPA_cols(1):MPA_cols(2),NW))
  MPA_E_par%blc=cZERO
  !
  if (have_cuda.and..not.l_CPU_ONLY) then
    YAMBO_ALLOC(MPA_E_par%blc_d,(MPA_rows(1):MPA_rows(2),MPA_cols(1):MPA_cols(2),NW))
    MPA_E_par%blc_d=cZERO
  endif
  ! 
  YAMBO_ALLOC(MPA_R_par%blc,(MPA_rows(1):MPA_rows(2),MPA_cols(1):MPA_cols(2),NW))
  MPA_R_par%blc=cZERO
  !
  if (have_cuda.and..not.l_CPU_ONLY) then
    YAMBO_ALLOC(MPA_R_par%blc_d,(MPA_rows(1):MPA_rows(2),MPA_cols(1):MPA_cols(2),NW))
    MPA_R_par%blc_d=cZERO
  endif
  !
  call PARALLEL_live_message("RL vectors(X)",LOADED=MPA_E_par%ncols*MPA_E_par%nrows,&
&                            TOTAL=NG**2,NCPU=MPA_E_par%INTER_comm%n_CPU)
  !
 end subroutine

