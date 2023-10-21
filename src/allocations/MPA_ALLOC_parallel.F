!
! License-Identifier: GPL
!
! Copyright (C) 2023 The Yambo Team
!
! Authors (see AUTHORS file for details): MB
!
subroutine MPA_ALLOC_parallel(MPA_par,NG,NW,mode)
 !
 use pars,          ONLY:cZERO
 use parallel_m,    ONLY:COMM_copy,PAR_COM_RL_INDEX,PAR_COM_RL_A2A
 use matrix,        ONLY:MATRIX_reset,PAR_matrix
 use X_m,           ONLY:X_par_lower_triangle,X_rows,X_cols,&
&                        X_FILL_UP_matrix_only,X_lower_triangle_matrix_in_use
 use parallel_int,  ONLY:PARALLEL_live_message
 use cuda_m,        ONLY:have_cuda
 !
#include<memory.h>
 !
 type(PAR_matrix) :: MPA_E_par, MPA_R_par
 integer          :: NG,NW
 character(*)     :: mode
 !
 ! Work Space
 !
 !logical :: l_XUP,l_PAR,l_CPU_ONLY
 !
 ! Reset...
 !
 call MATRIX_reset(MPA_E_par)
 call MATRIX_reset(MPA_R_par)
 !
 ! Type of allocation....
 !
 !l_XUP      = index(mode,"XUP")>0
 !l_PAR      = index(mode,"X")>0.and.index(mode,"XUP")==0
 !l_CPU_ONLY = index(mode,"CPU_ONLY")>0
 !
 ! Initialization
 !
 !if (l_XUP) then
 !  call MATRIX_init( "XUP", X_par , NG, NW )
 !else if (l_PAR) then
 call MATRIX_init( "MPA_E"  , MPA_E_par , NG, NW )  
 call MATRIX_init( "MPA_R"  , MPA_R_par , NG, NW )
 !endif
 !
 MPA_rows = MPA_E_par%rows
 MPA_cols = MPA_E_par%cols
 !
!  if (l_XUP.and..not.X_FILL_UP_matrix_only.and.X_par%INTER_comm%n_CPU>1) then
!    !
!    call MATRIX_reset(X_par_lower_triangle)
!    X_lower_triangle_matrix_in_use=.TRUE.
!    X_par_lower_triangle%kind ="XDN"
!    X_par_lower_triangle%rows =X_par%cols
!    X_par_lower_triangle%cols =X_par%rows
!    X_par_lower_triangle%nrows=X_par%ncols
!    X_par_lower_triangle%ncols=X_par%nrows
!    !
!    YAMBO_ALLOC(X_par_lower_triangle%blc,(X_par%cols(1):X_par%cols(2),X_par%rows(1):X_par%rows(2),NW))
!    X_par_lower_triangle%blc=cZERO
!    if (have_cuda.and..not.l_CPU_ONLY) then
!      YAMBO_ALLOC(X_par_lower_triangle%blc_d,(X_par%cols(1):X_par%cols(2),X_par%rows(1):X_par%rows(2),NW))
!      X_par_lower_triangle%blc_d=cZERO
!    endif
!    !
!    call COMM_copy(PAR_COM_RL_A2A,   X_par_lower_triangle%INTRA_comm)
!    call COMM_copy(PAR_COM_RL_INDEX, X_par_lower_triangle%INTER_comm)
!  endif
 !
 YAMBO_ALLOC(MPA_E_par%blc,(MPA_E_par%rows(1):MPA_E_par%rows(2),MPA_E_par%cols(1):MPA_E_par%cols(2),NW))
 MPA_E_par%blc=cZERO
 if (have_cuda.and..not.l_CPU_ONLY) then
   YAMBO_ALLOC(MPA_E_par%blc_d,(MPA_E_par%rows(1):MPA_E_par%rows(2),MPA_E_par%cols(1):MPA_E_par%cols(2),NW))
   MPA_E_par%blc_d=cZERO
 endif

 YAMBO_ALLOC(MPA_R_par%blc,(MPA_R_par%rows(1):MPA_R_par%rows(2),MPA_R_par%cols(1):MPA_R_par%cols(2),NW))
 MPA_R_par%blc=cZERO
 if (have_cuda.and..not.l_CPU_ONLY) then
   YAMBO_ALLOC(MPA_R_par%blc_d,(MPA_R_par%rows(1):MPA_R_par%rows(2),MPA_R_par%cols(1):MPA_R_par%cols(2),NW))
   MPA_R_par%blc_d=cZERO
 endif
 !
 call PARALLEL_live_message("RL vectors(X)",LOADED=MPA_E_par%ncols*MPA_E_par%nrows,TOTAL=NG**2,NCPU=MPA_E_par%INTER_comm%n_CPU)
 !
end subroutine
