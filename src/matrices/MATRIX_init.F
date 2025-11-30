!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
! headers
!
#include <dev_defs.h>
#include <y_memory.h>
!
subroutine MATRIX_init( what, M_par, N, Nb)
 !
 ! I initialize a general parallel matrix here. Note the meaning of the two COMMS: INTRA and INTER.
 !
 ! INTER_comm is the one the connects CPU's with pieces of the matrix.
 ! INTRA_comm is the one the connects CPU's with the SAME piece of the matrix.
 !
 ! Thus...
 !
 ! INTER_comm is used to transfer pieces to pieces
 ! INTRA_comm is used to do ALL2ALL at the end of a single piece filling
 !
 use pars,           ONLY:SP
 use matrix,         ONLY:PAR_matrix,MATRIX_reset
 use parallel_m,     ONLY:COMM_copy,PAR_COM_SLK,PAR_COM_SLK_INDEX_local,&
&                         PAR_COM_RL_INDEX,PAR_COM_RL_A2A,ncpu
 use y_memory_alloc
 !
 implicit none
 !
 character(*)           :: what
 type(PAR_matrix)       :: M_par
 integer                :: N,Nb
 !
 ! Work Space
 !
 integer              :: M,i_c,N_rows
 !
 !DEV_ACC enter data copyin(M_par)
 !
 ! COMMs
 !
 if (what=="SLK") then
   call COMM_copy(PAR_COM_SLK_INDEX_local, M_par%INTRA_comm)
   call COMM_copy(PAR_COM_SLK,             M_par%INTER_comm)
 else if (what=="X".or.what=="MPA") then
   call COMM_copy(PAR_COM_RL_A2A,   M_par%INTRA_comm)
   call COMM_copy(PAR_COM_RL_INDEX, M_par%INTER_comm)
 endif
 !
 ! ... SERIAL fallback
 !
 if (M_par%INTER_comm%n_CPU==1.or.ncpu==1.or.what=="SER") then
   M_par%rows  = (/1,N/)
   M_par%cols  = (/1,N/)
   M_par%Nb    = Nb
   M_par%I     = 1
   M_par%Nb    = Nb
   M_par%N     = N
   M_par%kind="SER"
   call rows_and_cols_setup()
   return
 endif
 !
#if defined _SCALAPACK
 ! ... SLK
 !
 if (what=="SLK") then
   call SLK_matrix_init(M_par,N, Nb)
 endif
#endif
 !
 ! Response function. Entire Matrix
 !
 if (what=="X".or.what=="PAR".or.what=="MPA") then
   !
   M_par%I     = 1
   M_par%Nb    = Nb
   M_par%N     = N
   M_par%kind  ="PAR"
   M           = N/M_par%INTER_comm%n_CPU
   M_par%cols  = (/1,N/)
   M_par%rows  = (/M*M_par%INTER_comm%CPU_id+1,M*(M_par%INTER_comm%CPU_id+1)/)
   if (M_par%INTER_comm%CPU_id+1==M_par%INTER_comm%n_CPU) M_par%rows(2) = N
   call rows_and_cols_setup()
   return
   !
 endif
 !
 contains
   !
   subroutine rows_and_cols_setup()
     M_par%nrows    = M_par%rows(2)-M_par%rows(1)+1 
     M_par%ncols    = M_par%cols(2)-M_par%cols(1)+1 
     M_par%BLCnrows = M_par%nrows 
     M_par%BLCncols = M_par%ncols
     M_par%BLCrows  = M_par%rows
     M_par%BLCcols  = M_par%cols
   end subroutine
   !
end subroutine
