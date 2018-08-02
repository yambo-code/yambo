!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine PARALLEL_global_Response_G(E,Xk,q,ENVIRONMENT,X_type)
 !
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp,nXkibz,nXkbz
 use linear_algebra,ONLY:INV
 use IO_m,          ONLY:IO_and_Messaging_switch
 use openmp,        ONLY:n_threads_X,OPENMP_set_threads
 use interfaces,    ONLY:PARALLEL_index,PARALLEL_assign_chains_and_COMMs,PARALLEL_live_message
 use parallel_m,    ONLY:ncpu,CPU_structure,PARALLEL_n_structures_active,COMM_copy,PAR_INDEX_copy,PAR_Q_range
 ! SLK
 use parallel_m,    ONLY:PAR_COM_RL_A2A,PAR_COM_RL_INDEX,PAR_COM_X_WORLD,PAR_COM_X_WORLD_RL_resolved,PAR_build_index
 !
 ! COMMUNICATORS
 use parallel_m,    ONLY:PAR_COM_Xk_ibz_INDEX,PAR_COM_Xk_bz_INDEX,PAR_COM_Q_INDEX,&
&                        PAR_COM_Q_A2A,PAR_COM_Xk_bz_A2A,PAR_COM_DIPk_bz_A2A,PAR_COM_Xk_bz_A2A, &
&                        PAR_COM_Xk_ibz_A2A,PAR_COM_CON_INDEX_X,PAR_COM_VAL_INDEX_X,PAR_COM_DIPk_bz_index
 ! IND
 use parallel_m,    ONLY:PAR_IND_DIPk_ibz,PAR_IND_DIPk_bz,PAR_IND_Xk_ibz,PAR_IND_Xk_bz,PAR_IND_Q, &
&                        PAR_IND_OVLP_ibz
 ! INDEX
 use parallel_m,    ONLY:PAR_Xk_bz_index,PAR_Xk_ibz_index,PAR_DIPk_ibz_index,PAR_Q_index,&
&                        PAR_DIPk_bz_index,PAR_COM_DIPk_ibz_INDEX
 ! DIMENSIONS
 use parallel_m,    ONLY:PAR_DIPk_nibz,PAR_nQ,PAR_Xk_nbz,PAR_Xk_nibz,PAR_DIPk_nbz
 ! ID's
 use parallel_m,    ONLY:PAR_IND_Xk_ibz_ID,PAR_IND_Xk_bz_ID,PAR_IND_Q_ID,PAR_IND_DIPk_bz_ID,PAR_IND_DIPk_ibz_ID
 !
#include<memory.h>
 !
 type(levels)         :: E
 type(bz_samp)        :: Xk,q
 character(*)         :: ENVIRONMENT
 integer              :: X_type
 !
 ! Work space
 !
 character(10)        :: WHAT,WHATp
 !
 !===========================================================================================
 if (ENVIRONMENT=="Response_G_space".or.ENVIRONMENT=="Response_G_space_Finite_Momentum") then
   !=========================================================================================
   !
   CALL PARALLEL_structure(5,(/"q","g","k","c","v"/))
   !
   call PARALLEL_assign_chains_and_COMMs(5,COMM_index_1=PAR_COM_Q_INDEX,&
&                                          COMM_index_2=PAR_COM_RL_INDEX,&
&                                          COMM_index_3=PAR_COM_Xk_bz_INDEX,&
&                                          COMM_index_4=PAR_COM_CON_INDEX_X(X_type),&
&                                          COMM_index_5=PAR_COM_VAL_INDEX_X(X_type),&
&                                          COMM_A2A_1=PAR_COM_Q_A2A,&
&                                          COMM_A2A_2=PAR_COM_RL_A2A,&
&                                          COMM_A2A_3=PAR_COM_Xk_bz_A2A)
   !
   ! COMMs setup
   !
   call COMM_copy(PAR_COM_Q_A2A,PAR_COM_X_WORLD)
   call COMM_copy(PAR_COM_Q_A2A,PAR_COM_X_WORLD_RL_resolved)
   !
   ! AM May 2017. The _RL_ COMM is not meant to distribute the work like all other COMMs.
   ! It is rather an additional level of parallelization.
   !
   if (PAR_COM_RL_INDEX%n_CPU>1) call COMM_copy(PAR_COM_RL_A2A,PAR_COM_X_WORLD_RL_resolved)
   !
   ! K-points 
   !
   call PARALLEL_index(PAR_IND_Xk_bz,(/nXkbz/),COMM=PAR_COM_Xk_bz_INDEX,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
   PAR_IND_Xk_bz_ID=PAR_COM_Xk_bz_INDEX%CPU_id
   PAR_Xk_nbz=PAR_IND_Xk_bz%n_of_elements(PAR_IND_Xk_bz_ID+1)
   !
   call PARALLEL_live_message("K(bz)",ENVIRONMENT=ENVIRONMENT,&
&                             LOADED=PAR_IND_Xk_bz%n_of_elements(PAR_COM_Xk_bz_INDEX%CPU_id+1),TOTAL=nXkbz,&
&                             NCPU=PAR_COM_Xk_bz_INDEX%n_CPU)
   YAMBO_ALLOC(PAR_Xk_bz_index,(nXkbz))
   call PAR_build_index(PAR_IND_Xk_bz,nXkbz,PAR_Xk_bz_index,PAR_Xk_nbz)
   !
   ! Q-points 
   !
   WHAT="ibz"
   !
   call PARALLEL_index(PAR_IND_Q,(/PAR_Q_range(2)/),low_range=(/PAR_Q_range(1)/),COMM=PAR_COM_Q_INDEX,&
&                      CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
   PAR_IND_Q_ID=PAR_COM_Q_INDEX%CPU_id
   PAR_nQ=PAR_IND_Q%n_of_elements(PAR_IND_Q_ID+1)
   !
   call PARALLEL_live_message("Q("//trim(WHAT)//")",ENVIRONMENT=ENVIRONMENT,&
&                             LOADED=PAR_IND_Q%n_of_elements(PAR_IND_Q_ID+1),&
&                             TOTAL=PAR_Q_range(2)-PAR_Q_range(1)+1,&
&                             NCPU=PAR_COM_Q_INDEX%n_CPU)
   !
   YAMBO_ALLOC(PAR_Q_index,(PAR_Q_range(2)))
   call PAR_build_index(PAR_IND_Q,PAR_Q_range(2),PAR_Q_index,PAR_nQ)
   !
   ! K-points (IBZ) after shifting of Q (BZ/IBZ)
   !
   WHATp="k_bz_q_"//trim(WHAT) 
   !
   call PARALLEL_add_Q_to_K_list(trim(WHATp),PAR_IND_Xk_bz,PAR_IND_Xk_bz_ID,PAR_IND_Xk_ibz,PAR_IND_Xk_ibz_ID,&
&                                PAR_IND_Q,PAR_COM_Xk_bz_INDEX,PAR_Q_range,Xk,q)
   PAR_Xk_nibz=PAR_IND_Xk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1)
   !
   ! ... indexes
   !
   YAMBO_ALLOC(PAR_Xk_ibz_index,(nXkibz))
   call PAR_build_index(PAR_IND_Xk_ibz,nXkibz,PAR_Xk_ibz_index,PAR_Xk_nibz)
   !
   call PARALLEL_live_message("K-q(ibz)",ENVIRONMENT=ENVIRONMENT,LOADED=PAR_Xk_nibz,TOTAL=nXkibz,&
&                             NCPU=PAR_COM_Xk_ibz_INDEX%n_CPU)
   !
   ! Inversion
   !
   call PARALLEL_assign_LA_COMMs(ENVIRONMENT,INV,CPU_structure(1)%nCPU_lin_algebra_INV)
   !
   call PARALLEL_live_message("K(ibz)",ENVIRONMENT=ENVIRONMENT,LOADED=PAR_Xk_nibz,TOTAL=Xk%nibz,&
&                             NCPU=PAR_COM_Xk_ibz_INDEX%n_CPU)
   !
   ! DIPOLES
   !
   call COMM_copy(PAR_COM_Q_A2A,PAR_COM_DIPk_bz_index)
   call COMM_copy(PAR_COM_Xk_bz_A2A,PAR_COM_DIPk_bz_A2A)
   !
   call PARALLEL_minimal_index_from_overlaping(PAR_IND_Xk_ibz,PAR_IND_DIPk_ibz,PAR_COM_Xk_bz_INDEX)
   PAR_IND_DIPk_ibz_ID=PAR_IND_Xk_ibz_ID
   !
   allocate(PAR_DIPk_ibz_index(Xk%nibz))
   call PAR_build_index(PAR_IND_DIPk_ibz,Xk%nibz,PAR_DIPk_ibz_index,PAR_DIPk_nibz)
   !
   if(.not.allocated(PAR_DIPk_bz_index)) then
     allocate(PAR_DIPk_bz_index(Xk%nbz))
     call distribute_BZk_using_IBZk(PAR_COM_DIPk_ibz_INDEX,Xk,PAR_IND_DIPk_ibz,PAR_IND_DIPk_ibz_ID,&
&                                                         PAR_IND_DIPk_bz, PAR_IND_DIPk_bz_ID,&
&                                                         PAR_DIPk_bz_index,PAR_DIPk_nbz)
   endif
   !
   ! I/O privileges
   !
   if (ncpu>1) then
     call IO_and_Messaging_switch("+output",CONDITION=PAR_COM_X_WORLD%CPU_id==0)
#if defined _PAR_IO
     call IO_and_Messaging_switch("+io_out",CONDITION=.TRUE.)
#else
     call IO_and_Messaging_switch("+io_out",CONDITION=PAR_COM_X_WORLD%CPU_id==0)
#endif
   else
     call IO_and_Messaging_switch("+io_out",CONDITION=.TRUE.)
   endif
   !
   call OPENMP_set_threads(n_threads_in=n_threads_X)
   !
   if (ENVIRONMENT=="Response_G_space") call PARALLEL_global_Response_IO( )
   !
 endif
 !
end subroutine PARALLEL_global_Response_G
