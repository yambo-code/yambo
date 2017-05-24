!
!        Copyright (C) 2000-2017 the YAMBO team
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
 use com,           ONLY:warning
 use IO_m,          ONLY:io_RESPONSE,IO_and_Messaging_switch
 use openmp,        ONLY:n_threads_X,OPENMP_set_threads
 use interfaces,    ONLY:PARALLEL_index,PARALLEL_assign_chains_and_COMMs,PARALLEL_live_message
 use parallel_m,    ONLY:ncpu,CPU_structure,PARALLEL_n_structures_active,COMM_copy,PAR_INDEX_copy,PAR_Q_range
 ! SLK
 use parallel_m,    ONLY:PAR_COM_RL_A2A,PAR_COM_RL_INDEX,PAR_COM_X_WORLD,PAR_COM_X_WORLD_RL_resolved,PAR_build_index
 !
 ! COMMUNICATORS
 use parallel_m,    ONLY:PAR_COM_Xk_ibz_INDEX,PAR_COM_Xk_bz_INDEX,PAR_COM_Q_INDEX,&
&                        PAR_COM_Q_A2A,PAR_COM_DIPOLES,PAR_COM_DIPOLES_k_subgroup,PAR_COM_Xk_bz_A2A, &
&                        PAR_COM_Xk_ibz_A2A,PAR_COM_CON_INDEX,PAR_COM_VAL_INDEX
 ! IND
 use parallel_m,    ONLY:PAR_IND_DIPk_ibz,PAR_IND_Xk_ibz,PAR_IND_Xk_bz,PAR_IND_Q
 ! INDEX
 use parallel_m,    ONLY:PAR_Xk_bz_index,PAR_Xk_ibz_index,PAR_DIPk_ibz_index,PAR_Q_index
 ! DIMENSIONS
 use parallel_m,    ONLY:PAR_DIPk_nibz,PAR_nQ,PAR_Xk_nbz,PAR_Xk_nibz
 ! ID's
 use parallel_m,    ONLY:PAR_IND_Xk_ibz_ID,PAR_IND_Xk_bz_ID,PAR_IND_DIPk_ID,PAR_IND_Q_ID
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
 !======================================================
 if (ENVIRONMENT=="Response_G_space_Zero_Momentum") then
   !====================================================
   !
   CALL PARALLEL_structure(4,(/"g","k","c","v"/))
   !
   call PARALLEL_assign_chains_and_COMMs(4,COMM_index_1=PAR_COM_RL_INDEX,&
&                                          COMM_index_2=PAR_COM_Xk_ibz_INDEX,&
&                                          COMM_index_3=PAR_COM_CON_INDEX(X_type),&
&                                          COMM_index_4=PAR_COM_VAL_INDEX(X_type),&
&                                          COMM_A2A_1=PAR_COM_RL_A2A,&
&                                          COMM_A2A_2=PAR_COM_Xk_ibz_A2A)
   !
   ! COMMs setup
   !
   call COMM_copy(PAR_COM_Xk_ibz_A2A,PAR_COM_DIPOLES_k_subgroup)
   if (PAR_COM_RL_INDEX%n_CPU> 1) then
     call COMM_copy(PAR_COM_RL_A2A,PAR_COM_X_WORLD_RL_resolved)
   endif
   !
   ! K-points (IBZ)
   !
   call PARALLEL_index(PAR_IND_Xk_ibz,(/nXkibz/),COMM=PAR_COM_Xk_ibz_INDEX,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
   PAR_IND_Xk_ibz_ID=PAR_COM_Xk_ibz_INDEX%CPU_id
   !
   ! ... indexes
   !
   YAMBO_ALLOC(PAR_Xk_bz_index,(nXkbz))
   call distribute_BZk_using_IBZk(PAR_COM_Xk_ibz_INDEX,Xk,PAR_IND_Xk_ibz,PAR_IND_Xk_ibz_ID,&
&                                                         PAR_IND_Xk_bz, PAR_IND_Xk_bz_ID,&
&                                                         PAR_Xk_bz_index,PAR_Xk_nbz)
   YAMBO_ALLOC(PAR_Xk_ibz_index,(nXkibz))
   call PAR_build_index(PAR_IND_Xk_ibz,nXkibz,PAR_Xk_ibz_index,PAR_Xk_nibz)
   !
   ! Inversion
   !
   call PARALLEL_assign_LA_COMMs(ENVIRONMENT,INV,CPU_structure(1)%nCPU_lin_algebra_INV)
   !
   call PARALLEL_live_message("K(ibz)",ENVIRONMENT=ENVIRONMENT,LOADED=PAR_Xk_nibz,TOTAL=nXkibz,&
&                             NCPU=PAR_COM_Xk_ibz_INDEX%n_CPU)
   !
   call PAR_INDEX_copy(PAR_IND_Xk_ibz,PAR_IND_DIPk_ibz)
   PAR_IND_DIPk_ID=PAR_IND_Xk_ibz_ID
   YAMBO_ALLOC(PAR_DIPk_ibz_index,(nXkibz))
   call PAR_build_index(PAR_IND_DIPk_ibz,nXkibz,PAR_DIPk_ibz_index,PAR_DIPk_nibz)
   !
   ! I/O privileges
   !
   if (PARALLEL_n_structures_active>1) then
     call IO_and_Messaging_switch("+io_out",CONDITION=PAR_COM_Xk_ibz_INDEX%my_CHAIN==1.or.&
&                                                     PAR_COM_Xk_ibz_INDEX%n_CPU==ncpu)
   else
     call IO_and_Messaging_switch("+io_out",CONDITION=.TRUE.)
   endif
   !
   call OPENMP_set_threads(n_threads_in=n_threads_X)
   !
 endif
 !
 !===========================================================================================
 if (ENVIRONMENT=="Response_G_space_Finite_Momentum".or.ENVIRONMENT=="Response_G_space") then
   !=========================================================================================
   !
   CALL PARALLEL_structure(5,(/"q","g","k","c","v"/))
   !
   call PARALLEL_assign_chains_and_COMMs(5,COMM_index_1=PAR_COM_Q_INDEX,&
&                                          COMM_index_2=PAR_COM_RL_INDEX,&
&                                          COMM_index_3=PAR_COM_Xk_bz_INDEX,&
&                                          COMM_index_4=PAR_COM_CON_INDEX(X_type),&
&                                          COMM_index_5=PAR_COM_VAL_INDEX(X_type),&
&                                          COMM_A2A_1=PAR_COM_Q_A2A,&
&                                          COMM_A2A_2=PAR_COM_RL_A2A,&
&                                          COMM_A2A_3=PAR_COM_Xk_bz_A2A)
   !
   ! COMMs setup
   !
   ! To define proper indexes to calculate the dipoles I need to build up
   ! the PAR_IND_DIPk_ibz by avoiding the overlaps of PAR_IND_Xk_ibz.
   ! The COMM is anyway the one for the all2all of each q.
   !
   if (PAR_COM_RL_INDEX%n_CPU==1) call COMM_copy(PAR_COM_Q_A2A, PAR_COM_X_WORLD_RL_resolved)
   if (PAR_COM_RL_INDEX%n_CPU> 1) call COMM_copy(PAR_COM_RL_A2A,PAR_COM_X_WORLD_RL_resolved)
   call COMM_copy(PAR_COM_Q_A2A,PAR_COM_DIPOLES)
   call COMM_copy(PAR_COM_Q_A2A,PAR_COM_X_WORLD)
   call COMM_copy(PAR_COM_Xk_bz_A2A,PAR_COM_DIPOLES_k_subgroup)
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
   if (ENVIRONMENT=="Response_G_space_Finite_Momentum") then
     !
     call PARALLEL_assign_LA_COMMs(ENVIRONMENT,INV,CPU_structure(2)%nCPU_lin_algebra_INV)
     !
   else if (ENVIRONMENT=="Response_G_space") then
     !
     call PARALLEL_assign_LA_COMMs(ENVIRONMENT,INV,CPU_structure(3)%nCPU_lin_algebra_INV)
     !
   endif
   !
   ! I/O privileges
   !
   if (PARALLEL_n_structures_active>1) then
     call IO_and_Messaging_switch("+io_out +output",CONDITION=PAR_COM_Q_A2A%CPU_id==0)
   else
     call IO_and_Messaging_switch("+io_out",CONDITION=.TRUE.)
   endif
   !
   if (ENVIRONMENT/="Response_G_space_Finite_Momentum") then
     !
     call PARALLEL_minimal_index_from_overlaping(PAR_IND_Xk_ibz,PAR_IND_DIPk_ibz,PAR_COM_Xk_bz_INDEX)
     PAR_IND_DIPk_ID=PAR_IND_Xk_ibz_ID
     !
     YAMBO_ALLOC(PAR_DIPk_ibz_index,(nXkibz))
     call PAR_build_index(PAR_IND_DIPk_ibz,nXkibz,PAR_DIPk_ibz_index,PAR_DIPk_nibz)
     !
   endif
   !
   call OPENMP_set_threads(n_threads_in=n_threads_X)
   !
   if (ENVIRONMENT=="Response_G_space") then
     !
     ! io_RESPONSE check
     !
     ! No X I/O is possible ONLY when the number of cpu's allocated with the "q" field
     ! in the response function string is the same of the SE/RT/SC...string
     !  
     if (.not.io_RESPONSE) then
       !
       call PARALLEL_get_user_structure("Self_Energy",.FALSE.)
       !
       if (CPU_structure(3)%CPU(1)/=CPU_structure(5)%CPU(1)) io_RESPONSE=.TRUE.
       !
       if (io_RESPONSE) call warning('Response function I/O forced. Different CPU for the "q" field in X and SE')
       !
     endif
   endif
   !
 endif
 !
end subroutine PARALLEL_global_Response_G
