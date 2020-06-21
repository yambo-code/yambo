!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine PARALLEL_global_Real_Time(E,Xk,q,X_type)
 !
 use drivers,       ONLY:l_eval_collisions
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use IO_m,          ONLY:IO_and_Messaging_switch
 use openmp,        ONLY:n_threads_RT,OPENMP_set_threads
 use interfaces,    ONLY:PARALLEL_index,PARALLEL_assign_chains_and_COMMs,PARALLEL_live_message
 use collision_ext, ONLY:COH_collisions,HXC_collisions,GW_NEQ_collisions
 use matrix_operate,ONLY:UP_matrix_index
 use hamiltonian,   ONLY:B_mat_index
 use real_time,     ONLY:RT_bands,l_RT_uses_E_FineGd,RT_nk
 use parallel_m,    ONLY:ncpu,HEAD_QP_cpu,HEAD_k_cpu,HEAD_q_cpu,COMM_copy,PAR_INDEX_copy,&
&                        PAR_build_index,PP_indexes_reset
 ! COMMUNICATORS
 use parallel_m,    ONLY:PAR_COM_G_b_INDEX,PAR_COM_Q_INDEX,PAR_COM_WF_b_INDEX,PAR_COM_Xk_ibz_INDEX,    &
&                        PAR_COM_G_b_A2A,PAR_COM_Xk_ibz_A2A,PAR_COM_Plasma_INDEX,PAR_COM_Q_A2A
 ! IND
 use parallel_m,    ONLY:PAR_IND_Xk_ibz,PAR_IND_B_mat_ordered,PAR_IND_WF_b,PAR_IND_B_mat,PAR_IND_WF_k, &
&                        PAR_IND_Q,PAR_IND_G_k,PAR_IND_Bp_mat,PAR_IND_DIPk_ibz
 ! INDEX
 use parallel_m,    ONLY:PAR_DIPk_ibz_index,PAR_Xk_ibz_index,PAR_Q_index,PAR_COM_VAL_INDEX_X,PAR_COM_QP_INDEX,&
&                        PAR_COM_CON_INDEX_X
 ! DIMENSIONS
 use parallel_m,    ONLY:PAR_DIPk_nibz,PAR_nQ,PAR_Xk_nibz,PAR_G_k_range
 ! ID's
 use parallel_m,    ONLY:PAR_IND_Xk_ibz_ID,PAR_IND_Xk_ibz_ID,PAR_IND_Bp_mat_ID,PAR_IND_Xk_ibz_ID,      &
&                        PAR_IND_DIPk_ibz_ID,PAR_IND_Q_ID,PAR_IND_WF_b_ID
 !
#include<memory.h>
 !
 type(levels)         :: E
 type(bz_samp)        :: Xk,q
 integer              :: X_type
 !
 ! Work space
 !
 integer              :: ib1,ib2,ik,nk_G_IO,TOTAL_Nk
 logical              :: CONSECUTIVE
 !
 CALL PARALLEL_structure(4,(/"k ","b ","q ","qp"/))
 !
 call PARALLEL_assign_chains_and_COMMs(4,COMM_index_1=PAR_COM_Xk_ibz_INDEX,&
&                                        COMM_index_2=PAR_COM_G_b_INDEX,&
&                                        COMM_index_3=PAR_COM_Q_INDEX,&
&                                        COMM_index_4=PAR_COM_PLASMA_INDEX,&
&                                        COMM_A2A_1=PAR_COM_Xk_ibz_A2A,&
&                                        COMM_A2A_2=PAR_COM_G_b_A2A,&
&                                        COMM_A2A_3=PAR_COM_Q_A2A)
 !
 ! COMMs setup
 !
 ! The routine PARALLEL_assign_chains_and_COMMs cannot define COMMUNICATORS for internal
 ! A2A when there is no internal distribution
 !
 if (PAR_COM_G_b_INDEX%n_CPU==1) then
   call COMM_copy(PAR_COM_Xk_ibz_A2A,PAR_COM_G_b_A2A)
 endif
 if (PAR_COM_Q_INDEX%n_CPU==1) then
   call COMM_copy(PAR_COM_G_b_A2A,PAR_COM_Q_A2A)
 endif
 !
 call COMM_copy(PAR_COM_G_b_INDEX,PAR_COM_WF_b_INDEX)
 call COMM_copy(PAR_COM_QP_INDEX,PAR_COM_CON_INDEX_X(X_type))
 call COMM_copy(PAR_COM_G_b_INDEX,PAR_COM_VAL_INDEX_X(X_type))
 !
 ! K-points (IBZ)
 !
 CONSECUTIVE=.not.l_RT_uses_E_FineGd
#if defined _PAR_IO
 CONSECUTIVE=.TRUE.
 TOTAL_Nk   =Xk%nibz
#else
 CONSECUTIVE=.not.l_RT_uses_E_FineGd
 TOTAL_Nk   =RT_nk
#endif
 call PARALLEL_index(PAR_IND_Xk_ibz,(/Xk%nibz/),COMM=PAR_COM_Xk_ibz_INDEX,CONSECUTIVE=CONSECUTIVE,NO_EMPTIES=.TRUE.)
 PAR_IND_Xk_ibz_ID=PAR_COM_Xk_ibz_INDEX%CPU_id
 !
 call PARALLEL_live_message("K(ibz)",ENVIRONMENT="Real_Time",&
&         LOADED=PAR_IND_Xk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1),TOTAL=TOTAL_Nk,&
&         NCPU=PAR_COM_Xk_ibz_INDEX%n_CPU)
 !
 !.........................................................................
 ! Wave Functions (derived from PAR_COM_Xk_ibz_INDEX and PAR_COM_G_b_INDEX)
 !.........................................................................
 !
 call PAR_INDEX_copy(PAR_IND_Xk_ibz,PAR_IND_WF_k)
 PAR_IND_WF_b_ID=PAR_COM_WF_b_INDEX%CPU_id
 !
 call PARALLEL_index(PAR_IND_B_mat_ordered,(/ UP_matrix_index(1,RT_bands(2)-RT_bands(1)+1)-1 /),&
&                    COMM=PAR_COM_WF_b_INDEX,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
 !
 call PARALLEL_live_message("Bands Matrix (ordered)",ENVIRONMENT="Real_Time",&
&                           LOADED=PAR_IND_B_mat_ordered%n_of_elements(PAR_IND_WF_b_ID+1),&
&                           TOTAL=UP_matrix_index(1,RT_bands(2)-RT_bands(1)+1)-1,&
&                           NCPU=PAR_COM_WF_b_INDEX%n_CPU)
 !
 YAMBO_ALLOC(PAR_IND_WF_b%n_of_elements,(PAR_COM_WF_b_INDEX%n_CPU))
 YAMBO_ALLOC(PAR_IND_WF_b%element_1D,(RT_bands(2)))
 PAR_IND_WF_b%n_of_elements(PAR_IND_WF_b_ID+1)=RT_bands(1)-1
 PAR_IND_WF_b%element_1D(:)=.FALSE.
 if (RT_bands(1)>1) PAR_IND_WF_b%element_1D(1:RT_bands(1)-1)=.TRUE.
 !
 ! Non evolved bands loaded by all
 !
 PAR_IND_WF_b%element_1D(:max(RT_bands(1)-1,1))=.TRUE.
 !
 do ib1=RT_bands(1),RT_bands(2)
   do ib2=ib1,RT_bands(2)
     if (PAR_IND_B_mat_ordered%element_1D(  UP_matrix_index(ib1-RT_bands(1)+1,ib2-RT_bands(1)+1)-1 )) then
       if (.not.PAR_IND_WF_b%element_1D(ib1)) then
         PAR_IND_WF_b%element_1D(ib1)=.TRUE.
         PAR_IND_WF_b%n_of_elements(PAR_IND_WF_b_ID+1)=PAR_IND_WF_b%n_of_elements(PAR_IND_WF_b_ID+1)+1
       endif
       if (ib1/=ib2.and..not.PAR_IND_WF_b%element_1D(ib2)) then
         PAR_IND_WF_b%element_1D(ib2)=.TRUE.
         PAR_IND_WF_b%n_of_elements(PAR_IND_WF_b_ID+1)=PAR_IND_WF_b%n_of_elements(PAR_IND_WF_b_ID+1)+1
       endif
     endif
   enddo
 enddo
 !.........................................................................
 ! Oscillators for external field interaction
 !
 ! Note that PAR_IND_Xk_ibz = PAR_IND_DIPk_ibz while
 ! PAR_WF_k is like PAR_IND_Xk_ibz with WF loaded only for 1 cpu per k-block
 !
 !.........................................................................
 if (allocated(PAR_IND_Xk_ibz%n_of_elements)) then 
   !
   ! This allocation can be left over from previous calls to PARALLEL_global routine
   !
   call PP_indexes_reset(PAR_IND_DIPk_ibz)
   !
 endif
 call G_b_to_B_mat(PAR_COM_G_b_INDEX,'B',RT_bands)
 call PAR_INDEX_copy(PAR_IND_Xk_ibz,PAR_IND_DIPk_ibz)
 PAR_IND_DIPk_ibz_ID=PAR_IND_Xk_ibz_ID
 YAMBO_ALLOC(PAR_DIPk_ibz_index,(Xk%nibz))
 call PAR_build_index(PAR_IND_DIPk_ibz,Xk%nibz,PAR_DIPk_ibz_index,PAR_DIPk_nibz)
 YAMBO_ALLOC(PAR_Xk_ibz_index,(Xk%nibz))
 call PAR_build_index(PAR_IND_Xk_ibz,Xk%nibz,PAR_Xk_ibz_index,PAR_Xk_nibz)
 do ib1=RT_bands(1),RT_bands(2)
   do ib2=RT_bands(1),RT_bands(2)
     if (PAR_IND_B_mat%element_1D( B_mat_index(ib1,ib2,RT_bands) ) ) then
        PAR_IND_WF_b%element_1D(ib1)=.TRUE.
        PAR_IND_WF_b%element_1D(ib2)=.TRUE.
     endif
   enddo
 enddo
 !
 nk_G_IO=0
 PAR_G_k_range=0
 do ik = 1, Xk%nibz
   if (.not.PAR_IND_Xk_ibz%element_1D(ik) ) cycle
   if (nk_G_IO==0) PAR_G_k_range=ik
   if (nk_G_IO >0) then
#if defined _PAR_IO
     if (ik/=PAR_G_k_range(2)+1) call error(" Non contiguous G distr is not allowed")
#endif
     PAR_G_k_range(2)=ik
   endif
   nk_G_IO=nk_G_IO+1
 enddo
 !
 !.........................................................................
 ! WFs & QPs
 !......................................................................... 
 !
 ! QP "head"
 !
 HEAD_QP_cpu=PAR_COM_G_b_A2A%CPU_id==0
 !
 ! 0  0     0  0     <- qp (<- defined on the basis of k,b)
 ! x0 x0    x0 x0    <- q
 !
 ! QP_cpu corresponds to x marked CPU's. This flag is used when isolated QP loops are performed.
 !
 ! Q "head"
 !
 HEAD_q_cpu=PAR_COM_Q_A2A%CPU_id==0
 !
 ! oooo  oooo  oooo  oooo  <- b
 ! Xo Xo Xo Xo Xo Xo Xo Xo <- q
 !
 ! K "head"
 !
 HEAD_k_cpu=PAR_COM_Xk_ibz_A2A%CPU_id==0
 !
 ! oooo  oooo  oooo  oooo  <- k
 ! Xo Xo Xo Xo Xo Xo Xo Xo <- b
 !
 ! NOTE: HEAD_k/q_cpu=.TRUE. => HEAD_QP_cpu=.TRUE.
 !  
 ! but HEAD_QP_cpu=.TRUE. defines a larger set of CPU's.
 !
 ! Not all CPU's (o) load the WFs. Only X
 !
 if (.not.HEAD_QP_cpu) then
   PAR_IND_WF_b%element_1D=.FALSE.
   PAR_IND_WF_k%element_1D=.FALSE.
 endif
 !
 PAR_IND_WF_b%n_of_elements(PAR_IND_WF_b_ID+1)=count( PAR_IND_WF_b%element_1D )
 !
 call PARALLEL_live_message("Bands (WF)",ENVIRONMENT="Real_Time",&
&                           LOADED=PAR_IND_WF_b%n_of_elements(PAR_IND_WF_b_ID+1),&
&                           TOTAL=RT_bands(2),&
&                           NCPU=PAR_COM_WF_b_INDEX%n_CPU)
 !
 !.........................................................................
 !   "q" -> Q-points (BZ)
 !.........................................................................
 !
 call PARALLEL_index(PAR_IND_Q,(/q%nbz/),COMM=PAR_COM_Q_INDEX,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
 PAR_IND_Q_ID=PAR_COM_Q_INDEX%CPU_id
 PAR_nQ=PAR_IND_Q%n_of_elements(PAR_IND_Q_ID+1)
 !
 YAMBO_ALLOC(PAR_Q_index,(q%nbz))
 call PAR_build_index(PAR_IND_Q,q%nbz,PAR_Q_index,PAR_nQ)
 !
 call PARALLEL_live_message("Q(bz)",ENVIRONMENT="Real_Time",&
&                           LOADED=PAR_IND_Q%n_of_elements(PAR_IND_Q_ID+1),TOTAL=q%nbz,&
&                           NCPU=PAR_COM_Q_INDEX%n_CPU)
 !
 !.........................................................................
 !   "COLLISIONS" & "q" -> k-q
 !.........................................................................
 !
 ! The k-q distribution is present both the COLL evaulation and composition
 !
 ! In the eval case it is driven directly by IND_Q.
 ! IND_G_k is just used for the WF distribution
 !
 ! In the compose case, instead, IND_G_k is used instead of IND_Q.
 !.........................................................................
 !
 call PARALLEL_collisions( Xk,    COH_collisions )
 call PARALLEL_collisions( Xk,    HXC_collisions )
 call PARALLEL_collisions( Xk, GW_NEQ_collisions )
 !
 !.........................................................................
 !   "qp"  -> Bp_mat (m,m')
 !.........................................................................
 !
 call G_b_to_B_mat(PAR_COM_Plasma_INDEX,'Bp',RT_bands)
 !
 ! Messaging...
 !
 if (allocated( PAR_IND_G_k%element_1D)) then
   call PARALLEL_live_message("k-q",ENVIRONMENT="Real_Time",&
&                             LOADED=count( PAR_IND_G_k%element_1D ),&
&                             TOTAL=Xk%nibz)
 endif
 !
 call PARALLEL_live_message("Bands Matrix (prime)",ENVIRONMENT="Real_Time",&
&                           LOADED=PAR_IND_Bp_mat%n_of_elements(PAR_IND_Bp_mat_ID+1),&
&                           TOTAL=(RT_bands(2)-RT_bands(1)+1)**2,NCPU=PAR_COM_Plasma_INDEX%n_CPU)
 !
 ! When the collisons are note evaluated the I/O is dictated by Response_G_space
 ! and then resetted in RT_driver
 !
 if (l_eval_collisions) then
   call IO_and_Messaging_switch("+io_out",CONDITION=HEAD_QP_cpu)
 else
   call IO_and_Messaging_switch("+io_out",CONDITION=PAR_COM_Xk_ibz_INDEX%my_CHAIN==1.or.&
&                                                   PAR_COM_Xk_ibz_INDEX%n_CPU==ncpu)
 endif
 !
 call OPENMP_set_threads(n_threads_in=n_threads_RT)
 !
end subroutine Parallel_global_Real_Time
