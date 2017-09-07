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
subroutine PARALLEL_global_Self_Energy(E,Xk,q)
 !
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp,nqbz,nXkibz
 use drivers,       ONLY:l_eval_collisions
 use interfaces,    ONLY:PARALLEL_index,PARALLEL_assign_chains_and_COMMs,PARALLEL_live_message
 use IO_m,          ONLY:io_COLLs,IO_and_Messaging_switch
 use QP_m,          ONLY:QP_n_states
 use openmp,        ONLY:n_threads_SE,OPENMP_set_threads
 use QP_m,          ONLY:QP_n_states
 use parallel_m,    ONLY:HEAD_QP_cpu,master_cpu,n_WF_bands_to_load,&
&                        COMM_copy,PAR_Q_range,PAR_n_bands,PAR_build_index,ncpu
 ! COMMUNICATORS
 use parallel_m,    ONLY:PAR_COM_QP_INDEX,PAR_COM_Q_INDEX,PAR_COM_QP_A2A,PAR_COM_G_b_INDEX, &
&                        PAR_COM_Q_A2A
 ! IND
 use parallel_m,    ONLY:PAR_IND_QP,PAR_IND_Q,PAR_IND_G_b,PAR_IND_Q_bz,PAR_IND_Xk_ibz
 ! INDEX
 use parallel_m,    ONLY:PAR_QP_index,PAR_Q_index,PAR_Q_bz_index,PAR_G_bands_index
 ! DIMENSIONS
 use parallel_m,    ONLY:PAR_nG_bands,PAR_nQ,PAR_nQ_bz,PAR_nQP
 ! ID's
 use parallel_m,    ONLY:PAR_IND_Xk_ibz_ID,PAR_IND_G_b_ID,PAR_IND_Q_ID,PAR_IND_QP_ID
#if defined _SC || defined _RT || defined _QED
 use QP_m,          ONLY:QP_n_G_bands
 use drivers,       ONLY:l_elphoton_corr,l_life,l_real_time,l_sc_run
 use collision_ext, ONLY:COLL_bands,COH_collisions,HXC_collisions,P_collisions
 use parallel_m,    ONLY:PAR_n_c_bands,PAR_n_v_bands,COMM_copy
 ! COMMUNICATORS
 use parallel_m,    ONLY:PAR_COM_QP_INDEX,PAR_COM_G_b_INDEX
 ! IND
 use parallel_m,    ONLY:PAR_IND_DIPk_ibz,PAR_IND_Bp_mat,PAR_IND_QP,PAR_IND_Q
 ! INDEX
 use parallel_m,    ONLY:PAR_DIPk_ibz_index
 ! DIMENSIONS
 use parallel_m,    ONLY:PAR_DIPk_nibz
 ! ID's
 use parallel_m,    ONLY:PAR_IND_Bp_mat_ID,PAR_IND_DIPk_ID
#endif
#if defined _ELPH
 use drivers,       ONLY:l_elph_corr
 use ELPH,          ONLY:elph_use_q_grid
#endif
 !
#include<memory.h>
 !
 type(levels)         :: E
 type(bz_samp)        :: Xk,q
 !
 ! Work space
 !
 character(10)        :: WHAT
#if defined _SC || defined _RT
 integer              :: i_k,nb_mat
#endif
 !
 CALL PARALLEL_structure(3,(/"q ","qp","b "/))
 !
 call PARALLEL_assign_chains_and_COMMs(3,COMM_index_1=PAR_COM_Q_INDEX,&
&                                        COMM_index_2=PAR_COM_QP_INDEX,&
&                                        COMM_index_3=PAR_COM_G_b_INDEX,&
&                                        COMM_A2A_1=PAR_COM_Q_A2A,&
&                                        COMM_A2A_2=PAR_COM_QP_A2A)
 !
 ! The routine PARALLEL_assign_chains_and_COMMs cannot define COMMUNICATORS for internal
 ! A2A when there is no internal distribution
 !
 if (PAR_COM_QP_INDEX%n_CPU==1) then
   call COMM_copy(PAR_COM_Q_A2A,PAR_COM_QP_A2A)
 endif
 !
 ! QP states
 !
 call PARALLEL_index(PAR_IND_QP,(/QP_n_states/),COMM=PAR_COM_QP_INDEX,NO_EMPTIES=.TRUE.)
 PAR_IND_QP_ID=PAR_COM_QP_INDEX%CPU_id
 PAR_nQP=PAR_IND_QP%n_of_elements(PAR_IND_QP_ID+1)
 YAMBO_ALLOC(PAR_QP_index,(QP_n_states))
 PAR_QP_index=0
 call PAR_build_index(PAR_IND_QP,QP_n_states,PAR_QP_index,PAR_nQP)
 call PARALLEL_live_message("QPs",ENVIRONMENT="Self_Energy",&
&                           LOADED=PAR_IND_QP%n_of_elements(PAR_IND_QP_ID+1),TOTAL=QP_n_states,&
&                           NCPU=PAR_COM_QP_INDEX%n_CPU)
 !
 ! Q-points
 !
 WHAT="ibz"
 if (l_eval_collisions)                       WHAT="bz"
#if defined _ELPH
 if (l_elph_corr.and.elph_use_q_grid)         WHAT="bz"
 if (l_elph_corr.and..not.elph_use_q_grid)    WHAT="RIM"
#endif
 !
 call PARALLEL_index(PAR_IND_Q,(/PAR_Q_range(2)/),COMM=PAR_COM_Q_INDEX,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
 PAR_IND_Q_ID=PAR_COM_Q_INDEX%CPU_id
 PAR_nQ=PAR_IND_Q%n_of_elements(PAR_IND_Q_ID+1)
 call PARALLEL_live_message("Q("//trim(WHAT)//")",ENVIRONMENT="Self_Energy",&
&                           LOADED=PAR_IND_Q%n_of_elements(PAR_IND_Q_ID+1),TOTAL=PAR_Q_range(2),&
&                           NCPU=PAR_COM_Q_INDEX%n_CPU)
 !
 YAMBO_ALLOC(PAR_Q_index,(PAR_Q_range(2)))
 call PAR_build_index(PAR_IND_Q,PAR_Q_range(2),PAR_Q_index,PAR_nQ)
 !
 if (trim(WHAT)=="ibz") then
   YAMBO_ALLOC(PAR_Q_bz_index,(nqbz))
   call distribute_BZk_using_IBZk(PAR_COM_Q_INDEX,q,PAR_IND_Q   , PAR_IND_Q_ID,&
&                                                   PAR_IND_Q_bz, PAR_IND_Q_ID,&
&                                                   PAR_Q_bz_index,PAR_nQ_bz)
   call PAR_build_index(PAR_IND_Q_bz,nqbz,PAR_Q_bz_index,PAR_nQ_bz)
 endif
 !
#if defined _SC || defined _RT
 !
 if (l_sc_run.or.l_real_time.or.l_eval_collisions) then
   !
   ! 00000             <- q
   ! x0 x0    00 00    <- qp
   ! 0 0 0 0  0 0 0 0  <- b
   !
   ! QP_cpu corresponds to x marked CPU's. This flag is used when isolated QP loops are performed.
   !
   HEAD_QP_cpu=PAR_COM_Q_INDEX%CPU_id==0.and.PAR_COM_QP_A2A%CPU_id==0
   if (PAR_COM_Q_INDEX%n_CPU==1.and.PAR_COM_QP_A2A%n_CPU==1) then
     HEAD_QP_cpu=PAR_COM_G_b_index%CPU_id==0
   endif
   !
 else
   !   
#endif
   !
   ! 00000000          <- q
   ! 0000     0000     <- g
   ! x0 x0    x0 x0    <- qp
   ! 0 0 0 0  0 0 0 0  <- b
   !
   ! QP_cpu corresponds to x marked CPU's. This flag is used when no b loops are done
   !
   HEAD_QP_cpu=PAR_COM_QP_A2A%CPU_id==0
   !
#if defined _SC || defined _RT
 endif
#endif
 !
 ! K-points
 !
 call PARALLEL_add_Q_to_K_list('k_qp_q_'//trim(WHAT),PAR_IND_QP,PAR_IND_QP_ID,PAR_IND_Xk_ibz,PAR_IND_Xk_ibz_ID,&
&                              PAR_IND_Q,PAR_COM_QP_INDEX,(/0,0/),Xk,q)
 !
 ! G bands
 !=========
 !
 ! WF bands to load
 !
 n_WF_bands_to_load=PAR_n_bands(2)
 !
 call PARALLEL_index(PAR_IND_G_b,(/PAR_n_bands(2)/),low_range=(/PAR_n_bands(1)/),&
&                    COMM=PAR_COM_G_b_INDEX,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
 PAR_IND_G_b_ID=PAR_COM_G_b_INDEX%CPU_id
 PAR_nG_bands=PAR_IND_G_b%n_of_elements(PAR_IND_G_b_ID+1)
 YAMBO_ALLOC(PAR_G_bands_index,(PAR_n_bands(2)))
 call PAR_build_index(PAR_IND_G_b,PAR_n_bands(2),PAR_G_bands_index,PAR_nG_bands)
 !
 call PARALLEL_live_message("G bands",ENVIRONMENT="Self_Energy",&
&                           LOADED=PAR_IND_G_b%n_of_elements(PAR_IND_G_b_ID+1),&
&                           TOTAL=PAR_n_bands(2)-PAR_n_bands(1)+1,NCPU=PAR_COM_G_b_INDEX%n_CPU)
 !
#if defined _SC || defined _RT || defined _QED
 !
 ! Lamb is a special case. Collisions can be used also in simple Self-Energy mode.
 !
 if (l_elphoton_corr.or.l_eval_collisions) then
   call PARALLEL_collisions( Xk,   P_collisions )
 endif
 !
 ! To eval/use the COLLISIONS  in RT (and SC?) I need the PAR_Bp_mat_index, built here.
 !
 if (l_sc_run.or.l_real_time.or.l_eval_collisions) then
   !
   !.........................................................................
   !   "COLLISIONS"
   !.........................................................................
   !
   call PARALLEL_collisions( Xk, COH_collisions )
   call PARALLEL_collisions( Xk, HXC_collisions )
   !
   nb_mat=(COLL_bands(2)-COLL_bands(1)+1)**2
   call G_b_to_B_mat(PAR_COM_G_b_INDEX,'Bp',COLL_bands)
   call PARALLEL_live_message("Bands Matrix (prime)",ENVIRONMENT="Self_Energy",&
&                           LOADED=PAR_IND_Bp_mat%n_of_elements(PAR_IND_Bp_mat_ID+1),&
&                           TOTAL=nb_mat,NCPU=PAR_COM_G_b_INDEX%n_CPU)
   !
 endif
 !
 if (l_elphoton_corr.and.l_life) then
   !
   ! Oscillators needed to evaluate the Radiative Lifetimes
   ! 
   PAR_n_c_bands=QP_n_G_bands
   PAR_n_v_bands=QP_n_G_bands
   ! 
   ! As there will be a mismatch between the k-distribution here and in the self-energy (qp-loop)
   ! I cannot distribute the k-pts. Therefore the PAR_COM_DIPOLES_k_subgroup is the WORLD
   !
   call PARALLEL_index(PAR_IND_DIPk_ibz,(/nXkibz/),COMM=PAR_COM_Q_INDEX,NO_EMPTIES=.TRUE.)
   PAR_IND_DIPk_ID=PAR_IND_Q_ID
   YAMBO_ALLOC(PAR_DIPk_ibz_index,(nXkibz))
   do i_k=1,nXkibz
     PAR_DIPk_ibz_index(i_k)=i_k
   enddo
   PAR_DIPk_nibz=nXkibz
   !
 endif
 !
#endif
 !
 ! I/O privileges
 !
 if (ncpu>1) then
   if (l_eval_collisions.and.io_COLLs) then
     call IO_and_Messaging_switch("+io_out",CONDITION=PAR_COM_QP_A2A%CPU_id==0.and.PAR_COM_Q_INDEX%CPU_id==0)
   else 
     call IO_and_Messaging_switch("+io_out",CONDITION=master_cpu)
   endif
 else
   call IO_and_Messaging_switch("+io_out",CONDITION=.TRUE.)
 endif
 !
 call IO_and_Messaging_switch("+output",CONDITION=master_cpu)
 !
 call OPENMP_set_threads(n_threads_in=n_threads_SE)
 !
end subroutine PARALLEL_global_Self_Energy
