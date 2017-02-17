!
!        Copyright (C) 2000-2015 the YAMBO team
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
subroutine PARALLEL_global_indexes(E,Xk,q,ENVIRONMENT,X,RESET)
 !
 use pars,          ONLY:IP
 use parallel_m,    ONLY:PP_indexes,PP_indexes_reset,PAR_Xk_ibz_index,PAR_Xk_nibz,PAR_Xk_nbz,&
&                        PAR_Xk_bz_index,PARALLEL_n_structures_active,ncpu,mpi_comm_world,&
&                        PARALLEL_cpu_mat_inversion,CPU_structure,master_cpu,&
&                        PARALLEL_cpu_mat_diagonalization,PAR_Q_index,PAR_nQ,PAR_n_B_mat_elements,&
&                        PAR_B_mat_index,PAR_nQP,PAR_QP_index,PAR_INDEX_copy,&
&                        MPI_comm,PAR_DIPk_ibz_index,PAR_DIPk_nibz,COMM_copy,n_WF_bands_to_load,&
&                        PAR_n_Bp_mat_elements,PAR_Bp_mat_index,PAR_Plasma_index,&
&                        PAR_BS_T_grps_index,PAR_BS_nT_col_grps,PAR_Kk_nibz,PAR_nG_bands,PAR_G_bands_index,&
&                        PAR_Q_bz_index,PAR_nQ_bz,PAR_nRL,PAR_RL_index,&
&                        Q_range,QP_range,n_v_bands,n_c_bands,K_range,n_bands,EH_range
 use parallel_m,    ONLY:& ! LOGICALS
&                        l_par_X_T,l_par_X_G_q0,l_par_RT,l_par_SE,l_par_X_G_all_q,l_par_X_G_finite_q,&
&                        HEAD_QP_cpu,HEAD_k_cpu
 use parallel_m,    ONLY:& ! COMMUNICATORS
&                        COMM_reset,PAR_COM_WORLD,&  
&                        PAR_COM_VAL_INDEX,PAR_COM_CON_INDEX,PAR_COM_Q_A2A,&
&                        PAR_COM_Xk_ibz_INDEX,PAR_COM_Xk_ibz_A2A,PAR_COM_Q_INDEX,&
&                        PAR_COM_Xk_bz_INDEX,PAR_COM_INV_INDEX,PAR_COM_INV,&
&                        PAR_COM_DIAGO_INDEX,PAR_COM_DIAGO,&
&                        PAR_COM_QP_INDEX,PAR_COM_Plasma_INDEX,&
&                        PAR_COM_QP_A2A,PAR_COM_G_b_INDEX,PAR_COM_G_b_A2A,&
&                        PAR_COM_WF_k_A2A,PAR_COM_WF_b_INDEX,PAR_COM_WF_k_INDEX,&
&                        PAR_COM_q_for_Xo,PAR_COM_k_for_P,PAR_COM_Xk_bz_A2A,PAR_COM_density,&
&                        PAR_COM_eh_INDEX,PAR_COM_eh_A2A,PAR_COM_T_INDEX
 use parallel_m,    ONLY:& ! INDEXES
&                        PAR_IND_Xk_ibz,PAR_IND_CON_BANDS_X,PAR_IND_Xk_bz,&
&                        PAR_IND_VAL_BANDS_X,PAR_IND_Q,PAR_IND_INV,PAR_IND_DIAGO,&
&                        PAR_IND_QP,PAR_IND_G_b,PAR_IND_B_mat_ordered,PAR_IND_WF_b,PAR_IND_WF_k,&
&                        PAR_IND_B_mat,PAR_IND_Plasma,PAR_IND_DIPk_ibz,PAR_IND_WF_linear,&
&                        PAR_IND_Bp_mat,PAR_IND_G_k,PAR_IND_eh,PAR_IND_T_all,PAR_IND_WF_b_and_k,&
&                        PAR_IND_T_groups,PAR_IND_Kk_ibz,PAR_IND_T_ordered,PAR_IND_Q_bz,PAR_IND_RL
 use parallel_m,    ONLY:& ! ID's
&                        PAR_IND_CON_BANDS_X_ID,PAR_IND_VAL_BANDS_X_ID,PAR_IND_Xk_bz_ID,&
&                        PAR_IND_Xk_ibz_ID,PAR_IND_Q_ID,PAR_IND_INV_ID,PAR_IND_DIAGO_ID,&
&                        PAR_IND_QP_ID,PAR_IND_G_b_ID,myid,PAR_IND_Plasma_ID,&
&                        PAR_IND_WF_k_ID,PAR_IND_WF_b_ID,PAR_IND_B_mat_ID,&
&                        PAR_IND_Bp_mat_ID,PAR_IND_G_k_ID,&
&                        PAR_IND_Kk_ibz_ID,PAR_IND_RL_ID
 use interfaces,    ONLY:PARALLEL_index,PARALLEL_assign_chains_and_COMMs,PARALLEL_live_message,&
&                        PARALLEL_structure
 use X_m,           ONLY:X_t
 use drivers,       ONLY:l_eval_collisions,l_elph_corr,l_gw0,l_HF_and_locXC,l_life,l_sc_run,l_real_time,l_elphoton_corr
 use matrix_operate,ONLY:UP_matrix_index
 use BS,            ONLY:BS_bands,BS_nT_at_k,BS_nT_grps,BS_K_coupling
#if defined _ELPH
 use ELPH,          ONLY:elph_nDBs_used,QP_PH_n_G_bands,elph_use_q_grid
#endif
 use openmp,        ONLY:OPENMP_set_threads,n_threads_X,n_threads_SE,n_threads_RT,n_threads_K
 use electrons,     ONLY:levels
 use stderr,        ONLY:intc
 use com,           ONLY:warning
 use R_lattice,     ONLY:bz_samp,nqibz,nqbz
 use IO_m,          ONLY:IO_and_Messaging_switch,io_COLLs,io_RESPONSE
 use QP_m,          ONLY:QP_n_states,QP_nb,QP_table,QP_n_G_bands,QP_QED_ng
 use wave_func,     ONLY:states_to_load
 implicit none
 !
 type(levels)               ::E
 type(bz_samp)              ::Xk,q
 character(*)               ::ENVIRONMENT
 type(X_t), optional        ::X
 logical,   optional        ::RESET
 !
 ! Work Space
 !
 integer              :: i_k,i_k_bz,X_type,i_qp,ib1,ib2,i_field
 logical              :: reset_all
 character(10)        :: WHAT,WHATp
 !
 ! Resets...
 !
 reset_all=.TRUE.
 if (present(RESET)) then
   reset_all=RESET
 endif
 !
 ! Logicals
 !
 l_par_X_T          =ENVIRONMENT=="Response_T_space"
 l_par_X_G_q0       =ENVIRONMENT=="Response_G_space_Zero_Momentum"
 l_par_X_G_finite_q =ENVIRONMENT=="Response_G_space_Finite_Momentum"
 l_par_X_G_all_q    =ENVIRONMENT=="Response_G_space"
 l_par_SE           =ENVIRONMENT=="Self_Energy"
 l_par_RT           =ENVIRONMENT=="Real_Time"
 !
 X_type=1
 if (present(X)) then
   X_type=X%whoami
 endif
 !
 if (ENVIRONMENT=="Response_T_space") X_type=5
 !
 if (reset_all) then
   !
   call PP_indexes_reset(PAR_IND_INV)
   call PP_indexes_reset(PAR_IND_DIAGO)
   call PP_indexes_reset(PAR_IND_Q)
   call PP_indexes_reset(PAR_IND_Q_bz)
   call PP_indexes_reset(PAR_IND_T_groups)
   call PP_indexes_reset(PAR_IND_Kk_ibz)
   call PP_indexes_reset(PAR_IND_Xk_ibz)
   call PP_indexes_reset(PAR_IND_Xk_bz)
   call PP_indexes_reset(PAR_IND_CON_BANDS_X(X_type))
   call PP_indexes_reset(PAR_IND_VAL_BANDS_X(X_type))
   call PP_indexes_reset(PAR_IND_QP)
   call PP_indexes_reset(PAR_IND_Plasma)
   call PP_indexes_reset(PAR_IND_B_mat)
   call PP_indexes_reset(PAR_IND_Bp_mat)
   call PP_indexes_reset(PAR_IND_B_mat_ordered)
   call PP_indexes_reset(PAR_IND_G_b)
   call PP_indexes_reset(PAR_IND_G_k)
   call PP_indexes_reset(PAR_IND_WF_b)
   call PP_indexes_reset(PAR_IND_WF_k)
   call PP_indexes_reset(PAR_IND_WF_b_and_k)
   call PP_indexes_reset(PAR_IND_WF_linear)
   call PP_indexes_reset(PAR_IND_T_ordered)
   call PP_indexes_reset(PAR_IND_T_all)
   call PP_indexes_reset(PAR_IND_RL)
   !
   if (ENVIRONMENT=="Response_T_space") then
     if (.not.allocated(PAR_IND_eh)) then
       allocate(PAR_IND_eh(Xk%nibz))
       do i_k=1,Xk%nibz
         call PP_indexes_reset(PAR_IND_eh(i_k))
       enddo
     endif
   endif
   !
   if (allocated(PAR_B_mat_index))    deallocate(PAR_B_mat_index)
   if (allocated(PAR_Bp_mat_index))   deallocate(PAR_Bp_mat_index)
   if (allocated(PAR_QP_index))       deallocate(PAR_QP_index)
   if (allocated(PAR_G_bands_index))  deallocate(PAR_G_bands_index)
   if (allocated(PAR_Plasma_index))   deallocate(PAR_Plasma_index)
   if (allocated(PAR_Q_index))        deallocate(PAR_Q_index)
   if (allocated(PAR_Q_bz_index))     deallocate(PAR_Q_bz_index)
   if (allocated(PAR_Xk_ibz_index))   deallocate(PAR_Xk_ibz_index)
   if (allocated(PAR_Xk_bz_index))    deallocate(PAR_Xk_bz_index)
   if (allocated(states_to_load))     deallocate(states_to_load)
   if (allocated(PAR_DIPk_ibz_index)) deallocate(PAR_DIPk_ibz_index)
   if (allocated(PAR_BS_T_grps_index))deallocate(PAR_BS_T_grps_index)
   if (allocated(PAR_RL_index))       deallocate(PAR_RL_index)
   !
   PAR_IND_Q_ID=0
   PAR_IND_Xk_ibz_ID=0
   PAR_IND_Xk_bz_ID=0
   PAR_IND_CON_BANDS_X_ID(X_type)=0
   PAR_IND_VAL_BANDS_X_ID(X_type)=0
   PAR_IND_INV_ID=0
   PAR_IND_DIAGO_ID=0
   PAR_IND_QP_ID=0
   PAR_IND_Plasma_ID=0
   PAR_IND_G_b_ID=0
   PAR_IND_G_k_ID=0
   PAR_IND_WF_b_ID=0
   PAR_IND_WF_k_ID=0
   PAR_IND_B_mat_ID=0
   PAR_IND_Bp_mat_ID=0
   PAR_IND_RL_ID=0
   !
   call COMM_reset(PAR_COM_VAL_INDEX(X_type))
   call COMM_reset(PAR_COM_CON_INDEX(X_type))
   call COMM_reset(PAR_COM_Xk_ibz_INDEX)
   call COMM_reset(PAR_COM_Q_INDEX)
   call COMM_reset(PAR_COM_Xk_bz_INDEX)
   call COMM_reset(PAR_COM_Xk_ibz_A2A)
   call COMM_reset(PAR_COM_Xk_bz_A2A)
   call COMM_reset(PAR_COM_Q_A2A)
   call COMM_reset(PAR_COM_INV_INDEX)
   call COMM_reset(PAR_COM_INV)
   call COMM_reset(PAR_COM_DIAGO_INDEX)
   call COMM_reset(PAR_COM_DIAGO)
   call COMM_reset(PAR_COM_Plasma_INDEX)
   call COMM_reset(PAR_COM_QP_INDEX)
   call COMM_reset(PAR_COM_QP_A2A)
   call COMM_reset(PAR_COM_G_b_INDEX)
   call COMM_reset(PAR_COM_G_b_A2A)
   call COMM_reset(PAR_COM_WF_k_A2A)
   call COMM_reset(PAR_COM_WF_b_INDEX)
   call COMM_reset(PAR_COM_WF_k_INDEX)
   call COMM_reset(PAR_COM_WORLD)
   call COMM_reset(PAR_COM_q_for_Xo)
   call COMM_reset(PAR_COM_k_for_P)
   call COMM_reset(PAR_COM_density)
   call COMM_reset(PAR_COM_eh_INDEX)
   call COMM_reset(PAR_COM_eh_A2A)
   call COMM_reset(PAR_COM_T_INDEX)
   !
   call OPENMP_set_threads( )
   !
 endif
 !
 if (present(RESET)) then
   if (RESET) return
 endif
 !
 call PARALLEL_get_user_structure(ENVIRONMENT,.TRUE.)
 !
 PAR_COM_WORLD%COMM  =mpi_comm_world
 PAR_COM_WORLD%CPU_id=myid
 PAR_COM_WORLD%n_CPU =ncpu
 !
 !============
 ! Dimensions
 !============
 !
 select case(ENVIRONMENT)
   case("Response_G_space_Zero_Momentum")
     K_range=Xk%nibz
     n_c_bands= (/E%nbf+1,X%ib(2)/)
     n_v_bands= (/X%ib(1),E%nbm/)
   case("Response_G_space") 
     K_range=Xk%nbz
     Q_range=(/1,nqibz/)
     n_c_bands= (/E%nbf+1,X%ib(2)/)
     n_v_bands= (/X%ib(1),E%nbm/)
   case("Response_G_space_Finite_Momentum")
     K_range=Xk%nbz
     Q_range=(/max(X%iq(1),2),X%iq(2)/)
     n_c_bands= (/E%nbf+1,X%ib(2)/)
     n_v_bands= (/X%ib(1),E%nbm/)
   case("Self_Energy") 
     QP_range=QP_n_states
     Q_range=(/1,q%nibz/)
     if (l_eval_collisions)  Q_range=(/1,q%nbz/)
#if defined _ELPH
     if (l_elph_corr.and.elph_use_q_grid)         Q_range=(/1,q%nbz/)
     if (l_elph_corr.and..not.elph_use_q_grid)    Q_range=(/1,elph_nDBs_used/)
#endif
     if (l_HF_and_locXC)  n_bands=(/1,max(E%nbm,QP_nb)/)
     if (l_gw0.or.l_life) n_bands=(/1,max(QP_n_G_bands(2),QP_nb)/)
     if (.not.l_HF_and_locXC) n_bands(1)=QP_n_G_bands(1)
#if defined _ELPH
     if (l_elph_corr)                    n_bands=(/1,QP_PH_n_G_bands/)
#endif
   case("Response_T_space") 
     K_range=Xk%nibz
     EH_range=minval( BS_nT_at_k )
     n_c_bands= (/E%nbf+1,BS_bands(2)/)
     n_v_bands= (/BS_bands(1),E%nbm/)
 end select
 !
 !==========
 ! DEFAULTS
 !==========
 !
 CALL PARALLEL_defaults(ENVIRONMENT)
 !
 !======================================================
 if (ENVIRONMENT=="Response_G_space_Zero_Momentum") then
   !====================================================
   !
   CALL PARALLEL_structure(ENVIRONMENT,3,ROLE=(/"k","c","v"/))
   !
   call PARALLEL_assign_chains_and_COMMs(3,ROLE=(/"k","c","v"/),&
&                                        COMM_index_1=PAR_COM_Xk_ibz_INDEX,COMM_index_2=PAR_COM_CON_INDEX(X_type),&
&                                        COMM_index_3=PAR_COM_VAL_INDEX(X_type),COMM_A2A_1=PAR_COM_Xk_ibz_A2A)
   !
   ! K-points (IBZ)
   !
   call PARALLEL_index(PAR_IND_Xk_ibz,(/Xk%nibz/),COMM=PAR_COM_Xk_ibz_INDEX,CONSECUTIVE=.TRUE.)
   PAR_IND_Xk_ibz_ID=PAR_COM_Xk_ibz_INDEX%CPU_id
   !
   ! ... indexes
   !
   allocate(PAR_Xk_bz_index(Xk%nbz))
   call distribute_BZk_using_IBZk(PAR_COM_Xk_ibz_INDEX,Xk,PAR_IND_Xk_ibz,PAR_IND_Xk_ibz_ID,&
&                                                         PAR_IND_Xk_bz, PAR_IND_Xk_bz_ID,&
&                                                         PAR_Xk_bz_index,PAR_Xk_nbz)
   allocate(PAR_Xk_ibz_index(Xk%nibz))
   call Build_up_index(PAR_IND_Xk_ibz,Xk%nibz,PAR_Xk_ibz_index,PAR_Xk_nibz)
   !
   ! Inversion
   !
   PARALLEL_cpu_mat_inversion=CPU_structure(1)%nCPU_inversion
   call PARALLEL_assign_LIN_ALGEBRA_COMMs(ENVIRONMENT,'INV',PAR_COM_INV_INDEX,PAR_COM_INV)
   PAR_IND_INV_ID=PAR_COM_INV_INDEX%CPU_id
   !
   call PARALLEL_live_message("K(ibz)",ENVIRONMENT=ENVIRONMENT,LOADED=PAR_Xk_nibz,TOTAL=Xk%nibz,&
&                             NCPU=PAR_COM_Xk_ibz_INDEX%n_CPU)
   !
   call COMM_copy(PAR_COM_WORLD,PAR_COM_q_for_Xo)
   call COMM_copy(PAR_COM_Xk_ibz_A2A,PAR_COM_k_for_P)
   !
   call PAR_INDEX_copy(PAR_IND_Xk_ibz,PAR_IND_DIPk_ibz)
   allocate(PAR_DIPk_ibz_index(Xk%nibz))
   call Build_up_index(PAR_IND_DIPk_ibz,Xk%nibz,PAR_DIPk_ibz_index,PAR_DIPk_nibz)
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
   CALL PARALLEL_structure(ENVIRONMENT,4,ROLE=(/"q","k","c","v"/))
   !
   call PARALLEL_assign_chains_and_COMMs(4,ROLE=(/"q","k","c","v"/),&
&                                        COMM_index_1=PAR_COM_Q_INDEX,COMM_index_2=PAR_COM_Xk_bz_INDEX,&
&                                        COMM_index_3=PAR_COM_CON_INDEX(X_type),&
&                                        COMM_index_4=PAR_COM_VAL_INDEX(X_type),COMM_A2A_1=PAR_COM_Q_A2A,&
&                                        COMM_A2A_2=PAR_COM_Xk_bz_A2A)
   !
   ! The routine PARALLEL_assign_chains_and_COMMs cannot define COMMUNICATORS for internal
   ! A2A when there is no internal distribution
   !
   if (PAR_COM_Xk_bz_INDEX%n_CPU==1) then
     call COMM_copy(PAR_COM_Q_A2A,PAR_COM_Xk_bz_A2A)
   endif
   !
   ! Q-points 
   !
   WHAT="ibz"
   !
   ! K-points 
   !
   call PARALLEL_index(PAR_IND_Xk_bz,(/Xk%nbz/),COMM=PAR_COM_Xk_bz_INDEX,CONSECUTIVE=.TRUE.)
   PAR_IND_Xk_bz_ID=PAR_COM_Xk_bz_INDEX%CPU_id
   PAR_Xk_nbz=PAR_IND_Xk_bz%n_of_elements(PAR_IND_Xk_bz_ID+1)
   !
   call PARALLEL_live_message("K(bz)",ENVIRONMENT=ENVIRONMENT,&
&                             LOADED=PAR_IND_Xk_bz%n_of_elements(PAR_COM_Xk_bz_INDEX%CPU_id+1),TOTAL=Xk%nbz,&
&                             NCPU=PAR_COM_Xk_bz_INDEX%n_CPU)
   allocate(PAR_Xk_bz_index(Xk%nbz))
   call Build_up_index(PAR_IND_Xk_bz,Xk%nbz,PAR_Xk_bz_index,PAR_Xk_nbz)
   !
   ! Q-points 
   !
   call PARALLEL_index(PAR_IND_Q,(/Q_range(2)/),low_range=(/Q_range(1)/),COMM=PAR_COM_Q_INDEX,CONSECUTIVE=.TRUE.)
   PAR_IND_Q_ID=PAR_COM_Q_INDEX%CPU_id
   PAR_nQ=PAR_IND_Q%n_of_elements(PAR_IND_Q_ID+1)
   !
   call PARALLEL_live_message("Q("//trim(WHAT)//")",ENVIRONMENT=ENVIRONMENT,&
&                             LOADED=PAR_IND_Q%n_of_elements(PAR_IND_Q_ID+1),&
&                             TOTAL=Q_range(2)-Q_range(1)+1,&
&                             NCPU=PAR_COM_Q_INDEX%n_CPU)
   !
   allocate(PAR_Q_index(Q_range(2))) 
   call Build_up_index(PAR_IND_Q,Q_range(2),PAR_Q_index,PAR_nQ)
   !
   ! K-points (IBZ) after shifting of Q (BZ/IBZ)
   !
   WHATp="k_bz_q_"//trim(WHAT) 
   !
   call PARALLEL_add_Q_to_K_list(trim(WHATp),PAR_IND_Xk_bz,PAR_IND_Xk_bz_ID,PAR_IND_Xk_ibz,PAR_IND_Xk_ibz_ID,&
&                                PAR_IND_Q,PAR_COM_Xk_bz_INDEX,Q_range,Xk,q)
   PAR_Xk_nibz=PAR_IND_Xk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1)
   !
   !
   ! ... indexes
   !
   allocate(PAR_Xk_ibz_index(Xk%nibz))
   call Build_up_index(PAR_IND_Xk_ibz,Xk%nibz,PAR_Xk_ibz_index,PAR_Xk_nibz)
   !
   call PARALLEL_live_message("K(ibz)",ENVIRONMENT=ENVIRONMENT,LOADED=PAR_Xk_nibz,TOTAL=Xk%nibz)
   !
   if (ENVIRONMENT=="Response_G_space_Finite_Momentum") then
     !
     PARALLEL_cpu_mat_inversion=CPU_structure(2)%nCPU_inversion
     !
   else if (ENVIRONMENT=="Response_G_space") then
     !
     PARALLEL_cpu_mat_inversion=CPU_structure(3)%nCPU_inversion
     !
   endif
   !
   ! Inversion
   !
   call PARALLEL_assign_LIN_ALGEBRA_COMMs(ENVIRONMENT,'INV',PAR_COM_INV_INDEX,PAR_COM_INV)
   PAR_IND_INV_ID=PAR_COM_INV_INDEX%CPU_id
   !
   ! I/O privileges
   !
   if (PARALLEL_n_structures_active>1) then
     call IO_and_Messaging_switch("+io_out +output",CONDITION=PAR_COM_Q_A2A%CPU_id==0)
   else
     call IO_and_Messaging_switch("+io_out",CONDITION=.TRUE.)
   endif
   !
   ! To define proper indexes to calculate the dipoles I need to build up
   ! the PAR_IND_DIPk_ibz by avoiding the overlaps of PAR_IND_Xk_ibz.
   ! The COMM is anyway the one for the all2all of each q.
   !
   call COMM_copy(PAR_COM_Q_A2A,PAR_COM_q_for_Xo)
   call COMM_copy(PAR_COM_Xk_bz_A2A,PAR_COM_k_for_P)
   !
   if (ENVIRONMENT/="Response_G_space_Finite_Momentum") then
     !
     call PARALLEL_minimal_index_from_overlaping(PAR_IND_Xk_ibz,PAR_IND_DIPk_ibz,PAR_COM_Xk_bz_INDEX)
     !
     allocate(PAR_DIPk_ibz_index(Xk%nibz))
     call Build_up_index(PAR_IND_DIPk_ibz,Xk%nibz,PAR_DIPk_ibz_index,PAR_DIPk_nibz)
     !
   endif
   !
   call OPENMP_set_threads(n_threads_in=n_threads_X)
   !
   if (ENVIRONMENT=="Response_G_space") then
     !
     ! io_RESPONSE check
     !
     ! No X I/O is possible ONLY when the number of cpu's associated with the "q" field
     ! in the response function string is the same of the SE/RT/SC...string
     !  
     if (.not.io_RESPONSE) then
       !
       i_field=3
       call PARALLEL_get_user_structure("Self_Energy",.FALSE.)
       !
       if (CPU_structure(5)%CPU(1)>1) i_field=5
       !
       if (CPU_structure(3)%CPU(1)/=CPU_structure(i_field)%CPU(1)) io_RESPONSE=.TRUE.
       !
       if (io_RESPONSE) call warning('Response function I/O forced. Different CPU for the "q" field in X and SE')
       !
     endif
   endif
   !
 endif
 !
 !=================================
 if (ENVIRONMENT=="Real_Time") then
   !===============================
   !
   !
   call OPENMP_set_threads(n_threads_in=n_threads_RT)
   !
 endif
 !
 !==============================
 if (ENVIRONMENT=="Self_Energy") then
   !=================================
   !
   CALL PARALLEL_structure(ENVIRONMENT,3,ROLE=(/"q ","qp","b "/))
   !
   call PARALLEL_assign_chains_and_COMMs(3,ROLE=(/"q ","qp","b "/),&
&                                        COMM_index_1=PAR_COM_Q_INDEX,COMM_index_2=PAR_COM_QP_INDEX,&
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
   call PARALLEL_index(PAR_IND_QP,(/QP_n_states/),COMM=PAR_COM_QP_INDEX)
   PAR_IND_QP_ID=PAR_COM_QP_INDEX%CPU_id
   PAR_nQP=PAR_IND_QP%n_of_elements(PAR_IND_QP_ID+1)
   allocate(PAR_QP_index(QP_n_states))
   PAR_QP_index=0
   call Build_up_index(PAR_IND_QP,QP_n_states,PAR_QP_index,PAR_nQP)
   call PARALLEL_live_message("QPs",ENVIRONMENT=ENVIRONMENT,&
&                             LOADED=PAR_IND_QP%n_of_elements(PAR_IND_QP_ID+1),TOTAL=QP_n_states,&
&                             NCPU=PAR_COM_QP_INDEX%n_CPU)
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
   call PARALLEL_index(PAR_IND_Q,(/Q_range(2)/),COMM=PAR_COM_Q_INDEX,CONSECUTIVE=.TRUE.)
   PAR_IND_Q_ID=PAR_COM_Q_INDEX%CPU_id
   PAR_nQ=PAR_IND_Q%n_of_elements(PAR_IND_Q_ID+1)
   call PARALLEL_live_message("Q("//trim(WHAT)//")",ENVIRONMENT=ENVIRONMENT,&
&                             LOADED=PAR_IND_Q%n_of_elements(PAR_IND_Q_ID+1),TOTAL=Q_range(2),&
&                             NCPU=PAR_COM_Q_INDEX%n_CPU)
   !
   allocate(PAR_Q_index(Q_range(2))) 
   call Build_up_index(PAR_IND_Q,Q_range(2),PAR_Q_index,PAR_nQ)
   !
   if (trim(WHAT)=="ibz") then
     allocate(PAR_Q_bz_index(nqbz))
     call distribute_BZk_using_IBZk(PAR_COM_Q_INDEX,q,PAR_IND_Q   , PAR_IND_Q_ID,&
&                                                     PAR_IND_Q_bz, PAR_IND_Q_ID,&
&                                                     PAR_Q_bz_index,PAR_nQ_bz)
     call Build_up_index(PAR_IND_Q_bz,nqbz,PAR_Q_bz_index,PAR_nQ_bz)
   endif
   !
   !
   ! 0000     0000     <- q
   ! x0 x0    x0 x0    <- qp
   ! 0 0 0 0  0 0 0 0  <- b
   !
   ! QP_cpu corresponds to x marked CPU's. This flag is used when no b loops are done
   !
   HEAD_QP_cpu=PAR_COM_QP_A2A%CPU_id==0
   !
   !
   ! K-points
   !
   call PARALLEL_add_Q_to_K_list('k_qp_q_'//trim(WHAT),PAR_IND_QP,PAR_IND_QP_ID,PAR_IND_Xk_ibz,PAR_IND_Xk_ibz_ID,&
  &                                PAR_IND_Q,PAR_COM_QP_INDEX,(/0,0/),Xk,q)
   !
   ! G bands
   !=========
   !
   ! e-e correlation (HF and COHSEX)
   !=================================
   !
   ! n,k
   ! --<--.
   !      | 
   !      |
   !      |
   !     \|/ m,k-q
   !      |
   !      |
   !      | 
   !      .-->-- n',k
   !      
   ! "SC"="q.qp.b" 
   !  
   !   "q"   -> q 
   !   "qp"  -> QP (k,n,n')  
   !   "b"   -> m and PAR_IND_Bp_mat (this second is for the COLLISIONS case)
   !
   ! WF bands to load
   !
   n_WF_bands_to_load=n_bands(2)
   !
   call PARALLEL_index(PAR_IND_G_b,(/n_bands(2)/),low_range=(/n_bands(1)/),&
&                      COMM=PAR_COM_G_b_INDEX,CONSECUTIVE=.TRUE.)
   PAR_IND_G_b_ID=PAR_COM_G_b_INDEX%CPU_id
   PAR_nG_bands=PAR_IND_G_b%n_of_elements(PAR_IND_G_b_ID+1)
   allocate(PAR_G_bands_index(n_bands(2)))
   call Build_up_index(PAR_IND_G_b,n_bands(2),PAR_G_bands_index,PAR_nG_bands)
   !
   call PARALLEL_live_message("G bands",ENVIRONMENT=ENVIRONMENT,&
&                             LOADED=PAR_IND_G_b%n_of_elements(PAR_IND_G_b_ID+1),&
&                             TOTAL=n_bands(2)-n_bands(1)+1,NCPU=PAR_COM_G_b_INDEX%n_CPU)
   !
   !
   ! I/O privileges
   !
   if (PARALLEL_n_structures_active>1) then
     if (l_eval_collisions.and.io_COLLs) then
       call IO_and_Messaging_switch("+io_out",CONDITION=PAR_COM_QP_A2A%CPU_id==0.and.PAR_COM_Q_index%CPU_id==0)
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
 endif
 !
 !========================================================================================================
 if (ENVIRONMENT=="Response_T_space") then
   !======================================================================================================
   !
   CALL PARALLEL_structure(ENVIRONMENT,3,ROLE=(/"k ","eh","t "/))
   !
   call PARALLEL_assign_chains_and_COMMs(3,ROLE=(/"k ","eh","t "/),&
&                                        COMM_index_1=PAR_COM_Xk_ibz_INDEX,&
&                                        COMM_index_2=PAR_COM_eh_INDEX,&
&                                        COMM_index_3=PAR_COM_T_INDEX,&
&                                        COMM_A2A_1=PAR_COM_Xk_ibz_A2A,&
&                                        COMM_A2A_2=PAR_COM_eh_A2A) 
   if (PAR_COM_eh_INDEX%n_CPU==1) then
     call COMM_copy(PAR_COM_Xk_ibz_A2A,PAR_COM_eh_A2A)
   endif
   !
   ! Dipoles are calculated using PAR_COM_Xk_bz_INDEX, PAR_COM_eh_INDEX and PAR_COM_T_INDEX communicators
   !
   call COMM_copy(PAR_COM_Xk_ibz_A2A,PAR_COM_k_for_P)
   call COMM_copy(PAR_COM_eh_INDEX,PAR_COM_CON_INDEX(X_type))
   call COMM_copy(PAR_COM_T_INDEX,PAR_COM_VAL_INDEX(X_type))
   !
   ! K-points (IBZ)
   !
   call PARALLEL_index(PAR_IND_Kk_ibz,(/Xk%nibz/),COMM=PAR_COM_Xk_ibz_INDEX,CONSECUTIVE=.TRUE.)
   PAR_IND_Xk_ibz_ID=PAR_COM_Xk_ibz_INDEX%CPU_id
   PAR_IND_Kk_ibz_ID=PAR_COM_Xk_ibz_INDEX%CPU_id
   PAR_Kk_nibz=PAR_IND_Kk_ibz%n_of_elements(PAR_IND_Kk_ibz_ID+1)
   !
   call PARALLEL_live_message("K(ibz)",ENVIRONMENT=ENVIRONMENT,&
&           LOADED=PAR_IND_Kk_ibz%n_of_elements(PAR_IND_Kk_ibz_ID+1),TOTAL=Xk%nibz,&
&           NCPU=PAR_COM_Xk_ibz_INDEX%n_CPU)
   ! 
   ! Dipoles k-points uses same distribution of K k-points
   !
   call PAR_INDEX_copy(PAR_IND_Kk_ibz,PAR_IND_DIPk_ibz)
   call PAR_INDEX_copy(PAR_IND_Kk_ibz,PAR_IND_Xk_ibz)
   PAR_Xk_nibz  =PAR_Kk_nibz
   PAR_DIPk_nibz=PAR_Kk_nibz
   allocate(PAR_DIPk_ibz_index(Xk%nibz))
   call Build_up_index(PAR_IND_DIPk_ibz,Xk%nibz,PAR_DIPk_ibz_index,PAR_DIPk_nibz)
   allocate(PAR_Xk_ibz_index(Xk%nibz))
   call Build_up_index(PAR_IND_Xk_ibz,Xk%nibz,PAR_Xk_ibz_index,PAR_Xk_nibz)
   allocate(PAR_Xk_bz_index(Xk%nbz))
   call distribute_BZk_using_IBZk(PAR_COM_Xk_ibz_INDEX,Xk,PAR_IND_Xk_ibz,PAR_IND_Xk_ibz_ID,&
&                                                         PAR_IND_Xk_bz, PAR_IND_Xk_bz_ID,&
&                                                         PAR_Xk_bz_index,PAR_Xk_nbz)
   !
   ! E/h pairs (k resolved)
   !
   ! In this part I distribute the eh transitions within each k. The COMM for this indexing is PAR_COM_eh_INDEX.
   ! I fill the PAR_IND_eh for all k in order to define the total number of Transition groups
   !
   do i_k=1,Xk%nibz
     !
     call PARALLEL_index(PAR_IND_eh(i_k),(/BS_nT_at_k(i_k)/),COMM=PAR_COM_eh_INDEX,CONSECUTIVE=.TRUE.)
     !
   enddo
   !
   ! Now I find calculate the total (BS_nT_grps) and cpu-restricted (PAR_BS_nT_grps) number of Transition groups.
   ! In this case the PAR_BS_nT_grps groups belong only to the columns of the kernel.
   !
   call PARALLEL_Transitions_grouping(Xk)
   !
   call PARALLEL_live_message("(e/h) Groups",ENVIRONMENT=ENVIRONMENT,LOADED=PAR_BS_nT_col_grps,TOTAL=BS_nT_grps)
   !
   ! Now each CPU of the PAR_COM_eh_INDEX has PAR_BS_nT_grps  groups of e/h pairs
   !
   ! The task now is to distribute the transitions:
   !  
   ! Group@k (among BS_nT_grps) ->Group'@p (among BS_nT_grps)
   !
   if (BS_K_coupling) then
     call PARALLEL_index(PAR_IND_T_all,    (/BS_nT_grps,BS_nT_grps/),COMM=PAR_COM_T_INDEX,&
&                        MASK=PAR_IND_T_groups%element_1D)
     !
     call PARALLEL_live_message("(e/h)->(e/h)' Transitions (all)",ENVIRONMENT=ENVIRONMENT,&
&                               LOADED=PAR_IND_T_all%n_of_elements(PAR_COM_T_INDEX%CPU_id+1),TOTAL=BS_nT_grps*BS_nT_grps,&
&                               NCPU=PAR_COM_T_INDEX%n_CPU)
     !
   endif
   !
   call PARALLEL_index(PAR_IND_T_ordered,(/BS_nT_grps,BS_nT_grps/),COMM=PAR_COM_T_INDEX,&
&                      MASK=PAR_IND_T_groups%element_1D,ORDERED=.TRUE.)
   !
   call PARALLEL_live_message("(e/h)->(e/h)' Transitions (ordered)",ENVIRONMENT=ENVIRONMENT,&
&                             LOADED=PAR_IND_T_ordered%n_of_elements(PAR_COM_T_INDEX%CPU_id+1),&
&                             TOTAL=BS_nT_grps*(BS_nT_grps-1)/2+BS_nT_grps,&
&                             NCPU=PAR_COM_T_INDEX%n_CPU)
   !
   ! Inversion
   !
   PARALLEL_cpu_mat_inversion=CPU_structure(4)%nCPU_inversion
   call PARALLEL_assign_LIN_ALGEBRA_COMMs(ENVIRONMENT,'INV',PAR_COM_INV_INDEX,PAR_COM_INV)
   PAR_IND_INV_ID=PAR_COM_INV_INDEX%CPU_id
   !
   ! Diagonalization
   !
   PARALLEL_cpu_mat_diagonalization=CPU_structure(4)%nCPU_diagonalization
   call PARALLEL_assign_LIN_ALGEBRA_COMMs(ENVIRONMENT,'DIAGO',PAR_COM_DIAGO_INDEX,PAR_COM_DIAGO)
   PAR_IND_DIAGO_ID=PAR_COM_DIAGO_INDEX%CPU_id
   !
   ! I/O privileges
   !
   call IO_and_Messaging_switch("+io_out",CONDITION=.TRUE.)
   !
   call IO_and_Messaging_switch("+output",CONDITION=master_cpu)
   !
   call OPENMP_set_threads(n_threads_in=n_threads_K)
   !  
 endif
 !
 !==========================================================================================================
 if (ENVIRONMENT=="Response_G_space_Zero_Momentum".or.ENVIRONMENT=="Response_G_space_Finite_Momentum".or.&
&    ENVIRONMENT=="Response_G_space".or.ENVIRONMENT=="Response_T_space") then
   !========================================================================================================
   !
   ! Response functions conduction bands
   !
   call PARALLEL_index(PAR_IND_CON_BANDS_X(X_type),(/n_c_bands(2)/),low_range=(/n_c_bands(1)/),&
&                      COMM=PAR_COM_CON_INDEX(X_type),CONSECUTIVE=.TRUE.)
   PAR_IND_CON_BANDS_X_ID(X_type)=PAR_COM_CON_INDEX(X_type)%CPU_id
   !
   call PARALLEL_live_message("CON bands",ENVIRONMENT=ENVIRONMENT,&
&                             LOADED=PAR_IND_CON_BANDS_X(X_type)%n_of_elements(PAR_COM_CON_INDEX(X_type)%CPU_id+1),&
&                             TOTAL=n_c_bands(2)-n_c_bands(1)+1,&
&                             NCPU=PAR_COM_CON_INDEX(X_type)%n_CPU)
   !
   ! Response functions valence bands
   !
   call PARALLEL_index(PAR_IND_VAL_BANDS_X(X_type),(/n_v_bands(2)/),low_range=(/n_v_bands(1)/),&
&                      COMM=PAR_COM_VAL_INDEX(X_type),CONSECUTIVE=.TRUE.)
   PAR_IND_VAL_BANDS_X_ID(X_type)=PAR_COM_VAL_INDEX(X_type)%CPU_id
   !
   call PARALLEL_live_message("VAL bands",ENVIRONMENT=ENVIRONMENT,&
&                             LOADED=PAR_IND_VAL_BANDS_X(X_type)%n_of_elements(PAR_COM_VAL_INDEX(X_type)%CPU_id+1),&
&                             TOTAL=n_v_bands(2)-n_v_bands(1)+1,&
&                             NCPU=PAR_COM_VAL_INDEX(X_type)%n_CPU)
   !
 endif
 !
 call PARALLEL_live_message("Matrix Inversion uses "//trim(intc(PAR_COM_INV%n_CPU))//" CPUs")
 call PARALLEL_live_message("Matrix Diagonalization uses "//trim(intc(PAR_COM_DIAGO%n_CPU))//" CPUs")
 !
 contains
   !
   !
   subroutine Build_up_index(PAR_ind,n_PAR_ind,V_ind,n_V_ind)
     !
     use parallel_m,ONLY:PP_indexes
     integer           ::n_PAR_ind,n_V_ind,V_ind(n_PAR_ind),i_p
     type(PP_indexes)  ::PAR_ind
     !
     V_ind  =0
     n_V_ind=0
     !
     do i_p=1,n_PAR_ind
       !
       if (PAR_IND%element_1D(i_p)) then
         n_V_ind=n_V_ind+1
         V_ind(i_p)=n_V_ind
       endif
       !
     enddo
     !
   end subroutine
   !
   subroutine fill_IBZk_using_BZk(IND_ibz,IND_bz,COMM)
     !
     type(PP_indexes) :: IND_ibz,IND_bz
     type(MPI_comm)   :: COMM
     !
     if (.not.associated(IND_ibz%element_1D)) then
       allocate(IND_ibz%element_1D(Xk%nibz))
       allocate(IND_ibz%n_of_elements( COMM%n_CPU ))
       IND_ibz%element_1D=.FALSE.
     endif
     !
     do i_k_bz=1,Xk%nbz
       !
       i_k=Xk%sstar(i_k_bz,1)
       !
       if (IND_bz%element_1D(i_k_bz)) IND_ibz%element_1D(i_k)=.TRUE.
       !
     enddo
     !
     IND_ibz%n_of_elements(COMM%CPU_ID+1)=count(IND_ibz%element_1D)
     !
   end subroutine
   !
   subroutine distribute_BZk_using_IBZk(COMM,K,IND_ibz,IBZ_id,IND_bz,BZ_id,BZ_index,PAR_n_bz)
     !
     type(MPI_comm)   :: COMM
     type(PP_indexes) :: IND_ibz,IND_bz
     type(bz_samp)    :: K
     integer          :: IBZ_id,BZ_id,BZ_index(K%nbz),PAR_n_bz
     !
     ! Work Space
     integer :: i_bz,i_ibz,i_p
     !
     ! K-points in the BZ 
     !
     allocate(IND_bz%element_1D(K%nbz))
     IND_bz%element_1D=.FALSE.
     !
     allocate(IND_bz%n_of_elements(COMM%n_CPU))
     ! 
     BZ_index=0
     !
     BZ_id=IBZ_id
     !
     i_p=0
     !
     do i_bz=1,K%nbz
       !
       i_ibz=K%sstar(i_bz,1)
       !
       if (IND_ibz%element_1D(i_ibz)) then
         i_p=i_p+1
         IND_bz%element_1D(i_bz)=.TRUE.
         BZ_index(i_bz)=i_p
       else
         IND_bz%element_1D(i_bz)=.FALSE.
       endif
       !
     enddo
     !
     PAR_n_bz=i_p
     !
     IND_bz%n_of_elements(BZ_ID+1)=PAR_n_bz
     !
   end subroutine
   !
end subroutine
