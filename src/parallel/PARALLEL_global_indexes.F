!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine PARALLEL_global_indexes(E,Xk,q,ENVIRONMENT,X,RESET,SERIAL)
 !
 use parallel_m,    ONLY:PP_indexes,PP_indexes_reset,PAR_Xk_ibz_index,PAR_Xk_nibz,PAR_Xk_nbz,&
&                        PAR_Xk_bz_index,PARALLEL_n_structures_active,ncpu,&
&                        PARALLEL_cpu_mat_inversion,CPU_structure,PP_redux_wait,master_cpu,&
&                        PARALLEL_cpu_mat_diagonalization
 use parallel_m,    ONLY:& ! COMMUNICATORS
&                        COMM_reset,&  
&                        PAR_COM_VAL_INDEX,PAR_COM_CON_INDEX,PAR_COM_Q_A2A,&
&                        PAR_COM_Xk_ibz_INDEX,PAR_COM_Xk_ibz_A2A,PAR_COM_Q_INDEX,&
&                        PAR_COM_Xk_bz_INDEX,PAR_COM_INV_INDEX,PAR_COM_INV,&
&                        PAR_COM_DIAGO_INDEX,PAR_COM_DIAGO,&
&                        PAR_COM_QP_INDEX,&
&                        PAR_COM_QP_A2A,PAR_COM_QP_G_b_INDEX
 use parallel_m,    ONLY:& ! INDEXES
&                        PAR_IND_Xk_ibz,PAR_IND_CON_BANDS_X,PAR_IND_Xk_bz,&
&                        PAR_IND_VAL_BANDS_X,PAR_IND_Q,PAR_IND_INV,PAR_IND_DIAGO,&
                         PAR_IND_QP,PAR_IND_QP_G_b
 use parallel_m,    ONLY:& ! ID's
&                        PAR_IND_CON_BANDS_X_ID,PAR_IND_VAL_BANDS_X_ID,PAR_IND_Xk_bz_ID,&
&                        PAR_IND_Xk_ibz_ID,PAR_IND_Q_ID,PAR_IND_INV_ID,PAR_IND_DIAGO_ID,&
&                        PAR_IND_QP_ID,PAR_IND_QP_G_b_ID,myid
 use interfaces,    ONLY:PARALLEL_index,PARALLEL_assign_chains_and_COMMs,&
&                        PARALLEL_live_message
 use X_m,           ONLY:X_t
 use BS,            ONLY:BS_bands
 use electrons,     ONLY:levels
 use stderr,        ONLY:intc
 use com,           ONLY:error
 use R_lattice,     ONLY:bz_samp,nqibz,nqbz
 use IO_m,          ONLY:IO_and_Messaging_switch
 use QP_m,          ONLY:QP_n_states,QP_nb,QP_table,QP_n_G_bands
 use wave_func,     ONLY:states_to_load
 implicit none
 !
 type(levels)            ::E
 type(bz_samp)           ::Xk,q
 character(*)            ::ENVIRONMENT
 type(X_t), optional     ::X
 logical,   optional     ::RESET
 logical,   optional     ::SERIAL
 !
 ! Work Space
 !
 integer       :: i_k,i_p,i_k_bz,X_type,iq_range(2),i_qp,n_bands(2)
 logical       :: reset_all
 character(10) :: WHAT
 !
 ! Resets...
 !
 reset_all=.TRUE.
 if (present(RESET)) then
   reset_all=RESET
 endif
 !
 X_type=1
 if (present(X)) then
   X_type=X%whoami
 endif
 !
 if (ENVIRONMENT=="BSE") X_type=5
 !
 if (reset_all) then
   !
   call PP_indexes_reset(PAR_IND_INV)
   call PP_indexes_reset(PAR_IND_DIAGO)
   call PP_indexes_reset(PAR_IND_Q)
   call PP_indexes_reset(PAR_IND_Xk_ibz)
   call PP_indexes_reset(PAR_IND_Xk_bz)
   call PP_indexes_reset(PAR_IND_CON_BANDS_X(X_type))
   call PP_indexes_reset(PAR_IND_VAL_BANDS_X(X_type))
   call PP_indexes_reset(PAR_IND_QP)
   call PP_indexes_reset(PAR_IND_QP_G_b)
   !
   if (allocated(PAR_Xk_ibz_index)) deallocate(PAR_Xk_ibz_index)
   if (allocated(PAR_Xk_bz_index)) deallocate(PAR_Xk_bz_index)
   if (allocated(states_to_load)) deallocate(states_to_load)
   !
   PAR_IND_Q_ID=0
   PAR_IND_Xk_ibz_ID=0
   PAR_IND_Xk_bz_ID=0
   PAR_IND_CON_BANDS_X_ID(X_type)=0
   PAR_IND_VAL_BANDS_X_ID(X_type)=0
   PAR_IND_INV_ID=0
   PAR_IND_DIAGO_ID=0
   PAR_IND_QP_ID=0
   PAR_IND_QP_G_b_ID=0
   !
   call COMM_reset(PAR_COM_VAL_INDEX(X_type))
   call COMM_reset(PAR_COM_CON_INDEX(X_type))
   call COMM_reset(PAR_COM_Xk_ibz_INDEX)
   call COMM_reset(PAR_COM_Q_INDEX)
   call COMM_reset(PAR_COM_Xk_bz_INDEX)
   call COMM_reset(PAR_COM_Xk_ibz_A2A)
   call COMM_reset(PAR_COM_Q_A2A)
   call COMM_reset(PAR_COM_INV_INDEX)
   call COMM_reset(PAR_COM_INV)
   call COMM_reset(PAR_COM_DIAGO_INDEX)
   call COMM_reset(PAR_COM_DIAGO)
   call COMM_reset(PAR_COM_QP_INDEX)
   call COMM_reset(PAR_COM_QP_A2A)
   call COMM_reset(PAR_COM_QP_G_b_INDEX)
   !
   call IO_and_Messaging_switch("RESET")
   !
 endif
 !
 if (present(RESET)) then
   if (RESET) return
 endif
 !
 call PARALLEL_get_user_structure(ENVIRONMENT)
 !
 if (PARALLEL_n_structures_active==0.and.ncpu>1) call error('Please provide a CPU structure in the input file')
 !
 ! SERIAL
 !========
 if (present(SERIAL)) then
   !
   if (SERIAL) then
     !
     if (ENVIRONMENT=="DENSITY") then
       !
       PAR_IND_QP_G_b_ID=myid
       allocate(PAR_IND_QP_G_b%n_of_elements(ncpu),PAR_IND_QP_G_b%element_1D(E%nbm))
       PAR_IND_QP_G_b%element_1D=.TRUE.
       PAR_IND_QP_G_b%n_of_elements(myid+1)=E%nbm
       !
     endif
     !
     if (ENVIRONMENT=="BSE") then
       !
       PAR_COM_Xk_ibz_INDEX%CPU_id=myid
       allocate(PAR_IND_Xk_ibz%n_of_elements(ncpu),PAR_IND_Xk_ibz%element_1D(Xk%nibz))
       PAR_IND_Xk_ibz%element_1D=.TRUE.
       PAR_IND_Xk_ibz%n_of_elements(myid+1)=Xk%nibz
       !
       PAR_COM_Xk_bz_INDEX%CPU_id=myid
       allocate(PAR_IND_Xk_bz%n_of_elements(ncpu),PAR_IND_Xk_bz%element_1D(Xk%nbz))
       PAR_IND_Xk_bz%element_1D=.TRUE.
       PAR_IND_Xk_bz%n_of_elements(myid+1)=Xk%nbz
       !
       PAR_IND_CON_BANDS_X_ID(X_type)=myid
       allocate(PAR_IND_CON_BANDS_X(X_type)%n_of_elements(ncpu),PAR_IND_CON_BANDS_X(X_type)%element_1D(BS_bands(2)))
       PAR_IND_CON_BANDS_X(X_type)%element_1D=.FALSE.
       PAR_IND_CON_BANDS_X(X_type)%element_1D(E%nbf+1:BS_bands(2))=.TRUE.
       PAR_IND_CON_BANDS_X(X_type)%n_of_elements(myid+1)=BS_bands(2)-E%nbf
       !
       PAR_IND_VAL_BANDS_X_ID(X_type)=myid
       allocate(PAR_IND_VAL_BANDS_X(X_type)%n_of_elements(ncpu),PAR_IND_VAL_BANDS_X(X_type)%element_1D(E%nbm))
       PAR_IND_VAL_BANDS_X(X_type)%element_1D=.FALSE.
       PAR_IND_VAL_BANDS_X(X_type)%element_1D(BS_bands(1):E%nbm)=.TRUE.
       PAR_IND_VAL_BANDS_X(X_type)%n_of_elements(myid+1)=E%nbm-BS_bands(1)+1
       !
       PAR_Xk_nbz =Xk%nbz
       PAR_Xk_nibz=Xk%nibz
       allocate (PAR_Xk_ibz_index(Xk%nibz),PAR_Xk_bz_index(Xk%nbz))
       do i_k=1,Xk%nbz
         PAR_Xk_bz_index(i_k)=i_k
         if (i_k<=Xk%nibz) then
           PAR_Xk_ibz_index(i_k)=i_k
         endif
       enddo
       !
     endif
     !
   endif
   !
   return
   !
 endif
 !
 ! ENVIROMENTS
 !=============
 !
 ! DENSITY
 !---------
 !
 if (ENVIRONMENT=="DENSITY") then
   !
   call PARALLEL_assign_chains_and_COMMs(2,ROLE=(/"k","v"/),&
&                                        COMM_index_1=PAR_COM_Xk_ibz_INDEX,&
&                                        COMM_index_2=PAR_COM_QP_G_b_INDEX)
   !
   ! K-points
   !
   call PARALLEL_index(PAR_IND_Xk_ibz,(/Xk%nibz/),COMM=PAR_COM_Xk_ibz_INDEX,CONSECUTIVE=.TRUE.)
   PAR_IND_Xk_ibz_ID=PAR_COM_Xk_ibz_INDEX%CPU_id
   PAR_Xk_nibz=PAR_IND_Xk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1)
   !
   ! Bands
   !
   call PARALLEL_index(PAR_IND_QP_G_b,(/E%nbm/),COMM=PAR_COM_QP_G_b_INDEX,CONSECUTIVE=.TRUE.)
   PAR_IND_QP_G_b_ID=PAR_COM_QP_G_b_INDEX%CPU_id
   !
   call PARALLEL_live_message("K(ibz)",LOADED=PAR_Xk_nibz,TOTAL=Xk%nibz,&
&                             NCPU=PAR_COM_Xk_ibz_INDEX%n_CPU,CHAIN=PAR_COM_Xk_ibz_INDEX%chain_order)
   !
   call PARALLEL_live_message("G bands",&
&                             LOADED=PAR_IND_QP_G_b%n_of_elements(PAR_IND_QP_G_b_ID+1),&
&                             TOTAL=E%nbm,NCPU=PAR_COM_QP_G_b_INDEX%n_CPU,&
&                             CHAIN=PAR_COM_QP_G_b_INDEX%chain_order)
   !
 endif
 !
 ! Response_G_space_Zero_Momentum
 !--------------------------------
 !
 if (ENVIRONMENT=="Response_G_space_Zero_Momentum") then
   !
   call PARALLEL_assign_chains_and_COMMs(3,ROLE=(/"k","c","v"/),&
&                                        COMM_index_1=PAR_COM_Xk_ibz_INDEX,COMM_index_2=PAR_COM_CON_INDEX(X_type),&
&                                        COMM_index_3=PAR_COM_VAL_INDEX(X_type),COMM_A2A_1=PAR_COM_Xk_ibz_A2A)
   !
   ! K-points
   !
   call PARALLEL_index(PAR_IND_Xk_ibz,(/Xk%nibz/),COMM=PAR_COM_Xk_ibz_INDEX,CONSECUTIVE=.TRUE.)
   PAR_IND_Xk_ibz_ID=PAR_COM_Xk_ibz_INDEX%CPU_id
   PAR_Xk_nibz=PAR_IND_Xk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1)
   !
   call distribute_BZk_using_IBZk( )
   !
   PARALLEL_cpu_mat_inversion=CPU_structure(1)%nCPU_inversion
   !
   ! Inversion
   !
   call PARALLEL_assign_LIN_ALGEBRA_COMMs(ENVIRONMENT,'INV',PAR_COM_INV_INDEX,PAR_COM_INV)
   PAR_IND_INV_ID=PAR_COM_INV_INDEX%CPU_id
   !
   call PARALLEL_live_message("Linear Algebra uses "//trim(intc(PAR_COM_INV%n_CPU))//" CPU")
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
   call PARALLEL_live_message("K(ibz)",LOADED=PAR_Xk_nibz,TOTAL=Xk%nibz,&
&                             NCPU=PAR_COM_Xk_ibz_INDEX%n_CPU,CHAIN=PAR_COM_Xk_ibz_INDEX%chain_order)
   !
 endif
 !
 ! Response_G_space_Finite_Momentum
 !----------------------------------
 !
 if (ENVIRONMENT=="Response_G_space_Finite_Momentum".or.ENVIRONMENT=="BSE") then
   !
   if (ENVIRONMENT=="Response_G_space_Finite_Momentum") then
     WHAT="ibz"
     iq_range=(/max(X%iq(1),2),X%iq(2)/)
   else
     WHAT="bz"
     iq_range=(/1,nqbz/)
   endif
   !
   call PARALLEL_assign_chains_and_COMMs(4,ROLE=(/"q","k","c","v"/),&
&                                        COMM_index_1=PAR_COM_Q_INDEX,COMM_index_2=PAR_COM_Xk_bz_INDEX,&
&                                        COMM_index_3=PAR_COM_CON_INDEX(X_type),&
&                                        COMM_index_4=PAR_COM_VAL_INDEX(X_type),COMM_A2A_1=PAR_COM_Q_A2A)
   !
   ! K-points (BZ)
   !
   call PARALLEL_index(PAR_IND_Xk_bz,(/Xk%nbz/),COMM=PAR_COM_Xk_bz_INDEX,CONSECUTIVE=.TRUE.)
   !
   call PARALLEL_live_message("K(bz)",LOADED=PAR_IND_Xk_bz%n_of_elements(PAR_COM_Xk_bz_INDEX%CPU_id+1),TOTAL=Xk%nbz,&
&                             NCPU=PAR_COM_Xk_bz_INDEX%n_CPU,CHAIN=PAR_COM_Xk_bz_INDEX%chain_order)
   !
   ! Q-points (IBZ in LLR, in BZ BSK)
   !
   call PARALLEL_index(PAR_IND_Q,(/iq_range(2)/),low_range=(/iq_range(1)/),COMM=PAR_COM_Q_INDEX,CONSECUTIVE=.TRUE.)
   PAR_IND_Q_ID=PAR_COM_Q_INDEX%CPU_id
   !
   call PARALLEL_live_message("Q("//trim(WHAT)//")",LOADED=PAR_IND_Q%n_of_elements(PAR_IND_Q_ID+1),&
&                             TOTAL=iq_range(2)-iq_range(1)+1,&
&                             NCPU=PAR_COM_Q_INDEX%n_CPU,CHAIN=PAR_COM_Q_INDEX%chain_order)
   !
   ! K-points (IBZ) after shifting of Q
   !
   call PARALLEL_add_Q_to_K_list("k_bz_q_"//trim(WHAT),PAR_IND_Xk_bz,PAR_IND_Xk_bz_ID,PAR_IND_Xk_ibz,PAR_IND_Xk_ibz_ID,&
&                                PAR_IND_Q,PAR_COM_Xk_bz_INDEX,iq_range,Xk,q)
   PAR_Xk_nibz=PAR_IND_Xk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1)
   call PARALLEL_live_message("K(ibz)",LOADED=PAR_Xk_nibz,TOTAL=Xk%nibz)
   call PARALLEL_live_message("K(ibz)",LOADED=PAR_Xk_nibz,TOTAL=Xk%nibz)
   !
   PARALLEL_cpu_mat_inversion=CPU_structure(2)%nCPU_inversion
   if (ENVIRONMENT=="BSE") PARALLEL_cpu_mat_inversion=CPU_structure(5)%nCPU_inversion
   !
   ! Inversion
   !
   call PARALLEL_assign_LIN_ALGEBRA_COMMs(ENVIRONMENT,'INV',PAR_COM_INV_INDEX,PAR_COM_INV)
   PAR_IND_INV_ID=PAR_COM_INV_INDEX%CPU_id
   !
   if (ENVIRONMENT=="BSE") then
     !
     PARALLEL_cpu_mat_diagonalization=CPU_structure(5)%nCPU_diagonalization
     !
     ! Diagonalization
     !
     call PARALLEL_assign_LIN_ALGEBRA_COMMs(ENVIRONMENT,'DIAGO',PAR_COM_DIAGO_INDEX,PAR_COM_DIAGO)
     PAR_IND_DIAGO_ID=PAR_COM_DIAGO_INDEX%CPU_id
     !
   endif
   !
   call PARALLEL_live_message("Linear Algebra uses "//trim(intc(PAR_COM_INV%n_CPU))//" CPU")
   !
   ! I/O privileges
   !
   if (PARALLEL_n_structures_active>1) then
     call IO_and_Messaging_switch("+io_out +output",CONDITION=PAR_COM_Q_INDEX%my_CHAIN==1)
   else
     call IO_and_Messaging_switch("+io_out",CONDITION=.TRUE.)
   endif
   !
 endif
 !
 ! Hartree-Fock & GW
 !-------------------
 !
 if (ENVIRONMENT=="HF".or.ENVIRONMENT=="GW") then
   !
   call PARALLEL_assign_chains_and_COMMs(3,ROLE=(/"qp","q ","b "/),&
&                                        COMM_index_1=PAR_COM_QP_INDEX,COMM_index_2=PAR_COM_Q_INDEX,&
&                                        COMM_index_3=PAR_COM_QP_G_b_INDEX,&
&                                        COMM_A2A_1=PAR_COM_QP_A2A,&
&                                        COMM_A2A_2=PAR_COM_Q_A2A)
   !
   ! QP states
   !
   call PARALLEL_index(PAR_IND_QP,(/QP_n_states/),COMM=PAR_COM_QP_INDEX)
   PAR_IND_QP_ID=PAR_COM_QP_INDEX%CPU_id
   call PARALLEL_live_message("QP",LOADED=PAR_IND_QP%n_of_elements(PAR_IND_QP_ID+1),TOTAL=QP_n_states,&
&                             NCPU=PAR_COM_QP_INDEX%n_CPU,CHAIN=PAR_COM_QP_INDEX%chain_order)
   !
   ! Q-points
   !
   call PARALLEL_index(PAR_IND_Q,(/nqibz/),COMM=PAR_COM_Q_INDEX)
   PAR_IND_Q_ID=PAR_COM_Q_INDEX%CPU_id
   call PARALLEL_live_message("Q(ibz)",LOADED=PAR_IND_Q%n_of_elements(PAR_IND_Q_ID+1),TOTAL=nqibz,&
&                             NCPU=PAR_COM_Q_INDEX%n_CPU,CHAIN=PAR_COM_Q_INDEX%chain_order)
   !
   ! K-points
   !
   call PARALLEL_add_Q_to_K_list('k_qp_q_bz',PAR_IND_QP,PAR_IND_QP_ID,PAR_IND_Xk_ibz,PAR_IND_Xk_ibz_ID,&
&                                PAR_IND_Q,PAR_COM_QP_INDEX,(/0,0/),Xk,q)
   !
   ! G bands
   !
   if (ENVIRONMENT=="HF") n_bands=(/1,max(E%nbm,QP_nb)/)
   if (ENVIRONMENT=="GW") n_bands=(/QP_n_G_bands(1),max(QP_n_G_bands(2),QP_nb)/)
   !
   call PARALLEL_index(PAR_IND_QP_G_b,(/n_bands(2)/),low_range=(/n_bands(1)/),&
&                      COMM=PAR_COM_QP_G_b_INDEX,CONSECUTIVE=.TRUE.)
   PAR_IND_QP_G_b_ID=PAR_COM_QP_G_b_INDEX%CPU_id
   !
   call PARALLEL_live_message("K(ibz)",LOADED=PAR_IND_Xk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1),TOTAL=Xk%nibz)
   call PARALLEL_live_message("G bands",&
&                             LOADED=PAR_IND_QP_G_b%n_of_elements(PAR_IND_QP_G_b_ID+1),&
&                             TOTAL=n_bands(2)-n_bands(1)+1,NCPU=PAR_COM_QP_G_b_INDEX%n_CPU,&
&                             CHAIN=PAR_COM_QP_G_b_INDEX%chain_order)
   !
   ! I/O privileges
   !
   if (PARALLEL_n_structures_active>1) then
     call IO_and_Messaging_switch("+io_out +output",CONDITION=master_cpu)
   else
     call IO_and_Messaging_switch("+io_out",CONDITION=.TRUE.)
   endif
   !
 endif
 !
 if (ENVIRONMENT=="Response_G_space_Zero_Momentum".or.ENVIRONMENT=="Response_G_space_Finite_Momentum".or.&
&    ENVIRONMENT=="BSE") then
   !
   if (ENVIRONMENT=="BSE") then
     n_bands= (/E%nbf+1,BS_bands(2)/)
   else
     n_bands= (/E%nbf+1,X%ib(2)/)
   endif
   !
   ! Response functions conduction bands
   !
   call PARALLEL_index(PAR_IND_CON_BANDS_X(X_type),(/n_bands(2)/),low_range=(/n_bands(1)/),&
&                      COMM=PAR_COM_CON_INDEX(X_type),CONSECUTIVE=.TRUE.)
   PAR_IND_CON_BANDS_X_ID(X_type)=PAR_COM_CON_INDEX(X_type)%CPU_id
   !
   call PARALLEL_live_message("CON bands",&
&                             LOADED=PAR_IND_CON_BANDS_X(X_type)%n_of_elements(PAR_COM_CON_INDEX(X_type)%CPU_id+1),&
&                             TOTAL=n_bands(2)-n_bands(1)+1,&
&                             NCPU=PAR_COM_CON_INDEX(X_type)%n_CPU,CHAIN=PAR_COM_CON_INDEX(X_type)%chain_order)
   !
   !
   ! Response functions valence bands
   !
   if (ENVIRONMENT=="BSE") then
     n_bands= (/BS_bands(1),E%nbm/)
   else
     n_bands= (/X%ib(1),E%nbm/)
   endif
   !
   call PARALLEL_index(PAR_IND_VAL_BANDS_X(X_type),(/n_bands(2)/),low_range=(/n_bands(1)/),&
&                      COMM=PAR_COM_VAL_INDEX(X_type),CONSECUTIVE=.TRUE.)
   PAR_IND_VAL_BANDS_X_ID(X_type)=PAR_COM_VAL_INDEX(X_type)%CPU_id
   !
   call PARALLEL_live_message("VAL bands",&
&                             LOADED=PAR_IND_VAL_BANDS_X(X_type)%n_of_elements(PAR_COM_VAL_INDEX(X_type)%CPU_id+1),&
&                             TOTAL=n_bands(2)-n_bands(1)+1,&
&                             NCPU=PAR_COM_VAL_INDEX(X_type)%n_CPU,CHAIN=PAR_COM_VAL_INDEX(X_type)%chain_order)
   !
 endif
 !
 contains
   !
   subroutine distribute_BZk_using_IBZk( )
     !
     allocate(PAR_Xk_ibz_index(Xk%nibz))
     PAR_Xk_ibz_index=0
     !
     PAR_IND_Xk_bz_ID=PAR_IND_Xk_ibz_ID
     !
     i_p=0
     !
     do i_k=1,Xk%nibz
       !
       if (PAR_IND_Xk_ibz%element_1D(i_k)) then
         i_p=i_p+1
         PAR_Xk_ibz_index(i_k)=i_p
       endif
       !
     enddo
     !
     ! K-points in the BZ 
     !
     allocate(PAR_IND_Xk_bz%element_1D(Xk%nbz))
     PAR_IND_Xk_bz%element_1D=.FALSE.
     !
     allocate(PAR_IND_Xk_bz%n_of_elements(PAR_COM_Xk_ibz_INDEX%n_CPU))
     PAR_IND_Xk_bz%element_1D=.FALSE.
     ! 
     allocate(PAR_Xk_bz_index(Xk%nbz))
     PAR_Xk_bz_index=0
     !
     i_p=0
     !
     do i_k_bz=1,Xk%nbz
       !
       i_k=Xk%sstar(i_k_bz,1)
       !
       if (PAR_IND_Xk_ibz%element_1D(i_k)) then
         i_p=i_p+1
         PAR_IND_Xk_bz%element_1D(i_k_bz)=.TRUE.
         PAR_Xk_bz_index(i_k_bz)=i_p
       else
         PAR_IND_Xk_bz%element_1D(i_k_bz)=.FALSE.
       endif
       !
     enddo
     !
     PAR_Xk_nbz=i_p
     !
     PAR_IND_Xk_bz%n_of_elements(PAR_IND_Xk_ibz_ID+1)=PAR_Xk_nbz
     !
   end subroutine
   !
end subroutine
