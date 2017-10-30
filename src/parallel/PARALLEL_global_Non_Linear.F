!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA MG DS
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
subroutine PARALLEL_global_Non_Linear(E,Xk,q,ENVIRONMENT)
 !
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use collision_ext, ONLY:HXC_collisions
 use matrix_operate,ONLY:UP_matrix_index
 use hamiltonian,   ONLY:B_mat_index
 use nl_optics,     ONLY:NL_bands
 use IO_m,          ONLY:IO_and_Messaging_switch
 use interfaces,    ONLY:PARALLEL_index,PARALLEL_assign_chains_and_COMMs,PARALLEL_live_message
 use parallel_m,    ONLY:ncpu,PAR_Q_range,PAR_n_bands,COMM_copy,PAR_index_copy,PAR_build_index
 ! COMMUNICATORS
 use parallel_m,    ONLY:PAR_COM_freqs_INDEX,PAR_COM_freqs_A2A,PAR_COM_freqs, &
&                        PAR_COM_Xk_ibz_INDEX,PAR_COM_G_b_A2A,PAR_COM_G_b_INDEX,PAR_COM_WF_k_INDEX,&
&                        PAR_COM_WF_b_INDEX,PAR_COM_DIPOLES_k_subgroup,PAR_COM_Q_INDEX,PAR_COM_Xk_ibz_A2A
 ! IND
 use parallel_m,    ONLY:PAR_IND_freqs,PAR_IND_OVLP_ibz,PAR_IND_OVLP_ibz,PAR_IND_Xk_ibz, &
&                        PAR_IND_G_b,PAR_IND_WF_b,PAR_IND_B_mat_ordered,PAR_IND_QP,PAR_IND_B_mat, &
&                        PAR_IND_DIPk_ibz,PAR_IND_Q,PAR_IND_WF_k,PAR_IND_Xk_bz
 ! INDEX
 use parallel_m,    ONLY:PAR_freqs_index,PAR_Xk_ibz_index,PAR_Xk_bz_index,PAR_DIPk_ibz_index, &
&                        PAR_G_bands_index,PAR_QP_index,PAR_G_bands_index
 ! DIMENSIONS
 use parallel_m,    ONLY:PAR_n_freqs,PAR_DIPk_nibz,PAR_nG_bands,PAR_nQP,PAR_Xk_nbz,PAR_Xk_nibz
 ! ID's
 use parallel_m,    ONLY:PAR_IND_freqs_ID,PAR_IND_G_b_ID,PAR_IND_Q_ID,PAR_IND_WF_b_ID,PAR_IND_WF_k_ID,&
&                        PAR_IND_Xk_bz_ID,PAR_IND_Xk_ibz_ID
 use nl_optics,     ONLY:n_frequencies
 use openmp,        ONLY:n_threads_NL,OPENMP_set_threads
 use QP_m,          ONLY:QP_n_states,QP_table
 !
 implicit none
 !
 type(levels)         :: E
 type(bz_samp)        :: Xk,q
 character(*)         :: ENVIRONMENT
 !
 ! Work space
 !
 integer      ::  i_qp,ib1,ib2,i_k
 !
 if (ENVIRONMENT=="Non_Linear") then
   !
   CALL PARALLEL_structure(4,(/"w ","k ","b ","q "/))
   !
   call PARALLEL_assign_chains_and_COMMs(3,COMM_index_1=PAR_COM_freqs_INDEX, &
&                                          COMM_index_2=PAR_COM_Xk_ibz_INDEX,&
&                                          COMM_index_3=PAR_COM_G_b_INDEX,   &
&                                          COMM_index_4=PAR_COM_Q_INDEX,     &
&                                          COMM_A2A_1=PAR_COM_freqs_A2A,     &
&                                          COMM_A2A_2=PAR_COM_Xk_ibz_A2A,    &
&                                          COMM_A2A_3=PAR_COM_G_b_A2A)
   !
   ! The routine PARALLEL_assign_chains_and_COMMs cannot define COMMUNICATORS for internal
   ! A2A when there is no internal distribution
   !
   if (PAR_COM_Xk_ibz_INDEX%n_CPU==1) then
     call COMM_copy(PAR_COM_freqs_A2A,PAR_COM_Xk_ibz_A2A)
   endif
   if (PAR_COM_G_b_A2A%n_CPU==1) then
     call COMM_copy(PAR_COM_Xk_ibz_A2A,PAR_COM_G_b_A2A)
   endif
   !
   !
   ! Freq-points
   !
   call PARALLEL_index(PAR_IND_freqs,(/n_frequencies/),COMM=PAR_COM_freqs_INDEX)
   PAR_IND_freqs_ID=PAR_COM_freqs_INDEX%CPU_id
   PAR_n_freqs     =PAR_IND_freqs%n_of_elements(PAR_IND_Q_ID+1)
   call PARALLEL_live_message("Freqs",ENVIRONMENT=ENVIRONMENT,         &
&                             LOADED=PAR_IND_freqs%n_of_elements(PAR_IND_freqs_ID+1), &
&                             TOTAL=n_frequencies,NCPU=PAR_COM_freqs_INDEX%n_CPU)
   !
   allocate(PAR_freqs_index(n_frequencies)) 
   call PAR_build_index(PAR_IND_freqs,n_frequencies,PAR_freqs_index,PAR_n_freqs)
   !
   ! K-points
   !
   call PARALLEL_index(PAR_IND_Xk_ibz,(/Xk%nibz/),COMM=PAR_COM_Xk_ibz_INDEX)
   PAR_IND_Xk_ibz_ID=PAR_COM_Xk_ibz_INDEX%CPU_id
   PAR_Xk_nibz=PAR_IND_Xk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1)
   !
   allocate(PAR_Xk_ibz_index(Xk%nibz))
   call PAR_build_index(PAR_IND_Xk_ibz,Xk%nibz,PAR_Xk_ibz_index,PAR_Xk_nibz)
   call PAR_INDEX_copy(PAR_IND_Xk_ibz,PAR_IND_DIPk_ibz)
   !
   ! Define the Xk_bz indeces for BZ parallelization
   ! 
   allocate(PAR_Xk_bz_index(Xk%nbz))
   call distribute_BZk_using_IBZk(PAR_COM_Xk_ibz_INDEX,Xk,PAR_IND_Xk_ibz,PAR_IND_Xk_ibz_ID,&
&                                                         PAR_IND_Xk_bz, PAR_IND_Xk_bz_ID,&
&                                                         PAR_Xk_bz_index,PAR_Xk_nbz)

   allocate(PAR_DIPk_ibz_index(Xk%nibz))
   call PAR_build_index(PAR_IND_DIPk_ibz,Xk%nibz,PAR_DIPk_ibz_index,PAR_DIPk_nibz)
   !
   ! Overlap indeces for WF distribution
   !
   call PAR_INDEX_copy(PAR_IND_DIPk_ibz,PAR_IND_OVLP_ibz)
   call PARALLEL_add_Q_to_K_list("overlaps",PAR_IND_Xk_bz,PAR_IND_Xk_bz_ID,PAR_IND_OVLP_ibz,PAR_IND_Xk_ibz_ID,&
&                                PAR_IND_Q,PAR_COM_Xk_ibz_INDEX,PAR_Q_range,Xk,q)
   !
   call PARALLEL_live_message("H(ibz)",ENVIRONMENT=ENVIRONMENT,&
&           LOADED=PAR_IND_Xk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1),TOTAL=Xk%nibz,&
&           NCPU=PAR_COM_Xk_ibz_INDEX%n_CPU)
   ! Bands
   ! 
   call PARALLEL_index(PAR_IND_G_b,(/NL_bands(2)/),low_range=(/NL_bands(1)/),&
&                      COMM=PAR_COM_G_b_INDEX,CONSECUTIVE=.TRUE.)
   PAR_IND_G_b_ID=PAR_COM_G_b_INDEX%CPU_id
   PAR_nG_bands=PAR_IND_G_b%n_of_elements(PAR_IND_G_b_ID+1)
   allocate(PAR_G_bands_index(PAR_n_bands(2)))
   call G_b_to_B_mat(PAR_COM_G_b_INDEX,'B',NL_bands)
   !
   ! Copy to WF
   !
   call PAR_INDEX_copy(PAR_IND_Xk_ibz,PAR_IND_WF_k)
   call COMM_copy(PAR_COM_Xk_ibz_INDEX,PAR_COM_WF_k_INDEX)
   PAR_IND_WF_k_ID=PAR_COM_WF_k_INDEX%CPU_id
   !
   call PAR_INDEX_copy(PAR_IND_G_b,   PAR_IND_WF_b)
   call COMM_copy(PAR_COM_G_b_INDEX,PAR_COM_WF_b_INDEX)
   PAR_IND_WF_b_ID=PAR_COM_WF_b_INDEX%CPU_id
   !
   ! Build Indexes
   !
   call PAR_build_index(PAR_IND_G_b,PAR_n_bands(2),PAR_G_bands_index,PAR_nG_bands)
   call PARALLEL_index(PAR_IND_B_mat_ordered,(/ UP_matrix_index(1,NL_bands(2)-NL_bands(1)+1)-1 /),&
&                      COMM=PAR_COM_WF_b_INDEX,CONSECUTIVE=.TRUE.)
   ! Non evolved bands loaded by all
   !
   PAR_IND_WF_b%element_1D(:max(NL_bands(1)-1,1))=.TRUE.
   !
   do ib1=NL_bands(1),NL_bands(2)
     do ib2=ib1,NL_bands(2)
       if (PAR_IND_B_mat_ordered%element_1D(  UP_matrix_index(ib1-NL_bands(1)+1,ib2-NL_bands(1)+1)-1 )) then
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
   !
   ! Build QP indexes for the Collisions
   !
   allocate(PAR_QP_index(QP_n_states),PAR_IND_QP%n_of_elements(ncpu),PAR_IND_QP%element_1D(QP_n_states)) 
   PAR_IND_QP%element_1D=.FALSE.
   PAR_IND_QP%n_of_elements=0
   PAR_nQP=0
   PAR_QP_index=0
   do i_qp=1,QP_n_states
     ib1 = QP_table(i_qp,1)
     ib2 = QP_table(i_qp,2)
     i_k = QP_table(i_qp,3)
     if (.not.PAR_IND_Xk_ibz%element_1D(i_k)) cycle
     if (.not.PAR_IND_B_mat%element_1D( B_mat_index(ib1,ib2,NL_bands) ) ) cycle
     PAR_nQP=PAR_nQP+1
     PAR_QP_index(i_qp)=PAR_nQP
     PAR_IND_QP%element_1D(i_qp)=.TRUE.
     PAR_IND_QP%n_of_elements=PAR_nQP
   enddo
   call PARALLEL_live_message("QPs",ENVIRONMENT=ENVIRONMENT,LOADED=PAR_nQP,TOTAL=QP_n_states)

   !.........................................................................
   !   "COLLISIONS"
   !.........................................................................
   !
   call PARALLEL_collisions( Xk,    HXC_collisions )
   ! 
   !
   ! Io and messaging
   !
   call COMM_copy(PAR_COM_freqs_INDEX,PAR_COM_DIPOLES_k_subgroup)
   call PARALLEL_live_message("SCbands",ENVIRONMENT=ENVIRONMENT,&
&                             LOADED=PAR_IND_G_b%n_of_elements(PAR_IND_G_b_ID+1),&
&                             TOTAL=PAR_n_bands(2)-PAR_n_bands(1)+1,NCPU=PAR_COM_G_b_INDEX%n_CPU)
   !
   !
   call IO_and_Messaging_switch("+io_out",CONDITION=PAR_COM_Xk_ibz_INDEX%CPU_id==0)
   !
   call IO_and_Messaging_switch("+output",CONDITION=PAR_COM_Xk_ibz_INDEX%CPU_id==0)
   !
   call OPENMP_set_threads(n_threads_in=n_threads_NL)
   !
 endif
 !
end subroutine PARALLEL_global_Non_Linear
