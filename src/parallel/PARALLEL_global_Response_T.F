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
subroutine PARALLEL_global_Response_T(Xk,X_type)
 !
 use R_lattice,     ONLY:bz_samp,nXkbz,nXkibz
 use interfaces,    ONLY:PARALLEL_index,PARALLEL_assign_chains_and_COMMs,PARALLEL_live_message
 use IO_m,          ONLY:IO_and_Messaging_switch
 use BS,            ONLY:BS_nT_at_k,BS_nT_grps,BS_res_ares_dim
 use openmp,        ONLY:n_threads_K,OPENMP_set_threads
 use parallel_m,    ONLY:master_cpu,COMM_copy,PAR_INDEX_copy,PAR_build_index, &
&                        PAR_n_c_bands,PAR_n_v_bands
 ! COMMUNICATORS
 use parallel_m,    ONLY:PAR_COM_eh_INDEX,PAR_COM_eh_A2A,PAR_COM_T_INDEX, &
&                        PAR_COM_Xk_ibz_INDEX,PAR_IND_T_ordered, &
&                        PAR_COM_Xk_ibz_A2A,PAR_COM_CON_INDEX_DIP,PAR_COM_VAL_INDEX_DIP,&
&                        PAR_COM_Xk_bz_INDEX,PAR_COM_Xk_bz_A2A
 ! IND
 use parallel_m,    ONLY:PAR_IND_Kk_ibz,PAR_IND_T_groups,PAR_IND_T_ordered, &
&                        PAR_IND_Xk_ibz,PAR_IND_Xk_bz,PAR_IND_eh,            &
&                        PAR_IND_CON_BANDS_DIP,PAR_IND_VAL_BANDS_DIP
 ! INDEX
 use parallel_m,    ONLY:PAR_Xk_ibz_index,PAR_Xk_bz_index
 ! DIMENSIONS
 use parallel_m,    ONLY:PAR_BS_nT_col_grps,PAR_Kk_nibz,PAR_Xk_nbz,PAR_Xk_nibz
 ! ID's
 use parallel_m,    ONLY:PAR_IND_Kk_ibz_ID,PAR_IND_Xk_ibz_ID,PAR_IND_Xk_bz_ID, &
&                        PAR_IND_CON_BANDS_DIP_ID,PAR_IND_VAL_BANDS_DIP_ID
 !
#include<memory.h>
 !
 type(bz_samp)        :: Xk
 integer              :: X_type
 !
 ! Work space
 !
 integer              :: i_k
 !
 CALL PARALLEL_structure(3,(/"k ","eh","t "/))
 !
 call PARALLEL_assign_chains_and_COMMs(3,COMM_index_1=PAR_COM_Xk_ibz_INDEX,&
&                                        COMM_index_2=PAR_COM_eh_INDEX,&
&                                        COMM_index_3=PAR_COM_T_INDEX,&
&                                        COMM_A2A_1=PAR_COM_Xk_ibz_A2A,&
&                                        COMM_A2A_2=PAR_COM_eh_A2A) 
 if (PAR_COM_eh_INDEX%n_CPU==1) then
   call COMM_copy(PAR_COM_Xk_ibz_A2A,PAR_COM_eh_A2A)
 endif
 !
 ! Dipoles are calculated using PAR_COM_Xk_ibz_INDEX, PAR_COM_eh_INDEX and PAR_COM_T_INDEX communicators
 !
 !call COMM_copy(PAR_COM_WORLD,PAR_COM_DIPk_ibz_INDEX)
 !call COMM_copy(PAR_COM_Xk_ibz_A2A,PAR_COM_DIPk_ibz_A2A)
 call COMM_copy(PAR_COM_eh_INDEX,PAR_COM_CON_INDEX_DIP)
 call COMM_copy(PAR_COM_T_INDEX,PAR_COM_VAL_INDEX_DIP)
 call COMM_copy(PAR_COM_Xk_ibz_INDEX,PAR_COM_Xk_bz_INDEX)
 call COMM_copy(PAR_COM_Xk_ibz_A2A,PAR_COM_Xk_bz_A2A)
 !
 ! K-points (IBZ)
 !
 call PARALLEL_index(PAR_IND_Kk_ibz,(/nXkibz/),COMM=PAR_COM_Xk_ibz_INDEX,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
 PAR_IND_Xk_ibz_ID=PAR_COM_Xk_ibz_INDEX%CPU_id
 PAR_IND_Kk_ibz_ID=PAR_COM_Xk_ibz_INDEX%CPU_id
 PAR_Kk_nibz=PAR_IND_Kk_ibz%n_of_elements(PAR_IND_Kk_ibz_ID+1)
 !
 call PARALLEL_live_message("K(ibz)",ENVIRONMENT="Response_T_space",&
&       LOADED=PAR_IND_Kk_ibz%n_of_elements(PAR_IND_Kk_ibz_ID+1),TOTAL=nXkibz,&
&         NCPU=PAR_COM_Xk_ibz_INDEX%n_CPU)
 ! 
 ! Dipoles k-points uses same distribution of K k-points
 !
 call PAR_INDEX_copy(PAR_IND_Kk_ibz,PAR_IND_Xk_ibz)
 PAR_Xk_nibz  =PAR_Kk_nibz
 !
 !call PAR_INDEX_copy(PAR_IND_Kk_ibz,PAR_IND_DIPk_ibz)
 !PAR_IND_DIPk_ibz_ID=PAR_IND_Kk_ibz_ID
 !PAR_DIPk_nibz=PAR_Kk_nibz
 !call PAR_build_index(PAR_IND_DIPk_ibz,nXkibz,PAR_DIPk_ibz_index,PAR_DIPk_nibz)
 !
! if(.not.allocated(PAR_DIPk_bz_index)) then
!   call distribute_BZk_using_IBZk(PAR_COM_DIPk_ibz_INDEX,Xk,PAR_IND_DIPk_ibz,PAR_IND_DIPk_ibz_ID,&
!&                                                           PAR_IND_DIPk_bz, PAR_IND_DIPk_bz_ID,&
!&                                                           PAR_DIPk_bz_index,PAR_DIPk_nbz)
! endif
 !
 YAMBO_ALLOC(PAR_Xk_ibz_index,(nXkibz))
 call PAR_build_index(PAR_IND_Xk_ibz,nXkibz,PAR_Xk_ibz_index,PAR_Xk_nibz)
 YAMBO_ALLOC(PAR_Xk_bz_index,(nXkbz))
 call distribute_BZk_using_IBZk(PAR_COM_Xk_ibz_INDEX,Xk,PAR_IND_Xk_ibz,PAR_IND_Xk_ibz_ID,&
&                                                       PAR_IND_Xk_bz, PAR_IND_Xk_bz_ID,&
&                                                       PAR_Xk_bz_index,PAR_Xk_nbz)
 !
 ! E/h pairs (k resolved)
 !
 ! In this part I distribute the eh transitions within each k. The COMM for this indexing is PAR_COM_eh_INDEX.
 ! I fill the PAR_IND_eh for all k in order to define the total number of Transition groups
 !
 do i_k=1,nXkibz*BS_res_ares_dim
   !
   call PARALLEL_index(PAR_IND_eh(i_k),(/BS_nT_at_k(i_k)/),COMM=PAR_COM_eh_INDEX,CONSECUTIVE=.TRUE.,NO_EMPTIES=.FALSE.)
   !
 enddo
 !
 ! Now I find calculate the total (BS_nT_grps) and cpu-restricted (PAR_BS_nT_grps) number of Transition groups.
 ! In this case the PAR_BS_nT_grps groups belong only to the columns of the kernel.
 !
 call PARALLEL_Transitions_grouping(Xk)
 !
 call PARALLEL_live_message("(e/h) Groups",ENVIRONMENT="Response_T_space",LOADED=PAR_BS_nT_col_grps,TOTAL=BS_nT_grps,&
&                           NCPU=PAR_COM_eh_INDEX%n_CPU)
 !
 ! Now each CPU of the PAR_COM_eh_INDEX has PAR_BS_nT_grps  groups of e/h pairs
 !
 ! The task now is to distribute the transitions:
 !  
 ! Group@k (among BS_nT_grps) ->Group'@p (among BS_nT_grps)
 !
 call PARALLEL_index(PAR_IND_T_ordered,(/BS_nT_grps,BS_nT_grps/),COMM=PAR_COM_T_INDEX,&
&                    MASK=PAR_IND_T_groups%element_1D,ORDERED=.TRUE.,NO_EMPTIES=.FALSE.)
 !
 call PARALLEL_live_message("(e/h)->(e/h)' Transitions (ordered)",ENVIRONMENT="Response_T_space",&
&                           LOADED=PAR_IND_T_ordered%n_of_elements(PAR_COM_T_INDEX%CPU_id+1),&
&                           TOTAL=BS_nT_grps*(BS_nT_grps+1)/2,NCPU=PAR_COM_T_INDEX%n_CPU)
 !
 ! Linear Algebra setup moved in the solver_driver...
 !
 ! Conduction band
 !
 call PARALLEL_index(PAR_IND_CON_BANDS_DIP,(/PAR_n_c_bands(2)/),low_range=(/PAR_n_c_bands(1)/),&
&                     COMM=PAR_COM_CON_INDEX_DIP,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
 PAR_IND_CON_BANDS_DIP_ID=PAR_COM_CON_INDEX_DIP%CPU_id
 !   
 call PARALLEL_live_message("CON bands",ENVIRONMENT="Response_T_space",&
&                             LOADED=PAR_IND_CON_BANDS_DIP%n_of_elements(PAR_COM_CON_INDEX_DIP%CPU_id+1),&
&                             TOTAL=PAR_n_c_bands(2)-PAR_n_c_bands(1)+1,&
&                             NCPU=PAR_COM_CON_INDEX_DIP%n_CPU)
 !
 ! Response functions valence bands
 !
 call PARALLEL_index(PAR_IND_VAL_BANDS_DIP,(/PAR_n_v_bands(2)/),low_range=(/PAR_n_v_bands(1)/),&
&                    COMM=PAR_COM_VAL_INDEX_DIP,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
 PAR_IND_VAL_BANDS_DIP_ID=PAR_COM_VAL_INDEX_DIP%CPU_id
 !
 call PARALLEL_live_message("VAL bands",ENVIRONMENT="Response_T_space",&
&                           LOADED=PAR_IND_VAL_BANDS_DIP%n_of_elements(PAR_COM_VAL_INDEX_DIP%CPU_id+1),&
&                           TOTAL=PAR_n_v_bands(2)-PAR_n_v_bands(1)+1,&
&                           NCPU=PAR_COM_VAL_INDEX_DIP%n_CPU)
 !
 ! I/O privileges
 !
 call IO_and_Messaging_switch("+io_out",CONDITION=.TRUE.)
 !
 call IO_and_Messaging_switch("+output",CONDITION=master_cpu)
 !
 call OPENMP_set_threads(n_threads_in=n_threads_K)
 !  
end subroutine PARALLEL_global_Response_T
