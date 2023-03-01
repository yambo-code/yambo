!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine PARALLEL_global_Response_T_base(Xk)
 !
 use R_lattice,     ONLY:bz_samp,nXkbz,nXkibz
 use parallel_int,  ONLY:PARALLEL_index,PARALLEL_assign_chains_and_COMMs,PARALLEL_live_message
 use openmp,        ONLY:n_threads_K,OPENMP_set_threads
 use parallel_m,    ONLY:COMM_copy,PAR_INDEX_copy,PAR_build_index
 ! COMMUNICATORS
 use parallel_m,    ONLY:PAR_COM_eh_INDEX,PAR_COM_eh_A2A,PAR_COM_T_INDEX, &
                         PAR_COM_Xk_ibz_INDEX, &
&                        PAR_COM_Xk_ibz_A2A,PAR_COM_CON_INDEX_DIP,PAR_COM_VAL_INDEX_DIP,&
&                        PAR_COM_Xk_bz_INDEX,PAR_COM_Xk_bz_A2A
 ! IND
 use parallel_m,    ONLY:PAR_IND_Kk_ibz,PAR_IND_Xk_ibz,PAR_IND_Xk_bz
 ! INDEX
 use parallel_m,    ONLY:PAR_Xk_ibz_index,PAR_Xk_bz_index
 ! DIMENSIONS
 use parallel_m,    ONLY:PAR_Kk_nibz,PAR_Xk_nbz,PAR_Xk_nibz
 ! ID's
 use parallel_m,    ONLY:PAR_IND_Kk_ibz_ID,PAR_IND_Xk_ibz_ID,PAR_IND_Xk_bz_ID
 !
#include<memory.h>
 !
 type(bz_samp)        :: Xk
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
 call PAR_INDEX_copy(PAR_IND_Kk_ibz,PAR_IND_Xk_ibz)
 PAR_Xk_nibz  =PAR_Kk_nibz
 !
 YAMBO_ALLOC(PAR_Xk_ibz_index,(nXkibz))
 call PAR_build_index(PAR_IND_Xk_ibz,nXkibz,PAR_Xk_ibz_index,PAR_Xk_nibz)
 YAMBO_ALLOC(PAR_Xk_bz_index,(nXkbz))
 call PARALLEL_distribute_BZk_using_IBZk(PAR_COM_Xk_ibz_INDEX,Xk,PAR_IND_Xk_ibz,PAR_IND_Xk_ibz_ID,&
&                                                       PAR_IND_Xk_bz, PAR_IND_Xk_bz_ID,&
&                                                       PAR_Xk_bz_index,PAR_Xk_nbz)
 !
 call OPENMP_set_threads(n_threads_in=n_threads_K)
 !
end subroutine PARALLEL_global_Response_T_base
