!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine PARALLEL_global_DIPOLES(E,Xk,q,ENVIRONMENT,X_type)
 !
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use linear_algebra,ONLY:INV
 use com,           ONLY:warning
 use IO_m,          ONLY:IO_and_Messaging_switch
 use openmp,        ONLY:n_threads_DIP,OPENMP_set_threads
 use interfaces,    ONLY:PARALLEL_index,PARALLEL_assign_chains_and_COMMs,PARALLEL_live_message
 use parallel_m,    ONLY:ncpu,PARALLEL_n_structures_active,COMM_copy,PAR_index_copy,PAR_build_index,PAR_n_c_bands,PAR_n_v_bands
 ! COMMUNICATORS
 use parallel_m,    ONLY:PAR_COM_DIPk_ibz_A2A,PAR_COM_DIPk_ibz_INDEX, &
&                        PAR_COM_CON_INDEX_DIP,PAR_COM_VAL_INDEX_DIP
 ! IND
 use parallel_m,    ONLY:PAR_IND_DIPk_ibz,PAR_IND_CON_BANDS_DIP,PAR_IND_OVLPk_ibz,&
&                        PAR_IND_VAL_BANDS_DIP,PAR_IND_DIPk_bz
 ! INDEX
 use parallel_m,    ONLY:PAR_DIPk_ibz_index,PAR_DIPk_bz_index
 ! DIMENSIONS
 use parallel_m,    ONLY:PAR_DIPk_nibz,PAR_DIPk_nbz
 ! ID's
 use parallel_m,    ONLY:PAR_IND_DIPk_bz_ID,PAR_IND_DIPk_ibz_ID,PAR_IND_OVLPk_ibz_ID,&
&                        PAR_IND_CON_BANDS_DIP_ID,PAR_IND_VAL_BANDS_DIP_ID
 use DIPOLES,       ONLY:eval_OVERLAPS
 use parallel_m,    ONLY:PAR_Q_range,PAR_IND_Q
 !
 implicit none
 !
 type(levels)         :: E
 type(bz_samp)        :: Xk,q
 character(*)         :: ENVIRONMENT
 integer              :: X_type
 !
 ! Work space
 !
 !
 !======================================================
 if (ENVIRONMENT=="DIPOLES") then
   !====================================================
   !
   call PARALLEL_structure(3,(/"k","c","v"/))
   !
   call PARALLEL_assign_chains_and_COMMs(3,COMM_index_1=PAR_COM_DIPk_ibz_INDEX,&
&                                          COMM_index_2=PAR_COM_CON_INDEX_DIP, &
&                                          COMM_index_3=PAR_COM_VAL_INDEX_DIP, &
&                                          COMM_A2A_1=PAR_COM_DIPk_ibz_A2A)
   !
   ! K-points (IBZ)
   !
   call PARALLEL_index(PAR_IND_DIPk_ibz,(/Xk%nibz/),COMM=PAR_COM_DIPk_ibz_INDEX,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
   PAR_IND_DIPk_ibz_ID=PAR_COM_DIPk_ibz_INDEX%CPU_id
   !
   ! ... indexes
   !
   allocate(PAR_DIPk_bz_index(Xk%nbz))
   call distribute_BZk_using_IBZk(PAR_COM_DIPk_ibz_INDEX,Xk,PAR_IND_DIPk_ibz,PAR_IND_DIPk_ibz_ID,&
&                                                         PAR_IND_DIPk_bz, PAR_IND_DIPk_bz_ID,&
&                                                         PAR_DIPk_bz_index,PAR_DIPk_nbz)
   if(eval_OVERLAPS) then
     call PARALLEL_add_Q_to_K_list("overlaps",PAR_IND_DIPk_bz,PAR_IND_DIPk_bz_ID,PAR_IND_OVLPk_ibz,PAR_IND_OVLPk_ibz_ID,&
&                                  PAR_IND_Q,PAR_COM_DIPk_ibz_INDEX,PAR_Q_range,Xk,q)
   endif
   
   !
   allocate(PAR_DIPk_ibz_index(Xk%nibz))
   call PAR_build_index(PAR_IND_DIPk_ibz,Xk%nibz,PAR_DIPk_ibz_index,PAR_DIPk_nibz)
   !
   call PARALLEL_live_message("K(ibz)",ENVIRONMENT=ENVIRONMENT,LOADED=PAR_DIPk_nibz,TOTAL=Xk%nibz,&
&                             NCPU=PAR_COM_DIPk_ibz_INDEX%n_CPU)
   !
   ! I/O privileges
   !
   if (PARALLEL_n_structures_active>1) then
     call IO_and_Messaging_switch("+io_out",CONDITION=PAR_COM_DIPk_ibz_INDEX%my_CHAIN==1.or.&
&                                                     PAR_COM_DIPk_ibz_INDEX%n_CPU==ncpu)
   else
     call IO_and_Messaging_switch("+io_out",CONDITION=.TRUE.)
   endif
   !
   call OPENMP_set_threads(n_threads_in=n_threads_DIP)
   !
   !
   ! Dipoles conduction bands
   !
   call PARALLEL_index(PAR_IND_CON_BANDS_DIP,(/PAR_n_c_bands(2)/),low_range=(/PAR_n_c_bands(1)/),&
&                      COMM=PAR_COM_CON_INDEX_DIP,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
   PAR_IND_CON_BANDS_DIP_ID=PAR_COM_CON_INDEX_DIP%CPU_id
   !
   call PARALLEL_live_message("CON bands",ENVIRONMENT=ENVIRONMENT,&
&                             LOADED=PAR_IND_CON_BANDS_DIP%n_of_elements(PAR_COM_CON_INDEX_DIP%CPU_id+1),&
&                             TOTAL=PAR_n_c_bands(2)-PAR_n_c_bands(1)+1,&
&                             NCPU=PAR_COM_CON_INDEX_DIP%n_CPU)
   !
   ! Dipoles valence bands
   !
   call PARALLEL_index(PAR_IND_VAL_BANDS_DIP,(/PAR_n_v_bands(2)/),low_range=(/PAR_n_v_bands(1)/),&
&                      COMM=PAR_COM_VAL_INDEX_DIP,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
   PAR_IND_VAL_BANDS_DIP_ID=PAR_COM_VAL_INDEX_DIP%CPU_id
   !
   call PARALLEL_live_message("VAL bands",ENVIRONMENT=ENVIRONMENT,&
&                             LOADED=PAR_IND_VAL_BANDS_DIP%n_of_elements(PAR_COM_VAL_INDEX_DIP%CPU_id+1),&
&                             TOTAL=PAR_n_v_bands(2)-PAR_n_v_bands(1)+1,&
&                             NCPU=PAR_COM_VAL_INDEX_DIP%n_CPU)
   !
 endif
 !
end subroutine PARALLEL_global_DIPOLES

