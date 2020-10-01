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
subroutine PARALLEL_global_Phonon_Self_Energy(E,Xk,q)
 !
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use ELPH,          ONLY:QP_PH_n_G_bands
 use interfaces,    ONLY:PARALLEL_index,PARALLEL_assign_chains_and_COMMs,PARALLEL_live_message
 use parallel_m,    ONLY:COMM_copy,PAR_build_index
 ! COMMUNICATORS
 use parallel_m,    ONLY:PAR_COM_G_b_INDEX,PAR_COM_Q_INDEX,PAR_COM_Xk_ibz_INDEX,    &
&                        PAR_COM_G_b_A2A,PAR_COM_Xk_ibz_A2A
 ! IND
 use parallel_m,    ONLY:PAR_IND_Xk_ibz,PAR_IND_Q_ibz
 ! INDEX
 use parallel_m,    ONLY:PAR_Xk_ibz_index,PAR_Q_ibz_index
 ! DIMENSIONS
 use parallel_m,    ONLY:PAR_nQ_ibz,PAR_Xk_nibz
 ! ID's
 use parallel_m,    ONLY:PAR_IND_Xk_ibz_ID,PAR_IND_Xk_ibz_ID,PAR_IND_Xk_ibz_ID,      &
&                        PAR_IND_Q_ibz_ID
#include<memory.h>
 !
 type(levels)         :: E
 type(bz_samp)        :: Xk,q
 !
 ! Work space
 !
 logical              :: CONSECUTIVE
 !
 CALL PARALLEL_structure(3,(/"k","b","q"/))
 !
 call PARALLEL_assign_chains_and_COMMs(3,COMM_index_1=PAR_COM_Xk_ibz_INDEX,&
&                                        COMM_index_2=PAR_COM_G_b_INDEX,&
&                                        COMM_index_3=PAR_COM_Q_INDEX,&
&                                        COMM_A2A_1=PAR_COM_Xk_ibz_A2A,&
&                                        COMM_A2A_2=PAR_COM_G_b_A2A)
 !
 ! COMMs setup
 !
 ! The routine PARALLEL_assign_chains_and_COMMs cannot define COMMUNICATORS for internal
 ! A2A when there is no internal distribution
 !
 if (PAR_COM_G_b_INDEX%n_CPU==1) then
   call COMM_copy(PAR_COM_Xk_ibz_A2A,PAR_COM_G_b_A2A)
 endif
 !
 ! K-points (IBZ)
 !
 call PARALLEL_index(PAR_IND_Xk_ibz,(/Xk%nibz/),COMM=PAR_COM_Xk_ibz_INDEX,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
 PAR_IND_Xk_ibz_ID=PAR_COM_Xk_ibz_INDEX%CPU_id
 YAMBO_ALLOC(PAR_Xk_ibz_index,(Xk%nibz))
 call PAR_build_index(PAR_IND_Xk_ibz,Xk%nibz,PAR_Xk_ibz_index,PAR_Xk_nibz)
 !
 call PARALLEL_live_message("K(ibz)",ENVIRONMENT="Phonon_Self_Energy",&
&         LOADED=PAR_IND_Xk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1),TOTAL=Xk%nibz,&
&         NCPU=PAR_COM_Xk_ibz_INDEX%n_CPU)
 !
 !.........................................................................
 ! X bands loop
 !.........................................................................
 call G_b_to_B_mat(PAR_COM_G_b_INDEX,'B',(/1,QP_PH_n_G_bands/))
 !
 !.........................................................................
 !   "q" -> Q-points (BZ)
 !.........................................................................
 call PARALLEL_index(PAR_IND_Q_ibz,(/q%nibz/),COMM=PAR_COM_Q_INDEX,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
 PAR_IND_Q_ibz_ID=PAR_COM_Q_INDEX%CPU_id
 PAR_nQ_ibz=PAR_IND_Q_ibz%n_of_elements(PAR_IND_Q_ibz_ID+1)
 !
 YAMBO_ALLOC(PAR_Q_ibz_index,(q%nibz))
 call PAR_build_index(PAR_IND_Q_ibz,q%nibz,PAR_Q_ibz_index,PAR_nQ_ibz)
 !
 call PARALLEL_live_message("Q(ibz)",ENVIRONMENT="Phonon_Self_Energy",&
&                           LOADED=PAR_IND_Q_ibz%n_of_elements(PAR_IND_Q_ibz_ID+1),TOTAL=q%nibz,&
&                           NCPU=PAR_COM_Q_INDEX%n_CPU)
 !
! call IO_and_Messaging_switch("+io_out",CONDITION=PAR_COM_Xk_ibz_INDEX%my_CHAIN==1.or.&
!&                                                 PAR_COM_Xk_ibz_INDEX%n_CPU==ncpu)
! !
! call OPENMP_set_threads(n_threads_in=n_threads_RT)
 !
end subroutine Parallel_global_Phonon_Self_Energy
