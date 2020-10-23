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
 use parallel_int,  ONLY:PARALLEL_index,PARALLEL_assign_chains_and_COMMs,&
&                        PARALLEL_live_message,PARALLEL_MATRIX_distribute
 use parallel_m,    ONLY:COMM_copy,PAR_build_index
 ! COMMUNICATORS
 use parallel_m,    ONLY:PAR_PH_COM_eh_bands_INDEX,PAR_PH_COM_Q_ibz_INDEX,PAR_PH_COM_Xk_bz_INDEX,    &
&                        PAR_PH_COM_Xk_bz_A2A,PAR_PH_COM_Q_ibz_A2A
 ! IND
 use parallel_m,    ONLY:PAR_PH_IND_Xk_bz,PAR_PH_IND_Q_ibz,PAR_PH_IND_eh_bands
 ! INDEX
 use parallel_m,    ONLY:PAR_PH_Q_ibz_index
 ! DIMENSIONS
 use parallel_m,    ONLY:PAR_PH_nQ_ibz
 ! ID's
 use parallel_m,    ONLY:PAR_PH_IND_Xk_bz_ID,PAR_PH_IND_Q_ibz_ID
#include<memory.h>
 !
 type(levels)         :: E
 type(bz_samp)        :: Xk,q
 !
 ! Work space
 !
 integer :: ID
 !
 CALL PARALLEL_structure(3,(/"q","k","b"/))
 !
 call PARALLEL_assign_chains_and_COMMs(3,COMM_index_1=PAR_PH_COM_Q_ibz_INDEX,&
&                                        COMM_index_2=PAR_PH_COM_Xk_bz_INDEX,&
&                                        COMM_index_3=PAR_PH_COM_eh_bands_INDEX,&
&                                        COMM_A2A_1=PAR_PH_COM_Q_ibz_A2A,&
&                                        COMM_A2A_2=PAR_PH_COM_Xk_bz_A2A)
 !
 ! COMMs setup
 !
 ! The routine PARALLEL_assign_chains_and_COMMs cannot define COMMUNICATORS for internal
 ! A2A when there is no internal distribution
 !
 if (PAR_PH_COM_eh_bands_INDEX%n_CPU==1) call COMM_copy(PAR_PH_COM_Q_ibz_A2A,PAR_PH_COM_Xk_bz_A2A)
 !
 !.........................................................................
 !   "q" -> Q-points (IBZ)
 !.........................................................................
 call PARALLEL_index(PAR_PH_IND_Q_ibz,(/q%nibz/),COMM=PAR_PH_COM_Q_ibz_INDEX)
 PAR_PH_IND_Q_ibz_ID=PAR_PH_COM_Q_ibz_INDEX%CPU_id
 PAR_PH_nQ_ibz=PAR_PH_IND_Q_ibz%n_of_elements(PAR_PH_IND_Q_ibz_ID+1)
 !
 YAMBO_ALLOC(PAR_PH_Q_ibz_index,(q%nibz))
 call PAR_build_index(PAR_PH_IND_Q_ibz,q%nibz,PAR_PH_Q_ibz_index,PAR_PH_nQ_ibz)
 call PARALLEL_live_message("Q(ibz)",ENVIRONMENT="Phonon_Self_Energy",&
&                           LOADED=PAR_PH_IND_Q_ibz%n_of_elements(PAR_PH_IND_Q_ibz_ID+1),TOTAL=q%nibz,&
&                           NCPU=PAR_PH_COM_Q_ibz_INDEX%n_CPU)
 !
 !.........................................................................
 ! Eh bands loop
 !.........................................................................
 call PARALLEL_MATRIX_distribute(PAR_PH_COM_eh_bands_INDEX,PAR_PH_IND_eh_bands,(/1,QP_PH_n_G_bands/),PAR_ID=ID)
 call PARALLEL_live_message("eh-bands",ENVIRONMENT="Phonon_Self_Energy",&
&         LOADED=PAR_PH_IND_eh_bands%n_of_elements(ID+1),TOTAL=QP_PH_n_G_bands**2,&
&         NCPU=PAR_PH_COM_eh_bands_INDEX%n_CPU)
 !.........................................................................
 ! K-points (BZ) for e-h pairs
 !.........................................................................
 call PARALLEL_index(PAR_PH_IND_Xk_bz,(/Xk%nbz/),COMM=PAR_PH_COM_Xk_bz_INDEX)
 PAR_PH_IND_Xk_bz_ID=PAR_PH_COM_Xk_bz_INDEX%CPU_id
 !
 call PARALLEL_live_message("K(bz)",ENVIRONMENT="Phonon_Self_Energy",&
&         LOADED=PAR_PH_IND_Xk_bz%n_of_elements(PAR_PH_IND_Xk_bz_ID+1),TOTAL=Xk%nbz,&
&         NCPU=PAR_PH_COM_Xk_bz_INDEX%n_CPU)
 !.........................................................................
 ! I/O
 !.........................................................................
 !
! call IO_and_Messaging_switch("+io_out",CONDITION=PAR_COM_Xk_ibz_INDEX%my_CHAIN==1.or.&
!&                                                 PAR_COM_Xk_ibz_INDEX%n_CPU==ncpu)
 !
end subroutine Parallel_global_Phonon_Self_Energy
