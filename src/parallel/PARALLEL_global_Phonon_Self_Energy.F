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
 ! SCHEMES
 use parallel_m,    ONLY:PARs_PH_eh_bands,PARs_PH_Q_ibz,PARs_PH_K_bz 
 !
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
 call PARALLEL_assign_chains_and_COMMs(3,COMM_index_1=PARs_PH_Q_ibz%COMM_i,&
&                                        COMM_index_2=PARs_PH_K_bz%COMM_i,&
&                                        COMM_index_3=PARs_PH_eh_bands%COMM_i,&
&                                        COMM_A2A_1=PARs_PH_Q_ibz%COMM_a2a,&
&                                        COMM_A2A_2=PARs_PH_K_bz%COMM_a2a)
 !
 ! COMMs setup
 !
 ! The routine PARALLEL_assign_chains_and_COMMs cannot define COMMUNICATORS for internal
 ! A2A when there is no internal distribution
 !
 if (PARs_PH_eh_bands%COMM_i%n_CPU==1) call COMM_copy(PARs_PH_Q_ibz%COMM_a2a,PARs_PH_K_bz%COMM_a2a)
 !
 !.........................................................................
 !   "q" -> Q-points (IBZ)
 !.........................................................................
 call PARALLEL_index(PARs_PH_Q_ibz%IND,(/q%nibz/),COMM=PARs_PH_Q_ibz%COMM_i)
 PARs_PH_Q_ibz%ID=PARs_PH_Q_ibz%COMM_i%CPU_id
 PARs_PH_Q_ibz%N_par=PARs_PH_Q_ibz%IND%n_of_elements(PARs_PH_Q_ibz%ID+1)
 !
 YAMBO_ALLOC(PARs_PH_Q_ibz%table,(q%nibz))
 call PAR_build_index(PARs_PH_Q_ibz%IND,q%nibz,PARs_PH_Q_ibz%table,PARs_PH_Q_ibz%N_par)
 call PARALLEL_live_message("Q(ibz)",ENVIRONMENT="Phonon_Self_Energy",&
&                           LOADED=PARs_PH_Q_ibz%IND%n_of_elements(PARs_PH_Q_ibz%ID+1),TOTAL=q%nibz,&
&                           NCPU=PARs_PH_Q_ibz%COMM_i%n_CPU)
 !
 !.........................................................................
 ! Eh bands loop
 !.........................................................................
 call PARALLEL_MATRIX_distribute(PARs_PH_eh_bands%COMM_i,PARs_PH_eh_bands%IND,(/1,QP_PH_n_G_bands/),PAR_ID=ID)
 call PARALLEL_live_message("eh-bands",ENVIRONMENT="Phonon_Self_Energy",&
&         LOADED=PARs_PH_eh_bands%IND%n_of_elements(ID+1),TOTAL=QP_PH_n_G_bands**2,&
&         NCPU=PARs_PH_eh_bands%COMM_i%n_CPU)
 !.........................................................................
 ! K-points (BZ) for e-h pairs
 !.........................................................................
 call PARALLEL_index(PARs_PH_K_bz%IND,(/Xk%nbz/),COMM=PARs_PH_K_bz%COMM_i)
 PARs_PH_K_bz%ID=PARs_PH_K_bz%COMM_i%CPU_id
 !
 call PARALLEL_live_message("K(bz)",ENVIRONMENT="Phonon_Self_Energy",&
&         LOADED=PARs_PH_K_bz%IND%n_of_elements(PARs_PH_K_bz%ID+1),TOTAL=Xk%nbz,&
&         NCPU=PARs_PH_K_bz%COMM_i%n_CPU)
 !.........................................................................
 ! I/O
 !.........................................................................
 !
! call IO_and_Messaging_switch("+io_out",CONDITION=PAR_COM_Xk_ibz_INDEX%my_CHAIN==1.or.&
!&                                                 PAR_COM_Xk_ibz_INDEX%n_CPU==ncpu)
 !
end subroutine Parallel_global_Phonon_Self_Energy
