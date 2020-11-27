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
subroutine PARALLEL_global_Phonon_Self_Energy( )
 !
 use R_lattice,     ONLY:nqibz,nXkbz
 use ELPH,          ONLY:QP_PH_n_G_bands
 use parallel_int,  ONLY:PARALLEL_assign_chains_and_COMMs,PARALLEL_scheme_initialize
 use parallel_m,    ONLY:COMM_copy,PARs_PH_eh_bands,PARs_PH_Q_ibz,PARs_PH_K_bz,PAR_n_bands,&
&                        PAR_K_range,PAR_Q_ibz_range
 !
#include<memory.h>
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
 call PARALLEL_scheme_initialize("Q(ibz)","Phonon_Self_Energy",PARs_PH_Q_ibz,(/nqibz/),TABLE=.TRUE.)
 PAR_Q_ibz_range=nqibz
 !.........................................................................
 ! Eh bands loop
 !.........................................................................
 call PARALLEL_scheme_initialize("eh-bands","Phonon_Self_Energy",PARs_PH_eh_bands,&
&                                (/QP_PH_n_G_bands/),MATRIX=.TRUE.)
 PAR_n_bands=(/1,QP_PH_n_G_bands**2/)
 !.........................................................................
 ! K-points (BZ) for e-h pairs
 !.........................................................................
 call PARALLEL_scheme_initialize("K(bz)","Phonon_Self_Energy",PARs_PH_K_bz,(/nXkbz/),TABLE=.TRUE.)
 PAR_K_range=nXkbz
 !
end subroutine Parallel_global_Phonon_Self_Energy
