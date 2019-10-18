!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine PARALLEL_FREQS_setup(W,IND,COMM)
 !
 use frequency,   ONLY:w_samp
 use parallel_m,  ONLY:PP_indexes,MPI_comm,PAR_IND_FREQ_ID,PAR_n_freqs,PP_indexes_reset,&
&                      PAR_build_index,PAR_FREQS_index
 use interfaces,  ONLY:PARALLEL_index,PARALLEL_live_message
 !
#include<memory.h>
 !
 type(w_samp)    :: W
 type(PP_indexes):: IND
 type(MPI_comm)  :: COMM
 !
 call PP_indexes_reset(IND)
 !
 ! ... and frequencies distribution
 !
 call PARALLEL_index(IND,(/W%n_freqs/),COMM=COMM,CONSECUTIVE=.TRUE.)
 PAR_IND_FREQ_ID=COMM%CPU_id+1
 call PARALLEL_live_message("X Frequencies",LOADED=IND%n_of_elements(PAR_IND_FREQ_ID),&
&                                           TOTAL=W%n_freqs,NCPU=COMM%n_CPU)
 PAR_n_freqs=IND%n_of_elements(PAR_IND_FREQ_ID)
 !
 if (PAR_n_freqs==0) return
 !
 YAMBO_ALLOC(PAR_FREQS_index,(PAR_n_freqs))
 call PAR_build_index(IND,PAR_n_freqs,PAR_FREQS_index,PAR_n_freqs)
 !
end subroutine
