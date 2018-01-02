!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine PARALLEL_WF_index(COMM)
 !
 ! Here I used states_to_load to define the WF_linear index.
 ! I avoid overlaps so that those two indexes can be used at any time
 ! to loop on the total k and b in the MPI_comm_world
 !
 use parallel_m,      ONLY:PP_indexes,ncpu,PAR_IND_WF_linear,&
&                          PP_indexes_reset,myid,PAR_COM_WORLD,MPI_comm,&
&                          COMM_copy,PAR_COM_density
 use wave_func,       ONLY:states_to_load
 use parallel_m,      ONLY:PP_redux_wait
#include<memory.h>
 !
 type(MPI_comm), optional :: COMM
 !
 ! Work Space
 !
 integer              :: ik,ib,NB,NK,NC,ID
 type(PP_indexes)     :: WF_linear,WF_linear_no_overlaps
 integer, allocatable :: b_filling(:)
 !
 call PP_indexes_reset(PAR_IND_WF_linear)
 call PP_indexes_reset(WF_linear)
 call PP_indexes_reset(WF_linear_no_overlaps)
 !
 NC=ncpu
 ID=myid
 if (present(COMM)) then
   NC=COMM%n_CPU
   ID=COMM%CPU_id
   call COMM_copy(COMM,PAR_COM_density)
 else
   call COMM_copy(PAR_COM_WORLD,PAR_COM_density)
 endif
 !
 NB=size(states_to_load,1)
 NK=size(states_to_load,2)
 !
 YAMBO_ALLOC(WF_linear%element_1D,(NB*NK))
 YAMBO_ALLOC(WF_linear%n_of_elements,(NC))
 YAMBO_ALLOC(PAR_IND_WF_linear%element_2D,(NB,NK))
 !
 WF_linear%element_1D=.FALSE.
 PAR_IND_WF_linear%element_2D=.FALSE.
 !
 allocate(b_filling(NB))
 !
 do ik=1,NK
   !
   ! In some cases there could be holes in the states filling.
   ! In these cases not all bands and k-points are available and the minimal
   ! overalapping set can have holes as well. The following lines fill those holes.
   !
   do ib=1,NB
     b_filling(ib)=0
     if (states_to_load(ib,ik,1)) b_filling(ib)=1
   enddo
   !
   call PP_redux_wait(b_filling,COMM=PAR_COM_density%COMM)
   !
   do ib=1,NB
     if (b_filling(ib)==0.and.PAR_COM_density%CPU_id==0)  states_to_load(ib,ik,1)=.TRUE.
   enddo
   !
   do ib=1,NB
     if (states_to_load(ib,ik,1)) WF_linear%element_1D(NK*(ib-1)+ik)=.TRUE.
   enddo
 enddo
 !
 deallocate(b_filling)
 !
 call PARALLEL_minimal_index_from_overlaping(WF_linear,WF_linear_no_overlaps,PAR_COM_density)
 !
 do ik=1,NK
   do ib=1,NB
     if (WF_linear_no_overlaps%element_1D(NK*(ib-1)+ik)) PAR_IND_WF_linear%element_2D(ib,ik)=.TRUE.
   enddo
 enddo
 !
 YAMBO_FREE(WF_linear%element_1D)
 YAMBO_FREE(WF_linear%n_of_elements)
 call PP_indexes_reset(WF_linear_no_overlaps)
 !
end subroutine PARALLEL_WF_index
