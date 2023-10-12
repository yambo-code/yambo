!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PARALLEL_WF_index(COMM)
 !
 ! Here I used states_to_load to define the WF_linear index.
 ! I avoid overlaps so that those two indexes can be used at any time
 ! to loop on the total k and b in the yMPI_comm_world
 !
 use parallel_m,      ONLY:PP_indexes,ncpu,PAR_IND_WF_linear,&
&                          PP_indexes_reset,myid,PAR_COM_WORLD,yMPI_comm,&
&                          COMM_copy,PAR_COM_density
 use wave_func,       ONLY:states_to_load
 use parallel_int,    ONLY:PP_redux_wait
#include<memory.h>
 !
 type(yMPI_comm), optional :: COMM
 !
 ! Work Space
 !
 integer              :: ik,ib,NS,NK,NB,NC,ID
 type(PP_indexes)     :: WF_linear,WF_linear_no_overlaps
 integer, allocatable :: b_filling(:,:)
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
 NS=size(states_to_load,3)
 !
 YAMBO_ALLOC(WF_linear%element_1D,(NB*NK))
 YAMBO_ALLOC(WF_linear%n_of_elements,(NC))
 YAMBO_ALLOC(PAR_IND_WF_linear%element_2D,(NB,NK))
 !
 WF_linear%element_1D=.FALSE.
 PAR_IND_WF_linear%element_2D=.FALSE.
 !
 YAMBO_ALLOC(b_filling,(NB,NK))
 b_filling(:,:)=0
 !
 ! In some cases there could be holes in the states filling.
 ! In these cases not all bands and k-points are available and the minimal
 ! overalapping set can have holes as well. The following lines fill those holes.
 !
 do ik=1,NK
   do ib=1,NB
     if (states_to_load(ib,ik,1)) b_filling(ib,ik)=1
   enddo
 enddo
 !
 call PP_redux_wait(b_filling,COMM=PAR_COM_density%COMM)
 !
 do ik=1,NK
   do ib=1,NB
     if (b_filling(ib,ik)==0.and.PAR_COM_density%CPU_id==0)  states_to_load(ib,ik,:)=.TRUE.
     !if (states_to_load(ib,ik,1)) WF_linear%element_1D(ib+(ik-1)*NB)=.TRUE.                           ! this should be more correct
     if (states_to_load(ib,ik,1)) WF_linear%element_1D(NK*(ib-1)+ik)=.TRUE.
   enddo
 enddo
 !
 YAMBO_FREE(b_filling)
 !
 call PARALLEL_minimal_index_from_overlaping(WF_linear,WF_linear_no_overlaps,PAR_COM_density)
 !
 do ik=1,NK
   do ib=1,NB
     !if (WF_linear_no_overlaps%element_1D(ib+(ik-1)*NB)) PAR_IND_WF_linear%element_2D(ib,ik)=.TRUE.   ! this should be more correct
     if (WF_linear_no_overlaps%element_1D(NK*(ib-1)+ik)) PAR_IND_WF_linear%element_2D(ib,ik)=.TRUE.
   enddo
 enddo
 !
 YAMBO_FREE(WF_linear%element_1D)
 YAMBO_FREE(WF_linear%n_of_elements)
 call PP_indexes_reset(WF_linear_no_overlaps)
 !
end subroutine PARALLEL_WF_index
