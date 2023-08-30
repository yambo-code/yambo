!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PARALLEL_minimal_index_from_overlaping(IND_in,IND_out,PAR_COMM)
 !
 use parallel_m,     ONLY:PP_indexes,yMPI_comm,PAR_INDEX_copy
 use parallel_int,   ONLY:PP_redux_wait
 use vec_operate,    ONLY:sort
 !
 implicit none
 !
 type(PP_indexes)       ::IND_in,IND_out
 type(yMPI_comm)         ::PAR_COMM
 !
 ! Work Space
 !
 integer               :: i1,D_,i2,i3,i_cpu,ID_,NC_
 integer,  allocatable :: I_tmp(:),J_indx(:),J_tmp(:)
 !
 D_ =size( IND_in%element_1D)
 ID_=PAR_COMM%CPU_id
 NC_=PAR_COMM%n_CPU
 allocate(I_tmp(NC_),J_indx(NC_),J_tmp(NC_))
 !
 call PAR_INDEX_copy(IND_in,IND_out)
 !
 IND_out%element_1D(:)=.FALSE.
 IND_out%n_of_elements(ID_+1)=0
 !
 do i2=1,D_
   !
   J_tmp=0
   I_tmp=0
   i_cpu=0
   !
   J_tmp(ID_+1)=count( IND_out%element_1D(:) )
   if (IND_in%element_1D(i2)) I_tmp(ID_+1)=1
   !
   call PP_redux_wait(I_tmp,COMM=PAR_COMM%COMM)
   call PP_redux_wait(J_tmp,COMM=PAR_COMM%COMM)
   !
   call sort(J_tmp,indx=J_indx)
   !
   do i1=1,NC_
     i3=J_indx(i1)
     if (I_tmp(i3)>0.and.i_cpu==0) i_cpu=i3
   enddo
   !
   do i1=1,NC_
     if ((ID_+1)==i_cpu) IND_out%element_1D(i2)=.TRUE.
   enddo
   !
 enddo
 !
 IND_out%n_of_elements(ID_+1)=count(IND_out%element_1D)
 !
 deallocate(I_tmp,J_indx,J_tmp)
 !
end subroutine PARALLEL_minimal_index_from_overlaping
