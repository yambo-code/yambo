! License-Identifier: GPL
!
! Copyright (C) 2014 The Yambo Team
!
! Authors (see AUTHORS file for details): MG DS
!
subroutine PARALLEL_Haydock_VEC_COMMs(what)
 !
 ! Set up the mask and communicators for the Haydock part.  
 !
 use BS,          ONLY:BS_nT_grps
 use parallel_m,  ONLY:PAR_IND_T_groups,PAR_IND_T_Haydock,&
&                      PAR_COM_T_Haydock,PAR_COM_WORLD,CREATE_the_COMM,ncpu
#include<memory.h>
 !
 character(*),       intent(in) :: what     
 !
 ! Work Space
 !
 integer :: i_g,local_key,ierr
 !
 select case (what)
   case('assign')
     !
#if defined _MPI
     !
     ! Communicators along each groups (this will be row of the _full_ matrix)
     ! Needed in distributed M|V> and <V|W> (redux) and |Vn> initialization (bcast)
     ! Notice that here I need one different communicator for each group becasue
     ! the distribution of the groups is derived from the distribution of the matrix
     ! elements
     !
     allocate( PAR_COM_T_Haydock(BS_nT_grps))
     PAR_COM_T_Haydock(:)%n_CPU=0
     PAR_COM_T_Haydock(:)%chain_order=0
     !
     do i_g=1,BS_nT_grps
       !
       if (.not.PAR_IND_T_Haydock%element_1D(i_g)) then
         local_key=-1
         PAR_COM_T_Haydock(i_g)%my_CHAIN=BS_nT_grps+1
       else
         !
         local_key = 1
         if (PAR_IND_T_groups%element_1D(i_g)) local_key = 0
         !
         PAR_COM_T_Haydock(i_g)%n_CPU=PAR_COM_T_Haydock(i_g)%n_CPU+1
         PAR_COM_T_Haydock(i_g)%my_CHAIN = i_g
         !
       endif
       !
       call CREATE_the_COMM(PAR_COM_WORLD%COMM,PAR_COM_T_Haydock(i_g),local_key)
       !
     enddo
     !
#else
     !
     return
     !
#endif
     !
     case('reset')
       !
#if defined _MPI
       if(ncpu>1) then
         do i_g=1,BS_nT_grps
           call MPI_Comm_free(PAR_COM_T_Haydock(i_g)%COMM,ierr)
         enddo
       endif
       deallocate(PAR_COM_T_Haydock)
#endif
       !
   end select
   !
end subroutine PARALLEL_Haydock_VEC_COMMs
