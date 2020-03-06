!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG, DS
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
