!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG
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
 use BS,          ONLY:BS_nT_grps,n_BS_blks,BS_blk,&
&                      Haydock_parallel_mask,Haydock_parallel_group_comm,&
&                      Haydock_parallel_col_comm    
#if defined _MPI
 use parallel_m,  ONLY:myid,mpi_comm_world,PAR_IND_T_groups
#endif
 implicit none
 character(*),       intent(in) :: what     
 !
 ! Work Space
 !
 integer :: i_b,iT_k,iT_p,i_g,i_err,dummy,iloc(1)
 integer :: local_color, local_key, local_undefined\
 !
 select case (what)
   case('assign')
     !
     if (.not.allocated(Haydock_parallel_mask)) &
&         allocate(Haydock_parallel_mask(BS_nT_grps))
     !
     ! This mask tells which vectors have to be stored on each processor
     !
     Haydock_parallel_mask = .false.
     do i_b=1,n_BS_blks
       iT_k = BS_blk(i_b)%iT_k
       iT_p = BS_blk(i_b)%iT_p
       if (.not.Haydock_parallel_mask(iT_k)) Haydock_parallel_mask(iT_k)=.true.
       if (.not.Haydock_parallel_mask(iT_p)) Haydock_parallel_mask(iT_p)=.true.
     end do
     !
     ! Communicators along each groups (this will be row of the _full_ matrix)
     ! Needed in distributed M|V> and <V|W> (redux) and |Vn> initialization (bcast)
     !
#if defined _MPI
     if (.not.allocated( Haydock_parallel_group_comm)) &
          &allocate( Haydock_parallel_group_comm(BS_nT_grps))
     if (.not.allocated(Haydock_parallel_col_comm)) &
          &allocate(Haydock_parallel_col_comm(BS_nT_grps))
     !
     local_undefined = BS_nT_grps*(1 + myid) + 1
     do i_g=1,BS_nT_grps
       local_key = 1
       if (PAR_IND_T_groups%element_1D(i_g)) local_key = 0
       if (Haydock_parallel_mask(i_g)) then
         local_color = i_g
       else
         local_color = local_undefined
         local_undefined = local_undefined + 1  
       endif
       !
#if defined _MPI
       call MPI_COMM_SPLIT(mpi_comm_world,local_color,local_key,Haydock_parallel_group_comm(i_g),i_err)
#endif
       !
     end do
     !
     ! Communicators along each column (this are the column of the _stored_ matrix, with i_Tk<=i_Tp)
     ! This structure is needed for broadcasting in the distributed dot_product. 
     !
     do i_g=1,BS_nT_grps 
       if (Haydock_parallel_mask(i_g)) then
         if (any(BS_blk(:)%iT_p==i_g)) then
           iloc=minloc(BS_blk(:)%iT_p,BS_blk(:)%iT_p==i_g) 
           local_color = i_g
           local_key = BS_blk(iloc(1))%iT_k
         else
           local_color = local_undefined
           local_key   = 0 
           local_undefined = local_undefined + 1  
         endif
       else
         local_color = local_undefined
         local_key   = 0
         local_undefined = local_undefined + 1
       endif
       !
#if defined _MPI
       call MPI_COMM_SPLIT(mpi_comm_world,local_color,local_key,Haydock_parallel_col_comm(i_g),i_err)
#endif
       !
     end do
#endif
     !
     case('reset')
       if (allocated(Haydock_parallel_mask)) deallocate(Haydock_parallel_mask)
#if defined _MPI
       if (allocated(Haydock_parallel_group_comm)) then
       !
#if defined _MPI
         call MPI_Comm_free(Haydock_parallel_group_comm)
#endif
         !
         deallocate(Haydock_parallel_group_comm)
       end if
       !
       if (allocated(Haydock_parallel_col_comm)) then
         !  
#if defined _MPI
         call MPI_Comm_free(Haydock_parallel_col_comm)
#endif
         !
         deallocate(Haydock_parallel_col_comm)
       end if
#endif
       !
   end select
   !
end subroutine PARALLEL_Haydock_VEC_COMMs
