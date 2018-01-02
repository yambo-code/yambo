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
module matrix
 !
 use pars,       ONLY:schlen,SP
 use parallel_m, ONLY:MPI_comm
 !
 implicit none
 !
 ! Distributed Matrix structure
 !
 integer, parameter   :: desc_len= 9
 logical              :: I_do_RCV=.TRUE.
 logical              :: I_do_SND=.TRUE.
 type(MPI_comm)       :: TRANSFER_world
 !
 ! PAR to PAR special COMMs
 !
 integer              :: N_MAX_PAR_to_PAR_comms=0
 integer              :: N_PAR_to_PAR_comms    =0
 character(schlen)    :: PAR_to_PAR_comm_NAME
 type(MPI_comm),    allocatable  :: PAR_to_PAR_comm_SAVE(:)
 character(schlen), allocatable  :: PAR_to_PAR_comm_SAVE_NAME(:)
 !
 type PAR_matrix
   !
   character(3) :: kind = "NAN" ! "PAR" "SLK" "XUP" "XDN" "SER"
   !
   ! Dimensions
   !
   integer :: I          = 1  ! Block element
   integer :: N          = -1 ! Global dimension (Parent Matrix)
   integer :: rows(2)    = 0  ! Real
   integer :: cols(2)    = 0  !
   integer :: nrows      = -1 ! 
   integer :: ncols      = -1 ! 
   !
   ! BLACS
   !
   integer :: BLCnrows      = -1 ! Dimension of the BLACS matrix (in general larger then the Real's)
   integer :: BLCncols      = -1 !
   integer :: BLCrows(2)    = 0  ! Dimension of the BLACS matrix (in general larger then the Real's)
   integer :: BLCcols(2)    = 0  !
   integer :: desc(desc_len)
   integer :: lwork      = -1
   integer :: lrwork     = -1
   integer :: liwork     = -1
   integer :: info       = -1  
   !
   ! COMMs
   !
   type(MPI_comm) :: INTRA_comm
   type(MPI_comm) :: INTER_comm 
   !
   ! DATA
   !
   integer :: Nb         = 1  ! # of Blocks
   !
   complex(SP), allocatable :: blc(:,:,:)
   !
 end type PAR_matrix 
 !
 contains
   !
   subroutine MATRIX_reset(M_par)
     use parallel_m, ONLY:COMM_reset
     type(PAR_matrix)   :: M_par
     !
     M_par%kind       = "NAN"
     !
     M_par%I          = 1
     M_par%N          = -1
     M_par%rows       = 0
     M_par%cols       = 0
     M_par%nrows      = -1
     M_par%ncols      = -1
     !
     M_par%BLCrows    = 0
     M_par%BLCcols    = 0
     M_par%BLCnrows   = -1
     M_par%BLCncols   = -1
     M_par%desc       = 0
     M_par%lwork      = -1
     M_par%lrwork     = -1
     M_par%liwork     = -1
     M_par%info       = -1
     !
     M_par%Nb         = 1
     !
     call COMM_reset(M_par%INTER_comm)
     call COMM_reset(M_par%INTRA_comm)
     !
   end subroutine
 !
end module matrix
