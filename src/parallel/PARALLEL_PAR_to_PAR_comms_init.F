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
subroutine PARALLEL_PAR_to_PAR_comms_init()
 !
 use parallel_m,    ONLY:COMM_reset,PAR_COM_SLK_INDEX_local
 use matrix,        ONLY:N_PAR_to_PAR_comms,PAR_to_PAR_comm_SAVE,PAR_to_PAR_comm_SAVE_NAME,&
&                        N_MAX_PAR_to_PAR_comms
 !
 implicit none
 !
 integer :: i_comm
 !
 ! PAR to PAR communicators
 !
 if (allocated(PAR_to_PAR_comm_SAVE_NAME)) then
   deallocate(PAR_to_PAR_comm_SAVE_NAME)
   do i_comm=1,N_MAX_PAR_to_PAR_comms
     call COMM_reset(PAR_to_PAR_comm_SAVE(i_comm))
   enddo  
   deallocate(PAR_to_PAR_comm_SAVE)
 endif
 !
 N_MAX_PAR_to_PAR_comms=PAR_COM_SLK_INDEX_local%n_CPU*5
 !                                                    |_ are the potential transfer structures
 !                                                       3  in X_redux, 1 in X_dielectric_matrix, 1 in BSE
 !
 allocate(PAR_to_PAR_comm_SAVE(N_MAX_PAR_to_PAR_comms))
 allocate(PAR_to_PAR_comm_SAVE_NAME(N_MAX_PAR_to_PAR_comms))
 PAR_to_PAR_comm_SAVE_NAME=" "
 N_PAR_to_PAR_comms=0
 !
end subroutine
