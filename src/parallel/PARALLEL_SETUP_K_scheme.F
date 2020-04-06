!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine PARALLEL_SETUP_K_scheme(PAR_K_scheme_kind)
 !
 use parallel_m,   ONLY:COMM_copy,PAR_INDEX_copy,COMM_reset,PP_indexes_reset,PAR_K_scheme
 ! DIP
 use parallel_m,  ONLY:PAR_COM_DIPk_ibz_INDEX,PAR_COM_DIPk_ibz_A2A,PAR_IND_DIPk_ibz,PAR_IND_DIPk_bz,&
 &                     PAR_DIPk_ibz_index,PAR_DIPk_bz_index,PAR_DIPk_nibz,PAR_DIPk_nbz,             &
 &                     mpi_comm_world
 ! X
 use parallel_m,  ONLY:PAR_COM_Xk_ibz_INDEX,PAR_COM_Xk_ibz_A2A,PAR_IND_Xk_ibz,PAR_IND_Xk_bz,&
 &                     PAR_Xk_ibz_index,PAR_Xk_bz_index,PAR_Xk_nibz,PAR_Xk_nbz,             &
 &                     PAR_COM_X_WORLD
 !
 implicit none
 !
 character(*),  intent(in)    :: PAR_K_scheme_kind
 !
 call COMM_reset(PAR_K_scheme%COM_ibz_INDEX)
 call COMM_reset(PAR_K_scheme%COM_ibz_A2A)
 call PP_indexes_reset(PAR_K_scheme%IND_ibz)
 call PP_indexes_reset(PAR_K_scheme%IND_bz)
 if (allocated(PAR_K_scheme%ibz_index)) deallocate(PAR_K_scheme%ibz_index)
 if (allocated(PAR_K_scheme%bz_index))  deallocate(PAR_K_scheme%bz_index)
 !
 select case(trim(PAR_K_scheme_kind))
 case ("DIP","RT","SC")
   call COMM_copy(PAR_COM_DIPk_ibz_INDEX,PAR_K_scheme%COM_ibz_INDEX)
   call COMM_copy(PAR_COM_DIPk_ibz_A2A,  PAR_K_scheme%COM_ibz_A2A)
   call PAR_INDEX_copy(PAR_IND_DIPk_ibz,PAR_K_scheme%IND_ibz)
   call PAR_INDEX_copy(PAR_IND_DIPk_bz, PAR_K_scheme%IND_bz )
   PAR_K_scheme%nibz=PAR_DIPk_nibz
   PAR_K_scheme%nbz =PAR_DIPk_nbz
   if (allocated(PAR_DIPk_ibz_index)) then
     allocate(PAR_K_scheme%ibz_index(size(PAR_DIPk_ibz_index)))
     PAR_K_scheme%ibz_index=PAR_DIPk_ibz_index
   endif
   if (allocated(PAR_DIPk_bz_index)) then
     allocate(PAR_K_scheme%bz_index(size(PAR_DIPk_bz_index)))
     PAR_K_scheme%bz_index=PAR_DIPk_bz_index
   endif
   PAR_K_scheme%comm_world=mpi_comm_world
 case ("X","NL")
   call COMM_copy(PAR_COM_Xk_ibz_INDEX,PAR_K_scheme%COM_ibz_INDEX)
   call COMM_copy(PAR_COM_Xk_ibz_A2A,  PAR_K_scheme%COM_ibz_A2A)
   call PAR_INDEX_copy(PAR_IND_Xk_ibz,PAR_K_scheme%IND_ibz)
   call PAR_INDEX_copy(PAR_IND_Xk_bz, PAR_K_scheme%IND_bz )
   PAR_K_scheme%nibz=PAR_Xk_nibz
   PAR_K_scheme%nbz =PAR_Xk_nbz
   if (allocated(PAR_Xk_ibz_index)) then
     allocate(PAR_K_scheme%ibz_index(size(PAR_Xk_ibz_index)))
     PAR_K_scheme%ibz_index=PAR_Xk_ibz_index
   endif
   if (allocated(PAR_Xk_bz_index)) then
     allocate(PAR_K_scheme%bz_index(size(PAR_Xk_bz_index)))
     PAR_K_scheme%bz_index=PAR_Xk_bz_index
   endif
   PAR_K_scheme%comm_world=PAR_COM_X_WORLD%COMM
 case ("K","QED")
   call COMM_copy(PAR_COM_Xk_ibz_INDEX,PAR_K_scheme%COM_ibz_INDEX)
   call COMM_copy(PAR_COM_Xk_ibz_A2A,  PAR_K_scheme%COM_ibz_A2A)
   call PAR_INDEX_copy(PAR_IND_Xk_ibz,PAR_K_scheme%IND_ibz)
   call PAR_INDEX_copy(PAR_IND_Xk_bz, PAR_K_scheme%IND_bz )
   PAR_K_scheme%nibz=PAR_Xk_nibz
   PAR_K_scheme%nbz =PAR_Xk_nbz
   if (allocated(PAR_Xk_ibz_index)) then
     allocate(PAR_K_scheme%ibz_index(size(PAR_Xk_ibz_index)))
     PAR_K_scheme%ibz_index=PAR_Xk_ibz_index
   endif
   if (allocated(PAR_Xk_bz_index)) then
     allocate(PAR_K_scheme%bz_index(size(PAR_Xk_bz_index)))
     PAR_K_scheme%bz_index=PAR_Xk_bz_index
   endif
   PAR_K_scheme%comm_world=mpi_comm_world
 end select
 !
end subroutine PARALLEL_SETUP_K_scheme
