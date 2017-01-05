!
!        Copyright (C) 2000-2017 the YAMBO team
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
module memory
 !
 use pars,          ONLY:SP,schlen
 use openmp,        ONLY:master_thread
 use iso_c_binding, ONLY:c_int
 !
 implicit none
 !
 !...parameters
 integer, parameter  :: N_MEM_max      = 10000
 integer, parameter  :: N_MEM_SAVE_max = 1000
 integer, parameter  :: MEM_jump_treshold   = 200000 ! Kb
 integer, parameter  :: MEM_treshold        = 1000 ! Kb
 integer, parameter  :: MEM_SAVE_treshold   = 5000  ! Kb
 !
 integer :: N_MEM_SAVE_elements      = 0
 integer :: N_MEM_elements           = 0
 integer :: MEM_err                  = 0
 integer(kind=c_int) :: TOT_MEM_kilobytes_MEMSTAT 
 integer             :: MAX_MEM_kilobytes_MEMSTAT = 0
 integer             :: TOT_MEM_kilobytes_MEMSTAT_SAVE
 integer             :: TOT_MEM_kilobytes
 integer             :: MEM_jump_save= 0
 !
 character(schlen) :: MEM_element_name(N_MEM_max) = " "
 integer           :: MEM_element_use (N_MEM_max) =0  
 character(schlen) :: MEM_element_name_SAVE(N_MEM_SAVE_max) = " "
 integer           :: MEM_element_use_SAVE (N_MEM_SAVE_max) =0  
 !
 interface
   !
   subroutine MEM_dri(what,Sz,Kn,Er,REPORT)
     character(*)         :: what
     integer,optional     :: Sz
     integer,optional     :: Kn
     integer,optional     :: Er
     character(*),optional:: REPORT
   end subroutine
   !
 end interface
 !
end module memory
