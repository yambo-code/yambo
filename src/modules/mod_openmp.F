!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
module OPENMP
 !
 implicit none
 !
 ! ... Threads
 !     the default value of 0 means that the actual number of threads is obtained
 !     from the environment var OMP_NUM_THREADS
 !     any other value given from input will overwrite the environment
 !
 integer     :: n_threads = 0
 integer     :: n_threads_now = 0
 logical     :: master_thread = .TRUE.
 !
#if defined _OPENMP
 !
 integer, external :: omp_get_num_threads
 integer, external :: omp_get_thread_num
 integer, external :: omp_get_max_threads
 !
#endif
 !
 contains
   !
   subroutine OPENMP_initialize(DEEP_initialize)
     !
     logical :: DEEP_initialize
     !
     ! if threads are set from input pass the info to the omp env
     !
#if defined _OPENMP
     if ( n_threads > 0 ) then 
       call omp_set_num_threads( n_threads )
     else
       n_threads=omp_get_max_threads()
     endif
     !
     n_threads_now=1
     !
     if (.not.DEEP_initialize) return
     !
     call omp_set_dynamic(.false.)
     call omp_set_nested(.false.)
#else
     !
     n_threads=1
     n_threads_now=1
     !
#endif
   end subroutine
   !
   subroutine OPENMP_update()
     n_threads_now=n_threads
#if defined _OPENMP
     n_threads_now =   omp_get_num_threads()
     master_thread = ( omp_get_thread_num() == 0)
#endif
   end subroutine
   !
end module openmp
