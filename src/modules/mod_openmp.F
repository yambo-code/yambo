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
 integer     :: n_threads = 0        ! Max number of threads possible
 integer     :: n_threads_X  = 0    
 integer     :: n_threads_SE = 0   
 integer     :: n_threads_RT = 0  
 integer     :: n_threads_now = 0
 logical     :: master_thread=.TRUE.
 !
#if defined _OPENMP
 !
!$OMP threadprivate(master_thread)
 !
 integer, external :: omp_get_num_threads
 integer, external :: omp_get_thread_num
 integer, external :: omp_get_max_threads
 !
#endif
 !
 contains
   !
   subroutine OPENMP_initialize(DEEP_initialize,n_threads_in)
     !
     logical           :: DEEP_initialize
     integer, optional :: n_threads_in
     !
     ! if threads are set from input pass the info to the omp env
     !
#if defined _OPENMP
     if (present(n_threads_in)) then
       call omp_set_num_threads( n_threads_in )
       n_threads_now=n_threads_in
     else
       if ( n_threads > 0 ) then 
         call omp_set_num_threads( n_threads )
       else
         n_threads=omp_get_max_threads()
       endif
       n_threads_now=n_threads
     endif
     !
     if (.not.DEEP_initialize) return
     !
     n_threads_X  = n_threads
     n_threads_SE = n_threads   
     n_threads_RT = n_threads  
     !
     call omp_set_dynamic(.false.)
     call omp_set_nested(.false.)
#else
     !
     n_threads     = 1
     n_threads_now = 1
     n_threads_X   = 1
     n_threads_SE  = 1   
     n_threads_RT  = 1  
     if (present(n_threads_in)) then
       n_threads_in = 1
     endif
     !
#endif
     !
   end subroutine
   !
   subroutine OPENMP_update(master_)
     !
     logical :: master_
     integer :: threads_
     master_=.TRUE.
     n_threads_now=1
#if defined _OPENMP
     n_threads_now =   omp_get_num_threads()
     master_ = ( omp_get_thread_num() == 0)
#endif
     !
   end subroutine
   !
end module openmp
