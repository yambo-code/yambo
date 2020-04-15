!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM,AF
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
module openmp
 !
#if defined _OPENMP
 use omp_lib,   ONLY:omp_lock_kind
#endif
 !
 implicit none
 !
 ! ... Threads
 !     the default value of 0 means that the actual number of threads is obtained
 !     from the environment var OMP_NUM_THREADS
 !     any other value given from input will overwrite the environment
 !
 integer     :: n_threads       = 0   ! Max number of threads possible
 integer     :: n_threads_limit = 0
 integer     :: n_threads_FFT   = 0    
 integer     :: n_threads_X     = 0    
 integer     :: n_threads_SE    = 0   
 integer     :: n_threads_RT    = 0  
 integer     :: n_threads_DIP   = 0  
 integer     :: n_threads_K     = 0  
 integer     :: n_threads_NL    = 0
 integer     :: n_threads_now   = 0
 logical     :: master_thread   = .TRUE.
 !
#if defined _OPENMP
 !
!$omp threadprivate(master_thread)
!$omp threadprivate(n_threads_now)
 !
 integer, external :: omp_get_num_threads
 integer, external :: omp_get_thread_num
 integer, external :: omp_get_thread_limit
 integer, external :: omp_get_max_threads
 logical, external :: omp_get_nested
 !
#endif
 !
 logical           :: omp_is_off
 !
 ! lock vars
 !
#if defined _OPENMP
 integer(omp_lock_kind), allocatable :: omp_locks(:)
#endif
 integer :: openmp_num_locks=0
 !
 ! nesting
 !
 integer       :: n_out_threads = 0
 integer       :: n_inn_threads = 0
 !
 character(25) :: s_outthr, s_innthr
 !
 contains
   !
   subroutine OPENMP_initialize( )
     !
#if !defined _OPENMP
     !
     n_threads     = 1
     n_threads_limit = 1
     n_threads_now = 1
     n_threads_FFT = 0
     n_threads_X   = 1
     n_threads_SE  = 1
     n_threads_RT  = 1
     n_threads_K   = 1
     n_threads_DIP = 1
     n_threads_NL  = 1
     !
     n_out_threads = 1
     n_inn_threads = 1
     !
#endif
     !
#if defined _OPENMP
     !
     if (omp_is_off) then
       !
       n_threads=1
       n_threads_now=1
       n_threads_limit=1
       !
     else
       !
       n_threads=omp_get_max_threads()
       n_threads_now=n_threads
       n_threads_limit=omp_get_thread_limit()
       !
       ! set defaults against OMP_THREAD_LIMIT being not set
       !
       if(n_threads_limit<0.or.n_threads_limit>1024) n_threads_limit=0
       !
     endif
     !
     call omp_set_dynamic(.false.)
     call omp_set_nested(.false.)
#endif
     !
   end subroutine
   !
   subroutine OPENMP_set_threads(n_threads_in,use_nested)
     !
     integer, optional :: n_threads_in
     logical, optional :: use_nested
     !
     logical :: use_nested_=.false.
     !
     if (omp_is_off) then
       !      
       if (     present(n_threads_in)) n_threads_in=1
       if (.not.present(n_threads_in)) n_threads=1
       !
       n_threads=1
       n_threads_now=1
       !
#if defined _OPENMP
       call omp_set_num_threads( n_threads_now )
#endif
       n_out_threads=1
       n_inn_threads=1
       !
     else
#if defined _OPENMP
       if (present(n_threads_in)) then
         if (n_threads_in==0) n_threads_in=n_threads
         call omp_set_num_threads( n_threads_in )
         n_threads_now=n_threads_in
       else
         call omp_set_num_threads( n_threads )
         n_threads_now=n_threads
       endif
       !
       n_out_threads=n_threads_now
       n_inn_threads=1
       !
#  if defined _NESTING
       if (present(use_nested)) use_nested_=use_nested
       !
       if (use_nested_) then
          call omp_set_dynamic(.true.)
          call omp_set_nested(.true.)
          call omp_set_max_active_levels(2)
          !     
          call getenv("YAMBO_OUTER_THREADS",s_outthr)
          call getenv("YAMBO_INNER_THREADS",s_innthr)
          !
          s_outthr=trim(s_outthr)
          s_innthr=trim(s_innthr)
          read(s_outthr,*,iostat=ierr) n_out_threads
          if (ierr/=0) n_out_threads=0
          read(s_innthr,*,iostat=ierr) n_inn_threads
          if (ierr/=0) n_in_threads=0
          !
       endif
#  endif 
       !
#else
       if (present(n_threads_in)) then
          n_threads_in=1
       else
          n_threads=1
       endif
       n_threads_now=1
       !
       n_out_threads=n_threads_now
       n_inn_threads=1
#endif
     endif
     !
   end subroutine
   !
   subroutine OPENMP_update(master_)
     !
     logical, optional :: master_
     logical :: master__
     !
     if (omp_is_off) then
       master__=.TRUE.
       n_threads_now=1
     else
#if defined _OPENMP
       master__= ( omp_get_thread_num() == 0)
       n_threads_now= omp_get_num_threads()
#else
       master__=.TRUE.
       n_threads_now=1       
#endif
     endif
     if (present(master_)) master_=master__
     !
   end subroutine
   !
   subroutine OPENMP_locks_reset(INIT,nlocks)
     implicit none
     ! 
     logical, optional :: INIT
     integer, optional :: nlocks
     !
     logical :: INIT_ = .false.
     integer :: nlocks_ = 16
     integer :: i
     !
     !if (omp_is_off) return
     !
#if defined _OPENMP
     if (present(INIT))   INIT_ = INIT
     if (present(nlocks)) nlocks_ = nlocks
     !
     if (allocated(omp_locks)) then
        openmp_num_locks=0
        do i = 1, size(omp_locks)
           call omp_destroy_lock(omp_locks(i))
        enddo
        deallocate(omp_locks)
     endif
     !
     if (INIT_) then
       openmp_num_locks=nlocks_
       allocate(omp_locks(nlocks_))
       do i = 1, nlocks_
         call omp_init_lock(omp_locks(i))
       enddo
     endif
#else
     return
#endif
     ! 
   end subroutine OPENMP_locks_reset
   ! 
   subroutine OPENMP_compute_mutex(id,mtx)
     implicit none
     integer :: id, mtx
     !
     mtx=mod(id,openmp_num_locks)+1
     !
     ! alternatives
     !mtx=i
     !mtx=IEOR(mtx, RSHIFT(mtx, 12))
     !mtx=IEOR(mtx, LSHIFT(mtx, 25))
     !mtx=IEOR(mtx, RSHIFT(mtx, 27))
     !mtx=IAND(mtx, potnum_locks - 1) + 1
     !
   end subroutine OPENMP_compute_mutex
   !
end module openmp
