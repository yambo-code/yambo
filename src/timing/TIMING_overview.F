! 
!        Copyright (C) 2004 WanT Group
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, AF
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
subroutine TIMING_overview(list,main_name)
 !
 use pars,          ONLY:DP,lchlen,SP
 use timing_m,      ONLY:clock,clock_list,MAX_name_length,MAX_calls_length,nclockx
 use LIVE_t,        ONLY:LIVE_message
 use com,           ONLY:msg
 use stderr,        ONLY:log_as_a_file
 use parallel_m,    ONLY:ncpu,myid,master_cpu,PP_redux_wait,PP_bcast
 use vec_operate,   ONLY:sort
 !
 implicit none
 !
 TYPE(clock_list),       INTENT(in) :: list
 CHARACTER(*),           INTENT(in) :: main_name
 ! 
 ! Work Space
 !
 TYPE(clock_list)   :: list_all_cpu
 TYPE(clock)        :: tmp_clock(2)
 !
 ! clocks parallel collection
 !
 INTEGER            :: i,j,k,call_number(ncpu),i_cpu,n_clocks(ncpu),cpu_id(ncpu),bcast_id
 real(DP)           :: total_time(ncpu),min_max_time(2)
 character(lchlen)  :: clock_name
 !
 integer, allocatable :: sorted_times_index(:)
 real(SP),allocatable :: sorted_times(:)
 !
#ifndef _TIMING
 return
#endif
 IF ( .NOT. list%alloc ) CALL error('TIMING_overview:  list not allocated')
 !
 if (ncpu==1.or..not.master_cpu) then
   call msg('r','Clock: '//TRIM(list%name))
 else
   call msg('r','Clock: '//TRIM(list%name)//' (MAX - min (if any spread is present) clocks)')
 endif
 !
 IF ( list%nclock == 0 ) THEN
   if (log_as_a_file) call LIVE_message(message="[TIMING]"//'No clock to display',lfmt="n")
   call msg('r','No clock to display')
   RETURN
 ENDIF
 !
 if (ncpu>1) then
   !
   ! Parallel timing: Collect all clocks
   !=================
   !
   n_clocks=0
   n_clocks(myid+1)=list%nclock
   call PP_redux_wait(n_clocks)
   !
   allocate(list_all_cpu%clock(sum(n_clocks)))
   list_all_cpu%name=list%name
   list_all_cpu%nclock=sum(n_clocks)
   do i=1,list_all_cpu%nclock 
     list_all_cpu%clock(i)%name=" "
     list_all_cpu%clock(i)%total_time=0
     list_all_cpu%clock(i)%call_number=0
     list_all_cpu%clock(i)%cpu_id=0
     list_all_cpu%clock(i)%running=.TRUE.
     list_all_cpu%clock(i)%alloc=.TRUE.
     j=i-sum(n_clocks(:myid))
     if (j>0.and.j<=list%nclock) then
       list_all_cpu%clock(i)%name=list%clock(j)%name
       list_all_cpu%clock(i)%total_time=list%clock(j)%total_time
       list_all_cpu%clock(i)%call_number=list%clock(j)%call_number
       list_all_cpu%clock(i)%cpu_id=myid+1
     endif
   enddo
   !
   call PP_redux_wait(list_all_cpu%clock(:)%total_time)
   call PP_redux_wait(list_all_cpu%clock(:)%call_number)
   call PP_redux_wait(list_all_cpu%clock(:)%cpu_id)
   !
   do i=1,list_all_cpu%nclock 
     if (list_all_cpu%clock(i)%cpu_id>=0) bcast_id=list_all_cpu%clock(i)%cpu_id-1
     call PP_bcast(list_all_cpu%clock(i)%name,bcast_id)
   enddo
   !
 endif
 !
 ! Logs & Serial Report
 !======================
 MAX_name_length=0
 MAX_calls_length=0
 !
 allocate(sorted_times(list%nclock),sorted_times_index(list%nclock))
 !
 do i=1,list%nclock
   MAX_name_length=max(MAX_name_length,len_trim(list%clock(i)%name))
   MAX_calls_length=max( MAX_calls_length, int( log10(real(list%clock(i)%call_number)) ) )
   sorted_times(i)=list%clock(i)%total_time
 ENDDO
 call sort(arrin=sorted_times(:list%nclock),indx=sorted_times_index(:list%nclock))
 do i=1,list%nclock
   j=sorted_times_index(i)
   tmp_clock = list%clock(j)
   CALL TIMING_clock_write(tmp_clock,"LOG")
   if (ncpu==1) CALL TIMING_clock_write(tmp_clock,"REP")
 enddo
 !
 deallocate(sorted_times,sorted_times_index)
 !
 ! Parallel Report
 !=================
 if (.not.master_cpu.or.ncpu==1) return
 !
 allocate(sorted_times(list_all_cpu%nclock),sorted_times_index(list_all_cpu%nclock))
 !
 MAX_name_length=0
 MAX_calls_length=0
 DO i=1,list_all_cpu%nclock 
   MAX_name_length=max(MAX_name_length,len_trim(list_all_cpu%clock(i)%name))
   MAX_calls_length=max( MAX_calls_length, int( log10(real(list_all_cpu%clock(i)%call_number)) ) )
   sorted_times(i)=list_all_cpu%clock(i)%total_time
 ENDDO
 call sort(arrin=sorted_times(:list_all_cpu%nclock),indx=sorted_times_index(:list_all_cpu%nclock))
 !
 do i=1,list_all_cpu%nclock
   k=sorted_times_index(i)
   if (.not.list_all_cpu%clock(k)%running) cycle
   tmp_clock=list_all_cpu%clock(k)
   min_max_time=list_all_cpu%clock(k)%total_time
   do j=1,list_all_cpu%nclock
     if (j==k) cycle
     if (trim(list_all_cpu%clock(k)%name)==trim(list_all_cpu%clock(j)%name)) then
       list_all_cpu%clock(j)%running=.FALSE.
       if (list_all_cpu%clock(j)%total_time>min_max_time(2)) then
         min_max_time(2)=list_all_cpu%clock(j)%total_time
         tmp_clock(2)=list_all_cpu%clock(j)
       endif
       if (list_all_cpu%clock(j)%total_time<min_max_time(1)) then
         min_max_time(1)=list_all_cpu%clock(j)%total_time
         tmp_clock(1)=list_all_cpu%clock(j)
       endif
     endif
   enddo
   CALL TIMING_clock_write(tmp_clock,"REP")
 enddo
 !
 deallocate(sorted_times,sorted_times_index)
 !
end subroutine TIMING_overview
