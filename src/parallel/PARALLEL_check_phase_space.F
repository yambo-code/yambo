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
subroutine PARALLEL_check_phase_space(N_par,N,COMM,N_cpu,MSG)
 !
 use pars,        ONLY:schlen
 use parallel_m,  ONLY:ncpu,mpi_comm_world,PP_redux_wait,myid
 use stderr,      ONLY:intc
 use com,         ONLY:error
 !
 implicit none
 !
 integer                :: N_par,N
 integer,      optional :: COMM,N_cpu
 character(*), optional :: MSG
 ! 
 ! Work Space
 !
 integer              :: N_cpu_here,COMM_here,i_c
 integer, allocatable :: VALUES(:,:)
 character(schlen)    :: MSG_here="USER parallel structure does not fit the current run parameters"
 !
 N_cpu_here= ncpu
 COMM_here = mpi_comm_world
 !
 if (present(N_cpu)) then
   N_cpu_here=N_cpu
 endif
 if (present(COMM)) then
   COMM_here=COMM
 endif
 ! 
 allocate(VALUES(N_cpu_here,2))
 !
 VALUES=0
 VALUES(myid+1,:)=(/N_par,N/)
 !
 call PP_redux_wait(VALUES,COMM=COMM_here)
 !
 do i_c=1,N_cpu_here
   if (VALUES(i_c,1)==0) then
     if (present(MSG)) then
       call error(trim(MSG_here)//" :"//trim(MSG))
     else
       call error(trim(MSG_here)//" : NULL parallel world for (at least) CPU#"//trim(intc(i_c)))
     endif
   endif
 enddo
 !
 deallocate(VALUES)
 !
end subroutine PARALLEL_check_phase_space
