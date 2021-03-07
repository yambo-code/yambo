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
subroutine PARALLEL_check_phase_space(N_PAR_elements,WORLD,WORLD_size,MSG)
 !
 use pars,        ONLY:schlen
 use parallel_m,  ONLY:ncpu,mpi_comm_world,myid
 use parallel_int,ONLY:PP_redux_wait
 use stderr,      ONLY:intc
 use LIVE_t,      ONLY:LIVE_message
 !
 implicit none
 !
 integer                :: N_PAR_elements
 integer,      optional :: WORLD,WORLD_size
 character(*), optional :: MSG
 ! 
 ! Work Space
 !
 integer              :: Ncpu_tot,COMM,i_c
 integer, allocatable :: VALUES(:)
 character(schlen)    :: MSG_here="USER parallel structure does not fit the current run parameters"
 !
 Ncpu_tot= ncpu
 COMM    = mpi_comm_world
 !
 if (present(WORLD_size)) then
   Ncpu_tot=WORLD_size
 endif
 if (present(WORLD)) then
   COMM=WORLD
 endif
 ! 
 allocate(VALUES(Ncpu_tot))
 !
 VALUES=0
 VALUES(myid+1)=N_PAR_elements
 !
 call PP_redux_wait(VALUES,COMM=COMM)
 !
 do i_c=1,Ncpu_tot
   if (VALUES(i_c)==0) then
     call LIVE_message(message="NULL parallel world for CPU#"//trim(intc(i_c)),CPU_TIME=.FALSE.)
     if (present(MSG)) then
       MSG_here=trim(MSG_here)//". "//trim(MSG)
     endif
     call error(trim(MSG_here))
   endif
 enddo
 !
 deallocate(VALUES)
 !
end subroutine PARALLEL_check_phase_space
