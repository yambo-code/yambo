!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
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
