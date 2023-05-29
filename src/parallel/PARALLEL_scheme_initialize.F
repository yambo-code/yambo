!
!        Copyright (C) 2000-2023 the YAMBO team
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
subroutine PARALLEL_scheme_initialize(WHAT,ENVIRONMENT,PARALL_PARs,N,TABLE,MATRIX)
 !
 use parallel_m,   ONLY:PAR_scheme
 use parallel_int, ONLY:PARALLEL_live_message,PARALLEL_index,PARALLEL_MATRIX_distribute
 !
#include<memory.h>
 !
 character(*)               :: WHAT
 character(*)               :: ENVIRONMENT
 type(PAR_scheme)           :: PARALL_PARs
 integer                    :: N(:)
 logical, optional          :: TABLE
 logical, optional          :: MATRIX
 !
 ! Work Space
 !
 logical                   :: MATRIX_,TABLE_
 integer                   :: i_p
 !
 ! ... setup
 if (size(N)==1) then
   PARALL_PARs%D=(/1,N/)
 else
   PARALL_PARs%D=N
 endif
 TABLE_=.FALSE.
 if (present(TABLE)) then
   TABLE_=TABLE
 endif
 MATRIX_=.FALSE.
 if (present(MATRIX)) then
   MATRIX_=MATRIX
 endif
 !
 ! ... Parallel distribution
 if (MATRIX_) then
   call PARALLEL_MATRIX_distribute(PARALL_PARs%COMM_i,PARALL_PARs%IND,PARALL_PARs%D)
   PARALL_PARs%N_ser=(PARALL_PARs%D(2)-PARALL_PARs%D(1)+1)**2
 else
   call PARALLEL_index(PARALL_PARs%IND,(/PARALL_PARs%D(2)/),COMM=PARALL_PARs%COMM_i,&
&                      low_range=(/PARALL_PARs%D(1)/),CONSECUTIVE=PARALL_PARs%consecutive)
   PARALL_PARs%N_ser=PARALL_PARs%D(2)-PARALL_PARs%D(1)+1
 endif
 !
 PARALL_PARs%ID   =PARALL_PARs%COMM_i%CPU_id
 PARALL_PARs%N_par=PARALL_PARs%IND%n_of_elements(PARALL_PARs%ID+1)
 !
 if (TABLE_) then
   ! ... Table
   YAMBO_ALLOC(PARALL_PARs%table,(PARALL_PARs%D(1):PARALL_PARs%D(2)))
   PARALL_PARs%N_par=0
   do i_p=PARALL_PARs%D(1),PARALL_PARs%D(2)
     if (PARALL_PARs%IND%element_1D(i_p)) then
       PARALL_PARs%N_par=PARALL_PARs%N_par+1
       PARALL_PARs%table(i_p)=PARALL_PARs%N_par
     endif
   enddo
 endif
 !
 ! ... Message
 call PARALLEL_live_message(WHAT,ENVIRONMENT=ENVIRONMENT,LOADED=PARALL_PARs%N_par,&
&                           TOTAL=PARALL_PARs%N_ser,NCPU=PARALL_PARs%COMM_i%n_CPU)
 !
end subroutine
