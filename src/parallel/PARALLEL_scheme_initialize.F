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
subroutine PARALLEL_scheme_initialize(WHAT,ENVIRONMENT,PARs,N,TABLE,MATRIX)
 !
 use parallel_m,   ONLY:PAR_scheme,PAR_build_index
 use parallel_int, ONLY:PARALLEL_live_message,PARALLEL_index,PARALLEL_MATRIX_distribute
 !
#include<memory.h>
 !
 character(*)               :: WHAT
 character(*)               :: ENVIRONMENT
 type(PAR_scheme)           :: PARs
 integer                    :: N(:)
 logical, optional          :: TABLE
 logical, optional          :: MATRIX
 !
 ! Work Space
 !
 logical                   :: MATRIX_,TABLE_
 !
 ! ... setup
 if (size(N)==1) then
   PARs%D=(/1,N/)
 else
   PARs%D=N
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
   call PARALLEL_MATRIX_distribute(PARs%COMM_i,PARs%IND,PARs%D)
   PARs%N_ser=(PARs%D(2)-PARs%D(1)+1)**2
 else
   call PARALLEL_index(PARs%IND,(/PARs%D(2)/),COMM=PARs%COMM_i,low_range=(/PARs%D(1)/),CONSECUTIVE=PARs%consecutive)
   PARs%N_ser=PARs%D(2)-PARs%D(1)+1
 endif
 !
 PARs%ID   =PARs%COMM_i%CPU_id
 PARs%N_par=PARs%IND%n_of_elements(PARs%ID+1)
 !
 if (TABLE_) then
   ! ... Table
   YAMBO_ALLOC(PARs%table,(PARs%N_ser))
   call PAR_build_index(PARs%IND,PARs%N_ser,PARs%table,PARs%N_par)
 endif
 !
 ! ... Message
 call PARALLEL_live_message(WHAT,ENVIRONMENT=ENVIRONMENT,LOADED=PARs%N_par,TOTAL=PARs%N_ser,NCPU=PARs%COMM_i%n_CPU)
 !
end subroutine
