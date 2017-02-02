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
subroutine X_PARALLEL_alloc(X,NW,mode)
 !
 use pars,          ONLY:cZERO
 use IO_m,          ONLY:io_RESPONSE
 use parallel_m,    ONLY:COMM_copy,PAR_COM_RL_INDEX,PAR_nq,PAR_COM_RL_A2A
 use linear_algebra,ONLY:MATRIX_reset
 use X_m,           ONLY:X_t,X_par,iW_offset,X_par_lower_triangle,&
&                        X_FILL_UP_matrix_only,X_lower_triangle_matrix_in_use
 use interfaces,    ONLY:PARALLEL_live_message
 !
#include<memory.h>
 !
 type(X_t)       :: X 
 integer         :: NW
 character(*)    :: mode
 !
 ! Work Space
 !
 integer         :: X_size
 logical         :: l_XUP,l_PAR,l_ALL_in_MEM
 !
 ! Reset...
 !
 call MATRIX_reset(X_par)
 !
 ! Type of allocation....
 !
 l_XUP        =index(mode,"XUP")>0
 l_PAR        =index(mode,"XFULL")>0
 l_ALL_in_MEM =index(mode,"ALL_FREQUENCIES")>0
 !
 ! FREQs
 !
 X_size = NW
 if (l_ALL_in_MEM) then
   if (.not.io_RESPONSE) X_size=NW*PAR_nq
   if (     io_RESPONSE) X_size=NW
 endif
 !
 ! Initialization
 !
 if (l_XUP) then
   call MATRIX_init( "XUP", X_par , X%ng, X_size )
 else if (l_PAR) then
   call MATRIX_init( "X"  , X_par , X%ng, X_size )
 endif
 !
 iW_offset = 0
 !
 if (l_XUP.and..not.X_FILL_UP_matrix_only.and.X_par%INTER_comm%n_CPU>1) then
   call MATRIX_reset(X_par_lower_triangle)
   X_lower_triangle_matrix_in_use=.TRUE.
   X_par_lower_triangle%kind ="XDN"
   X_par_lower_triangle%rows =X_par%cols
   X_par_lower_triangle%cols =X_par%rows
   X_par_lower_triangle%nrows=X_par%ncols
   X_par_lower_triangle%ncols=X_par%nrows
   Y_ALLOCATE(X_par_lower_triangle%blc(X_par%cols(1):X_par%cols(2),X_par%rows(1):X_par%rows(2),NW))
   Y_MEM(X_par_lower_triangle%blc)
   X_par_lower_triangle%blc=cZERO
   call COMM_copy(PAR_COM_RL_A2A,   X_par_lower_triangle%INTRA_comm)
   call COMM_copy(PAR_COM_RL_INDEX, X_par_lower_triangle%INTER_comm)
 endif
 !
 Y_ALLOCATE(X_par%blc(X_par%rows(1):X_par%rows(2),X_par%cols(1):X_par%cols(2),X_size))
 Y_MEM(X_par%blc)
 X_par%blc=cZERO
 !
 call PARALLEL_live_message("RL vectors(X)",LOADED=X_par%ncols*X_par%nrows,TOTAL=X%ng**2,NCPU=X_par%INTER_comm%n_CPU)
 !
end subroutine
