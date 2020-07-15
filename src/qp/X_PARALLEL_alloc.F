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
subroutine X_PARALLEL_alloc(X_par,NG,NW,mode)
 !
 use pars,          ONLY:cZERO
 use parallel_m,    ONLY:COMM_copy,PAR_COM_RL_INDEX,PAR_COM_RL_A2A
 use matrix,        ONLY:MATRIX_reset,PAR_matrix
 use X_m,           ONLY:X_par_lower_triangle,X_rows,X_cols,&
&                        X_FILL_UP_matrix_only,X_lower_triangle_matrix_in_use
 use interfaces,    ONLY:PARALLEL_live_message
 use cuda_m,        ONLY:have_cuda
 !
#include<memory.h>
 !
 type(PAR_matrix) :: X_par
 integer          :: NG,NW
 character(*)     :: mode
 !
 ! Work Space
 !
 logical :: l_XUP,l_PAR,l_CPU_ONLY
 !
 ! Reset...
 !
 call MATRIX_reset(X_par)
 !
 ! Type of allocation....
 !
 !l_XUP        =mode=="XUP"
 !l_PAR        =mode=="X"
 l_XUP      = index(mode,"XUP")>0
 l_PAR      = index(mode,"X")>0.and.index(mode,"XUP")==0
 l_CPU_ONLY = index(mode,"CPU_ONLY")>0
 !
 ! Clean-up
 !
 if (mode=="CLEAN") then
   YAMBO_FREE(X_par%blc)
   YAMBO_FREE(X_par%blc_d)
   call MATRIX_reset(X_par)
   return
 endif
 !
 ! Initialization
 !
 if (l_XUP) then
   call MATRIX_init( "XUP", X_par , NG, NW )
 else if (l_PAR) then
   call MATRIX_init( "X"  , X_par , NG, NW )
 endif
 !
 X_rows = X_par%rows
 X_cols = X_par%cols
 !
 if (l_XUP.and..not.X_FILL_UP_matrix_only.and.X_par%INTER_comm%n_CPU>1) then
   !
   call MATRIX_reset(X_par_lower_triangle)
   X_lower_triangle_matrix_in_use=.TRUE.
   X_par_lower_triangle%kind ="XDN"
   X_par_lower_triangle%rows =X_par%cols
   X_par_lower_triangle%cols =X_par%rows
   X_par_lower_triangle%nrows=X_par%ncols
   X_par_lower_triangle%ncols=X_par%nrows
   !
   YAMBO_ALLOC(X_par_lower_triangle%blc,(X_par%cols(1):X_par%cols(2),X_par%rows(1):X_par%rows(2),NW))
   X_par_lower_triangle%blc=cZERO
   if (have_cuda.and..not.l_CPU_ONLY) then
     YAMBO_ALLOC(X_par_lower_triangle%blc_d,(X_par%cols(1):X_par%cols(2),X_par%rows(1):X_par%rows(2),NW))
     X_par_lower_triangle%blc_d=cZERO
   endif
   !
   call COMM_copy(PAR_COM_RL_A2A,   X_par_lower_triangle%INTRA_comm)
   call COMM_copy(PAR_COM_RL_INDEX, X_par_lower_triangle%INTER_comm)
 endif
 !
 YAMBO_ALLOC(X_par%blc,(X_par%rows(1):X_par%rows(2),X_par%cols(1):X_par%cols(2),NW))
 X_par%blc=cZERO
 if (have_cuda.and..not.l_CPU_ONLY) then
   YAMBO_ALLOC(X_par%blc_d,(X_par%rows(1):X_par%rows(2),X_par%cols(1):X_par%cols(2),NW))
   X_par%blc_d=cZERO
 endif
 !
 call PARALLEL_live_message("RL vectors(X)",LOADED=X_par%ncols*X_par%nrows,TOTAL=NG**2,NCPU=X_par%INTER_comm%n_CPU)
 !
end subroutine
