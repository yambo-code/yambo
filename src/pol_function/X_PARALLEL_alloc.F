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
subroutine X_PARALLEL_alloc(X,E,k,W,NW,mode)
 !
 use pars,          ONLY:cZERO
 use R_lattice,     ONLY:bz_samp
 use IO_m,          ONLY:io_RESPONSE
 use electrons,     ONLY:levels
 use frequency,     ONLY:w_samp
 use parallel_m,    ONLY:COMM_copy,PAR_COM_RL_INDEX,PAR_nq
 use linear_algebra,ONLY:PAR_matrix_reset
 use X_m,           ONLY:X_t,X_par,iW_offset,X_par_lower_triangle,UPPER_X_matrix_only,X_mat
 !
#include<memory.h>
 !
 type(X_t)       :: X 
 type(levels)    :: E 
 type(bz_samp)   :: k
 type(w_samp)    :: W
 integer         :: NW
 character(*)    :: mode
 !
 ! Work Space
 !
 integer         :: X_size
 !
 call PAR_matrix_reset(X_par)
 call PAR_matrix_reset(X_par_lower_triangle)
 !
 call X_half_mat_check(E,k,W)
 !
 call COMM_copy(PAR_COM_RL_INDEX, X_par%COMM)
 !
 call PARALLEL_matrix_distribute( "X", X_par , X%ng )
 !
 if (mode=="DIELETRIC_MATRIX") then
   if (.not.io_RESPONSE) X_size=NW*PAR_nq
   if (     io_RESPONSE) X_size=NW
 else if (mode=="OPTICS") then
   X_size                   = NW
 endif
 !
 iW_offset = 0
 !
 if (.not.UPPER_X_matrix_only.and.X_par%COMM%n_CPU>1) then
   X_par_lower_triangle%rows =X_par%cols
   X_par_lower_triangle%cols =X_par%rows
   X_par_lower_triangle%nrows=X_par%ncols
   X_par_lower_triangle%ncols=X_par%nrows
   Y_ALLOCATE(X_par_lower_triangle%blc_3d(X_par%cols(1):X_par%cols(2),X_par%rows(1):X_par%rows(2),NW))
   Y_MEM(X_par_lower_triangle%blc_3d)
   X_par_lower_triangle%blc_3d=cZERO
 endif
 !
 Y_ALLOCATE(X_par%blc_3d(X_par%rows(1):X_par%rows(2),X_par%cols(1):X_par%cols(2),X_size))
 Y_MEM(X_par%blc_3d)
 X_par%blc_3d=cZERO
 !
 ! Back compatibility
 !
 Y_ALLOCATE(X_mat(X%ng,X%ng,X_size))
 Y_MEM(X_mat)
 X_mat=cZERO
 !
end subroutine
