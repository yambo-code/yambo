!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine X_AVERAGE_do_it(what,X_par)
 !
 use pars,          ONLY:cZERO,SP
 use cuda_m,        ONLY:have_cuda
 use X_m,           ONLY:X_par_average,n_OPTICAL_dir_to_eval
 use matrix,        ONLY:PAR_matrix,MATRIX_reset
 !
 implicit none
 !
 character(*)     :: what
 type(PAR_matrix) :: X_par
 !
 if (what=="ACCUMULATE") then
   X_par_average%blc=X_par_average%blc+X_par%blc*1._SP/real(n_OPTICAL_dir_to_eval,SP)
   X_par%blc=cZERO
   if (have_cuda) then
     X_par_average%blc_d=X_par_average%blc_d+X_par%blc_d*1._SP/real(n_OPTICAL_dir_to_eval,SP)
     X_par%blc_d=cZERO
   endif
   return
 endif
 !
 if (what=="DUMP") then
   X_par%blc=X_par_average%blc
   if (have_cuda) X_par%blc_d=X_par_average%blc_d
   call MATRIX_reset(X_par_average)
   return
 endif
 !
end subroutine X_AVERAGE_do_it
