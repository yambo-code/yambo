!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM,AF
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
subroutine LINEAR_ALGEBRA_driver(idriver,lib_in,M,M_slk,M_par,M_bse,B,C,V_left,V_right,E_real,E_cmpl,&
&                                SVD_digits,det,V_par)
 !
 use pars,          ONLY:SP,cONE,cZERO
 use linear_algebra,ONLY:INV,DIAGO,LIN_SYS,SVD_INV,MAT_MUL,USE_LK,USE_SLK,&
&                        LINEAR_ALGEBRA_WS_clean_up,WS,LINEAR_ALGEBRA_error,&
&                        PAR_matrix,PAR_matrix_reset
 use wrapper,       ONLY:M_by_M
 use parallel_m,    ONLY:linear_algebra_is_parallel,ncpu
 use interfaces,    ONLY:SLK_matrix_transfer
 use BS,            ONLY:BS_block,BS_H_dim
 use com,           ONLY:warning
#if defined _SCALAPACK
 use SLK_m,         ONLY:SLK_ORTHO
#endif
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
#include<memory.h>
 !
 integer                    :: idriver
 integer,          optional :: lib_in
 complex(SP),      optional :: M(:,:)
 type(PAR_matrix), optional :: M_slk
 type(PAR_matrix), optional :: M_par
 type(BS_block),   optional :: M_bse(:)
 complex(SP),      optional :: B(:,:)
 complex(SP),      optional :: C(:,:)
 complex(SP),      optional :: V_left(:,:)
 complex(SP),      optional :: V_right(:,:)
 real(SP),         optional :: E_real(:)
 complex(SP),      optional :: E_cmpl(:)
 integer,          optional :: SVD_digits 
 complex(SP),      optional :: det
 type(PAR_matrix), optional :: V_par
 !
 ! Work Spaces ...
 !
 complex(SP)         :: det_
 integer             :: N,lib_in_use
 type (PAR_matrix)   :: M_loc_slk,B_slk,C_slk,EIGEN_slk
 !
#if defined _TIMING
 call timing("LINEAR ALGEBRA",OPR="start")
#endif
 !
 ! M dimension
 !
 if (present(M_slk)) then
   N=M_slk%N
 else if (present(M_bse)) then
   N=BS_H_dim
 else if (present(M)) then
   N=size(M,1)
 endif
 !
 ! Decide which to use among LK or SLK
 !
 lib_in_use=USE_LK
#if defined _SCALAPACK 
 lib_in_use=USE_SLK
#endif
 !
 if (present(lib_in)) then
   lib_in_use=lib_in
 endif
 if (idriver==SVD_INV.or.(idriver==DIAGO.and.present(E_cmpl))) lib_in_use=USE_LK 
 !
 if (.not.linear_algebra_is_parallel) lib_in_use=USE_LK
 !
 if (idriver==LIN_SYS.and..not.present(B)) then
   call LINEAR_ALGEBRA_error(" ",'B must be present for LIN_SYS')
 endif
 !
 if (idriver==MAT_MUL.and.(.not.present(B).or..not.present(C))) then
   call LINEAR_ALGEBRA_error(" ",'B,C must be present for MAT_MUL')
 endif
 !
#if defined _SCALAPACK
 if (idriver==DIAGO.and.present(E_cmpl).and.ncpu>1) then
   !
   call warning('Parallel non-herm diag not yet implemented. Switching to LK')
   lib_in_use=USE_LK
   !
 endif
 !
 ! =============== PARALLEL SECTION ==========================================================
 !
 if (lib_in_use==USE_SLK) then 
   if (present(M_slk)) then
     call SCALAPACK_section(M_slk,.FALSE.)
   else
     call SCALAPACK_section(M_loc_slk,.TRUE.)
   endif
 endif
 !
#endif
 !
 ! =============== SERIAL SECTION ============================================================
 !
 if (lib_in_use==USE_LK) then
   !
   ! ####> SVD INVERSION (Using Singular Value decomposition)
   !
   if (idriver==SVD_INV) then
     call SERIAL_SVD_inversion(N,M,SVD_digits)
   endif
   !
   ! ###> SERIAL INVERSION 
   !
   if (idriver==INV) then
     call SERIAL_inversion(N,M,det_,present(det))
     if(present(det)) det=det_
   endif
   !
   ! ###> SERIAL DIAGONALIZATION
   !
   if (idriver==DIAGO) then
     !
     if (present(E_cmpl)) then
       !
       !... non-Hermitian case
       call SERIAL_diagonalization(N,M,E_cmpl,V_left,V_right)
     else
       !
       !... Hermitian case
       call SERIAL_HERMITIAN_diagonalization(N,M,E_real)
     endif
     !
   endif
   !
   ! ####> LINEAR SYSTEM SOLUTION
   !
   if (idriver==LIN_SYS) then
     call SERIAL_lin_system(N,size(B,2),M,B)
   endif
   !
   ! ####> MAT_MUL
   ! C = M * B
   if (idriver==MAT_MUL) then
     call M_by_M('N','N',size(C,1),size(C,2),size(M,2),cONE,M,size(M,1),&
&                        B,size(B,1),cZERO,C,size(C,1))
   endif
   !
   call LINEAR_ALGEBRA_WS_clean_up(WS) 
   !
 endif
 !
 ! ===========================================================================================
 !
#if defined _TIMING
 call timing("LINEAR ALGEBRA",OPR="stop")
#endif
 !
 contains
   !
#if defined _SCALAPACK
   !
   subroutine SCALAPACK_section(MAT,INIT)  
     !
     type (PAR_matrix)   :: MAT
     logical             :: INIT
     !
     if (INIT) then
       !
       call SLK_matrix_init( SLK_ORTHO, MAT, N )
       Y_ALLOCATE(MAT%blc(MAT%nrows,MAT%ncols))
       Y_MEM(MAT%blc)
       !
       if (present(M_bse)) then
         call SLK_matrix_transfer( "<",  MAT, M_bse=M_bse )
       else if (present(M)) then
         call SLK_matrix_transfer( "<",  MAT, M=M )
       endif
       !
     else if (idriver==DIAGO) then
       !
       call SLK_matrix_init( SLK_ORTHO, EIGEN_slk, N )
       Y_ALLOCATE(EIGEN_slk%blc(EIGEN_slk%nrows,EIGEN_slk%ncols))
       Y_MEM(EIGEN_slk%blc)
       !
       call PARALLEL_HERMITIAN_diagonalization( SLK_ORTHO, MAT, EIGEN_slk, N, E_real )
       !
       if (present(M)) then
         call SLK_matrix_transfer( ">", EIGEN_slk , M=M )
       endif
       !
     else if (idriver==LIN_SYS) then
       !
       call SLK_matrix_init( SLK_ORTHO, B_slk, N )
       Y_ALLOCATE(B_slk%blc(B_slk%nrows,B_slk%ncols))
       Y_MEM(B_slk%blc)
       call SLK_matrix_transfer( "<", B_slk, M=B )
       !
       call PARALLEL_lin_system( SLK_ORTHO, MAT, B_slk, N, size(B,2), B )
       call SLK_matrix_transfer( ">",  B_slk, M=B )
       !
     else if (idriver==MAT_MUL) then
       ! 
       call SLK_matrix_init( SLK_ORTHO, B_slk, N )
       Y_ALLOCATE(B_slk%blc(B_slk%nrows,B_slk%ncols))
       Y_MEM(B_slk%blc)
       call SLK_matrix_transfer( "<", B_slk, M=B )
       call SLK_matrix_init( SLK_ORTHO, C_slk, N )
       Y_ALLOCATE(C_slk%blc(C_slk%nrows,C_slk%ncols))
       Y_MEM(C_slk%blc)
       call SLK_matrix_transfer( "<", C_slk, M=C )
       !
       call PARALLEL_M_by_M( SLK_ORTHO, MAT, B_slk, C_slk, 'N', 'N', size(C,1), size(C,2), N , C )
       call SLK_matrix_transfer( ">",  C_slk, M=C )
       !
     endif
     !
     call LINEAR_ALGEBRA_WS_clean_up(WS) 
     !
     if (INIT) then
       if (.not.present(M_slk)) then
         Y_FREE(M_loc_slk%blc)
         call PAR_matrix_reset(M_loc_slk)
       endif
       if (.not.present(V_par)) then
         Y_FREE(EIGEN_slk%blc)
         call PAR_matrix_reset(EIGEN_slk)
       endif
     endif
     call PAR_matrix_reset(B_slk)
     Y_FREE(B_slk%blc)
     call PAR_matrix_reset(C_slk)
     Y_FREE(C_slk%blc)
     !
   end subroutine
   !
#endif
 !
end subroutine
