!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine LINEAR_ALGEBRA_driver(idriver,lib_in,M,M_slk,M_bse,B,C,V_left,V_right,E_real,E_cmpl,&
&                                SVD_digits,det,V_slk)
 !
 use pars,          ONLY:SP,cONE,cZERO
 use linear_algebra,ONLY:INV,DIAGO,LIN_SYS,SVD_INV,MAT_MUL,USE_LK,USE_SLK,&
&                        LINEAR_ALGEBRA_WS_clean_up,WS,LINEAR_ALGEBRA_error
 use wrapper,       ONLY:M_by_M
 use parallel_m,    ONLY:linear_algebra_is_parallel,ncpu
 use SLK_m,         ONLY:SLK_matrix
 use interfaces,    ONLY:MATRIX_transfer
 use BS,            ONLY:BS_block,BS_H_dim
 use com,           ONLY:warning
#if defined _SCALAPACK
 use SLK_m,         ONLY:SLK_ORTHO,SLK_POOL,SLK_matrix_reset
#endif
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 !
 implicit none
 !
 integer                    :: idriver
 integer,          optional :: lib_in
 complex(SP),      optional :: M(:,:)
 type(SLK_matrix), optional :: M_slk
 type(BS_block),   optional :: M_bse(:)
 complex(SP),      optional :: B(:,:)
 complex(SP),      optional :: C(:,:)
 complex(SP),      optional :: V_left(:,:)
 complex(SP),      optional :: V_right(:,:)
 real(SP),         optional :: E_real(:)
 complex(SP),      optional :: E_cmpl(:)
 integer,          optional :: SVD_digits 
 complex(SP),      optional :: det
 type(SLK_matrix), optional :: V_slk
 !
 ! Work Spaces ...
 !
 complex(SP)         :: det_
 integer             :: N,lib_in_use
 type (SLK_matrix)   :: M_loc_slk,B_slk,C_slk,EIGEN_slk
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
#if defined _SCALAPACK
 !
 contains
   !
   subroutine SCALAPACK_section(MAT,INIT)  
     !
     type (SLK_matrix)   :: MAT
     logical             :: INIT
     !
     if (INIT) then
       !
       call SLK_matrix_init( SLK_ORTHO, SLK_POOL, MAT, N )
       !
       if (present(M_bse)) then
         call MATRIX_transfer( "fill_the_SLK",  MAT, M_bse=M_bse, M=M )
       else if (present(M)) then
         call MATRIX_transfer( "fill_the_SLK",  MAT, M=M )
       endif
       !
     endif
     !
     if (idriver==INV) then
       !
       call PARALLEL_inversion( SLK_ORTHO, SLK_POOL, MAT, N ) 
       !
       if (present(M)) then
         call MATRIX_transfer( "transfer_from_the_SLK",  MAT, M=M )
       endif
       !
     else if (idriver==DIAGO) then
       !
       call SLK_matrix_init( SLK_ORTHO, SLK_POOL, EIGEN_slk, N )
       !
       call PARALLEL_HERMITIAN_diagonalization( SLK_ORTHO, SLK_POOL, MAT, EIGEN_slk, N, E_real )
       !
       if (present(M)) then
         call MATRIX_transfer( "transfer_from_the_SLK", EIGEN_slk , M=M )
       endif
       !
     else if (idriver==LIN_SYS) then
       !
       call SLK_matrix_init( SLK_ORTHO, SLK_POOL, B_slk, N )
       call MATRIX_transfer( "fill_the_SLK", B_slk, M=B )
       !
       call PARALLEL_lin_system( SLK_ORTHO, SLK_POOL, MAT, B_slk, N, size(B,2), B )
       call MATRIX_transfer( "transfer_from_the_SLK",  B_slk, M=B )
       !
     else if (idriver==MAT_MUL) then
       ! 
       call SLK_matrix_init( SLK_ORTHO, SLK_POOL, B_slk, N )
       call SLK_matrix_init( SLK_ORTHO, SLK_POOL, C_slk, N )
       call MATRIX_transfer( "fill_the_SLK", B_slk, M=B )
       call MATRIX_transfer( "fill_the_SLK", C_slk, M=C )
       !
       call PARALLEL_M_by_M( SLK_ORTHO, SLK_POOL, MAT, B_slk, C_slk, 'N', 'N', size(C,1), size(C,2), N , C )
       call MATRIX_transfer( "transfer_from_the_SLK",  C_slk, M=C )
       !
     endif
     !
     call LINEAR_ALGEBRA_WS_clean_up(WS) 
     !
     if (INIT) then
       if (.not.present(M_slk)) call SLK_matrix_reset(M_loc_slk)
       if (.not.present(V_slk)) call SLK_matrix_reset(EIGEN_slk)
     endif
     call SLK_matrix_reset(B_slk)
     call SLK_matrix_reset(C_slk)
     !
   end subroutine
   !
#endif
 !
end subroutine
