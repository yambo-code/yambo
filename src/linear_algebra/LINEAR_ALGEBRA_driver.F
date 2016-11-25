!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine LINEAR_ALGEBRA_driver(idriver,M,B,C,lib_in,V_left,V_right,E_real,E_cmpl,&
&                                SVD_digits,det)
 !
 use pars,          ONLY:SP,cONE,cZERO
 use linear_algebra,ONLY:INV,DIAGO,LIN_SYS,SVD_INV,MAT_MUL,USE_LK,USE_SLK,min_cpu_block_size,&
&                        LINEAR_ALGEBRA_WS_clean_up,WS,LINEAR_ALGEBRA_error
 use wrapper,       ONLY:M_by_M
 use parallel_m,    ONLY:linear_algebra_is_parallel
#if defined _SCALAPACK
 use SLK_m,         ONLY:SLK_ORTHO,SLK_POOL,SLK_matrix,SLK_matrix_reset
#endif
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 !
 implicit none
 !
 integer                :: idriver
 integer,      optional :: lib_in
 complex(SP)            :: M(:,:)
 complex(SP),  optional :: B(:,:)
 complex(SP),  optional :: C(:,:)
 complex(SP),  optional :: V_left(:,:)
 complex(SP),  optional :: V_right(:,:)
 real(SP),     optional :: E_real(:)
 complex(SP),  optional :: E_cmpl(:)
 integer,      optional :: SVD_digits 
 complex(SP),  optional :: det
 !
 ! Work Spaces ...
 !
 complex(SP)         :: det_
 integer             :: N,lib_in_use,Ncpu_in_use
#if defined _SCALAPACK
 type (SLK_matrix)   :: M_slk,B_slk,C_slk,EIGEN_slk
#endif
 !
#if defined _TIMING
 call timing("LINEAR ALGEBRA",OPR="start")
#endif
 !
 ! M dimension
 !
 N=size(M,2)
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
 if (.not.present(lib_in).and.N/sqrt(real(Ncpu_in_use))<min_cpu_block_size) lib_in_use=USE_LK 
 !
 if (idriver==LIN_SYS.and..not.present(B)) &
&   call LINEAR_ALGEBRA_error('LINEAR_ALGEBRA_driver','B must be present for LIN_SYS')
 if (idriver==MAT_MUL.and.(.not.present(B).or..not.present(C))) &
&   call LINEAR_ALGEBRA_error('LINEAR_ALGEBRA_driver','B,C must be present for MAT_MUL')
 !
#if defined _SCALAPACK
 !
 ! =============== PARALLEL SECTION ==========================================================
 !
 if (lib_in_use==USE_SLK) then
   !
   if (idriver==INV) then
     !
     call PARALLEL_distribute( SLK_ORTHO, SLK_POOL, M_slk, N, M )
     !
     call PARALLEL_inversion( SLK_ORTHO, SLK_POOL, M_slk, N, M )
     !
   else if (idriver==DIAGO) then
     !
     call PARALLEL_distribute( SLK_ORTHO, SLK_POOL, M_slk,     N, M )
     call PARALLEL_distribute( SLK_ORTHO, SLK_POOL, EIGEN_slk, N, M )
     !
     if (present(E_cmpl)) then
       !
       call LINEAR_ALGEBRA_error('LINEAR_ALGEBRA_driver','parallel non-herm diag not yet implemented')
       !
       !call PARALLEL_diagonalization( SLK_ORTHO, SLK_POOL, M_slk, EIGEN_slk, N, M, E_cmpl )
       !
     else
       !
       call PARALLEL_HERMITIAN_diagonalization( SLK_ORTHO, SLK_POOL, M_slk, EIGEN_slk, N, M, E_real )
       !
     endif
     !
   else if (idriver==LIN_SYS) then
     !
     call PARALLEL_distribute( SLK_ORTHO, SLK_POOL, M_slk, N, M )
     call PARALLEL_distribute( SLK_ORTHO, SLK_POOL, B_slk, N, B )
     !
     call PARALLEL_lin_system( SLK_ORTHO, SLK_POOL, M_slk, B_slk, N, size(B,2), B )
     !
   else if (idriver==MAT_MUL) then
     ! working for square matrices 
     call PARALLEL_distribute( SLK_ORTHO, SLK_POOL, M_slk, N, M )
     call PARALLEL_distribute( SLK_ORTHO, SLK_POOL, B_slk, N, B )
     call PARALLEL_distribute( SLK_ORTHO, SLK_POOL, C_slk, N, C )
     !
     call PARALLEL_mat_mul( SLK_ORTHO, SLK_POOL, M_slk, B_slk, C_slk, 'N', 'N', size(C,1), size(C,2), size(M,2), C )
     !
   endif
   !
   call LINEAR_ALGEBRA_WS_clean_up(WS) 
   !
   call SLK_matrix_reset(M_slk)
   call SLK_matrix_reset(B_slk)
   call SLK_matrix_reset(C_slk)
   call SLK_matrix_reset(EIGEN_slk)
   !
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
end subroutine

