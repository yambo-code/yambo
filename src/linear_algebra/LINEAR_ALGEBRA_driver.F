!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine LINEAR_ALGEBRA_driver(idriver,M,B,lib_in,V_left,V_right,E_real,E_cmpl,SVD_digits,det)
 !
 use pars,          ONLY:SP
 use linear_algebra,ONLY:INV,DIAGO,LIN_SYS,SVD_INV,USE_LK,USE_SLK,min_cpu_block_size,&
&                        LINEAR_ALGEBRA_WS_clean_up,WS,LINEAR_ALGEBRA_error
 use com,           ONLY:msg,warning
#if defined _SCALAPACK
 use parallel_m,    ONLY:PAR_COM_INV,PAR_COM_DIAGO,PAR_COM_INV_INDEX,PAR_COM_DIAGO_INDEX,&
&                        PARALLEL_cpu_mat_inversion,PARALLEL_cpu_mat_diagonalization
 use SLK_m,         ONLY:ORTHO_group,POOL_group,SLK_POOL_clean_up,&
&                        SLK_ORTHO_GROUP_clean_up,SLK_matrix,SLK_matrix_reset
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
 integer             :: N,lib_in_use,Ncpu_in_use,slk_error
#if defined _SCALAPACK
 type (POOL_group)   :: POOL
 type (ORTHO_group)  :: ORTHO
 type (SLK_matrix)   :: M_slk,B_slk,EIGEN_slk
#endif

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
 !
#if defined _SCALAPACK 
 lib_in_use=USE_SLK
 if (idriver==INV.or.idriver==LIN_SYS) then
   Ncpu_in_use=PAR_COM_INV%n_CPU
   if (PARALLEL_cpu_mat_inversion>0) Ncpu_in_use=min(Ncpu_in_use,PARALLEL_cpu_mat_inversion)
 endif
 if (idriver==DIAGO) then 
   Ncpu_in_use=PAR_COM_DIAGO%n_CPU
   if (PARALLEL_cpu_mat_diagonalization>0) Ncpu_in_use=min(Ncpu_in_use,PARALLEL_cpu_mat_diagonalization)
 endif
#else
 Ncpu_in_use=1
#endif
 !
 if (present(lib_in)) then
   lib_in_use=lib_in
 endif
 if (idriver==SVD_INV.or.idriver==DIAGO) lib_in_use=USE_LK 
 !
 if (Ncpu_in_use==1) lib_in_use=USE_LK 
 if (N/sqrt(real(Ncpu_in_use))<min_cpu_block_size) lib_in_use=USE_LK 
 !
 if (idriver==LIN_SYS.and..not.present(B)) &
&   call LINEAR_ALGEBRA_error('LINEAR_ALGEBRA_driver','B must be present')
 !
#if defined _SCALAPACK
 !
 ! =============== PARALLEL SECTION ==========================================================
 !
 if (lib_in_use==USE_SLK) then
   !
   if (idriver==INV)     call SLK_setup( idriver, Ncpu_in_use, ORTHO, POOL, PAR_COM_INV,   PAR_COM_INV_INDEX,   slk_error)
   if (idriver==LIN_SYS) call SLK_setup( idriver, Ncpu_in_use, ORTHO, POOL, PAR_COM_INV,   PAR_COM_INV_INDEX,   slk_error)
   if (idriver==DIAGO)   call SLK_setup( idriver, Ncpu_in_use, ORTHO, POOL, PAR_COM_DIAGO, PAR_COM_DIAGO_INDEX, slk_error)
   ! 
   if (slk_error<0) then 
     lib_in_use=USE_LK
     call warning("SLK setup failed. Cannot build a proper BLACS grid. Switching to standard LK")
   endif
   !
 endif
 !
 if (lib_in_use==USE_SLK) then
   !
   if (idriver==INV) then
     !
     call PARALLEL_distribute( ORTHO, POOL, M_slk, N, M )
     !
     call msg('s','[LA] PARALLEL inversion (SLK) using grid: ',ORTHO%grid(1:2))
     call PARALLEL_inversion( ORTHO, POOL, M_slk, N, M )
     !
   else if (idriver==DIAGO) then
     !
     call PARALLEL_distribute( ORTHO, POOL, M_slk,     N, M )
     call PARALLEL_distribute( ORTHO, POOL, EIGEN_slk, N, M )
     !
     call msg('s','[LA] PARALLEL diag (SLK) using grid: ',ORTHO%grid(1:2))
     call PARALLEL_diagonalization( ORTHO, POOL, M_slk, EIGEN_slk, N, M, E_real )
     !
   else if (idriver==LIN_SYS) then
     !
     call PARALLEL_distribute( ORTHO, POOL, M_slk, N, M )
     call PARALLEL_distribute( ORTHO, POOL, B_slk, N, B )
     !
     call msg('s','[LA] PARALLEL linear system solution (SLK) using grid: ',ORTHO%grid(1:2))
     call PARALLEL_lin_system( ORTHO, POOL, M_slk, B_slk, N, SIZE(B,2), B )
     !
   endif
   !
   call SLK_POOL_clean_up(POOL)
   call SLK_ORTHO_GROUP_clean_up(ORTHO)
   call LINEAR_ALGEBRA_WS_clean_up(WS) 
   !
   call SLK_matrix_reset(M_slk)
   call SLK_matrix_reset(B_slk)
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
     call msg('s','[LA] SERIAL SVD inversion (LK)')
     call SERIAL_SVD_inversion(N,M,SVD_digits)
   endif
   !
   ! ###> SERIAL INVERSION 
   !
   if (idriver==INV) then
     call msg('s','[LA] SERIAL inversion (LK)')
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
       call msg('s','[LA] SERIAL diagonalization (LK)')
       call SERIAL_diagonalization(N,M,E_cmpl,V_left,V_right)
     else
       !
       !... Hermitian case
       call msg('s','[LA] SERIAL Herm. diagonalization (LK)')
       call SERIAL_HERMITIAN_diagonalization(N,M,E_real)
     endif
     !
   endif
   !
   ! ####> LINEAR SYSTEM SOLUTION
   !
   if (idriver==LIN_SYS) then
     call msg('s','[LA] SERIAL linear system solution (LK)')
     call SERIAL_lin_system(N,SIZE(B,2),M,B)
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
