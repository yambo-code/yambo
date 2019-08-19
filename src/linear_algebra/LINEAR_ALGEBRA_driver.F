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
subroutine LINEAR_ALGEBRA_driver(idriver,lib_in,&
&                                M,M_slk,&
&                                M_bse,&
&                                B,B_slk,&
&                                C,C_slk,&
&                                V_left,V_right,E_real,E_cmpl,&
&                                SVD_digits,det)
 !
 ! Operations:
 !
 ! * INV:   M_slk/M => M_slk/M
 !
 ! * DIAGO: M => M + E_real/E_cmpl + V_left/V_right (SLK external arrays still to implement)
 !
 ! * LIN_SYSTEM: M_slk/M + B_slk/B => B_slk/B 
 !
 ! * MAT_MUL: M_slk/M + B_slk/B => C_slk/C 
 !
 use pars,          ONLY:SP,cONE,cZERO
 use linear_algebra,ONLY:INV,DIAGO,LIN_SYS,SVD_INV,MAT_MUL,USE_LK,USE_SLK
 use matrix,        ONLY:PAR_matrix,MATRIX_reset
 use wrapper,       ONLY:M_by_M
 use parallel_m,    ONLY:linear_algebra_is_parallel,ncpu
 use interfaces,    ONLY:MATRIX_transfer
 use BS,            ONLY:BS_block,BS_H_dim
 use com,           ONLY:warning
 use timing_m,      ONLY:timing
#include<memory.h>
 !
 integer                    :: idriver
 integer,          optional :: lib_in
 complex(SP),      optional :: M(:,:)
 type(PAR_matrix), optional :: M_slk
 type(BS_block),   optional :: M_bse(:)
 complex(SP),      optional :: B(:,:)
 type(PAR_matrix), optional :: B_slk
 complex(SP),      optional :: C(:,:)
 type(PAR_matrix), optional :: C_slk
 complex(SP),      optional :: V_left(:,:)
 complex(SP),      optional :: V_right(:,:)
 real(SP),         optional :: E_real(:)
 complex(SP),      optional :: E_cmpl(:)
 integer,          optional :: SVD_digits 
 complex(SP),      optional :: det
 !
 ! Work Spaces ...
 !
 complex(SP)         :: det_
 integer             :: N,lib_in_use
 type (PAR_matrix)   :: M_loc,B_loc,C_loc,V_loc
 !
 call timing("LINEAR ALGEBRA",OPR="start")
 !
 ! M dimension
 !
 if (present(M_slk)) then
   N=M_slk%N
#if defined _SCALAPACK 
   if (M_slk%kind=="SLK") lib_in_use=USE_SLK
#endif
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
 if (present(lib_in)) then
   lib_in_use=lib_in
 endif
 if (idriver==SVD_INV.or.(idriver==DIAGO.and.present(E_cmpl))) lib_in_use=USE_LK 
 !
 if (.not.linear_algebra_is_parallel) lib_in_use=USE_LK
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
   !
   if (.not.present(M_slk)) then
     !
     call MATRIX_init( "SLK", M_loc, N, 1 )
     YAMBO_ALLOC(M_loc%blc,(M_loc%BLCrows(1):M_loc%BLCrows(2),M_loc%BLCcols(1):M_loc%BLCcols(2),M_loc%Nb))
     !
     if (present(M_bse)) then
       call MATRIX_transfer( M_out= M_loc, M_bse=M_bse )
     else if (present(M)) then
       call MATRIX_transfer( M_out= M_loc, M=M )
     endif
     !
   endif
   !
   if (idriver==DIAGO) then
     !
     call MATRIX_init("SLK",  V_loc, N, 1 )
     YAMBO_ALLOC(V_loc%blc,(V_loc%BLCrows(1):V_loc%BLCrows(2),V_loc%BLCcols(1):V_loc%BLCcols(2),V_loc%Nb))
     !
     call PARALLEL_HERMITIAN_diagonalization( M_loc, V_loc, N, E_real )
     !
     call MATRIX_transfer( M_in = V_loc , M=M )
     YAMBO_FREE(V_loc%blc)
     call MATRIX_reset(V_loc)
     !
   else if (idriver==INV) then
     !
     if (present(M)) then
       call PARALLEL_inversion( M_loc, N ) 
       call MATRIX_transfer( M_in = M_loc , M=M )
     else
       call PARALLEL_inversion( M_slk, N ) 
     endif
     !
   else if (idriver==LIN_SYS) then
     !
     if (.not.present(B_slk)) then
       !
       call MATRIX_init("SLK",  B_loc, N, 1 )
       YAMBO_ALLOC(B_loc%blc,(B_loc%BLCrows(1):B_loc%BLCrows(2),B_loc%BLCcols(1):B_loc%BLCcols(2),B_loc%Nb))
       call MATRIX_transfer( M_out = B_loc, M=B )
       !
       call PARALLEL_lin_system( M_loc, B_loc, N, N)
       !
       call MATRIX_transfer( M_in =  B_loc, M=B )
       !
       YAMBO_FREE(B_loc%blc)
       call MATRIX_reset(B_loc)
       !
     else
       !
       call PARALLEL_lin_system( M_slk, B_slk, N, N)
       !
     endif
     !
   else if (idriver==MAT_MUL) then
     !
     if (.not.present(B_slk)) then
       !
       call MATRIX_init("SLK",  B_loc, N, 1 )
       YAMBO_ALLOC(B_loc%blc,(B_loc%BLCrows(1):B_loc%BLCrows(2),B_loc%BLCcols(1):B_loc%BLCcols(2),B_loc%Nb))
       call MATRIX_transfer( M_out =  B_loc, M=B )
       !
       call MATRIX_init("SLK",  C_loc, N, 1 )
       YAMBO_ALLOC(C_loc%blc,(C_loc%BLCrows(1):B_loc%BLCrows(2),C_loc%BLCcols(1):C_loc%BLCcols(2),C_loc%Nb))
       call MATRIX_transfer( M_out =  C_loc, M=C )
       !
       call PARALLEL_M_by_M( M_loc, B_loc, C_loc, 'N', 'N', N, N , N )
       call MATRIX_transfer( M_in =  C_loc, M=C )
       !
       call MATRIX_reset(B_loc)
       YAMBO_FREE(B_loc%blc)
       !
       call MATRIX_reset(C_loc)
       YAMBO_FREE(C_loc%blc)
       !
     else
       !
       call PARALLEL_M_by_M( M_slk, B_slk, C_slk, 'N', 'N', N, N , N )
       !
     endif
     !
   endif
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
     if (present(M_slk)) then
       call SERIAL_SVD_inversion(N,M_slk%blc(:,:,M_slk%I),SVD_digits)
     else
       call SERIAL_SVD_inversion(N,M,SVD_digits)
     endif
   endif
   !
   ! ###> SERIAL INVERSION 
   !
   if (idriver==INV) then
     if (present(M_slk)) then
       call SERIAL_inversion(N,M_slk%blc(:,:,M_slk%I),det_,present(det))
     else
       call SERIAL_inversion(N,M,det_,present(det))
     endif
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
       if (present(M_slk)) then
         call SERIAL_diagonalization(N,M_slk%blc(:,:,M_slk%I),E_cmpl,V_left,V_right)
       else
         call SERIAL_diagonalization(N,M,E_cmpl,V_left,V_right)
       endif
     else
       !
       !... Hermitian case
       if (present(M_slk)) then
         call SERIAL_HERMITIAN_diagonalization(N,M_slk%blc(:,:,M_slk%I),E_real)
       else
         call SERIAL_HERMITIAN_diagonalization(N,M,E_real)
       endif
     endif
     !
   endif
   !
   ! ####> LINEAR SYSTEM SOLUTION
   !
   if (idriver==LIN_SYS) then
     if (present(M_slk)) then
       call SERIAL_lin_system(N,N,M_slk%blc(:,:,M_slk%I),B_slk%blc(:,:,B_slk%I))
     else
       call SERIAL_lin_system(N,size(B,2),M,B)
     endif
   endif
   !
   ! ####> MAT_MUL
   ! C = M * B
   if (idriver==MAT_MUL) then
     if (present(M_slk)) then
       call M_by_M('N','N',N,N,N,cONE,M_slk%blc(:,:,M_slk%I),N,&
&                          B_slk%blc(:,:,B_slk%I),N,cZERO,C_slk%blc(:,:,C_slk%I),N)
     else
       call M_by_M('N','N',size(C,1),size(C,2),size(M,2),cONE,M,size(M,1),&
&                          B,size(B,1),cZERO,C,size(C,1))
     endif
   endif
   !
 endif
 !
 ! ===========================================================================================
 !
 call timing("LINEAR ALGEBRA",OPR="stop")
 !
end subroutine
