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
subroutine LINEAR_ALGEBRA_driver(idriver,M,lib_in,V_left,V_right,E_real,E_cmpl,SVD_digits,det)
 !
 use pars,          ONLY:SP
 use linear_algebra,ONLY:INV,DIAGO,USE_LK,SVD_INV,USE_SLK,min_cpu_block_size,LINEAR_ALGEBRA_WS_clean_up
#if defined _SCALAPACK
 use parallel_m,    ONLY:PAR_COM_INV,PAR_COM_DIAGO
 use SLK_m,         ONLY:SLK_COM_A2A,SLK_COM_INDEX,ORTHO_group,POOL_group,SLK_POOL_clean_up,&
&                        SLK_ORTHO_GROUP_clean_up
#endif
 !
 implicit none
 !
 integer                :: idriver
 integer,      optional :: lib_in
 complex(SP)            :: M(:,:)
 complex(SP),  optional :: V_left(:,:)
 complex(SP),  optional :: V_right(:,:)
 real(SP),     optional :: E_real(:)
 complex(SP),  optional :: E_cmpl(:)
 integer,      optional :: SVD_digits 
 complex(SP),  optional :: det
 !
 !Work Spaces ...
 !
 complex(SP)         :: det_
 integer             :: M_size,lib_in_use
#if defined _SCALAPACK
 type (POOL_group)   :: POOL
 type (ORTHO_group)  :: ORTHO
#endif
 !
 ! M dimension
 !
 M_size=size(M,2)
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
 if (idriver==SVD_INV) lib_in_use=USE_LK 
 !
 !if (Ncpu_in_use==1.or.M_size/Ncpu_in_use<min_cpu_block_size.or.idriver==SVD_INV) lib_in_use=USE_LK 
 !
#if defined _SCALAPACK
 !
 ! =============== PARALLEL SECTION ==========================================================
 !
 if (lib_in_use==USE_SLK) then
   !
   if (idriver==INV) then
     !
     call SLK_setup( ORTHO, POOL, SLK_COM_INDEX(1), SLK_COM_A2A(1) )
     !
   else if (idriver==DIAGO) then
     !
     call SLK_setup( ORTHO, POOL, SLK_COM_INDEX(1), SLK_COM_A2A(1) )
     !
   endif
   !
   call SLK_POOL_clean_up(POOL)
   call SLK_ORTHO_GROUP_clean_up(ORTHO)
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
   if (idriver==SVD_INV) call SERIAL_SVD_inversion(M_size,M,SVD_digits)
   !
   ! ###> SERIAL INVERSION 
   !
   if (idriver==INV)  call SERIAL_inversion(M_size,M,det_,present(det))
   !
   ! ###> SERIAL DIAGONALIZATION
   !
   if (idriver==DIAGO) then
     !
     if (present(E_cmpl)) then
       !
       !... non-Hermitian case
       call SERIAL_diagonalization(M_size,M,E_cmpl,V_left,V_right)
     else
       !
       !... Hermitian case
       call SERIAL_HERMITIAN_diagonalization(M_size,M,E_real)
     endif
     !
   endif
   !
   call LINEAR_ALGEBRA_WS_clean_up( ) 
   !
 endif
 !
 ! ===========================================================================================
 !
end subroutine
