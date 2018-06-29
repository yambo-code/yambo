!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, MG, DS
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
subroutine K_diago_driver(iq,W) 
 !
 ! Generic diagonalization method to solve resonant and non resonant
 ! Hamiltonians.
 !
 use pars,           ONLY:SP
 use BS,             ONLY:BS_K_coupling,BS_H_dim,BS_Blocks_symmetrize_K
 use BS_solvers,     ONLY:io_BSS_diago,BSS_perturbative_width,&
&                         BSS_write_eig_2_db,BSS_eh_W,BS_mat,BSS_n_eig
 use electrons,      ONLY:E_SOC_nbands,n_sp_pol
 use frequency,      ONLY:w_samp
 use com,            ONLY:isec
 use LIVE_t,         ONLY:live_timing
 use linear_algebra, ONLY:DIAGO
 use interfaces,     ONLY:K_diago_compute_epsilon,LINEAR_ALGEBRA_driver 
 use IO_m,           ONLY:io_control,REP,VERIFY,OP_WR_CL,OP_RD_CL
 use parser_m,       ONLY:parser
 use timing_m,       ONLY:timing
#if defined _KERR
 use drivers,        ONLY:l_kerr
 use KERR,           ONLY:BSS_KERR_free
#endif
 !
 !
#include<memory.h>
 !
 type(w_samp)  :: W
 integer       :: iq
 !
 ! Work Space
 !
 logical       :: K_is_not_hermitian
 !
 ! Residuals & Energies
 !
 complex(SP),allocatable :: BS_E(:)
 complex(SP),allocatable :: BS_R_left(:)       
 complex(SP),allocatable :: BS_R_right(:)      
 real(SP),   pointer     :: BS_R_PL(:,:) => null()
 real(SP)   ,pointer     :: BS_E_SOC_corr(:,:) => null()
 complex(SP),pointer     :: BS_R_right_kerr(:) => null()
 complex(SP),pointer     :: BS_overlap(:,:) => null()
 !
 ! Resonant K
 !
 real(SP),allocatable    :: BS_E_real(:)
 !
 ! Perturbative BSE
 complex(SP),allocatable :: BS_corrections(:,:)
 !
 ! Coupling
 !
 complex(SP),allocatable :: BS_V_left(:,:)     
 complex(SP),allocatable :: BS_V_right(:,:)    
 !
 ! I/O
 !
 integer                 :: io_err,ID
 !
 call timing('Diagonalization Solver',OPR='start')
 BSS_n_eig = BS_H_dim
 !  
 ! Sectioning
 !============
 if (isec(2)/=0) then
   call section('=','Diagonalization solver')
 else if (isec(2)==0) then
   call section('+','Diagonalization solver')
 endif
 !
 ! Eigenstates 2 DB ?
 !====================
 if (.not.BSS_write_eig_2_db) call parser('WRbsWF',BSS_write_eig_2_db)
 call parser('BSSPertWidth',BSS_perturbative_width)
 !
 ! Allocation (BS_R and BS_E)
 !===========================
 call local_alloc("E")
 !
 ! Diagonalization DB (IN)
 !========================
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
 io_err=io_BSS_diago(iq,ID,BS_H_dim,BSS_n_eig,BS_E,BS_R_left,BS_R_right,BS_E_SOC_corr,BS_R_right_kerr,BS_R_PL)
 !
 ! Kernel and/or BSS components loading
 !======================================
 if (io_err<0) call K_components_folded_in_serial_arrays(iq,"DIAGO")
 !
 K_is_not_hermitian=BS_K_coupling.or.(allocated(BSS_eh_W).and..not.BSS_perturbative_width)
 !
 ! Allocation (BS_E_real or BS_V)
 !===============================
 call local_alloc("V")
 !
 ! Initialize the output file 
 !============================
 call K_output_file(iq,"open Diago")
 !
 if (io_err<0) then
   !
#if defined _SCALAPACK
   !
   ! Symmetrize the K blocks (to be eventually sent to the BLACS)
   !
   ! This line will be needed when the BS kernel will not be folded in BS_mat. But in order to do so
   ! also linear_algebra_is_parallel must be swiched off when K is not hermitian.
   !=========================
   !if (linear_algebra_is_parallel) call BS_Blocks_symmetrize_K()
   !
#endif
   !
   ! Diagonalization of the excitonic hamiltonian
   !==============================================
   call live_timing('BSK diagonalize',1)
   !
   if (K_is_not_hermitian) then
     !
     call LINEAR_ALGEBRA_driver(DIAGO,M=BS_mat,E_cmpl=BS_E,V_left=BS_V_left,V_right=BS_V_right)
     !
     if (BSS_write_eig_2_db) BS_mat=BS_V_right
     !
   else
     !
     call LINEAR_ALGEBRA_driver(DIAGO,M=BS_mat,E_real=BS_E_real)
     !
     BS_E=cmplx(BS_E_real,0._SP,SP)
     !
   endif
   !
   call live_timing( )
   !
   ! Construct the residuals of epsilon
   !====================================
   if (K_is_not_hermitian) then
     !
     call K_diago_non_hermitian_residuals(BS_E,BS_R_left,BS_R_right,BS_V_left,BS_V_right,BS_overlap)
     !
#if defined _KERR
     if(l_kerr         ) call K_diago_kerr_residual(BS_E,BS_V_right,BS_R_right_kerr)
#endif
     !
     !
   else
     !
     call K_diago_hermitian_residuals(BS_E_real,BS_R_left,BS_R_right)
     !
     !
#if defined _KERR
     if(l_kerr         ) call K_diago_kerr_residual(BS_E,BS_mat,BS_R_right_kerr)
#endif
     !
   endif
   !
 endif
 !
 ! Define perturbative corrections
 !=================================
 if(E_SOC_nbands/=0) then
   !
   ! To check if the expression for the non hermitian case is correct
   if (     K_is_not_hermitian) call K_diago_perturbative(transpose(BS_V_left)*BS_V_right,BS_corrections,2/n_sp_pol,'SOC')
   if (.not.K_is_not_hermitian) call K_diago_perturbative(    conjg(BS_mat)   *BS_mat    ,BS_corrections,2/n_sp_pol,'SOC')
   BS_E_SOC_corr=real(BS_corrections)
   !
 endif
 !
 if(BSS_perturbative_width) then
   !
   if(     K_is_not_hermitian) call K_diago_perturbative(transpose(BS_V_left)*BS_V_right,BS_corrections(1,:),1,'WID')
   if(.not.K_is_not_hermitian) call K_diago_perturbative(    conjg(BS_mat)   *BS_mat    ,BS_corrections(1,:),1,'WID')
   BS_E=BS_E-aimag(BS_corrections(1,:))
   !
 endif
 !
 ! Now I calculate epsilon
 !=========================
 call K_diago_compute_epsilon(iq,W,BS_E,BS_R_left,BS_R_right,BS_E_SOC_corr,BS_R_right_kerr,BS_R_PL)
 !
 ! I write the output file 
 !
 call K_output_file(iq,"Diago")
 !
 ! Diagonalization DB (OUT)
 !==========================
 if (io_err/=0) then
   !
   call io_control(ACTION=OP_WR_CL,COM=REP,MODE=VERIFY,SEC=(/1,2,3/),ID=ID)
   io_err=io_BSS_diago(iq,ID,BS_H_dim,BSS_n_eig,BS_E,BS_R_left,BS_R_right,BS_E_SOC_corr,BS_R_right_kerr,BS_R_PL)
   !
 endif
 !
 ! CLEAN
 !=======
 call local_free( )
 !
 !
#if defined _KERR
 call BSS_KERR_free( )
#endif
 !
 call timing('Diagonalization Solver',OPR='stop')
 !
 contains
   !
   subroutine local_alloc(what)
     !
     character  :: what
     !
     select case(what)
     case("E")
       !
       YAMBO_ALLOC(BS_E,(BS_H_dim))
       if(E_SOC_nbands/=0) then
         YAMBO_ALLOC(BS_corrections,(2/n_sp_pol,BS_H_dim))
         YAMBO_ALLOC_P(BS_E_SOC_corr,(2/n_sp_pol,BS_H_dim))
       endif
       YAMBO_ALLOC(BS_R_left,(BS_H_dim))
       YAMBO_ALLOC(BS_R_right,(BS_H_dim))
       !
#if defined _KERR
       YAMBO_ALLOC_P(BS_R_right_kerr,(BS_H_dim))
#endif
       !
       !
     case("V")
       !
       if(K_is_not_hermitian) then
         YAMBO_ALLOC(BS_V_left,(BS_H_dim,BS_H_dim))
         YAMBO_ALLOC(BS_V_right,(BS_H_dim,BS_H_dim))
         YAMBO_ALLOC_P(BS_overlap,(BS_H_dim,BS_H_dim))
       else
         YAMBO_ALLOC(BS_E_real,(BS_H_dim))
         if(BSS_perturbative_width.and..not.allocated(BS_corrections)) then
           YAMBO_ALLOC(BS_corrections,(1,BS_H_dim))
         endif
       endif
       !
     end select
     !
   end subroutine local_alloc
   !
   subroutine local_free()
     !
     YAMBO_FREE(BS_E)
     YAMBO_FREE(BS_E_real)
     YAMBO_FREE(BS_corrections)
     YAMBO_FREE(BS_R_right)
     YAMBO_FREE(BS_R_left)
     YAMBO_FREE(BS_V_right)
     YAMBO_FREE(BS_V_left)
     YAMBO_FREE_P(BS_overlap)
     YAMBO_FREE_P(BS_E_SOC_corr)
#if defined _KERR
     YAMBO_FREE_P(BS_R_right_kerr)
#endif
     !
     ! BS_mat must be cleaned as it contains the eigenvectors. 
     !
     YAMBO_FREE(BS_mat)
     !
   end subroutine local_free
   !
end subroutine
