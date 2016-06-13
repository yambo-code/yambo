!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, MG, DS, HM
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
subroutine K_slepc(iq,W) 
 !
 ! 10/06/2015 Added by HM
 ! Generic diagonalization method to solve hermitian hamiltonians.
 !
 ! Based on the routine K_diago_driver.F modified to use Slepc
 !
 ! The implementation can be done in three different ways:
 ! 1. Generate the hamiltonian as for diagonalization with lapack/scalapack and 
 ! call the slepc routine
 ! 2. The relevant parts of the matrix are generated among the different mpi
 ! processes and slepc is called to diagonalize it
 ! 3. The matrix is not generated but instead calls to K_dot_product are used
 !
 ! The moficiations in this file only become relevant once we start steps 2 and
 ! 3 of the implementation
 !
 use pars,           ONLY:SP
 use BS,             ONLY:BS_mat,BS_K_coupling,io_BSS_diago,BSS_perturbative_width,&
&                         BSS_write_eig_2_db,BSS_eh_W,BS_mat_free,BS_H_dim,BSS_n_eig
 use electrons,      ONLY:E_SOC_nbands
 use matrix_operate, ONLY:DIAGO
 use frequency,      ONLY:w_samp
 use com,            ONLY:isec,warning
 use LIVE_t,         ONLY:live_timing
 use interfaces,     ONLY:MATRIX_slepc,K_diago_compute_epsilon
 use IO_m,           ONLY:io_control,REP,VERIFY,OP_WR_CL,OP_RD_CL
 use parser_m,       ONLY:parser
 !
 ! GPL_EXCLUDE_START
 !
#if defined _KERR
 use drivers,        ONLY:l_kerr
#endif
 !
 ! GPL_EXCLUDE_END
 !
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
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
 real(SP),   pointer     :: BS_R_PL(:) => null()
 real(SP)   ,pointer     :: BS_E_SOC_corr(:,:) => null()
 complex(SP),pointer     :: BS_R_right_kerr(:) => null()
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
 integer              ::io_err,ID
 !
#if defined _TIMING
 call timing('Slepc Solver',OPR='start')
#endif
 !  
 ! Sectioning
 !============
 if (isec(2)/=0) then
   call section('=','Slepc solver')
 else if (isec(2)==0) then
   call section('+','Slepc solver')
 endif
 !
 ! Number of stated for partial Diagonalization
 !=================================
 if (BSS_n_eig==0) then
   BSS_n_eig = BS_H_dim*.01
   call warning(' Calculating only 1% of the eigenvalues.')
 endif
 ! if the number of eigenvalued to calculate is larger than matrix dimensions
 if (BSS_n_eig>BS_H_dim) then
   call warning(' Number of eigenvalues is larger than hamiltonian. Calculating only 1% of the eigenvalues.')
   BSS_n_eig = BS_H_dim*.01
 endif
 ! if the number of eigenvalued to calculate is smaller than 1
 if (BSS_n_eig<1) then
   call warning(' Number of eigenvalues is smaller than hamiltonian. Calculating 1 eigenvalue.')
   BSS_n_eig = 1
 endif
 !
 ! Eigenstates 2 DB ?
 !====================
 if (.not.BSS_write_eig_2_db) call parser('WRbsWF',BSS_write_eig_2_db)
 !
 ! Allocation (BS_R and BS_E)
 !===========================
 call local_alloc(1)
 !
 ! Diagonalization DB (IN)
 !========================
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
 io_err=io_BSS_diago(iq,ID,BS_H_dim,BSS_n_eig,BS_E,BS_R_left,BS_R_right,BS_E_SOC_corr,BS_R_right_kerr)
 !
 ! Kernel loading
 !================
 if(io_err<0) call K_stored_in_a_BIG_matrix(iq)
 !
 K_is_not_hermitian=BS_K_coupling.or.(allocated(BSS_eh_W).and..not.BSS_perturbative_width)
 !
 ! Allocation (BS_E_real or BS_V)
 !===============================
 call local_alloc(2)
 !
 ! Initialize the output file 
 !============================
 call K_output_file(iq,"open Diago")
 !
 if(io_err<0) then
   !
   ! Diagonalization of the excitonic hamiltonian
   !==============================================
   call live_timing('BSK diagonalize',1)
   !
   if (K_is_not_hermitian) then
     !
     call MATRIX_slepc(BS_mat,BSS_n_eig,E_cmpl=BS_E,V_right=BS_V_right)
     !
     if (BSS_write_eig_2_db) BS_mat=BS_V_right
     !
   else
     !
     call MATRIX_slepc(BS_mat,BSS_n_eig,E_real=BS_E_real)
     !
     BS_E=cmplx(BS_E_real,0._SP,SP)
     !
   endif
   !
   call live_timing(steps=1)
   call live_timing
   !
   ! Construct the residuals of epsilon
   !====================================
   if (K_is_not_hermitian) then
     !
     call K_diago_non_hermitian_residuals(BS_E,BS_R_left,BS_R_right,BS_V_left,BS_V_right)
     !
     ! GPL_EXCLUDE_START
     !
#if defined _KERR
     if(l_kerr         ) call K_diago_kerr_residual(BS_E,BS_V_right,BS_R_right_kerr)
#endif
     !
     ! GPL_EXCLUDE_END
     !
   else
     !
     call K_diago_hermitian_residuals(BS_E_real,BS_R_left,BS_R_right)
     !
     ! GPL_EXCLUDE_START
     !
#if defined _KERR
     if(l_kerr         ) call K_diago_kerr_residual(BS_E,BS_mat,BS_R_right_kerr)
#endif
     !
     ! GPL_EXCLUDE_END
     !
   endif
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
   call io_control(ACTION=OP_WR_CL,COM=REP,MODE=VERIFY,SEC=(/1,2,3/),ID=ID)
   io_err=io_BSS_diago(iq,ID,BS_H_dim,BSS_n_eig,BS_E,BS_R_left,BS_R_right,BS_E_SOC_corr,BS_R_right_kerr)
 endif
 !
 ! CLEAN
 !=======
 call BS_mat_free( )
 !
 call local_free( )
 !
#if defined _TIMING
 call timing('Slepc Solver',OPR='stop')
#endif
 !
 contains
   !
   subroutine local_alloc(ii)
     !
     integer  :: ii
     !
     select case(ii)
     case(1)
       !
       allocate(BS_E(BS_H_dim))
       if(E_SOC_nbands/=0) then
         allocate(BS_corrections(2,BS_H_dim))
         allocate(BS_E_SOC_corr(2,BS_H_dim))
       endif
       allocate(BS_R_left(BS_H_dim))
       allocate(BS_R_right(BS_H_dim))
#if defined _KERR
       if(l_kerr) allocate(BS_R_right_kerr(BS_H_dim))
#endif
       !
     case(2)
       !
       if(K_is_not_hermitian) then
         allocate(BS_V_left(BS_H_dim,BS_H_dim))
         allocate(BS_V_right(BS_H_dim,BS_H_dim))
       else
         allocate(BS_E_real(BSS_n_eig))
         if(BSS_perturbative_width.and..not.allocated(BS_corrections)) &
&          allocate(BS_corrections(1,BS_H_dim))
       endif
       !
     end select
     !
   end subroutine local_alloc
   !
   subroutine local_free()
     !
     if(allocated(BS_E))            deallocate(BS_E)
     if(allocated(BS_E_real))       deallocate(BS_E_real)
     if(allocated(BS_corrections))  deallocate(BS_corrections)
     if(allocated(BS_R_right))      deallocate(BS_R_right)
     if(allocated(BS_R_left))       deallocate(BS_R_left)
     if(allocated(BS_V_right))      deallocate(BS_V_right)
     if(allocated(BS_V_left))       deallocate(BS_V_left)
     if(associated(BS_E_SOC_corr)) then
       deallocate(BS_E_SOC_corr)
       nullify(BS_E_SOC_corr)
     endif
#if defined _KERR
     if(associated(BS_R_right_kerr)) then
       deallocate(BS_R_right_kerr)
       nullify(BS_R_right_kerr)
     endif
#endif
     !
   end subroutine local_free
   !
end subroutine
