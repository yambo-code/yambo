!
!        Copyright (C) 2000-2015 the YAMBO team
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
subroutine K_diagonalization(iq,W) 
 !
 ! Generic diagonalization method to solve resonant and non resonant
 ! Hamiltonians.
 !
 use pars,           ONLY:SP,pi,cZERO,cI
 use wrapper_omp,    ONLY:M_by_M_omp,Vstar_dot_V_omp,V_dot_V_omp,real_V_dot_V_omp
 use BS,             ONLY:BS_mat,BS_K_dim,BSS_rhoq0,BS_K_coupling,io_BSS_diago,&
&                         BSS_write_eig_2_db,BSS_eh_W,BS_mat_free,BS_H_dim,&
&                         BSS_eh_E_SOC_corr
 use electrons,      ONLY:E_SOC_nbands
 use memory_m,       ONLY:mem_est
 use matrix_operate, ONLY:DIAGO,INV
 use frequency,      ONLY:w_samp
 use com,            ONLY:isec
 use LIVE_t,         ONLY:live_timing
 use parallel_m,     ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,     ONLY:PARALLEL_index,MATRIX_driver
 use IO_m,           ONLY:io_control,REP,VERIFY,OP_WR_CL,OP_RD_CL
 use parser_m,       ONLY:parser
 !
 ! GPL_EXCLUDE_START
 !
#if defined _KERR
 use KERR,           ONLY:BSS_rhoq0_kerr
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
 type(PP_indexes)     ::px
 integer              ::i1
 logical              ::K_is_not_hermitian
 !
 ! Residuals & Energies
 !
 complex(SP) ::BS_E(BS_H_dim),BS_E_SOC_corr(2,BS_H_dim)
 complex(SP) ::BS_R(BS_H_dim),BS_R_kerr(BS_H_dim)
 !
 ! Resonant K
 !
 real(SP)    ::BS_E_real(BS_H_dim)
 !
 ! Coupling
 !
 complex(SP) ::BS_overlap(BS_H_dim,BS_H_dim)
 complex(SP) ::BS_V_left(BS_H_dim,BS_H_dim)
 complex(SP) ::BS_V_right(BS_H_dim,BS_H_dim)
 complex(SP) ::BS_R_left(BS_H_dim)
 complex(SP) ::BS_R_right(BS_H_dim),BS_R_right_kerr(BS_H_dim)
 !
 ! I/O
 !
 integer              ::io_err,ID
 !
#if defined _TIMING
 call timing('Diagonalization Solver',OPR='start')
#endif
 !  
 ! Sectioning
 !============
 !
 if (isec(2)/=0) then
   call section('=','Diagonalization solver')
 else if (isec(2)==0) then
   call section('+','Diagonalization solver')
 endif
 !
 ! Eigenstates 2 DB ?
 !====================
 !
 if (.not.BSS_write_eig_2_db) call parser('WRbsWF',BSS_write_eig_2_db)
 !
 ! Diagonalization DB (IN)
 !========================
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
 io_err=io_BSS_diago(iq,ID,BS_H_dim,BS_E,BS_R)
 !
 if(io_err<0) then
   !
   ! Kernel loading
   !
   call K_stored_in_a_BIG_matrix(iq)
   !
   ! eh damping 
   !
   if (allocated(BSS_eh_W)) then
     do i1=1,BS_K_dim
       BS_mat(i1,i1)=BS_mat(i1,i1)+cI*BSS_eh_W(i1)
     enddo
   endif
   !
 endif
 !
 ! Allocation & Par Procs
 !========================
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/BS_H_dim/))
 !
 K_is_not_hermitian=BS_K_coupling.or.allocated(BSS_eh_W)
 !
 if (K_is_not_hermitian) BS_overlap=cZERO
 !
 ! Initialize the output file 
 !============================
 !
 call K_output_file(iq,-2)
 !
 if(io_err<0) then
   !
   ! Diagonalization of the excitonic hamiltonian
   !==============================================
   !
   call live_timing('BSK diagonalize',1)
   if (K_is_not_hermitian) then
     !
     call MATRIX_driver(DIAGO,BS_mat,E_cmpl=BS_E,V_left=BS_V_left,V_right=BS_V_right)
     !
     if (BSS_write_eig_2_db) BS_mat=BS_V_right
     !
   else
     !
     call MATRIX_driver(DIAGO,BS_mat,E_real=BS_E_real)
     !
     BS_E=BS_E_real
     !
   endif
   !
   call live_timing(steps=1)
   call live_timing
   !
   if (K_is_not_hermitian) then
     !
     !   The right eigenvector v(j) of A satisfies
     !                   A * v(j) = lambda(j) * v(j)
     !  where lambda(j) is its eigenvalue.
     !  The left eigenvector u(j) of A satisfies
     !                u(j)**H * A = lambda(j) * u(j)**H
     !  where u(j)**H denotes the conjugate transpose of u(j).
     !
     ! Remember that
     !
     ! 1/(w-H)= \sum_ij |i right><i left|j right>^{-1) <j left| /(w-E_i)
     !
     ! [1] BS_R_right(i)=<q0|j><j|i_R>
     !                  =conjg(BSS_rhoq0(j))*BS_V_right(j,i)
     !
     BS_R_right=cZERO
     !
     call live_timing('BSK R residuals',px%n_of_elements(myid+1))
     !
     do i1=1,BS_H_dim
       if (.not.px%element_1D(i1)) cycle
       !
       BS_R_right(i1)=Vstar_dot_V_omp(BS_H_dim,BSS_rhoq0,BS_V_right(:,i1))
       !
       call live_timing(steps=1)
     enddo
     call PP_redux_wait(BS_R_right)
     call live_timing
     !
     ! GPL_EXCLUDE_START
     !
     BS_R_right_kerr=cZERO
     !
#if defined _KERR
     if(l_kerr) then
       call live_timing(' KERR residuals',px%n_of_elements(myid+1))
       !
       do i1=1,BS_H_dim
         if (.not.px%element_1D(i1)) cycle
         !
         BS_R_right_kerr(i1)=Vstar_dot_V_omp(BS_H_dim,BSS_rhoq0_kerr,BS_V_right(:,i1))
         !
         call live_timing(steps=1)
       enddo
       call PP_redux_wait(BS_R_right_kerr)
       call live_timing
     endif
#endif
     !
     ! GPL_EXCLUDE_END
     !
     ! [2] BS_R(i)= <i_K|j><j|q0> 
     !            = conjg( BS_V_left(k,i))*BSS_rhoq0(k)*R_k
     !
     BS_R=cZERO
     !
     call live_timing('BSK L residuals',px%n_of_elements(myid+1))
     !
     do i1=1,BS_H_dim
       if (.not.px%element_1D(i1)) cycle
       !
       BS_R(i1)=Vstar_dot_V_omp(BS_H_dim,BS_V_left(:,i1), &
&                     (/BSS_rhoq0(1:BS_K_dim),-BSS_rhoq0(1+BS_K_dim:BS_H_dim)/) )
       !
       call live_timing(steps=1)
     enddo
     call PP_redux_wait(BS_R)
     !
     !
     call live_timing
     !
     ! [3] BS_overlap(i,j)=conjg(BS_V_left(k,i))*BS_V_right(k,j)
     !
     call live_timing('BSK overlap mat',1)
     !
     call M_by_M_omp('c','n',BS_H_dim,BS_V_left,BS_V_right,BS_overlap)
     !
     call MATRIX_driver(INV,BS_overlap)
     call live_timing(steps=1)
     call live_timing()
     !
     ! [4] BS_R_left(i)=BS_overlap(i,j)BS_R(j)
     !
     BS_R_left=cZERO
     !
     call live_timing('BSK L x overlap',px%n_of_elements(myid+1))
     do i1=1,BS_H_dim
       if (.not.px%element_1D(i1)) cycle
       !
       BS_R_left(i1)=V_dot_V_omp(BS_H_dim,BS_overlap(i1,:),BS_R)
       !
       call live_timing(steps=1)
     enddo
     call PP_redux_wait(BS_R_left)
     call live_timing
     !
     ! [5] BS_R(i)=BS_R_left(i)BS_R_right(i)
     !
     do i1=1,BS_H_dim
       BS_R(i1)=BS_R_left(i1)*BS_R_right(i1)
       !
       ! GPL_EXCLUDE_START
       !
#if defined _KERR
       if(l_kerr) BS_R_kerr(i1)=BS_R_left(i1)*BS_R_right_kerr(i1)
#endif
       !
       ! GPL_EXCLUDE_END
       !
     enddo
     !
   else
     !
     ! Resonant Residuals
     !====================
     !
     ! BS_R(i)=\sum_k <q0|k><k|i> =
     !         \sum_k BS_mat(k,i) x conjg( BSS_rhoq0(k) )
     !
     BS_R=cZERO
     if(E_SOC_nbands/=0) BS_E_SOC_corr=cZERO
     !
     call live_timing('BSK   residuals',px%n_of_elements(myid+1))
     !
     do i1=1,BS_K_dim
       if (.not.px%element_1D(i1)) cycle
       !
       BS_R(i1)=Vstar_dot_V_omp(BS_K_dim,BSS_rhoq0,BS_mat(:,i1))
       !
       if(E_SOC_nbands/=0) then
         BS_E_SOC_corr(1,i1)=real_V_dot_V_omp( BS_K_dim,BSS_eh_E_SOC_corr(1,:),real(BS_mat(:,i1)*conjg(BS_mat(:,i1)),SP) )
         BS_E_SOC_corr(2,i1)=real_V_dot_V_omp( BS_K_dim,BSS_eh_E_SOC_corr(2,:),real(BS_mat(:,i1)*conjg(BS_mat(:,i1)),SP) )
       endif
       !
       call live_timing(steps=1)
     enddo
     call live_timing
     !
     call PP_redux_wait(BS_R)
     if(E_SOC_nbands/=0) call PP_redux_wait(BS_E_SOC_corr) 
     !
     ! GPL_EXCLUDE_START
     !
     BS_R_kerr=cZERO
     !
#if defined _KERR
     if(l_kerr) then
       call live_timing(' KERR residuals',px%n_of_elements(myid+1))
       !
       do i1=1,BS_K_dim
         if (.not.px%element_1D(i1)) cycle
         !
         BS_R_kerr(i1)=Vstar_dot_V_omp(BS_K_dim,BSS_rhoq0_kerr,BS_mat(:,i1))
         !
         call live_timing(steps=1)
       enddo
       call live_timing
       !
       call PP_redux_wait(BS_R_kerr)
     endif
#endif
     !
     ! GPL_EXCLUDE_END
     !
   endif
 endif
 !
 ! Now I calculate epsilon
 !=========================
 call K_diag_compute_epsilon(iq,W,BS_E,BS_E_SOC_corr,BS_R,BS_R_kerr)
 !
 ! I write the output file 
 !
 call K_output_file(iq,2)
 !
 ! Diagonalization DB (OUT)
 !==========================
 if (io_err/=0) then
   !
   ! GPL_EXCLUDE_START
   !
#if defined _KERR
   if(l_kerr) then
     call io_control(ACTION=OP_WR_CL,COM=REP,MODE=VERIFY,SEC=(/1,2,3/),ID=ID)
     io_err=io_BSS_diago(iq,ID,BS_H_dim,BS_E,BS_R,bsR_kerr=BS_R_kerr)
   else
#endif
   !
   ! GPL_EXCLUDE_END
   !
   call io_control(ACTION=OP_WR_CL,COM=REP,MODE=VERIFY,SEC=(/1,2,3/),ID=ID)
   io_err=io_BSS_diago(iq,ID,BS_H_dim,BS_E,BS_R)
   !
   ! GPL_EXCLUDE_START
   !
#if defined _KERR
   endif
#endif
   !
   ! GPL_EXCLUDE_END
   !
 endif
 !
 ! CLEAN
 !=======
 !
 call BS_mat_free( )
 !
 call PP_indexes_reset(px)
 !
#if defined _TIMING
 call timing('Diagonalization Solver',OPR='stop')
#endif
 !
end subroutine
