!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM MG
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
module SC
 !
 use pars,            ONLY:SP,schlen,lchlen
#include<memory.h>
 !
 integer           :: SC_bands(2)
 integer           :: it_to_start
 integer           :: it_now
 integer           :: SC_iterations
 integer           :: SC_fft_size
 integer           :: SC_ng_Sx
 integer           :: SC_X_bands(2)
 integer           :: SC_X_size
 integer           :: SC_up_W_iters
 integer           :: n_SC_descriptions = 0
 real(SP)          :: E_bare_Ef
 real(SP)          :: rho_convergence
 real(SP)          :: magn_convergence
 real(SP)          :: E_convergence
 real(SP)          :: SC_rho_threshold
 real(SP)          :: SC_cycle_mixing
 !
 character(schlen) :: SC_preconditioning="simple"
 character(schlen) :: SC_potential
 character(lchlen) :: SC_description(100)
 logical           :: compatible_SC_DB
 logical           :: found_SC_DB
 !
 logical           :: l_sc_V_is_local=.TRUE.
 logical           :: l_sc_XC_is_local=.TRUE.
 !
 ! General flags
 !
 logical           :: l_NSC_shot
 logical           :: l_SC_RESTART
 logical           :: l_SC_cycle
 logical           :: l_write_qp_histo
 logical           :: l_Mean_Potential
 logical           :: l_SC_update_W
 logical           :: l_SC_diagonal
 logical           :: l_SC_nl_mix

 !
 ! Possible Kinds, functionals and external perturbations
 !
 integer           :: SC_kind
 integer           :: SC_xc_functional
 integer           :: SC_perturbation
 !
 ! OEP specific
 !
 integer,parameter    :: OEP_steps = 100 
 real(SP),parameter   :: OEP_threshold = 1.E-5
 character(schlen)    :: OEP_approx
 logical              :: l_oep_iterative,l_oep_EWeight
 !
 ! Rotation matrices and xc potential
 !
 complex(SP), allocatable :: SC_R(:,:,:,:)
 complex(SP), allocatable :: V_mean(:,:)
 real(SP),    allocatable :: V_xc_sc(:,:)
 real(SP),    allocatable :: V_hartree_sc(:,:)
 real(SP),    allocatable :: rho_n(:)
 real(SP),    allocatable :: magn_n(:,:)
 !
 ! Kinetic Energy and Ionic potential energy
 !
 complex(SP), allocatable :: Ho(:,:,:,:)
 real(SP),    allocatable :: E_bare(:,:,:)
 !
 ! Non-Local part (sigma_x, sigma_cohsex)
 !
 complex(SP), allocatable :: H_nl_sc(:,:,:)
 !
 ! Wavefunction indexes
 !
 integer :: WF_G_max,WF_Go_indx
 !
 interface
   !
   subroutine load_SC_components(what,E,COM_,MODE_,ik,n_bands,kind,xc_functional,&
&                                perturbation)
     use electrons, ONLY:levels
     implicit none
     character(*)           :: what
     type(levels), optional :: E
     integer,      optional :: COM_
     integer,      optional :: MODE_
     integer,      optional :: ik
     integer,      optional :: n_bands
     integer,      optional :: kind
     integer,      optional :: xc_functional
     integer,      optional :: perturbation
   end subroutine  
 end interface  
 !
 contains
   !
   subroutine SC_QP_table_setup(E)
     use R_lattice,      ONLY:nkibz
     use electrons,      ONLY:levels
     use QP_m,           ONLY:QP_nk,QP_state,QP_nb
     !
     implicit none
     !
     type(levels) :: E
     !
     ! Build-up QP table
     !
     QP_nb=SC_bands(2)
     QP_nk=nkibz
     ! 
     ! QP_state my be read from DBs at start-up
     !
     Y_FREE(QP_state)
     !
     Y_ALLOCATE(QP_state(QP_nb,QP_nk))
     Y_MEM(QP_state)
     QP_state=.TRUE.
     call QP_state_table_setup(E)
     !
   end subroutine
   !
   subroutine SC_alloc(E)
     use pars,           ONLY:SP
     use drivers,        ONLY:l_real_time,l_sc_sex,l_sc_coh,l_sc_srpa
     use electrons,      ONLY:n_spin,n_sp_pol,levels
     use FFT_m,          ONLY:fft_size
     use QP_m,           ONLY:QP_n_states,QP_nk,QP_Sc
     use memory_m,       ONLY:mem_est
     use xc_functionals, ONLY:magn,V_xc
     !
     implicit none
     !
     type(levels) :: E
     !
     call SC_QP_table_setup(E)
     !
     ! In the magnetic project Ho has already been allocated
     !
     Y_ALLOCATE(Ho(SC_bands(2),SC_bands(2),E%nk,n_sp_pol))
     Y_MEM(Ho)
     !
     Y_ALLOCATE(rho_n(fft_size))
     Y_MEM(rho_n)
     !
     if(n_spin>1) then
       Y_ALLOCATE(magn(fft_size,3))
       Y_MEM(magn)
       Y_ALLOCATE(magn_n(fft_size,3))
       Y_MEM(magn_n)
     endif
     !
     Y_ALLOCATE(V_xc_sc(fft_size,n_spin))
     Y_MEM(V_xc_sc)
     !
     Y_ALLOCATE(V_xc(fft_size,n_spin))
     Y_MEM(V_xc)
     !
     Y_ALLOCATE(V_hartree_sc(fft_size,n_spin))
     Y_MEM(V_hartree_sc)
     !
     Y_ALLOCATE(H_nl_sc(SC_bands(2),SC_bands(2),n_sp_pol))
     Y_MEM(H_nl_sc)
     !
     if (l_real_time) return
     !
     if (l_sc_coh.or.l_sc_sex.or.l_sc_srpa) then
       Y_ALLOCATE(QP_Sc(QP_n_states,1))
       Y_MEM(QP_Sc)
     endif
     !
     Y_ALLOCATE(SC_R(SC_bands(2),SC_bands(2),QP_nk,n_sp_pol))
     Y_MEM(SC_R)
     !
   end subroutine
   !
   subroutine SC_free()
     use drivers,        ONLY:l_real_time
     use QP_m,           ONLY:QP_Sc
     use xc_functionals, ONLY:magn,V_xc
     use X_m,            ONLY:X_alloc
     implicit none
     !
     call X_alloc('DIP_P')
     call X_alloc('DIP_iR')
     call X_alloc('DIP_spin')
     call X_alloc('P_square')
     !
     Y_FREE(Ho)
     Y_FREE(H_nl_sc)
     Y_FREE(rho_n)
     Y_FREE(magn_n)
     Y_FREE(magn)
     Y_FREE(V_xc)
     Y_FREE(V_xc_sc)
     Y_FREE(V_hartree_sc)
     Y_FREE(QP_Sc)
     Y_FREE(SC_R)
     !
   end subroutine
   !
   subroutine H_rotate(R,H,N,direction)
     !
     ! Transform H in the new/old basis defined by R
     !
     ! new basis |n'>
     ! old basis |n>
     !
     ! R(i,j)=<i|j'>
     !
     ! direction = +1 => <n'|H|k'>=sum_{m,p} <n'|m> H(m,p) <p|k'> =
     !                               sum_{m,p} R_(m,n')^* H(m,p) R(p,k) = 
     !                               sum_{m,p} R^+_(n',m) H(m,p) R(p,k)
     !
     ! direction = -1 => <n|H|k>=sum_{m,p} <n|m'> H(m',p') <p'|k> =
     !                               sum_{m,p} R_(n,m) H(m',p') R^*(k,p) = 
     !                               sum_{m,p} R_(n,m) H(m',p') R^+(p,k)
     !
     use wrapper,        ONLY:M_by_M
     !
     integer     :: direction,N
     complex(SP) :: H(N,N),R(N,N)
     !
     ! Work Space
     !
     integer     :: i,j
     complex(SP) :: H_rot(N,N),R_(N,N)
     !
     if (direction ==  1) R_=R
     if (direction == -1) forall(i=1:N,j=1:N) R_(i,j)=conjg(R(j,i))
     !
     ! [1] H_rot = (R_^{\dagger}) H
     !     H     = H_rot
     !
     call M_by_M('c','n',N,R_,H,H_rot)
     H = H_rot
     !
     ! [2] H_rot = H R_
     !     H     = H_rot
     !
     call M_by_M('n','n',N,H,R_,H_rot)
     H = H_rot
     !
   end subroutine 
   !
end module
