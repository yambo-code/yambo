!
! Copyright (C) 2000-2008 A. Marini, M. Gruening and the YAMBO team 
!              http://www.yambo-code.org
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
 use pars,      ONLY:SP,schlen
 implicit none
 !
 integer           :: SC_bands
 integer           :: it_now
 integer           :: SC_iterations
 integer           :: SC_kind
 integer           :: SC_xc_functional
 integer           :: SC_fft_size
 integer           :: SC_ng_Sx
 integer           :: SC_X_bands(2)
 integer           :: SC_X_size
 real(SP)          :: rho_convergence
 real(SP)          :: E_convergence
 real(SP)          :: SC_rho_threshold
 real(SP)          :: SC_cycle_mixing
 !
 character(schlen) :: SC_potential
 logical           :: using_SC_basis
 logical           :: local_V
 logical           :: NSC_shot
 !
 ! Rotation matrices and xc potential
 !
 complex(SP), allocatable :: SC_R(:,:,:,:)
 real(SP),    allocatable :: V_xc_sc(:,:)
 real(SP),    allocatable :: V_hartree_sc(:,:)
 real(SP),    allocatable :: rho_n(:)
 !
 ! Kinetic Energy and Ionic potential energy
 !
 complex(SP), allocatable :: Ho(:,:,:,:)
 real(SP),    allocatable :: E_bare(:,:,:)
 !
 ! Non-Local part (sigma_x, sigma_chosex)
 !
 complex(SP), allocatable :: H_nl_sc(:,:,:)
 !
 interface
   !
   subroutine load_SC_components(what,E,COM_,MODE_,ik,n_bands,kind,xc_functional,io_err_)
     use electrons, ONLY:levels
     implicit none
     character(*)           :: what
     type(levels), optional :: E
     integer,      optional :: COM_,MODE_,ik,n_bands,kind,xc_functional,io_err_
   end subroutine  
 end interface  
 !
 contains
   !
   subroutine SC_alloc(E)
     use pars,           ONLY:SP
     use R_lattice,      ONLY:nkibz
     use drivers,        ONLY:l_real_time,l_sc_chosex
     use electrons,      ONLY:n_spin,n_sp_pol,levels
     use FFT_m,          ONLY:fft_size
     use QP_m,           ONLY:QP_n_states,QP_nk,QP_Sc,QP_state,QP_nb
     use memory_m,       ONLY:mem_est
     implicit none
     !
     type(levels) :: E
     !
     ! Build-up QP table
     !
     QP_nb=SC_bands
     QP_nk=nkibz
     !
     allocate(QP_state(QP_nb,QP_nk))
     call mem_est("QP_state",(/QP_n_states*QP_nk/))
     QP_state=.TRUE.
     call QP_state_table_setup(E)
     !
     allocate(Ho(SC_bands,SC_bands,E%nk,n_sp_pol))
     call mem_est("Ho",(/size(Ho)/),(/2*SP/))
     !
     allocate(rho_n(fft_size))
     call mem_est("rho",(/fft_size/),(/SP/))
     !
     allocate(V_hartree_sc(fft_size,n_spin))
     call mem_est("Hartree_Potential",(/size(V_hartree_sc)/))
     !
     allocate(H_nl_sc(SC_bands,SC_bands,n_sp_pol))
     call mem_est("H_nl_sc",(/size(H_nl_sc)/),(/2*SP/))
     !
     if (l_sc_chosex) then
       allocate(QP_Sc(QP_n_states,1))
       call mem_est("QP_Sc",(/QP_n_states/))
     endif
     !
     if (.not.l_real_time) then
       allocate(SC_R(SC_bands,SC_bands,QP_nk,n_sp_pol))
       call mem_est("SC_R",(/size(SC_R)/))
     endif
     !
   end subroutine
   !
   subroutine SC_free()
     use drivers,        ONLY:l_real_time,l_sc_chosex
     use QP_m,           ONLY:QP_Sc
     use memory_m,       ONLY:mem_est
     implicit none
     !
     deallocate(Ho)
     call mem_est("Ho")
     !
     deallocate(H_nl_sc)
     call mem_est("H_nl_sc")
     !
     deallocate(rho_n)
     call mem_est("rho")
     !
     deallocate(V_hartree_sc)
     call mem_est("Hartree_Ptential")
     !
     if (l_sc_chosex) then
       deallocate(QP_Sc)
       call mem_est("QP_Sc")
     endif
     !
     if (.not.l_real_time) then
       deallocate(SC_R)
       call mem_est("SC_R")
     endif
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
#if defined _DOUBLE
     call zgemm('c','n',N,N,N,(1._SP,0._SP),R_,N,H,N,(0._SP,0._SP),H_rot,N)
#else
     call cgemm('c','n',N,N,N,(1._SP,0._SP),R_,N,H,N,(0._SP,0._SP),H_rot,N)
#endif
     H = H_rot
     !
     ! [2] H_rot = H R_
     !     H     = H_rot
     !
#if defined _DOUBLE
     call zgemm('n','n',N,N,N,(1._SP,0._SP),H,N,R_,N,(0._SP,0._SP),H_rot,N)
#else
     call cgemm('n','n',N,N,N,(1._SP,0._SP),H,N,R_,N,(0._SP,0._SP),H_rot,N)
#endif
     H = H_rot
     !
   end subroutine 
   !
end module
