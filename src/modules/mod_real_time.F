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
module real_time
 !
 use pars,      ONLY:DP,SP,schlen
 !
 implicit none
 !
 real(SP) :: RT_step
 real(SP) :: T_initial
 real(SP) :: Thermal_eta
 real(SP) :: Ext_Field_freq
 real(SP) :: Ext_Field_Int
 real(SP) :: EF_q0(3)
 integer  :: n_steps_mem
 integer  :: Tot_steps
 integer  :: Thermal_steps
 character(schlen) :: Ext_Field_name
 logical           :: l_RT_no_updates
 !
 ! If l_keep_vxc=.true. do not subtract Vxc from the self-energy
 !
 logical           :: l_RPA
 !
 ! Diagonal and off-diagonal relaxation time
 !
 real(SP)          :: LifeTime
 !
 ! Turn on qp-relaxation also during the adiabatic switching
 !
 logical           :: l_always_relax 
 !
 ! GFs
 !
 complex(SP), allocatable :: G_lesser(:,:,:)
 complex(SP), allocatable :: G_lesser_previous(:,:,:)
 complex(SP), allocatable :: G_lesser_reference(:,:,:)
 complex(SP), allocatable :: I1_lesser(:,:,:)
 complex(SP), allocatable :: I2_lesser(:,:,:)
 !
 ! External Field
 !
 logical                  :: l_rt_efield  
 logical                  :: RT_resonant
 logical                  :: RT_no_intraband
 logical                  :: l_no_diamagnetic
 ! 
 ! Other
 !
 real(SP)                :: Thermal_time
 complex(SP),allocatable :: rho_rho_gamp(:,:,:,:)
 integer                 :: DiagH_Iter
 logical                 :: l_qp_analyze
 logical                 :: l_fast_exchange
 !
 ! Integrator Variables
 !
 integer                      :: i_time   ! full time index
 integer                      :: ef1_time ! time index for extern field one
 integer                      :: ef2_time ! time index for extern field two
 !
 character(schlen), parameter :: EULER    = "EULER"
 character(schlen), parameter :: RK2      = "RK2"
 character(schlen), parameter :: EXACT    = "EXACT"
 character(schlen), parameter :: RK2EXACT = "RK2EXACT"
 !
 ! Integrate exactly the part of the dynamics due to H_o 
 ! as it is done in Comp. Phys. Comm. 123(1999)
 ! in this case I choosed H_o = H_lda ( rho_0 )
 !
 character(schlen)            :: Integrator_name
 !                                             
 ! Time dependent Hamiltonian, current and vec. potential
 !
 complex(SP) :: j_current(3)
 complex(SP) :: A_vec_pot(3)
 complex(SP) :: A_norm ! Vector potential A(t) norm
 !
 ! Hartree term
 !
 complex(SP), allocatable :: H_Hartree_0(:,:,:,:)
 complex(SP), allocatable :: H_t(:,:,:,:)
 complex(SP), allocatable :: QP_Hartree(:)
 !
 ! Non-interaction potential energies and potentials
 !
 real(SP),    allocatable :: V_xc_0(:,:)
 real(SP),    allocatable :: Occ_natural_orb(:,:) ! Occupation natural orbitals
 !
 ! Databases input/output
 !
 integer                  :: io_J,io_G
 integer                  :: ID(5)                
 !
 contains
   !
   subroutine RT_alloc(en)
     !
     use pars,           ONLY:SP,schlen
     use drivers,        ONLY:l_sc_hf
     use memory_m,       ONLY:mem_est
     use SC,             ONLY:SC_bands,V_hartree_sc,local_V,SC_alloc,E_bare,rho_n
     use FFT_m,          ONLY:fft_size
     use electrons,      ONLY:levels,n_spin,n_sp_pol
     use xc_functionals, ONLY:E_xc
     use QP_m,           ONLY:QP_nk,QP_n_states
     !
     implicit none
     !
     type(levels), intent(in) :: en
     !
     call SC_alloc(en)
     !
     allocate(E_xc(fft_size))
     call mem_est('Exc',(/fft_size/),(/SP/))
     !
     allocate(G_lesser(SC_bands,SC_bands,QP_nk))
     call mem_est("G_lesser",(/size(G_lesser)/),(/2*SP/))
     !
     allocate(V_xc_0(fft_size,n_spin))
     call mem_est("V_xc_0",(/size(V_xc_0)/))
     !
     if(l_RT_no_updates) then
       allocate(H_Hartree_0(SC_bands,SC_bands,QP_nk,n_sp_pol))
       call mem_est("H_Hartree_0",(/size(H_Hartree_0)/),(/2*SP/))
     endif
     !
     if(l_qp_analyze.or.trim(Integrator_name)==EXACT.or.trim(Integrator_name)==RK2EXACT) then
       allocate(H_t(SC_bands,SC_bands,QP_nk,n_sp_pol))
       call mem_est("H_t",(/size(H_t)/),(/2*SP/))
     endif
     !
     allocate(I1_lesser(SC_bands,SC_bands,QP_nk))
     call mem_est("I1_lesser",(/size(I1_lesser)/),(/2*SP/))
     !
     allocate(I2_lesser(SC_bands,SC_bands,QP_nk))
     call mem_est("I2_lesser",(/size(I2_lesser)/),(/2*SP/))
     !
     allocate(G_lesser_previous(SC_bands,SC_bands,QP_nk))
     call mem_est("Gf_l_old",(/size(G_lesser_previous)/),(/2*SP/))
     !
     if(LifeTime/=0.) then
       allocate(G_lesser_reference(SC_bands,SC_bands,QP_nk))
       call mem_est("Gf_l_old",(/size(G_lesser_reference)/),(/2*SP/))
     endif
     !
     if(l_sc_hf.and.l_fast_exchange) then
       allocate(rho_rho_gamp(SC_bands,SC_bands,QP_nk,QP_n_states))
       call mem_est("rho_rho_gamp",(/size(rho_rho_gamp)/),(/2*SP/))
     endif
     !
     ! Reference energies
     !
     allocate(E_bare(SC_bands,QP_nk,n_sp_pol))
     call mem_est("E_bare",(/size(E_bare)/))
     E_bare=en%E(:SC_bands,:,:)
     !
     if(l_qp_analyze) then
       allocate(Occ_natural_orb(SC_bands,QP_nk))
       call mem_est("Occ_natural",(/size(Occ_natural_orb)/))
     endif
     !
   end subroutine RT_alloc
   
   subroutine RT_free
     !
     use drivers,        ONLY:l_sc_hf
     use SC,             ONLY:V_hartree_sc,Ho,V_xc_Sc,SC_free,E_bare,rho_n
     use memory_m,       ONLY:mem_est
     use xc_functionals, ONLY:E_xc
     implicit none
     !
     call SC_free()
     !
     deallocate(E_xc)
     call mem_est('Exc')
     !
     deallocate(V_xc_0)
     call mem_est("V_xc_0")
     !
     if(l_RT_no_updates) then
       !      
       deallocate(H_Hartree_0)
       call mem_est("H_Hartree_0")
       !
     endif
     !
     if(l_qp_analyze.or.trim(Integrator_name)==EXACT.or.trim(Integrator_name)==RK2EXACT) then
       !
       deallocate(H_t)
       call mem_est("H_t")
       !
     endif
     !
     deallocate(G_lesser)
     call mem_est("G_lesser")
     !
     deallocate(I1_lesser)
     call mem_est("I1_lesser")
     !
     deallocate(I2_lesser)
     call mem_est("I2_lesser")
     !
     deallocate(G_lesser_previous)
     call mem_est("G_lesser_previous")
     !
     if(LifeTime/=0.) then
       deallocate(G_lesser_reference)
       call mem_est("G_lesser_previous")
     endif
     !
     if(l_sc_hf.and.l_fast_exchange) then
       deallocate(rho_rho_gamp)
       call mem_est("rho_rho_gamp")
     endif
     !
     deallocate(E_bare)
     call mem_est("E_bare")
     !
     if(l_qp_analyze) then
       deallocate(Occ_natural_orb)
       call mem_est("Occ_natural")
     endif
     !
   end subroutine RT_free
   !
end module
