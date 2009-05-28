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
 real(SP)          :: LongRelax
 real(SP)          :: TransRelax
 !
 ! GFs
 !
 complex(SP), allocatable :: G_lesser(:,:,:)
 complex(SP), allocatable :: G_lesser_previous(:,:,:)
 complex(SP), allocatable :: I1_lesser(:,:,:)
 complex(SP), allocatable :: I2_lesser(:,:,:)
 !
 ! External Field
 !
 logical                  :: l_eps_analyze  
 logical                  :: l_rt_efield  
 logical                  :: RT_resonant
 logical                  :: RT_no_intraband
 logical                  :: l_no_diamagnetic
 ! 
 ! Other
 !
 real(SP), allocatable   :: V_hartree_0(:,:)  
 real(SP), allocatable   :: V_xc_0(:,:) 
 real(SP)                :: Thermal_time
 complex(SP),allocatable :: rho_rho_gamp(:,:,:,:,:)
 integer                 :: DiagH_Iter
 logical                 :: l_qp_analyze
 !
 ! Integrator Variables
 !
 integer                      :: I_time
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
 complex(DP), allocatable :: j_current(:,:)
 complex(DP), allocatable :: A_vec_pot(:,:)
 complex(DP), allocatable :: A_norm(:) ! Vector potential A(t) norm
 !
 ! Non-interaction potential energies and potentials
 !
 complex(SP), allocatable :: Hartree_plus_Vxc_sc(:,:,:,:)
 complex(SP), allocatable :: H_t(:,:,:,:)
 real(SP),    allocatable :: Occ_natural_orb(:,:) ! Occupation natural orbitals
 !
 contains
   !
   subroutine RT_alloc(en)
     !
     use pars,           ONLY:SP,schlen
     use drivers,        ONLY:l_sc_hf
     use memory_m,       ONLY:mem_est
     use SC,             ONLY:SC_bands,V_hartree_sc,local_V,V_xc_sc,SC_alloc, &
&                             E_bare
     use FFT_m,          ONLY:fft_size
     use electrons,      ONLY:levels,n_spin,n_sp_pol
     use xc_functionals, ONLY:E_xc
     use QP_m,           ONLY:QP_nk

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
     allocate(V_hartree_0(fft_size,n_spin))
     allocate(V_xc_0(fft_size,n_spin))
     call mem_est('Potentials',(/fft_size/),(/(1+n_spin)*2*SP/))
     !
     allocate(G_lesser(SC_bands,SC_bands,QP_nk))
     call mem_est("G_lesser",(/size(G_lesser)/),(/2*SP/))
     !
     if(l_RT_no_updates) then
       allocate(Hartree_plus_Vxc_sc(SC_bands,SC_bands,QP_nk,n_sp_pol))
       call mem_est("Hartree_plus_Vxc_sc",(/size(Hartree_plus_Vxc_sc)/),(/2*SP/))
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
     allocate(j_current(Tot_steps,3))
     call mem_est("J_current",(/size(j_current)/),(/DP/))
     !
     allocate(A_vec_pot(Tot_steps,3))
     call mem_est("A_vec_pot",(/size(A_vec_pot)/),(/DP/))
     !
     allocate(A_norm(Tot_steps))
     call mem_est("A_norm",(/size(A_norm)/),(/DP/))
     !
     if(l_sc_hf) then
       allocate(rho_rho_gamp(SC_bands,SC_bands,SC_bands,SC_bands,QP_nk))
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
     use SC,             ONLY:V_hartree_sc,Ho,V_xc_Sc,SC_free,E_bare
     use memory_m,       ONLY:mem_est
     use xc_functionals, ONLY:E_xc
     implicit none
     !
     call SC_free()
     !
     deallocate(E_xc)
     call mem_est('Exc')
     !
     deallocate(V_hartree_0,V_xc_0)
     call mem_est('Potentials')
     !
     if(l_RT_no_updates) then
       !      
       deallocate(Hartree_plus_Vxc_sc)
       call mem_est("Hartree_plus_Vxc_sc")
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
     deallocate(j_current)
     call mem_est("j_current")
     !
     deallocate(A_vec_pot)
     call mem_est("A_vec_pot")
     !
     deallocate(A_norm)
     call mem_est("A_norm")
     !
     if(l_sc_hf) then
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
