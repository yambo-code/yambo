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
#if defined _SC
 !
 use pars,      ONLY:SP,schlen
 !
 implicit none
 !
 real(SP) :: RT_step
 real(SP) :: T_initial
 real(SP) :: Thermal_eta
 real(SP) :: Ext_Field_freq
 real(SP) :: Ext_Field_Int
 real(SP) :: EF_q0(3)
 integer  :: n_poles
 integer  :: n_steps_mem
 integer  :: Tot_steps
 integer  :: Thermal_steps
 character(schlen) :: Ext_Field_name
 logical           :: l_rt_exc
 real(SP)          :: LongRelax
 real(SP)          :: TransRelax
 !
 ! GFs
 !
 complex(SP), allocatable :: G_lesser(:,:,:)
 complex(SP), allocatable :: G_lesser_old(:,:,:)
 complex(SP), allocatable :: I1_lesser(:,:,:)
 complex(SP), allocatable :: I2_lesser(:,:,:)
 !
 ! External Field
 !
 logical                  :: l_rt_efield  
 logical                  :: RT_resonant
 logical                  :: RT_no_intraband
 complex(SP)              :: A_t ! Vector potential A(t)
 ! 
 ! Other
 !
 real(SP), allocatable   :: rho(:)
 real(SP), allocatable   :: V_hartree_0(:,:)  
 real(SP), allocatable   :: V_xc_0(:,:)  
 real(SP)                :: Thermal_time
 integer                 :: DiagH_Iter
 !
 ! Integrator Variables
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
 logical                      :: RT_phase_int
 character(schlen)            :: Integrator_name
 !                                             
 ! Time dependent Hamiltonian 
 !
 complex(SP), allocatable :: H_t(:,:,:)
 !
 ! Non-interaction potential energies and potentials
 !
 complex(SP), allocatable :: Ho_pot(:,:,:)
 !
 contains
   !
   subroutine RT_alloc(en)
     !
     use pars,           ONLY:SP,schlen
     use drivers,        ONLY:l_sc_hartree
     use memory_m,       ONLY:mem_est
     use SC,             ONLY:SC_bands,V_hartree_sc,local_V,Ho
     use FFT_m,          ONLY:fft_size
     use electrons,      ONLY:levels,n_spin,n_sp_pol
     use xc_functionals, ONLY:E_xc
     !
     implicit none
     !
     type(levels), intent(in) :: en
     !
     allocate(rho(fft_size))
     call mem_est('Density',(/fft_size/),(/SP/))
     !
     allocate(E_xc(fft_size))
     call mem_est('Exc',(/fft_size/),(/SP/))
     !
     allocate(V_hartree_0(fft_size,n_spin))
     call mem_est('Hartree',(/fft_size/),(/(1+n_spin)*SP/))
     !  
     allocate(V_xc_0(fft_size,n_spin),V_hartree_sc(fft_size,n_spin))
     call mem_est('Potentials',(/fft_size/),(/(1+n_spin)*2*SP/))
     !
     allocate(G_lesser(SC_bands,SC_bands,en%nk))
     call mem_est("G_lesser",(/size(G_lesser)/),(/2*SP/))
     !
     allocate(Ho(SC_bands,SC_bands,en%nk,n_spin),H_t(SC_bands,SC_bands,en%nk))
     call mem_est("Ho",(/size(Ho)/),(/2*2*SP/))
     !
     if(RT_phase_int.or..not.l_rt_exc) then
       allocate(Ho_pot(SC_bands,SC_bands,en%nk))
       call mem_est("Ho_pot",(/size(Ho_pot)/),(/2*SP/))
     endif
     !
     allocate(I1_lesser(SC_bands,SC_bands,en%nk))
     call mem_est("I1_lesser",(/size(I1_lesser)/),(/2*SP/))
     !
     allocate(I2_lesser(SC_bands,SC_bands,en%nk))
     call mem_est("I2_lesser",(/size(I2_lesser)/),(/2*SP/))
     !
     allocate(G_lesser_old(SC_bands,SC_bands,en%nk))
     call mem_est("Gf_l_old",(/size(G_lesser_old)/),(/2*SP/))
     !
   end subroutine RT_alloc
   
   subroutine RT_free
     !
     use SC,             ONLY:V_hartree_sc,Ho
     use memory_m,       ONLY:mem_est
     use xc_functionals, ONLY:E_xc
     implicit none
     !
     deallocate(E_xc)
     call mem_est('Exc')
     !
     deallocate(rho)
     call mem_est('Density')
     !
     deallocate(V_hartree_0)
     call mem_est('Hartree')
     !  
     deallocate(V_hartree_sc,V_xc_0)
     call mem_est('Potentials')
     !
     deallocate(Ho,H_t)
     call mem_est("Ho")
     !
     if(RT_phase_int.or..not.l_rt_exc) then
       !      
       deallocate(Ho_pot)
       call mem_est("Ho_pot")
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
     deallocate(G_lesser_old)
     call mem_est("G_lesser_old")
     !
   end subroutine RT_free
   !
#endif
   !
end module
