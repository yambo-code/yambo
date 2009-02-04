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
 real(SP) :: RTime  ! The real time on the diagonal RTime = it*RT_step
 real(SP) :: T_memory
 integer  :: n_poles
 integer  :: n_steps_mem
 integer  :: Tot_steps
 integer  :: Thermal_steps
 character(schlen) :: Ext_Field_name
 character(schlen) :: Integrator_name
 logical           :: Gf_diagonal
 logical           :: l_rt_exc
 !
 ! GFs
 !
 complex(SP), allocatable :: G_lesser (:,:,:)
 complex(SP), allocatable :: G_l_old(:,:,:)
 complex(SP), allocatable :: Go_Phases(:,:)
 complex(SP), allocatable :: I1_lesser(:,:),I2_lesser(:,:)
 !
 ! External Field
 !
 logical                  :: l_rt_efield  
 complex(SP), allocatable :: Ef_rhoq0(:,:,:)
 ! 
 ! Other
 !
 complex(SP)             :: Ekin,Ex,Ec
 complex(SP)             :: Trace_Gf
 real(SP), allocatable   :: rho(:)
 !
 ! Singular part (in time) of the self-energy 
 ! it contains External Field and Hartree-Fock
 !
 complex(SP), allocatable :: Sigma_s(:,:)
 !
 contains
   !
   subroutine RT_alloc(en)
   !
   use pars,           ONLY:SP,schlen
   use drivers,        ONLY:l_sc_hartree
   use memory_m,       ONLY:mem_est
   use SC,             ONLY:SC_bands,V_hartree_sc
   use FFT_m,          ONLY:fft_size
   use electrons,      ONLY:levels
   implicit none
   !
   type(levels), intent(in) :: en
   !
   allocate(Sigma_s(SC_bands,SC_bands))
   call mem_est('Sigma_s',(/size(Sigma_s)/),(/2*SP/)) 
   !
   allocate(rho(fft_size))
   call mem_est('Density',(/fft_size/),(/SP/))
   !
   if(l_sc_hartree) then
     allocate(V_hartree_sc(fft_size))
     call mem_est('V_Hartree',(/fft_size/),(/SP/))
   endif 
   !
   if(l_rt_efield) then
     allocate(Ef_rhoq0(SC_bands,SC_bands,en%nk))
     call mem_est('ExtField_rhoq0',(/fft_size/),(/2*SP/))
   endif
   !
   allocate(G_lesser(SC_bands,SC_bands,en%nk))
   call mem_est("G_lesser",(/size(G_lesser)/),(/2*SP/))
   !
   allocate(I1_lesser(SC_bands,SC_bands))
   call mem_est("I1_lesser",(/size(I1_lesser)/),(/2*SP/))
   !
   allocate(I2_lesser(SC_bands,SC_bands))
   call mem_est("I2_lesser",(/size(I2_lesser)/),(/2*SP/))
   !
   allocate(G_l_old(SC_bands,SC_bands,en%nk))
   call mem_est("Gf_l_old",(/size(G_l_old)/),(/2*SP/))
   !
   allocate(Go_Phases(SC_bands,en%nk))
   call mem_est("Go_phases",(/size(Go_Phases)/),(/2*SP/))
   !
   end subroutine RT_alloc
   
   subroutine RT_free
   !
   use drivers,        ONLY:l_sc_hartree
   use SC,             ONLY:V_hartree_sc
   use memory_m,       ONLY:mem_est
   implicit none
   !
   deallocate(Sigma_s)
   call mem_est('Sigma_s')
   !
   deallocate(rho)
   call mem_est('Density')
   !
   if(l_sc_hartree) then
     deallocate(V_hartree_sc)
     call mem_est('V_Hartree')
   endif
   !
   if(l_rt_efield) then
     deallocate(Ef_rhoq0)
     call mem_est('ExtField_rhoq0')
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
   deallocate(G_l_old)
   call mem_est("G_l_old")
   !
   deallocate(Go_Phases)
   call mem_est("Go_Phases")
   !
   end subroutine RT_free
   !
end module
