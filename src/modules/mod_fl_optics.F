! 
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG AC
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
module fl_optics
 !
 use descriptors,     ONLY:IO_desc
 use pars,            ONLY:SP,schlen,lchlen
 use electrons,       ONLY:levels
 use nl_optics
 !
#include<memory.h>
 ! Bands in terms of Kohn-Sham orbitals
 complex(SP), allocatable :: FL_V_bands(:,:,:,:,:)  ! Floquet bands
 complex(SP), allocatable :: FL_V_bands_hist(:,:,:,:,:)  ! Floquet bands
 complex(SP), allocatable :: FL_S_plus(:,:,:,:,:,:)  ! Floquet overlaps
 complex(SP), allocatable :: FL_S_minus(:,:,:,:,:,:)  ! Floquet overlaps
 complex(SP), allocatable :: FL_time_S_det(:,:,:,:)  ! Floquet overlaps determinant
 complex(SP), allocatable :: FL_Ho_plus_Sigma(:,:,:,:)  ! Floquet Hamiltonian
 complex(SP), allocatable :: FL_Chi_pol(:,:,:,:) !freq,dir,order,previous
 complex(SP), allocatable :: FL_FT_extra_M(:,:)
 complex(SP), allocatable :: FL_FT_extra_Mm1(:,:)
 real(SP),    allocatable :: FL_QE(:,:,:),FL_QE_hist(:,:,:),FL_ac_phase(:,:,:)
 real(SP),    allocatable :: FL_Ho_lift_degeneracy(:,:,:) ! Lift degeneracy in unperturbed H
 !
 !aim137 new variables
 integer  :: max_fl_mode
 integer  :: max_extra_mode
 integer  :: max_duffin_mode
 integer  :: max_time_step
 integer  :: n_tot_fl_modes
 integer  :: n_tot_extra_modes
 integer  :: n_tot_duffin_modes
 integer  :: n_tot_time_steps
 integer  :: FL_space_dim,FL_states(2)
 real(SP) :: FL_Chi_thr_1
 real(SP) :: FL_Chi_thr_2
 real(SP) :: FL_deg_thrs
 real(SP) :: FL_deg_lift
 integer  :: FL_Chi_order
 character(1) :: FL_S_inv_mode,FL_loc_S_inv_mode,FL_solver_mode
 !
 contains
   subroutine FL_alloc(en)
     !
     use pars,           ONLY:SP
     use FFT_m,          ONLY:fft_size
     use electrons,      ONLY:levels,n_spin,n_sp_pol
     use hamiltonian,    ONLY:FL_H_alloc
     use real_time,      ONLY:rho_reference,RT_Vnl_xc,NE_steps
     use R_lattice,      ONLY:nXkbz
     use QP_m,           ONLY:QP_nk
     use electric,       ONLY:ELECTRIC_alloc
     use drivers,        ONLY:l_use_Hxc_collisions
     use nl_optics,      ONLY:V_bands,n_frequencies,NL_P_t
     !
     implicit none
     !
     type(levels), intent(in) :: en
     !
     max_time_step   = FL_Chi_order
     max_fl_mode     = FL_Chi_order+1 !aim137 removed for debug
     !max_fl_mode     = 3 !aim137 remove after DEBUG
     max_extra_mode  = 2*max_fl_mode
     max_duffin_mode = 4*max_fl_mode
     !
     n_tot_time_steps   = 2*max_time_step+1
     n_tot_fl_modes     = 2*max_fl_mode+1
     n_tot_extra_modes  = 2*max_extra_mode+1
     n_tot_duffin_modes = 2*max_duffin_mode+1
     !
     FL_space_dim = NL_bands(2) * n_tot_fl_modes
     FL_states(1) = (NL_bands(1)-1)*n_tot_fl_modes+1
     FL_states(2) = FL_space_dim
     !
     call FL_H_alloc(en,.true.,.false.,FL_states)
     !
     ! aim137 allocate V_bands because I still need it for FL_start_and_restart
     YAMBO_ALLOC(V_bands,(NL_bands(2),en%nbf,QP_nk,n_sp_pol))
     ! aim137 FL_bands in FKS space broken down in bands x fl modes
     YAMBO_ALLOC(FL_V_bands,(NL_bands(2),n_tot_fl_modes,en%nbf,QP_nk,n_sp_pol))
     YAMBO_ALLOC(FL_V_bands_hist,(NL_bands(2),n_tot_fl_modes,en%nbf,QP_nk,n_sp_pol))
     !
     YAMBO_ALLOC(FL_Ho_lift_degeneracy,(FL_states(1):FL_states(2),QP_nk,n_sp_pol))
     !
     if (fft_size>0) then
       YAMBO_ALLOC(rho_reference,(fft_size))
       YAMBO_ALLOC(full_rho,(fft_size))
     endif
     !
     ! FL overlaps
     !
     YAMBO_ALLOC(FL_S_plus,(en%nbf,en%nbf,n_tot_extra_modes,6,QP_nk,n_sp_pol))
     YAMBO_ALLOC(FL_S_minus,(en%nbf,en%nbf,n_tot_extra_modes,6,QP_nk,n_sp_pol))
     YAMBO_ALLOC(FL_time_S_det,(n_tot_time_steps,6,QP_nk,n_sp_pol)) !same number of time steps as fl_modes
     !
     ! FL FT matrices
     !
     YAMBO_ALLOC(FL_FT_extra_M,  (n_tot_extra_modes,n_tot_extra_modes))
     YAMBO_ALLOC(FL_FT_extra_Mm1,(n_tot_extra_modes,n_tot_extra_modes))
     !
     ! Polarization
     ! 
     YAMBO_ALLOC(FL_Chi_pol,(n_frequencies,3,FL_Chi_order+1,2)) 
     !
     ! Reference energies
     !
     YAMBO_ALLOC(E_full,(1:NL_bands(2),QP_nk,n_sp_pol))
     !
     ! Floquet Quasi energies
     !
     YAMBO_ALLOC(FL_QE,(1:NL_bands(2),QP_nk,n_sp_pol))
     YAMBO_ALLOC(FL_QE_hist,(1:NL_bands(2),QP_nk,n_sp_pol))
     YAMBO_ALLOC(FL_ac_phase,(en%nbf,QP_nk,n_sp_pol))
     !
     if(.not.l_use_DIPOLES) then
       call  ELECTRIC_alloc(en)
     endif
     !
     YAMBO_ALLOC(NL_P_t,(NE_steps,3))
   end subroutine FL_alloc
   !
   subroutine FL_alloc_k_distributed(en) !aim137 still need to modify this subroutine
     !
     use parallel_m,     ONLY:PAR_Xk_nibz,PAR_Xk_ibz_index
     use pars,           ONLY:SP
     use electrons,      ONLY:levels,n_spin,n_sp_pol
     use real_time,      ONLY:Ho_plus_Sigma
     use R_lattice,      ONLY:nXkbz
     use QP_m,           ONLY:QP_nk
     use FFT_m,          ONLY:fft_size
     !
     implicit none
     !
     type(levels),  intent(in) :: en
     !
     ! Valence bands in fft space
     !
     if(fft_size>0) then
       YAMBO_ALLOC(VAL_BANDS,(fft_size,en%nbf,PAR_Xk_nibz,n_sp_pol))
     endif
     YAMBO_ALLOC(I_relax,(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),en%nbf,PAR_Xk_nibz,n_sp_pol))
     YAMBO_ALLOC(Ho_plus_Sigma,(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),PAR_Xk_nibz,n_sp_pol))
     !aim137 k-parallelization
     YAMBO_ALLOC(FL_Ho_plus_Sigma,(FL_states(1):FL_states(2),FL_states(1):FL_states(2),PAR_Xk_nibz,n_sp_pol))
     !
   end subroutine FL_alloc_k_distributed
   !
   subroutine FL_free
     !
     use hamiltonian,    ONLY:H_free
     use real_time,      ONLY:rho_reference,Ho_plus_Sigma,RT_Vnl_xc
     use electric,       ONLY:ELECTRIC_free
     use drivers,        ONLY:l_use_Hxc_collisions
     !
     implicit none
     !
     call H_free()
     !
     YAMBO_FREE(Ho_plus_Sigma)
     YAMBO_FREE(I_relax)
     YAMBO_FREE(V_bands)
     !
     YAMBO_FREE(rho_reference)
     YAMBO_FREE(full_rho)
     YAMBO_FREE(E_full)
     YAMBO_FREE(VAL_BANDS)
     !
     if(.not.l_use_DIPOLES) then
       call ELECTRIC_free()
     endif
     !
   end subroutine FL_free
 !
   subroutine FL_FT_matrices
     !
     use pars,           ONLY:SP,cI,pi
     !
     implicit none
     !
     complex(SP)  :: dummy_det
     integer      :: i_t,i1_extra,i1_shift_extra
     !
     do i_t=1,n_tot_extra_modes ! loop on time
       do i1_extra=1,n_tot_extra_modes
         !
         i1_shift_extra = i1_extra -(max_extra_mode+1)
         !
         FL_FT_extra_M(i_t,i1_extra) = exp(-1._SP*cI*i1_shift_extra*2._SP*pi*(i_t-1._SP)/n_tot_extra_modes)
         !
       enddo
     enddo ! loop on time
     !
     FL_FT_extra_Mm1 = FL_FT_extra_M
     call SERIAL_inversion(n_tot_extra_modes,FL_FT_extra_Mm1,dummy_det,.false.)     !
     !
     !
     !aim137 DEBUG
     !do i_t=1,n_tot_extra_modes ! loop on time
     !do i1_extra=1,n_tot_extra_modes ! loop on time
     !  print*, sum(FL_FT_extra_M(i_t,:)*FL_FT_extra_Mm1(:,i1_extra))
     !enddo
     !enddo
     !aim137 prelim - is this ok?
     !aim137 DEBUG this gives some terms 1.d-16
     !
   end subroutine FL_FT_matrices
 !
end module
