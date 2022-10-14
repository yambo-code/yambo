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
 use R_lattice,       ONLY:nXkbz
 use pars,            ONLY:SP,schlen,lchlen
 use electrons,       ONLY:levels
 !
#include<memory.h>
 !
 ! Bands in terms of Kohn-Sham orbitals
 !
 integer,     allocatable :: FL_table(:,:)
 complex(SP), allocatable :: FL_V_bands(:,:,:,:,:)  ! Floquet bands
 complex(SP), allocatable :: FL_V_bands_hist(:,:,:,:,:)  ! Floquet bands
 complex(SP), allocatable :: FL_S_plus(:,:,:,:,:,:)  ! Floquet overlaps
 complex(SP), allocatable :: FL_S_minus(:,:,:,:,:,:)  ! Floquet overlaps
 complex(SP), allocatable :: FL_time_S_det(:,:,:,:)  ! Floquet overlaps determinant
 complex(SP), allocatable :: FL_Ho_plus_Sigma(:,:,:,:)  ! Floquet Hamiltonian
 complex(SP), allocatable :: FL_Chi_pol(:,:,:,:) !freq,dir,order,previous
 complex(SP), allocatable :: FL_FT_extra_M(:,:)
 complex(SP), allocatable :: FL_FT_extra_Mm1(:,:)
 complex(SP), allocatable :: FL_eigenvectors(:,:,:,:)
 real(SP),    allocatable :: FL_eigenvalues(:,:,:)
 real(SP),    allocatable :: FL_QE(:,:,:),FL_QE_hist(:,:,:),FL_ac_phase(:,:,:)
 real(SP),    allocatable :: FL_Ho_lift_degeneracy(:,:,:) ! Lift degeneracy in unperturbed H
 real(SP),    allocatable :: FL_delta_rho(:,:)
 real(SP),    allocatable :: FL_delta_rho_hist(:,:)
 real(SP),    allocatable :: FL_beta
 complex(SP), allocatable :: FL_dG_time(:,:,:,:)  ! Density matrix at different time steps
 complex(SP), allocatable :: FL_dG(:,:,:,:)  ! Density matrix at different Floquet modes
 complex(SP), allocatable :: FL_Sigma(:,:,:,:)  ! Self-energy in FKS space
 real(SP),    allocatable :: Broyden_prev_n_out(:)
 real(SP),    allocatable :: Broyden_prev_n_in(:)
 real(SP),    allocatable :: Broyden_delta_F(:,:)
 real(SP),    allocatable :: Broyden_u(:,:)
 real(SP),    allocatable :: Broyden_a(:,:)
 !
 !aim137 new variables
 integer  :: max_fl_mode
 integer  :: FL_add_mode
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
 real(SP) :: FL_Chi_thr_3
 real(SP) :: FL_deg_thrs
 real(SP) :: FL_deg_lift
 integer  :: FL_Chi_order,FL_max_iterations
 character(1) :: FL_S_inv_mode,FL_loc_S_inv_mode,FL_solver_mode
 logical   :: l_FL_conv_Chi,l_FL_conv_Rho,l_FL_dephase,l_FL_precond
 logical   :: l_Broyden_mixing
 !
 type(IO_desc)    :: FL_desc
 !
 contains
   !
   subroutine FL_alloc(en)
     !
     use pars,           ONLY:SP
     use parallel_m,     ONLY:PAR_Xk_nibz
     use FFT_m,          ONLY:fft_size
     use electrons,      ONLY:levels,n_spin,n_sp_pol
     use hamiltonian,    ONLY:FL_H_alloc
     use real_time,      ONLY:rho_reference,RT_Vnl_xc,NE_steps
     use R_lattice,      ONLY:nXkbz
     use QP_m,           ONLY:QP_nk
     use electric,       ONLY:ELECTRIC_alloc
     use drivers,        ONLY:l_use_Hxc_collisions
     use nl_optics,      ONLY:V_bands,n_frequencies,NL_P_t,full_rho,E_full,NL_bands,l_use_DIPOLES
     !
     implicit none
     !
     type(levels), intent(in) :: en
     !
     call FL_H_alloc(en,.true.,.false.,FL_states)
     !
     ! aim137 allocate V_bands because I still need it for FL_start_and_restart
     YAMBO_ALLOC(V_bands,(NL_bands(2),maxval(en%nbf),QP_nk,n_sp_pol))
     ! aim137 FL_bands in FKS space broken down in bands x fl modes
     YAMBO_ALLOC(FL_V_bands,(NL_bands(2),n_tot_fl_modes,maxval(en%nbf),QP_nk,n_sp_pol))
     YAMBO_ALLOC(FL_V_bands_hist,(NL_bands(2),n_tot_fl_modes,maxval(en%nbf),QP_nk,n_sp_pol))
     !
     YAMBO_ALLOC(FL_table,(FL_states(1):FL_states(2),2))
     !
     YAMBO_ALLOC(FL_Ho_lift_degeneracy,(FL_states(1):FL_states(2),QP_nk,n_sp_pol))
     !
     if (fft_size>0) then
       YAMBO_ALLOC(rho_reference,(fft_size))
       YAMBO_ALLOC(full_rho,(fft_size))
       YAMBO_ALLOC(FL_delta_rho,(fft_size,n_tot_extra_modes))
       YAMBO_ALLOC(FL_delta_rho_hist,(fft_size,n_tot_extra_modes))
     endif
     !
     ! FL overlaps
     !
     YAMBO_ALLOC(FL_S_plus,(maxval(en%nbf),maxval(en%nbf),n_tot_extra_modes,6,nXkbz,n_sp_pol))
     YAMBO_ALLOC(FL_S_minus,(maxval(en%nbf),maxval(en%nbf),n_tot_extra_modes,6,nXkbz,n_sp_pol))
     YAMBO_ALLOC(FL_time_S_det,(n_tot_time_steps,6,nXkbz,n_sp_pol)) !same number of time steps as fl_modes
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
     ! Floquet Quasi energies at selected mode
     !
     YAMBO_ALLOC(FL_QE,(1:NL_bands(2),QP_nk,n_sp_pol))
     YAMBO_ALLOC(FL_QE_hist,(1:NL_bands(2),QP_nk,n_sp_pol))
     YAMBO_ALLOC(FL_ac_phase,(maxval(en%nbf),QP_nk,n_sp_pol))
     !
     ! Floquet Quasi energies for all modes and eigenvectors
     !
     YAMBO_ALLOC(FL_eigenvectors,(FL_states(1):FL_states(2),FL_states(1):FL_states(2),PAR_Xk_nibz,n_sp_pol))
     YAMBO_ALLOC(FL_eigenvalues,(FL_states(1):FL_states(2),QP_nk,n_sp_pol))
     !
     ! Density matrix and self-energy
     !
     if (l_use_Hxc_collisions) then
       YAMBO_ALLOC(FL_dG,(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),QP_nk,n_tot_extra_modes))
       YAMBO_ALLOC(FL_dG_time,(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),QP_nk,n_tot_extra_modes))
       YAMBO_ALLOC(FL_Sigma,(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),QP_nk,n_tot_extra_modes))
       YAMBO_ALLOC(RT_Vnl_xc,(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),QP_nk,n_sp_pol))
     endif
     !
     if(.not.l_use_DIPOLES) then
       call  ELECTRIC_alloc(en)
     endif
     !
     YAMBO_ALLOC(NL_P_t,(NE_steps,3))
     !
   end subroutine FL_alloc
   !
   subroutine FL_alloc_k_distributed(en)
     !
     use parallel_m,     ONLY:PAR_Xk_nibz,PAR_Xk_ibz_index
     use pars,           ONLY:SP
     use electrons,      ONLY:levels,n_spin,n_sp_pol
     use real_time,      ONLY:Ho_plus_Sigma
     use R_lattice,      ONLY:nXkbz
     use QP_m,           ONLY:QP_nk
     use FFT_m,          ONLY:fft_size
     use nl_optics,      ONLY:VAL_BANDS,I_relax,NL_bands
     !
     implicit none
     !
     type(levels),  intent(in) :: en
     !
     ! Valence bands in fft space
     !
     if(fft_size>0) then
       YAMBO_ALLOC(VAL_BANDS,(fft_size,maxval(en%nbf),PAR_Xk_nibz,n_sp_pol))
     endif
     YAMBO_ALLOC(I_relax,(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),maxval(en%nbf),PAR_Xk_nibz,n_sp_pol))
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
     use nl_optics,      ONLY:I_relax,V_bands,full_rho,E_full,VAL_BANDS,l_use_DIPOLES
     !
     implicit none
     !
     call H_free()
     !
     YAMBO_FREE(Ho_plus_Sigma)
     YAMBO_FREE(I_relax)
     YAMBO_FREE(V_bands)
     !
     YAMBO_FREE(FL_table)
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
     ! Floquet Quasi energies at selected mode
     !
     YAMBO_FREE(FL_QE)
     YAMBO_FREE(FL_QE_hist)
     YAMBO_FREE(FL_ac_phase)
     !
     ! Floquet Quasi energies for all modes and eigenvectors
     !
     YAMBO_FREE(FL_eigenvectors)
     YAMBO_FREE(FL_eigenvalues)
     !
   end subroutine FL_free
   !
   subroutine FL_FT_matrices()
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
   subroutine FL_build_table()
     use nl_optics,      ONLY:NL_bands
     integer :: ib,ifl,ifks
     do ib=NL_bands(1),NL_bands(2)
       do ifl=1,n_tot_fl_modes
         ifks = ifl + (ib-1)*n_tot_fl_modes
         FL_table(ifks,:)=(/ifl,ib/)
       enddo
     enddo
   end subroutine FL_build_table
   !
   subroutine FL_output_to_FL_E(E,E_eq,j_mode)
     use pars,           ONLY:rZERO
     use electrons,      ONLY:levels,E_reset,n_sp_pol
     use nl_optics,      ONLY:NL_bands
     use QP_m,           ONLY:QP_nk
     type(levels) :: E,E_eq
     integer, intent(in) :: j_mode
     integer             :: i_mode,i_sp_pol,ib,ik,i_fks
     !
     call E_reset(E)
     E%nk=QP_nk
     E%nb=NL_bands(2)
     E%nbf=E_eq%nbf
     E%nbm=E_eq%nbm
     E%E_Fermi  =E_eq%E_Fermi
     E%E_VBM    =E_eq%E_VBM
     E%E_CBm    =E_eq%E_CBm
     E%E_dir_gap=E_eq%E_dir_gap
     YAMBO_ALLOC(E%E,(E%nb,E%nk,n_sp_pol))
     YAMBO_ALLOC(E%f,(E%nb,E%nk,n_sp_pol))
     E%E=rZERO
     E%f=rZERO
     !
     do i_sp_pol=1,n_sp_pol
       do ik=1,QP_nk
         do i_fks=FL_states(1),FL_states(2)
           i_mode = FL_table(i_fks,1)
           ib     = FL_table(i_fks,2)
           if (i_mode/=j_mode) cycle
           E%E(ib,ik,i_sp_pol)=FL_eigenvalues(i_fks,ik,i_sp_pol)
         enddo
       enddo
     enddo
     !
   end subroutine
   !
end module fl_optics
