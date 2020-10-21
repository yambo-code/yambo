!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
module ELPH
 !
 use pars,       ONLY:SP,rZERO
 use electrons,  ONLY:levels
 use D_lattice,  ONLY:n_atoms_species
 !
#include<memory.h>
 !
 integer   :: ph_modes
 integer   :: elph_nb
 integer   :: elph_nk_bz
 integer   :: elph_nDBs
 integer   :: elph_nDBs_used
 integer   :: elph_branches(2)
 real(SP)  :: W_debye
 logical   :: elph_use_q_grid
 logical   :: elph_grids_are_expanded=.FALSE.
 real(SP),    allocatable :: QP_OMS_Fan(:)
 real(SP),    allocatable :: QP_DW(:)
 real(SP),    allocatable :: ph_kpt_bz(:,:)
 real(SP),    allocatable :: ph_qpt(:,:)
 real(SP),    allocatable :: ph_freqs_sq(:,:)
 real(SP),    allocatable :: elph_DW(:,:,:,:)
 real(SP),    allocatable :: E_k_plus_q(:,:,:)
 real(SP),    allocatable :: f_k_plus_q(:,:,:)
 complex(SP), allocatable :: elph_gkkp(:,:,:,:)
 complex(SP), allocatable :: pol_vector(:,:,:)
 type(levels), SAVE       :: E_k_p_q
 !
 ! Phonon properties
 !
 real(SP),    allocatable :: PH_Q_modulus(:)
 real(SP),    allocatable :: PH_acoustic_speed(:,:)
 logical,     allocatable :: PH_acoustic_branch(:)
 !
 ! El_h Hamiltonian
 !
 integer   :: elph_Ham_bands(2)
 integer   :: elph_Ham_ik
 real(SP)  :: RES_tresh
 ! 
 ! gFsq matrices
 !
 integer              :: gsqF_energy_steps
 real(SP),allocatable :: gsqF_fan(:,:,:,:)
 real(SP),allocatable :: gsqF_dw(:,:,:)
 real(SP),allocatable :: gsqF_ca_corr(:,:,:,:)
 real(SP),allocatable :: gsqF_life_bose(:,:,:,:)
 real(SP),allocatable :: gsqF_life_f(:,:,:,:)
 !
 ! el-ph Self-Energy
 !
 integer   :: QP_PH_n_G_bands
 logical   :: eval_G_using_KK ! Perform KK of the spectral function
 real(SP)  :: FAN_deltaE_treshold
 real(SP)  :: DW_deltaE_treshold
 real(SP)  :: MAX_gkkp=rZERO
 !
 contains
   !
   subroutine elph_global_alloc(what)
     use parallel_m,ONLY:PAR_nQ_bz
     use QP_m,      ONLY:QP_n_states
     use R_lattice, ONLY:nkbz
     character(*)      ::what
     !
     select case (trim(what))
       !
       case ('gFsq')
         !
         if (.not.allocated(gsqF_fan)) then
           if (.not.eval_G_using_KK) then
             YAMBO_ALLOC(gsqF_fan,(QP_n_states,PAR_nQ_bz,ph_modes,gsqF_energy_steps))
             YAMBO_ALLOC(gsqF_ca_corr,(QP_n_states,PAR_nQ_bz,ph_modes,gsqF_energy_steps))
             gsqF_fan=rZERO
             gsqF_ca_corr=rZERO
           endif
           YAMBO_ALLOC(gsqF_dw,(QP_n_states,PAR_nQ_bz,ph_modes))
           YAMBO_ALLOC(gsqF_life_bose,(QP_n_states,PAR_nQ_bz,ph_modes,gsqF_energy_steps))
           YAMBO_ALLOC(gsqF_life_f,(QP_n_states,PAR_nQ_bz,ph_modes,gsqF_energy_steps))
           gsqF_dw=rZERO
           gsqF_life_bose=rZERO
           gsqF_life_f=rZERO
         endif
         !
       case ('gkkp')
         !
         if (.not.allocated(elph_gkkp)) then
           YAMBO_ALLOC(elph_gkkp,(ph_modes,elph_nb,elph_nb,nkbz))
           YAMBO_ALLOC(elph_DW,(ph_modes,elph_nb,elph_nb,nkbz))
           YAMBO_ALLOC(f_k_plus_q,(elph_nb,nkbz,1))
         endif
         !
     end select
     !
     if (.not.allocated(ph_freqs_sq)) then
       YAMBO_ALLOC(ph_freqs_sq,(elph_nDBs,ph_modes))
       ph_freqs_sq=rZERO
       YAMBO_ALLOC(E_k_plus_q,(elph_nb,nkbz,1))
       YAMBO_ALLOC(PH_Q_modulus,(elph_nDBs))
       YAMBO_ALLOC(PH_acoustic_speed,(elph_nDBs,ph_modes))
       YAMBO_ALLOC(PH_acoustic_branch,(ph_modes))
       YAMBO_ALLOC(pol_vector,(ph_modes,sum(n_atoms_species),3))
       PH_Q_modulus=rZERO
       PH_acoustic_speed=rZERO
       PH_acoustic_branch=.FALSE.
     endif
     !
     if (.not.allocated(ph_qpt)) then
       YAMBO_ALLOC(ph_qpt,(elph_nDBs,3))
       YAMBO_ALLOC(ph_kpt_bz,(nkbz,3))
     endif
     !
   end subroutine
   !
   subroutine elph_global_free()
     YAMBO_FREE(gsqF_fan)
     YAMBO_FREE(gsqF_dw)
     YAMBO_FREE(gsqF_ca_corr)
     YAMBO_FREE(gsqF_life_bose)
     YAMBO_FREE(gsqF_life_f)
     YAMBO_FREE(elph_gkkp)
     YAMBO_FREE(elph_DW)
     YAMBO_FREE(f_k_plus_q)
     YAMBO_FREE(ph_freqs_sq)
     YAMBO_FREE(E_k_plus_q)
     YAMBO_FREE(pol_vector)
     YAMBO_FREE(ph_qpt)
     YAMBO_FREE(ph_kpt_bz)
     YAMBO_FREE(PH_Q_modulus)
     YAMBO_FREE(PH_acoustic_speed)
     YAMBO_FREE(PH_acoustic_branch)
   end subroutine
   !
   subroutine setup_k_plus_q_levels(E)
     !
     use electrons,  ONLY:spin_occ
     use D_lattice,  ONLY:Tel
     use R_lattice,  ONLY:nkbz
     use functions,  ONLY:Fermi_fnc
     !
     real(SP)    ::E
     integer     ::ib,ik
     do ib=1,elph_nb
       do ik=1,nkbz
         E_k_plus_q(ib,ik,1)=E_k_plus_q(ib,ik,1)-E
         f_k_plus_q(ib,ik,1)=spin_occ*Fermi_fnc(E_k_plus_q(ib,ik,1),Tel)
       enddo 
     enddo
     !
   end subroutine
   !
end module ELPH
