!
!        Copyright (C) 2000-2017 the YAMBO team
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
 use pars,       ONLY:SP
 use electrons,  ONLY:levels
 use D_lattice,  ONLY:n_atoms_species
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
 !
 contains
   !
   subroutine elph_global_alloc(what)
     use parallel_m,ONLY:PAR_nQ
     use QP_m,      ONLY:QP_n_states
     use R_lattice, ONLY:nkbz,nqibz
     character(*)      ::what
     !
     select case (trim(what))
       !
       case ('gFsq')
         !
         if (.not.allocated(gsqF_fan)) then
           if (.not.eval_G_using_KK) then
             Y_ALLOC(gsqF_fan(QP_n_states,PAR_nQ,ph_modes,gsqF_energy_steps))
             Y_MEM_ALLOC(gsqF_fan)
             Y_ALLOC(gsqF_ca_corr(QP_n_states,PAR_nQ,ph_modes,gsqF_energy_steps))
             Y_MEM_ALLOC(gsqF_ca_corr)
             gsqF_fan=0.
             gsqF_ca_corr=0.
           endif
           Y_ALLOC(gsqF_dw(QP_n_states,PAR_nQ,ph_modes))
           Y_MEM_ALLOC(gsqF_dw)
           Y_ALLOC(gsqF_life_bose(QP_n_states,PAR_nQ,ph_modes,gsqF_energy_steps))
           Y_MEM_ALLOC(gsqF_life_bose)
           Y_ALLOC(gsqF_life_f(QP_n_states,PAR_nQ,ph_modes,gsqF_energy_steps))
           Y_MEM_ALLOC(gsqF_life_f)
           gsqF_dw=0.
           gsqF_life_bose=0.
           gsqF_life_f=0.
         endif
         !
       case ('gkkp')
         !
         if (.not.allocated(elph_gkkp)) then
           Y_ALLOC(elph_gkkp(nkbz,ph_modes,elph_nb,elph_nb))
           Y_MEM_ALLOC(elph_gkkp)
           Y_ALLOC(elph_DW(nkbz,ph_modes,elph_nb,elph_nb))
           Y_MEM_ALLOC(elph_DW)
           Y_ALLOC(f_k_plus_q(elph_nb,nkbz,1))
           Y_MEM_ALLOC(f_k_plus_q)
         endif
         !
     end select
     !
     if (.not.allocated(ph_freqs_sq)) then
       Y_ALLOC(ph_freqs_sq(elph_nDBs,ph_modes))
       Y_MEM_ALLOC(ph_freqs_sq)
       ph_freqs_sq=0._SP
       Y_ALLOC(E_k_plus_q(elph_nb,nkbz,1))
       Y_MEM_ALLOC(E_k_plus_q)
       !
       Y_ALLOC(pol_vector(ph_modes,sum(n_atoms_species),3))
       Y_MEM_ALLOC(pol_vector)
       !
     endif
     !
     if (.not.allocated(ph_qpt)) then
       Y_ALLOC(ph_qpt(elph_nDBs,3))
       Y_MEM_ALLOC(ph_qpt)
       Y_ALLOC(ph_kpt_bz(nkbz,3))
       Y_MEM_ALLOC(ph_kpt_bz)
     endif
     !
   end subroutine
   !
   subroutine elph_global_free()
     Y_FREE(gsqF_fan)
     Y_FREE(gsqF_dw)
     Y_FREE(gsqF_ca_corr)
     Y_FREE(gsqF_life_bose)
     Y_FREE(gsqF_life_f)
     Y_FREE(elph_gkkp)
     Y_FREE(elph_DW)
     Y_FREE(f_k_plus_q)
     Y_FREE(ph_freqs_sq)
     Y_FREE(E_k_plus_q)
     Y_FREE(pol_vector)
     Y_FREE(ph_qpt)
     Y_FREE(ph_kpt_bz)
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
