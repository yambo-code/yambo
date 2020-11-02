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
module YPP_ELPH
 !
 use pars,                ONLY:SP,schlen,DP
 use D_lattice,           ONLY:n_atoms
 use R_lattice,           ONLY:nkbz
 !
#include<memory.h>
 !
 logical :: l_phonons
 logical :: l_eliashberg
 logical :: l_atomic_amplitude
 integer :: elph_steps
 real(SP):: elph_gamma_broad
 real(SP):: ph_broad
 real(SP):: elph_Ef
 real(SP):: elph_E_max
 real(SP):: elph_residual_treshold = 0.
 !
 ! ...GKKP I/O
 logical :: l_gkkp
 logical :: use_ext_ph_modes=.FALSE.
 logical :: use_ext_ph_freqs=.FALSE.
 character(schlen)    :: elph_dbs_path
 character(schlen)    :: ph_freqs_file
 character(schlen)    :: ph_modes_file
 !
 ! ... GKKP DB shadow objects
 character(schlen)        :: DB_kind="none"
 integer                  :: DB_nq
 integer                  :: DB_ph_modes
 integer                  :: DB_nk
 integer                  :: DB_nb
 integer,     allocatable :: DB_ph_K_sym_table(:,:)
 integer,     allocatable :: DB_Q_map(:)
 integer,     allocatable :: DB_K_map(:)
 logical,     allocatable :: YAMBO_K_found(:)
 real(DP),    allocatable :: DB_ph_kpt(:,:)
 real(DP),    allocatable :: DB_ph_qpt(:,:)
 complex(DP), allocatable :: DB_ph_pol_vec(:,:,:)
 real(DP),    allocatable :: DB_ph_freq_sq(:,:)
 real(DP)                 :: DB_alat
 ! ... Q-selected
 real(DP),    allocatable :: DB_E_k(:,:)
 real(DP),    allocatable :: DB_E_k_plus_q(:,:)
 complex(DP), allocatable :: DB_grad_at_gamma(:,:,:,:,:)
 complex(DP), allocatable :: DB_gkkp(:,:,:,:)
 !
 interface
   !
   subroutine  ELPH_general_gFsq(k,en,Xk,Xen,q,BS_E_degs,lambda_s,n_lambda)
     use R_lattice,     ONLY:bz_samp
     use electrons,     ONLY:levels
     implicit none
     type(bz_samp)      :: k,q,Xk
     type(levels)       :: en,Xen
     integer, optional  :: BS_E_degs(:)
     integer, optional  :: lambda_s(:)
     integer, optional  :: n_lambda
   end subroutine
   !
 end interface
 !
 contains
   !
   subroutine ELPH_DB_alloc()  
     YAMBO_ALLOC(DB_Q_map,(DB_nq))
     YAMBO_ALLOC(DB_K_map,(nkbz))
     YAMBO_ALLOC(YAMBO_K_found,(nkbz))
     YAMBO_ALLOC(DB_ph_qpt,(DB_nq,3))
     YAMBO_ALLOC(DB_ph_kpt,(3,nkbz))
     YAMBO_ALLOC(DB_grad_at_gamma,(DB_nb,DB_nb,n_atoms,3,nkbz))
     YAMBO_ALLOC(DB_gkkp,(DB_nb,DB_nb,DB_ph_modes,nkbz))
     YAMBO_ALLOC(DB_ph_pol_vec,(DB_ph_modes,n_atoms,3))
     YAMBO_ALLOC(DB_ph_freq_sq,(DB_nq,DB_ph_modes))
     YAMBO_ALLOC(DB_E_k,(DB_nb,nkbz))
     YAMBO_ALLOC(DB_E_k_plus_q,(DB_nb,nkbz))
   end subroutine
   !
   subroutine ELPH_DB_free()  
     YAMBO_FREE(DB_Q_map)
     YAMBO_FREE(DB_K_map)
     YAMBO_FREE(YAMBO_K_found)
     YAMBO_FREE(DB_ph_qpt)
     YAMBO_FREE(DB_ph_kpt)
     YAMBO_FREE(DB_grad_at_gamma)
     YAMBO_FREE(DB_gkkp)
     YAMBO_FREE(DB_ph_pol_vec)
     YAMBO_FREE(DB_ph_freq_sq)
     YAMBO_FREE(DB_E_k)
     YAMBO_FREE(DB_E_k_plus_q)
   end subroutine
   !
end module YPP_ELPH
