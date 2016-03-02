!
!        Copyright (C) 2000-2016 the YAMBO team
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
module interfaces
 !
 implicit none
 !
 interface
   !
   subroutine SCATTERING_GW_kinematics(i_qp,i_q_bz,i_m,I,k,q,E,N,G0,Ek,Ekmq,E_diff)
     use pars,                ONLY:SP
     use electrons,           ONLY:levels
     use R_lattice,           ONLY:bz_samp
     use QP_m,                ONLY:QP_table
     use collision,           ONLY:elemental_collision
     integer,       intent(in) ::i_qp,i_q_bz,i_m
     type(elemental_collision) ::I
     type(bz_samp)             ::k,q
     type(levels),  OPTIONAL   ::E
     integer,       OPTIONAL   ::N,G0
     real(SP),      OPTIONAL   ::Ek,Ekmq,E_diff
   end subroutine
   !
   subroutine el_density_and_current(E,k,rho,J,force_spatial_inversion)
     use pars,           ONLY:SP
     use electrons,      ONLY:levels
     use R_lattice,      ONLY:bz_samp
     use FFT_m,          ONLY:fft_size
     implicit none
     type(bz_samp)::k
     type(levels) ::E       
     real(SP), optional             ::rho(fft_size)
     real(SP), optional             ::J(fft_size,3)
     logical,  optional, intent(in) ::force_spatial_inversion
     !
   end subroutine
   !
   subroutine MATRIX_driver(idriver,M,lib_in,blt,V_left,V_right,E_real,E_cmpl,SVD_digits,fill,det)
     !
     use matrix_operate, ONLY:blacs_t
     use pars,           ONLY:SP
     !
     integer                :: idriver
     complex(SP)            :: M(:,:)
     complex(SP),  optional :: V_left(:,:)
     complex(SP),  optional :: V_right(:,:)
     complex(SP),  optional :: E_cmpl(:)
     type(blacs_t),optional :: blt
     real(SP),     optional :: E_real(:)
     integer,      optional :: lib_in
     integer,      optional :: fill
     complex(SP),  optional :: det
     integer,      optional :: SVD_digits 
     !
   end subroutine
   !
   integer function io_COLLISIONS(i_qp,ID,COLL,COLL_grp)
     use collision,  ONLY:COLLISIONS_element,COLLISIONS_group
     integer, intent(inout)             ::ID
     integer, intent(in)                ::i_qp
     type(COLLISIONS_element), optional :: COLL
     type(COLLISIONS_group),   optional :: COLL_grp
   end function
   !
   subroutine WF_load(WF,iG_max,iGo_max,bands_to_load,kpts_to_load,&
&                     spins_to_load,space,title,impose_free_and_alloc,&
&                     force_WFo,keep_states_to_load)
     use wave_func,       ONLY:WAVEs
     integer :: iG_max,iGo_max,bands_to_load(2),kpts_to_load(2)
     type(WAVEs)           :: WF
     integer,     optional :: spins_to_load(2)
     character(*),optional :: space
     character(*),optional :: title
     logical     ,optional :: impose_free_and_alloc
     logical     ,optional :: force_WFo
     logical     ,optional :: keep_states_to_load
   end subroutine
   !
   subroutine PARALLEL_WF_index(COMM)
     use parallel_m,      ONLY:MPI_comm
     type(MPI_comm), optional :: COMM
   end subroutine
   !
   subroutine PARALLEL_global_indexes(E,Xk,q,ENVIRONMENT,X,RESET)
     use X_m,           ONLY:X_t
     use electrons,     ONLY:levels
     use R_lattice,     ONLY:bz_samp
     implicit none
     type(levels)             ::E
     type(bz_samp)            ::Xk,q
     character(*)             ::ENVIRONMENT
     type(X_t),       optional::X
     logical,         optional::RESET
   end subroutine
   !
   subroutine PARALLEL_live_message(WHAT,ENVIRONMENT,LOADED,TOTAL,LOADED_r,TOTAL_r,NCPU)
     use pars,           ONLY:SP
     character(*)          :: WHAT
     character(*),optional :: ENVIRONMENT
     integer,     optional :: LOADED,TOTAL,NCPU
     real(SP),    optional :: LOADED_r,TOTAL_r
   end subroutine
   !
   subroutine PARALLEL_WF_distribute(B_index,Bp_index,K_index,B_and_K_index,&
&                                    QP_index,PLASMA_index,Bm_index,CLEAN_UP)
     !
     use parallel_m,      ONLY:PP_indexes
     !
     implicit none
     !
     type(PP_indexes),    optional :: K_index
     type(PP_indexes),    optional :: B_index
     type(PP_indexes),    optional :: Bp_index
     type(PP_indexes),    optional :: B_and_K_index
     type(PP_indexes),    optional :: QP_index
     type(PP_indexes),    optional :: PLASMA_index
     type(PP_indexes),    optional :: Bm_index
     logical, intent(in), optional :: CLEAN_UP
     !
   end subroutine 
   !
   subroutine OCCUPATIONS_Gaps(E,E_g_dir,E_g_ind,N_f,N_m,I_dir)
     use pars,          ONLY: SP
     use electrons,     ONLY:levels,n_sp_pol
     type(levels)       :: E
     real(SP), optional :: E_g_dir(n_sp_pol,2) ! min - max
     real(SP), optional :: E_g_ind(n_sp_pol,2) ! min - max
     integer , optional :: N_f(n_sp_pol)
     integer , optional :: N_m(n_sp_pol)
     integer , optional :: I_dir(2)
   end subroutine 
   !
   subroutine K_diago_compute_epsilon(iq,W,BS_E,BS_R_left,BS_R_right,BS_E_SOC_corr,BS_R_kerr)
     use BS,            ONLY:BS_H_dim
     use pars,          ONLY:SP
     use frequency,     ONLY:w_samp
     type(w_samp),intent(in)  :: W
     integer,     intent(in)  :: iq
     complex(SP), intent(in)  :: BS_E(BS_H_dim)
     complex(SP), intent(in)  :: BS_R_left(BS_H_dim),BS_R_right(BS_H_dim)
     complex(SP), pointer     :: BS_E_SOC_corr(:,:),BS_R_kerr(:)
   end subroutine
   !
   subroutine PARALLEL_assign_chains_and_COMMs(n_elements,ROLE,COMM_index_1,COMM_index_2,COMM_index_3,&
&                                              COMM_index_4,COMM_A2A_1,COMM_A2A_2,COMM_A2A_3)
     use parallel_m,    ONLY:MPI_comm
     integer                 :: n_elements
     character(*)            :: ROLE(:)
     type(MPI_comm)          :: COMM_index_1
     type(MPI_comm),optional :: COMM_index_2
     type(MPI_comm),optional :: COMM_index_3
     type(MPI_comm),optional :: COMM_index_4
     type(MPI_comm),optional :: COMM_A2A_1
     type(MPI_comm),optional :: COMM_A2A_2
     type(MPI_comm),optional :: COMM_A2A_3
   end subroutine
   !
   subroutine PARALLEL_index(px,uplim,low_range,COMM,CONSECUTIVE,ORDERED,MASK)
     use parallel_m, ONLY:PP_indexes,MPI_COMM
     type(PP_indexes)       ::px
     integer                ::uplim(:)
     integer, optional      ::low_range(:)
     type(MPI_COMM),optional::COMM
     logical,       optional::CONSECUTIVE
     logical,       optional::ORDERED
     logical,       optional::MASK(:)
   end subroutine
   !
 end interface
 !
end module
