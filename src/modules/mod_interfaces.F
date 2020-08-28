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
module interfaces
 !
#include<dev_defs.h>
 !
 implicit none
 !
 interface
   !
   subroutine DESC_write(dest,prefix,desc,range,skip_patt)
     use descriptors,ONLY:IO_desc
     character(*),           intent(in) :: dest
     character(*),           intent(in) :: prefix
     type(IO_desc),          intent(in) :: desc
     integer, optional,      intent(in) :: range(2)
     character(*), optional, intent(in) :: skip_patt
   end subroutine
   !
   subroutine OCCUPATIONS_Fermi(E,K,OBJ,mode,n_bands)
     use electrons,      ONLY:levels
     use R_lattice,      ONLY:bz_samp
     type(levels)  ::E
     type(bz_samp) ::K
     character(*), OPTIONAL ::mode,OBJ
     integer,      OPTIONAL ::n_bands
   end subroutine
   !
   subroutine MATRIX_duplicate( operation, M_in, INDEX_in, M_out )
     use matrix,         ONLY:PAR_matrix
     character(*)               :: operation
     type(PAR_matrix), optional :: M_in
     type(PAR_matrix)           :: M_out
     integer         , optional :: INDEX_in
   end subroutine
   !
   subroutine MATRIX_transfer( M_in, M_bse, M, M_out, INDEX_in, INDEX_out, SND, RCV, COMM, COMM_name)
     !
     use parallel_m,     ONLY:MPI_comm
     use matrix,         ONLY:PAR_matrix
     use BS,             ONLY:BS_block
     use pars,           ONLY:SP
     !
     type(PAR_matrix), optional :: M_in
     type(PAR_matrix), optional :: M_out
     type(BS_block),   optional :: M_bse(:)
     integer,          optional :: INDEX_in
     integer,          optional :: INDEX_out
     complex(SP),      optional :: M(:,:)
     logical,          optional :: SND
     logical,          optional :: RCV
     type(MPI_comm),   optional :: COMM
     character(*),     optional :: COMM_name
     !
   end subroutine
   !
   subroutine LINEAR_ALGEBRA_driver(idriver,lib_in,M,M_slk,M_bse,B,B_slk,C,C_slk,V_left,V_right,E_real,&
&                                   E_cmpl,SVD_digits,det)
     !
     use matrix,         ONLY:PAR_matrix
     use pars,           ONLY:SP
     use BS,             ONLY:BS_block
     !
     integer                    :: idriver
     integer,          optional :: lib_in
     complex(SP),      optional :: M(:,:)
     type(PAR_matrix), optional :: M_slk
     type(BS_block),   optional :: M_bse(:)
     complex(SP),      optional :: B(:,:)
     type(PAR_matrix), optional :: B_slk
     complex(SP),      optional :: C(:,:)
     type(PAR_matrix), optional :: C_slk
     complex(SP),      optional :: V_left(:,:)
     complex(SP),      optional :: V_right(:,:)
     real(SP),         optional :: E_real(:)
     complex(SP),      optional :: E_cmpl(:)
     integer,          optional :: SVD_digits 
     complex(SP),      optional :: det
     !
   end subroutine
   !
   subroutine el_density_and_current(E,k,rho,J,bands,force_spatial_inversion)
     use pars,           ONLY:SP
     use electrons,      ONLY:levels
     use R_lattice,      ONLY:bz_samp
     use FFT_m,          ONLY:fft_size
     implicit none
     type(bz_samp)::k
     type(levels) ::E       
     real(SP), optional             ::rho(fft_size)
     real(SP), optional             ::J(fft_size,3)
     integer,  optional, intent(in) ::bands(2)
     logical,  optional, intent(in) ::force_spatial_inversion
   end subroutine el_density_and_current
   !
   subroutine el_density_of_states(Xk,Xen,dos_E,DOS_broadening,DOS_bands,dos_value,&
&                                  USE_the_DbGd,USE_Eo,USE_occupations,WF_fac,PDOS_fac)
     use pars,           ONLY:SP
     use electrons,      ONLY:levels
     use R_lattice,      ONLY:bz_samp 
     use electrons,      ONLY:n_spin
     implicit none
     type(bz_samp), intent(in)  :: Xk
     type(levels),  intent(in)  :: Xen
     real(SP),      intent(in)  :: dos_E,DOS_broadening
     integer,       intent(in)  :: DOS_bands(2)
     real(SP),      intent(out) :: dos_value(n_spin)
     logical,  optional, intent(in) :: USE_Eo,USE_occupations,USE_the_DbGd
     real(SP), optional, pointer, intent(in) :: WF_fac(:,:,:)
     real(SP), optional, pointer, intent(in) :: PDOS_fac(:,:,:)
   end subroutine el_density_of_states
   !
#if defined _SLEPC && !defined _SLEPC_OFF
   subroutine MATRIX_slepc(A,n_eig,target_energy,extraction,slepc_ncv,slepc_tol,V_right,V_left,E_real,E_cmpl)
     !
     use pars,           ONLY:SP,schlen
     use petscmatdef,    ONLY:tmat
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscmat.h>
     !
     Mat                         :: A            ! matrix to diagonalize of stype from slepc
     integer                     :: n_eig        ! number of eigenvalues to compute
     character(schlen)           :: extraction   ! extraction method for the eigenvalues
     PetscComplex                :: target_energy! energy near where to find the eigenvalues
     PetscReal                   :: slepc_tol    ! tolerance of the iterative solver
     PetscFortranInt             :: slepc_ncv    ! dimension of the subspace
     complex(SP)                 :: V_right(:,:) ! right eigenvalues
     complex(SP),  optional      :: V_left(:,:)  ! left eigenvalues
     complex(SP),  optional      :: E_cmpl(:)    ! complex eigenvalues
     real(SP),     optional      :: E_real(:)    ! real eigenvalues
     !
   end subroutine
#endif
   !
   integer function eval_G_minus_G(iG,iGo,force_recompute,COMM)
     use parallel_m,      ONLY:MPI_comm
     implicit none
     integer :: iG,iGo
     logical,        optional :: force_recompute
     type(MPI_comm), optional :: COMM
   end function
   !
   subroutine WF_load(WF,iG_max,iGo_max,bands_to_load,kpts_to_load,&
&                     sp_pol_to_load,space,title,impose_free_and_alloc,&
&                     force_WFo,keep_states_to_load)
     use wave_func,       ONLY:WAVEs
     integer :: iG_max,iGo_max,bands_to_load(2),kpts_to_load(2)
     type(WAVEs)           :: WF
     integer,     optional :: sp_pol_to_load(2)
     character(*),optional :: space
     character(*),optional :: title
     logical     ,optional :: impose_free_and_alloc
     logical     ,optional :: force_WFo
     logical     ,optional :: keep_states_to_load
   end subroutine
   !
   subroutine WF_free(WF,keep_fft)
     use wave_func,       ONLY:WAVEs
     type(WAVEs)           :: WF
     logical     ,optional :: keep_fft
   end subroutine
   !
   subroutine PARALLEL_WF_index(COMM)
     use parallel_m,      ONLY:MPI_comm
     type(MPI_comm), optional :: COMM
   end subroutine
   !
   subroutine PARALLEL_global_indexes(E,Xk,q,ENVIRONMENT,X,Dip,RESET)
     use X_m,           ONLY:X_t
     use DIPOLES,       ONLY:DIPOLE_t
     use electrons,     ONLY:levels
     use R_lattice,     ONLY:bz_samp
     implicit none
     type(levels)             ::E
     type(bz_samp)            ::Xk,q
     character(*)             ::ENVIRONMENT
     type(X_t),       optional::X
     type(DIPOLE_t),  optional::Dip
     logical,         optional::RESET
   end subroutine
   !
   subroutine PARALLEL_check_phase_space( N_PAR_elements,WORLD,WORLD_size,MSG )
     integer               :: N_par_elements
     integer,     optional :: WORLD,WORLD_size
     character(*),optional :: MSG
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
     real(SP), optional :: E_g_dir(1+(n_sp_pol-1)*2,2) ! min - max
     real(SP), optional :: E_g_ind(1+(n_sp_pol-1)*2,2) ! min - max
     integer , optional :: N_f(n_sp_pol)
     integer , optional :: N_m(n_sp_pol)
     integer , optional :: I_dir(2)
   end subroutine 
   !
   subroutine K_diago_compute_epsilon(iq,W,BS_E,BS_R_left,BS_R_right,BS_E_SOC_corr,BS_R_kerr,BS_R_PL)
     use BS,            ONLY:BS_H_dim
     use pars,          ONLY:SP
     use frequency,     ONLY:w_samp
     type(w_samp),intent(in)  :: W
     integer,     intent(in)  :: iq
     complex(SP), intent(in)  :: BS_E(BS_H_dim)
     complex(SP), intent(in)  :: BS_R_left(BS_H_dim),BS_R_right(BS_H_dim)
     real(SP),    pointer     :: BS_E_SOC_corr(:,:)
     complex(SP), pointer     :: BS_R_kerr(:)
     real(SP),    pointer     :: BS_R_PL(:,:)
   end subroutine
   !
   subroutine PARALLEL_assign_chains_and_COMMs(n_elements,COMM_index_1,COMM_index_2,COMM_index_3,&
&                                              COMM_index_4,COMM_index_5,COMM_A2A_1,COMM_A2A_2,&
&                                              COMM_A2A_3,COMM_A2A_4)
     use parallel_m,    ONLY:MPI_comm
     integer                 :: n_elements
     type(MPI_comm)          :: COMM_index_1
     type(MPI_comm),optional :: COMM_index_2
     type(MPI_comm),optional :: COMM_index_3
     type(MPI_comm),optional :: COMM_index_4
     type(MPI_comm),optional :: COMM_index_5
     type(MPI_comm),optional :: COMM_A2A_1
     type(MPI_comm),optional :: COMM_A2A_2
     type(MPI_comm),optional :: COMM_A2A_3
     type(MPI_comm),optional :: COMM_A2A_4
   end subroutine
   !
   subroutine PARALLEL_index(px,uplim,low_range,COMM,CONSECUTIVE,ORDERED,NO_EMPTIES,MASK)
     use parallel_m, ONLY:PP_indexes,MPI_COMM
     type(PP_indexes)       ::px
     integer                ::uplim(:)
     integer, optional      ::low_range(:)
     type(MPI_COMM),optional::COMM
     logical,       optional::CONSECUTIVE
     logical,       optional::ORDERED
     logical,       optional::NO_EMPTIES
     logical,       optional::MASK(:)
   end subroutine
   !
   subroutine QP_DBs_merge(N,DBs,plan,N_qp,qp,OP,PRE_FAC,MAP)
     use pars, ONLY:SP
     use QP_m, ONLY:QP_t
     implicit none
     integer              :: N,N_qp
     type(QP_t)           :: DBs(N)
     logical              :: plan(N,3)
     type(QP_t),  optional:: qp(N_qp)
     character(*),optional:: OP(N)
     real(SP)    ,optional:: PRE_FAC(N)
     integer     ,optional:: MAP(N)
   end subroutine QP_DBs_merge
   !
   integer function io_DB1_selective_scan(what,DB1_path,E,k,db1_alat,db1_a)
     use pars,       ONLY:SP
     use electrons,  ONLY:levels
     use R_lattice,  ONLY:bz_samp
     type(levels)      :: E
     type(bz_samp)     :: k
     character(*)      :: DB1_path,what
     real(SP),optional :: db1_a(3,3)
     real(SP),optional :: db1_alat(3) 
   end function
   !
   subroutine QP_state_print(qp)
     use QP_m,          ONLY:QP_t
     implicit none
     type(QP_t),optional ::qp              
   end subroutine
   !
 end interface
 !
 !
 interface WF_shift_kpoint
   !
   subroutine DEV_SUB_ALT(WF_shift_kpoint)(b_to_shift,ikbz,wf_shift,Xk,WF_k_in,WF_k_out)
     use pars,          ONLY:SP
     use electrons,     ONLY:n_spinor,n_sp_pol
     use wave_func,     ONLY:wf_ng_1st_BZ,wf_ng_overlaps
     use R_lattice,     ONLY:bz_samp
     implicit none
     integer,       intent(in) :: wf_shift(3),ikbz,b_to_shift(2)
     type(bz_samp), intent(in) :: Xk
     complex(SP), intent(in)  DEV_ATTR :: &
&         WF_k_in (wf_ng_1st_BZ,  n_spinor,b_to_shift(1):b_to_shift(2),n_sp_pol)
     complex(SP), intent(out) DEV_ATTR :: &
&         WF_k_out(wf_ng_overlaps,n_spinor,b_to_shift(1):b_to_shift(2),n_sp_pol)
   end subroutine
 end interface 
 !
 interface WF_symm_kpoint
   !
   subroutine WF_symm_kpoint_cpu(b_to_load,ikbz,Xk,WF_k_out)
     use pars,          ONLY:SP 
     use R_lattice,     ONLY:bz_samp
     use wave_func,     ONLY:wf_ng_1st_BZ
     use electrons,     ONLY:n_spinor,n_sp_pol
     implicit none
     integer,       intent(in)  :: ikbz,b_to_load(2)
     type(bz_samp), intent(in)  :: Xk
     complex(SP),   intent(out) :: WF_k_out(wf_ng_1st_BZ,n_spinor,b_to_load(1):b_to_load(2),n_sp_pol)
   end subroutine WF_symm_kpoint_cpu
   !
#ifdef _CUDA
   subroutine WF_symm_kpoint_gpu(b_to_load,ikbz,Xk,WF_k_out)
     use pars,          ONLY:SP 
     use R_lattice,     ONLY:bz_samp
     use wave_func,     ONLY:wf_ng_1st_BZ
     use electrons,     ONLY:n_spinor,n_sp_pol
     implicit none
     integer,       intent(in)  :: ikbz,b_to_load(2)
     type(bz_samp), intent(in)  :: Xk
     complex(SP),   intent(out) DEV_ATTR :: &
&       WF_k_out(wf_ng_1st_BZ,n_spinor,b_to_load(1):b_to_load(2),n_sp_pol)
   end subroutine WF_symm_kpoint_gpu
#endif
 end interface
 !
 interface WF_apply_symm
   !
   subroutine WF_apply_symm_cpu(isc,WF_symm)
     use pars,          ONLY:SP
     use FFT_m,         ONLY:fft_size
     use electrons,     ONLY:n_spinor
     implicit none
     integer,     intent(in)  :: isc(4)
     complex(SP), intent(out) :: WF_symm(fft_size,n_spinor)
   end subroutine WF_apply_symm_cpu
   !
#ifdef _CUDA
   subroutine WF_apply_symm_gpu(isc,WF_symm_d)
     use pars,          ONLY:SP
     use FFT_m,         ONLY:fft_size
     use electrons,     ONLY:n_spinor
     implicit none
     integer,     intent(in)  :: isc(4)
     complex(SP), intent(out) DEV_ATTR :: WF_symm_d(fft_size,n_spinor)
   end subroutine WF_apply_symm_gpu
#endif
 end interface
 !
end module interfaces
