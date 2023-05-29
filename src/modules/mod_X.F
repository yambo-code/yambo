!
!        Copyright (C) 2000-2023 the YAMBO team
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
module X_m
 !
 use descriptors,    ONLY:IO_desc
 use pars,           ONLY:SP,lchlen,schlen
 use units,          ONLY:HA2EV
 use matrix,         ONLY:PAR_matrix
 use OUTPUT,         ONLY:N_MAX_columns
 use cuda_m,         ONLY:have_cuda
 !
#include <dev_defs.h>
#include <memory.h>
 !
 ! Observables
 !
 real(SP)                 :: eps_2_alpha
 character(6)             :: alpha_dim ! 'adim' 'area' 'length' 'volume'
 logical                  :: l_drude
 integer                  :: drude_n_states
 !
 ! Main Gauge
 !
 character(schlen)        :: global_gauge
 !
 ! Average along the non-zero optical directions
 !
 integer                  :: n_OPTICAL_dir_to_eval=1
 integer                  :: OPTICAL_do_average(3)=0
 character(schlen)        :: OPTICAL_averaged_dirs_string=""
 type(PAR_matrix)         :: X_par_average
 !
 ! Velocity gauge specific variables
 !
 logical                  :: skip_cond_sum_rule
 !
 ! Large Q-vector support 
 !
 real(SP)                 :: q_plus_G_direction(3)
 integer                  :: Q_Shift_Order
 !
 ! Double Grid
 !
 ! Logical used to decid how to use the double grid in X_os
 ! and in particular in X_GreenF_analytical, when the DbGd is used
 !
 integer              :: X_DbGd_nkpts
 logical              :: use_X_DbGd
 real(SP)             :: X_DbGd_percentual
 integer ,allocatable :: X_DbGd_nkpt_at_q(:)
 integer ,allocatable :: X_DbGd_nkpt_at_q_table(:,:,:)
 !
 ! workspace for X_irredux_residuals on GPUs
 !
 complex(SP), allocatable DEV_ATTR :: rhotw_save_d(:), rhotw_save2_d(:)
 !
 ! Absorption & Polarizability
 !
 integer                  :: N_BS_E_sorted=0
 complex(SP), allocatable :: BS_E_sorted(:,:,:)
 complex(SP), allocatable :: Epsilon_ii(:,:)
 complex(SP), allocatable :: Epsilon_ij(:,:)
 complex(SP), allocatable :: Joint_DOS(:,:) 
 complex(SP), allocatable :: EEL(:,:)
 complex(SP), allocatable :: Alpha(:,:)
 complex(SP), allocatable :: X_over_qpG(:,:)
 complex(SP), allocatable :: X_drude_term(:)
 complex(SP), allocatable :: X_fxc(:)
 complex(SP), allocatable :: X_magnons(:,:,:)
 complex(SP), allocatable :: X_dichroism(:,:)
 !
 ! Response function
 !
 character(schlen)        :: Chi_mode
 character(schlen)        :: Chi_linalg_mode
 logical                  :: X_use_gpu=have_cuda
 logical                  :: X_use_lin_sys=.false.
 real(SP),    allocatable :: X_poles(:)
 real(SP),    allocatable :: X_Ein_poles(:)
 integer ,    allocatable :: X_poles_tab(:,:)
 integer ,    allocatable :: X_RLcomp_ig(:)
 integer :: X_ng = 0
 integer :: current_iq
 logical :: X_FILL_UP_matrix_only
 logical :: self_detect_E_range
 real(SP):: X_terminator_E
 real(SP):: X_term_E
 logical :: l_write_disk_Xo=.false.
 logical :: l_X_terminator=.false.
 logical :: l_recalculate_X=.false.
 character(schlen) :: X_terminator_Kind="none"
 !
 ! I/O DB
 ! 
 integer :: i_X_kind_existing_DB=-1
 !
 ! I/O parameters
 !
 integer,parameter:: n_MAX_blocks_of_rows=1000 ! Note that in NETCDF the max number of vars is 8192
 integer          :: n_blocks_of_rows
 integer          :: n_rows_each_block
 !
 ! I/O descriptions
 !
 type(IO_desc)    :: X_desc
 type(IO_desc)    :: MPA_desc
 !
 ! PARALLEL distribution
 !
 type(PAR_matrix), allocatable, target :: X_par(:)
 type(PAR_matrix), target      :: X_par_lower_triangle
 logical                       :: X_lower_triangle_matrix_in_use=.FALSE.
 integer                       :: X_rows(2) = 0
 integer                       :: X_cols(2) = 0
 complex(SP),      allocatable :: X_mat(:,:,:)
 complex(SP),      allocatable DEV_ATTR :: X_mat_d(:,:,:)
 !
 type X_t
   ! DS: is 1 Xo (see X_dielectric_matrix:109) or Xx (see io_X.F) ?
   !     why is 5 reported as IP in the develop
   integer    :: whoami ! 1:Xo/Xx 2:em1s 3:em1d 4:ppa 5:mpa ?:IP
   integer    :: ng
   integer    :: ng_db
   integer    :: iq(2)
   integer    :: ib(2)
   integer    :: ib_lim(2)
   integer    :: db_ib(2)
   integer    :: db_ib_lim(2)
   integer    :: ngostnts
   integer    :: mpa_npoles
   real(SP)   :: ehe(2)
   real(SP)   :: q0(3)
   real(SP)   :: cg_percentual
   real(SP)   :: ppaE
   complex(SP):: Wd
   logical          :: Vnl_included
   character(2)     :: ordering
   character(schlen):: KERNEL
 end type
 !
 ! Observables
 type X_obs_t
   logical               :: active
   character(schlen)     :: what
   character(schlen)     :: title
   character(schlen)     :: prefix
   character(schlen)     :: scattering
   integer               :: N_columns
   character(schlen)     :: COL_header(N_MAX_columns)
   integer               :: N_messages
   character(schlen)     :: messages(N_MAX_columns)
 end type
 integer, parameter     :: N_X_obs=15
 type(X_obs_t)          :: X_obs(N_X_obs) 
 !
 interface
   !
   subroutine X_OUTPUT_messages(iq,ig,Vnl,GF,ordering,Q_plus_G,MORE)
     use pars,      ONLY:schlen,SP,msg_len
     integer                :: iq
     integer,      optional :: ig
     logical,      optional :: Vnl
     logical,      optional :: GF
     character(2), optional :: ordering
     real(SP),     optional :: Q_plus_G(3)
     character(*), optional :: MORE
   end subroutine
   !
   subroutine X_OUTPUT_driver(WHAT,keys,solver,IQ,IG,DESC,data)
     use pars,           ONLY:SP
     use frequency,      ONLY:w_samp
     use descriptors,    ONLY:IO_desc
     character(*)          :: WHAT
     character(*), optional:: solver
     character(*), optional:: keys
     integer,      optional:: IQ,IG
     type(IO_desc),optional:: DESC
     real(SP),     optional:: data(:)
   end subroutine
   !
   subroutine X_Double_Grid_setup(iq,k,k_FGbz,nTrans_percent,Radius)
     use R_lattice,     ONLY:bz_samp,bz_fine_grid
     use pars,          ONLY:SP
     integer                       :: iq
     type(bz_samp)                 :: k
     type(bz_fine_grid),intent(in) :: k_FGbz
     real(SP),            optional :: nTrans_percent,Radius
   end subroutine
   !
   logical function EPS_2_eels(wv,nw,be_verbose,ordering,eps,eel)
     use pars,          ONLY:SP
     integer              :: nw 
     complex(SP)          :: wv(nw)
     logical              :: be_verbose
     character(2)         :: ordering
     complex(SP) ,optional:: eps(nw),eel(nw)
   end function
   !
   subroutine X_ALLOC_elemental(WHAT,DIM)
     character(*)      :: WHAT
     integer, optional :: DIM(:)
   end subroutine
   !
 end interface
 !
 contains
   !
   logical function X_do_obs(OBS)
     use stderr, ONLY:STRING_match
     character(*)   :: OBS
     integer        :: it
     X_do_obs=.FALSE.
     do it=1,N_X_obs
       if (.not.X_obs(it)%active) cycle
       if (STRING_match(OBS,trim(X_obs(it)%what))      ) X_do_obs=.TRUE.
       if (STRING_match(OBS,trim(X_obs(it)%scattering))) X_do_obs=.TRUE.
     enddo
   end function
   !
   subroutine X_obs_onoff(WHAT)
     use stderr, ONLY:STRING_match
     character(*)   :: WHAT
     integer        :: it
     do it=1,N_X_obs
       if (STRING_match(WHAT,trim(X_obs(it)%what)).and.STRING_match(WHAT,"+" )) X_obs(it)%active=.TRUE.
       if (STRING_match(WHAT,trim(X_obs(it)%what)).and.STRING_match(WHAT,"-")) X_obs(it)%active=.FALSE.
     enddo
   end subroutine
   !
   subroutine X_duplicate(Xi,Xo)
     type(X_t):: Xi,Xo
     Xo%ng           =Xi%ng
     Xo%ng_db        =Xi%ng_db
     Xo%iq           =Xi%iq
     Xo%ib           =Xi%ib
     Xo%ib_lim       =Xi%ib_lim
     Xo%db_ib        =Xi%db_ib
     Xo%db_ib_lim    =Xi%db_ib_lim
     Xo%ngostnts     =Xi%ngostnts
     Xo%mpa_npoles   =Xi%mpa_npoles
     Xo%ordering     =Xi%ordering
     Xo%Vnl_included =Xi%Vnl_included
     Xo%ehe          =Xi%ehe
     Xo%q0           =Xi%q0
     Xo%cg_percentual=Xi%cg_percentual
     Xo%ppaE         =Xi%ppaE
     Xo%Wd           =Xi%Wd
     Xo%KERNEL       =Xi%KERNEL
   end subroutine
   !
   subroutine X_reset(X,type)
     use pars,  ONLY:czero
     integer, optional :: type
     type(X_t)::X
     X%ng           =1
     X%ng_db        =0
     !DEBUG>
     !X%ib           =(/1,-1/)
     !X%ib_lim       =(/1,-1/)
     !DEBUG<
     X%iq           =(/1,-1/)
     X%ngostnts     =0
     X%mpa_npoles   =0
     X%whoami       =0
     X%Vnl_included =.false.
     if (present(type)) X%whoami=type
     X%ehe          =(/-1._SP,-1._SP/)/HA2EV
     X%q0           =(/1._SP,0._SP,0._SP/)
     X%Wd           =cZERO
     X%cg_percentual=100._SP
     X%ppaE         =1._SP
     X%ordering     ="T"
     X%KERNEL       ="none"       
   end subroutine
   !
end module
