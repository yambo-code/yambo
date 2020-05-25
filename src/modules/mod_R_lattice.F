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
module R_lattice
 !
 use pars, ONLY:SP,schlen, IP
 !
#include<dev_defs.h>
#include<memory.h>
 !
 ! K lattice
 !
 integer  :: k_grid(3)
 integer  :: k_grid_dim
 real(SP) :: k_grid_uc_vol
 real(SP) :: k_grid_b(3,3)
 logical  :: Xk_grid_is_uniform
 logical  :: Xk_grid_new_mapping
 !
 ! Q/K-sampling
 !
 integer  :: nqbz
 integer  :: nqibz
 integer  :: nkbz
 integer  :: nkibz
 integer  :: nXkbz
 integer  :: nXkibz
 real(SP) :: RL_vol
 real(SP) :: d3k_factor
 real(SP) :: d3q_factor
 real(SP) :: b(3,3)
 real(SP) :: q0_def_norm, q0_shift_norm
 real(SP),allocatable :: q_norm(:)
 character(schlen)    :: q_source="User defined / from ndb.kindx."
 !
 type bz_samp
   integer           :: nibz,nbz
   integer,  allocatable :: nstar(:)      ! n° of points in the star
   integer,  allocatable :: star(:,:)     ! ik,ikstar --> is     is sends ik in ikstar
   integer,  allocatable :: sstar(:,:)    ! ik_bz     --> ik,is  is sends ik in ikbz
   integer,  allocatable :: s_table(:,:)  ! ik,is     --> sym@k_bz  revers of sstar(2)
   integer,  allocatable :: k_table(:,:)  ! ik,is     --> ik_bz     revers of sstar(1)
   real(SP), allocatable :: pt(:,:)      
   real(SP), allocatable :: ptbz(:,:)    
   real(SP), allocatable :: weights(:)   
   character(6)      :: description
   character(1)      :: units
 end type bz_samp
 !
 type bz_map
   integer, allocatable :: k_map_inv(:,:)
   integer, allocatable :: k_map_dir(:,:,:)
   integer      :: g0_idx(3,2)
   integer      :: max_kdir(3)
   real(SP)     :: g0_length(3)
   real(SP)     :: q_step(3)
 end type bz_map 
 !
 ! Q/K sampling shadow tables
 !
 real(SP),allocatable :: k_pt(:,:)
 real(SP),allocatable :: q_pt(:,:)
 integer ,allocatable :: q_sstar(:,:)
 !
 ! Q/K-tables (scattering geometry)
 !
 integer  :: QP_states_k(2)
 logical  :: bse_scattering
 logical  :: coll_scattering
 integer, allocatable :: qindx_X(:,:,:)
 integer, allocatable :: qindx_B(:,:,:)
 integer, allocatable :: qindx_S(:,:,:)
 integer, allocatable :: qindx_C(:,:,:)
 integer  :: qindx_B_max
 !
 ! SYMs
 !
 real(SP),allocatable :: rl_sop(:,:,:)
 !
 ! RL lattice
 !
 integer              :: n_g_shells,ng_vec,ng_closed
 integer ,allocatable :: ng_in_shell(:)
 integer ,allocatable :: g_rot(:,:)
 integer ,allocatable :: G_m_G(:,:)
 integer ,allocatable :: minus_G(:)
 real(SP),allocatable :: g_vec(:,:)
 real(SP),allocatable :: E_of_shell(:)
 ! GPUs
 real(SP),allocatable DEV_ATTRIBUTE :: g_vec_d(:,:)
 integer ,allocatable DEV_ATTRIBUTE :: g_rot_d(:,:)
 integer ,allocatable DEV_ATTRIBUTE :: G_m_G_d(:,:)
 integer ,allocatable DEV_ATTRIBUTE :: minus_G_d(:)
 !
 ! RIM
 !
 integer :: RIM_ng
 integer :: RIM_id_epsm1_reference
 integer :: RIM_n_rand_pts
 logical :: RIM_is_diagonal
 real(SP):: RIM_RL_vol
 real(SP):: RIM_epsm1(3)
 real(SP):: RIM_anisotropy
 real(SP),   allocatable :: RIM_qpg(:,:,:)
 real(SP),   allocatable :: RIM_qpg_d(:,:,:)
#ifdef _CUDA
 attributes(device) :: RIM_qpg_d
#endif
 !
 ! Coulomb (including Cutoff)
 !
 real(SP)                :: cyl_ph_radius
 real(SP)                :: cyl_length
 real(SP)                :: box_length(3)
 real(SP)                :: cyl_cut
 real(SP)                :: ws_cutoff 
 character(schlen)       :: cut_geometry
 character(schlen)       :: cut_description
 complex(SP),allocatable :: bare_qpg(:,:)
 complex(SP),allocatable :: bare_qpg_d(:,:)
#ifdef _CUDA
 attributes(device) :: bare_qpg_d
 ! AF: for the sake of performance, this variable is defined 
 !     as the transpose of the host variable bare_qpg
#endif
 logical                 :: CUTOFF_plus_RIM
 real(SP)                :: cyl_vr_save
 real(SP)                :: cyl_vz_save
 real(SP)                :: cyl_zz_save
 !
 ! BZ map by direction
 !
 type(bz_map) :: k_map
 !
 ! Shift by a G-vector associated for points out of the BZ
 ! because of symmtry operations
 ! 
 integer, allocatable :: WF_shifts(:,:)
 !
 interface
   !
   integer function k_the_nearest(k_ref,k,n_k,k_dist,ROTATE_k_ref,k_symm,i_err)
     use pars, ONLY: SP
     integer :: n_k
     real(SP):: k_ref(3)
     real(SP):: k(n_k,3)
     real(SP),optional :: k_dist(n_k)
     logical, optional :: ROTATE_k_ref
     integer, optional :: k_symm
     integer, optional :: i_err
   end function 
   !
 end interface
 !
 contains
   !
   subroutine qindx_alloc(what)
     !
     use parallel_m,   ONLY:PAR_K_scheme
     !
     character(*), intent(in) :: what
     !
     if(  index(what,"X")/=0 .or. index(what,"ALL")/=0) then
       YAMBO_ALLOC(qindx_X,(nqibz,nXkbz,2))
       qindx_X=0
     endif
     !
     if( (index(what,"S")/=0 .or. index(what,"ALL")/=0) .and. Xk_grid_is_uniform) then
       YAMBO_ALLOC(qindx_S,(QP_states_k(2),nqbz,2))
       qindx_S=0
     endif
     !
     if( (index(what,"B")/=0 .or. index(what,"ALL")/=0) .and. Xk_grid_is_uniform .and. bse_scattering) then
       YAMBO_ALLOC(qindx_B,(nXkbz,PAR_K_scheme%nbz,2))
       qindx_B=0
     endif
     !
     if( (index(what,"C")/=0 .or. index(what,"ALL")/=0) .and. Xk_grid_is_uniform .and. coll_scattering) then
       YAMBO_ALLOC(qindx_C,(nXkbz,nXkbz,2))
       qindx_C=0
     endif
     !
   end subroutine
   !
   subroutine qindx_free(what)
     !
     character(*), intent(in) :: what
     !
     if(  index(what,"X")/=0 .or. index(what,"ALL")/=0) then
       YAMBO_FREE(qindx_X)
     endif
     if(  index(what,"B")/=0 .or. index(what,"ALL")/=0) then
       YAMBO_FREE(qindx_B)
     endif
     if(  index(what,"S")/=0 .or. index(what,"ALL")/=0) then
       YAMBO_FREE(qindx_S)
     endif
     if(  index(what,"C")/=0 .or. index(what,"ALL")/=0) then
       YAMBO_FREE(qindx_C)
     endif
   end subroutine qindx_free
   !
   integer function qindx_B_init(ID)
    !
    use pars,       ONLY:IP
    use IO_m,       ONLY:io_control,io_connect,def_variable_bulk,OP_RD,RD
    integer  :: ID
    !
    call io_control(ACTION=OP_RD,ID=ID)
    qindx_B_init=io_connect(desc='kindx',type=1,ID=ID)
    call def_variable_bulk(ID,"Bindx",3,(/nXkbz,nXkbz,2/),IP+100,par_io_kind='independent')
    call io_control(ACTION=RD,ID=ID)
    !
   end function qindx_B_init
   !
   integer function qindx_B_close(ID)
    !
    use IO_m,  ONLY:io_control,io_disconnect,RD_CL
    integer :: ID
    !
    call io_control(ACTION=RD_CL,ID=ID)
    call io_disconnect(ID=ID)
    qindx_B_close=0
    !
   end function qindx_B_close
   !
   function qindx_B_load(ikbz,ikp_bz,ID)
    !
    use IO_m,  ONLY:io_variable_bulk
    integer, dimension(2) :: qindx_B_load
    integer, intent(in) :: ID,ikbz,ikp_bz
    integer :: IO_var(1,1,2)
    !
    call io_variable_bulk(ID,3,I3=IO_var,IPOS=(/ikbz,ikp_bz,1/))
    qindx_B_load=IO_var(1,1,:)
    !
   end function qindx_B_load
   !
   subroutine bz_samp_reset(k)
     type(bz_samp)::k
     k%nibz=0
     k%nbz=0
     k%units=' '
     k%description=' '
     !
     YAMBO_FREE(k%s_table)
     YAMBO_FREE(k%k_table)
     YAMBO_FREE(k%nstar)
     YAMBO_FREE(k%star)
     YAMBO_FREE(k%sstar)
     YAMBO_FREE(k%pt)
     YAMBO_FREE(k%ptbz)
     YAMBO_FREE(k%weights)
     !
   end subroutine bz_samp_reset
   !
   subroutine bz_samp_duplicate(BZi, BZo)
     type(bz_samp), intent(in)    :: BZi
     type(bz_samp), intent(inout) :: BZo
     BZo%nibz  = BZi%nibz
     BZo%nbz   = BZi%nbz
     BZo%units = BZi%units
     BZo%description = BZi%description
     YAMBO_ALLOC(BZo%pt,(BZo%nibz,3))
     BZo%pt(:,:) = BZi%pt(:,:)
     if (allocated( BZi%nstar )) then
       YAMBO_ALLOC(BZo%nstar,(BZo%nibz))
       BZo%nstar=BZi%nstar
     endif
     if (allocated( BZi%weights )) then
       YAMBO_ALLOC(BZo%weights,(BZo%nibz))
       BZo%weights=BZi%weights
     endif
     if (allocated( BZi%star )) then
       YAMBO_ALLOC(BZo%star,(BZo%nibz,size(BZi%star,2)))
       BZo%star=BZi%star
     endif
     if (allocated( BZi%sstar )) then
       YAMBO_ALLOC(BZo%sstar,(BZo%nbz,2))
       BZo%sstar=BZi%sstar
     endif
     if (allocated( BZi%ptbz )) then
       YAMBO_ALLOC(BZo%ptbz,(BZo%nbz,3))
       BZo%ptbz=BZi%ptbz
     endif
   return
   end subroutine bz_samp_duplicate
   !
   subroutine cutoff_presets()
     Box_length=0.
     cyl_ph_radius=0.
     box_length=0.
     cyl_length=0.
     ws_cutoff=0.7
     cut_geometry='none'
     cut_description='none'
     CUTOFF_plus_RIM=.false.
   end subroutine
   !
end module R_lattice
