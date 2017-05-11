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
module R_lattice
 !
 use pars, ONLY:SP,schlen, IP
#include<memory.h>
 !
 ! K lattice
 !
 integer  :: k_grid(3)
 integer  :: k_grid_dim
 real(SP) :: k_grid_uc_vol
 real(SP) :: k_grid_b(3,3)
 logical  :: Xk_grid_is_uniform
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
 real(SP) :: q0_def_norm, q0_shift_norm, smallest_q_norm
 real(SP),allocatable :: q_norm(:)
 character(schlen)    :: q_source="User defined / from ndb.kindx."
 !
 type bz_samp
   !
   ! Coarse grid
   !
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
   !
   ! Fine grid
   !
   integer           :: nibz_FineGd
   integer           :: nbz_FineGd                     
   integer,  pointer :: sstar_FineGd(:)   => null()
   real(SP), pointer :: pt_FineGd(:,:)    => null()
   real(SP), pointer :: ptbz_FineGd(:,:)  => null() 
   real(SP), pointer :: weights_FineGd(:) => null()
   !
   ! Double grid: Mapping Fine to Coarse grid
   !
   integer           :: nibz_duplicate
   integer           :: nbz_duplicate
   integer,  pointer :: nibz_mapped(:)    => null()  ! For each IBZ kpt in the Cg, n° of IBZ kpt_FineGd associated
   integer,  pointer :: nbz_mapped(:)     => null()  ! For each IBZ kpt in the Cg, n° of IBZ kpt_FineGd associated
   integer,  pointer :: ibz_DbGd_map(:,:) => null()  ! map kpt_FineGd in the IBZ --> kpts in the IBZ
   integer,  pointer :: bz_DbGd_map(:,:)  => null()  ! map kpt_FineGd in the  BZ --> kpts in the  BZ
   real(SP), pointer :: weights_DbGd(:)   => null()  
   real(SP), pointer :: weights_ipol(:)   => null()  ! something used by mapping with wannier
   !                                               
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
 !
 ! Coulomb (including Cutoff)
 !
 real(SP)                :: cyl_ph_radius
 real(SP)                :: cyl_length
 real(SP)                :: box_length(3)
 real(SP)                :: cyl_cut
 character(schlen)       :: cut_geometry
 character(schlen)       :: cut_description
 complex(SP),allocatable :: bare_qpg(:,:)
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
   subroutine qindx_alloc()
     !
     YAMBO_ALLOC(qindx_X,(nqibz,nXkbz,2))
     if (.not.Xk_grid_is_uniform) return
     !
     YAMBO_ALLOC(qindx_S,(QP_states_k(2),nqbz,2))
     !     
     if (bse_scattering) then
       YAMBO_ALLOC(qindx_B,(nXkbz,nXkbz,2))
     endif
     !
     if (coll_scattering) then
       YAMBO_ALLOC(qindx_C,(nXkbz,nXkbz,2))
     endif
     !
   end subroutine
   !
   subroutine qindx_free()
     YAMBO_FREE(qindx_X)
     YAMBO_FREE(qindx_B)
     YAMBO_FREE(qindx_S)
     YAMBO_FREE(qindx_C)
   end subroutine qindx_free
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
     ! Double Grid
     !
     k%nbz_FineGd=0
     k%nibz_FineGd=0
     !
     if(associated(k%sstar_FineGd))    deallocate(k%sstar_FineGd)
     if(associated(k%pt_FineGd))       deallocate(k%pt_FineGd)
     if(associated(k%pt_FineGd))       deallocate(k%ptbz_FineGd)
     if(associated(k%weights_FineGd))  deallocate(k%weights_FineGd)
     nullify(k%sstar_FineGd,k%pt_FineGd,k%pt_FineGd,k%weights_FineGd)
     !
     if(associated(k%nibz_mapped))     deallocate(k%nibz_mapped)
     if(associated(k%nbz_mapped))      deallocate(k%nbz_mapped)
     if(associated(k%ibz_DbGd_map))    deallocate(k%ibz_DbGd_map)
     if(associated(k%bz_DbGd_map))     deallocate(k%bz_DbGd_map)
     if(associated(k%weights_DbGd))    deallocate(k%weights_DbGd)
     if(associated(k%weights_ipol))    deallocate(k%weights_ipol)
     nullify(k%nibz_mapped,k%nbz_mapped,k%ibz_DbGd_map,k%bz_DbGd_map,k%weights_DbGd,k%weights_ipol)
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
     !
     BZo%nbz_FineGd =BZi%nbz_FineGd
     BZo%nibz_FineGd=BZi%nibz_FineGd
     !
     if (associated( BZi%sstar_FineGd )) then
       allocate(BZo%sstar_FineGd(BZo%nbz_FineGd))
       BZo%sstar_FineGd=BZi%sstar_FineGd
     endif
     if (associated( BZi%pt_FineGd )) then
       allocate(BZo%pt_FineGd(BZo%nibz_FineGd,3))
       BZo%pt_FineGd=BZi%pt_FineGd
     endif
     if (associated( BZi%ptbz_FineGd )) then
       allocate(BZo%ptbz_FineGd(BZo%nbz_FineGd,3))
       BZo%ptbz_FineGd=BZi%ptbz_FineGd
     endif
     !
     BZo%nbz_duplicate =BZi%nbz_duplicate
     BZo%nibz_duplicate=BZi%nibz_duplicate
     !
     if (associated( BZi%nibz_mapped )) then
       allocate(BZo%nibz_mapped(BZo%nibz))
       BZo%nibz_mapped=BZi%nibz_mapped
     endif
     if (associated( BZi%nbz_mapped )) then
       allocate(BZo%nbz_mapped(BZo%nbz))
       BZo%nbz_mapped=BZi%nbz_mapped
     endif
     if (associated( BZi%ibz_DbGd_map )) then
       allocate(BZo%ibz_DbGd_map(2,BZo%nibz))
       BZo%ibz_DbGd_map=BZi%ibz_DbGd_map
     endif
     if (associated( BZi%bz_DbGd_map )) then
       allocate(BZo%bz_DbGd_map(2,BZo%nbz))
       BZo%ibz_DbGd_map=BZi%bz_DbGd_map
     endif
     !
     ! To code the duplication of the Fine grid and DbGd structure
     !
     !real(SP), pointer :: weights_DbGd(:)   => null()
     !real(SP), pointer :: weights_ipol(:)   => null()  ! something used by mapping with wannier
     !
     return
     !
   end subroutine bz_samp_duplicate
   !
   subroutine cutoff_presets()
     Box_length=0.
     cyl_ph_radius=0.
     box_length=0.
     cyl_length=0.
     cut_geometry='none'
     cut_description='none'
     CUTOFF_plus_RIM=.false.
   end subroutine
   !
end module R_lattice
