!
! Copyright (C) 2000-2011 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
 implicit none
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
 real(SP) :: q0_def_norm
 real(SP),allocatable :: q_norm(:)
 type bz_samp
   integer           :: nibz,nbz
   integer,  pointer :: nstar(:)
   integer,  pointer :: star(:,:)
   integer,  pointer :: sstar(:,:)
   real(SP), pointer :: pt(:,:)
   real(SP), pointer :: ptbz(:,:)
   real(SP), pointer :: weights(:)
   character(6)      :: description
   character(1)      :: units
 end type bz_samp
 !
 ! Q/K sampling shadow tables
 !
 real(SP),allocatable :: k_pt(:,:)
 real(SP),allocatable :: q_pt(:,:)
 integer ,allocatable :: q_sstar(:,:)
 !
 ! Q/K-tables (scattering geometry)
 !
 integer  :: qp_states_k(2)
 logical  :: bse_scattering
 integer, allocatable :: qindx_X(:,:,:)
 integer, allocatable :: qindx_B(:,:,:)
 integer, allocatable :: qindx_S(:,:,:)
 integer, allocatable :: ik_is_table(:,:)
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
 contains
   !
   subroutine qindx_alloc()
     use memory_m, ONLY:mem_est
     integer :: err(3)
     err=0
     allocate(qindx_X(nqibz,nXkbz,2),stat=err(1))
     if (.not.Xk_grid_is_uniform) then
       call mem_est("qindx_X",(/size(qindx_X)/),(/IP/),(/err(1)/))
       return
     endif
     allocate(qindx_S(qp_states_k(2),nqbz,2),stat=err(2))
     if (bse_scattering) allocate(qindx_B(nXkbz,nXkbz,2),stat=err(3))
     if (bse_scattering) call mem_est("qindx_X qindx_S qindx_B",&
&                        (/size(qindx_X),size(qindx_S),size(qindx_B)/),&
&                        elements_kind=(/IP,IP,IP/),errors=err)
     if (.not.bse_scattering) call mem_est("qindx_X qindx_S",&
&                             (/size(qindx_X),size(qindx_S)/),&
&                             elements_kind=(/IP,IP/),errors=err(:2))
   end subroutine
   !
   subroutine qindx_free()
     use memory_m, ONLY:mem_est
     deallocate(qindx_X)
     call mem_est("qindx_X")
     if (allocated(qindx_S)) then
       deallocate(qindx_S)
       call mem_est("qindx_S")
     endif
     if (allocated(qindx_B)) then
       deallocate(qindx_B)
       call mem_est("qindx_B")
     endif
   end subroutine qindx_free
   !
   subroutine bz_samp_reset(k)
     type(bz_samp)::k
     k%nibz=0
     k%nbz=0
     k%units=' '
     k%description=' '
     if(associated(k%nstar))    deallocate(k%nstar)
     if(associated(k%star))     deallocate(k%star)
     if(associated(k%sstar))    deallocate(k%sstar)
     if(associated(k%pt))       deallocate(k%pt)
     if(associated(k%ptbz))     deallocate(k%ptbz)
     if(associated(k%weights))  deallocate(k%weights)
     !
     nullify(k%nstar,k%star,k%sstar,k%pt,k%ptbz,k%weights)
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
     allocate( BZo%pt(BZo%nibz,3) )
     BZo%pt(:,:) = BZi%pt(:,:)
     if (associated( BZi%nstar )) then
       allocate( BZo%nstar(BZo%nibz) ) 
       BZo%nstar=BZi%nstar
     endif
     if (associated( BZi%weights )) then
       allocate( BZo%weights(BZo%nibz) ) 
       BZo%weights=BZi%weights
     endif
     if (associated( BZi%star )) then
       allocate( BZo%star(BZo%nibz, size(BZi%star,2) ) ) 
       BZo%star=BZi%star
     endif
     if (associated( BZi%sstar )) then
       allocate( BZo%sstar(BZo%nbz,2) ) 
       BZo%sstar=BZi%sstar
     endif
     if (associated( BZi%ptbz )) then
       allocate( BZo%ptbz(BZo%nbz,3) ) 
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
     cut_geometry='none'
     cut_description='none'
     CUTOFF_plus_RIM=.false.
   end subroutine
   !
end module R_lattice
