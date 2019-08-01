!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CH, AF, IM
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
subroutine PP_PWscf_driver(k)
 !
 use pw_struct_module
 use pw_atoms_module
 use pw_basis_module
 use pw_wfc_module
 !
 use pars,                  ONLY:SP,DP
 use electrons,             ONLY:levels,n_bands,n_spin,n_spinor,n_sp_pol
 use R_lattice,             ONLY:bz_samp,ng_vec
 use D_lattice,             ONLY:n_atomic_species
 use wave_func,             ONLY:wf_nc_k, wf_ncx, wf_igk, wf_ng
 use X_m,                   ONLY:X_t
 use pseudo,                ONLY:PP_alloc_PWscf,PP_free,pp_n_l_comp,     &
&                                pp_n_l_times_proj_max,pp_n_l_max,pp_table,&
&                                PP_nlcc_alloc,PP_nlcc_free
 use com,                   ONLY:msg,warning
 use LIVE_t,                ONLY:live_timing
 use IO_m,                  ONLY:io_control,OP_WR_CL,OP_APP_CL,REP
 use interfaces,            ONLY:PARALLEL_index
 use read_pseudo_mod,       ONLY: readpp
 implicit none
 !
 type(bz_samp),    intent(in)  :: k      ! K/Q points
 !
 type(pw_struct), target :: struct
 type(pw_basis), target  :: basis_con    ! Needs to be target for wfc%basis later
 type(pw_atoms)          :: atoms
 !
 ! Work space
 !
 integer :: ik, ngmax,io_err,ID
 integer :: ibeta,itype
 character(64)  :: input_dft="none"
 integer, external :: io_KB_pwscf
 integer, external :: io_NLCC_pwscf
 integer, external :: io_USPP_pwscf
 !
 interface
    subroutine fill_basis(basis,struct,ik,ngmax)
      use pw_struct_module
      use pw_basis_module
      implicit none
      type(pw_struct), target :: struct
      type(pw_basis)  :: basis
      integer, intent(in) :: ik, ngmax
    end subroutine fill_basis
    !
    subroutine fill_basis_rho(basis,struct,ngmax)
      use pw_struct_module
      use pw_basis_module
      implicit none
      type(pw_struct), target :: struct
      type(pw_basis)      :: basis
      integer, intent(in) :: ngmax
    end subroutine fill_basis_rho
 end interface
 !
 call k_expand(k)
 !
 ngmax  = wf_ng
 !
 ! Main allocations:
 !
 call fill_struct(struct)
 call fill_atoms(atoms,struct)
 !
 call pw_basis_init(basis_con,struct)
 !
 if( all(atoms%pseudo(:)%nbeta==0) ) return
 pp_n_l_times_proj_max=maxval((/atoms%pseudo(:)%nbeta/))
 !
 allocate(pp_table(3,n_atomic_species,pp_n_l_times_proj_max))
 allocate(pp_n_l_comp(n_atomic_species))
 call PP_alloc_PWscf()
 pp_table=0
 !
 pp_n_l_max=0
 do itype=1,n_atomic_species
   pp_n_l_comp(itype)=maxval((/atoms%pseudo(itype)%lbeta(:)/))
   if(atoms%pseudo(itype)%nbeta==0) pp_n_l_comp(itype)=0   
   do ibeta=1,atoms%pseudo(itype)%nbeta
     pp_n_l_max=(max(pp_n_l_max,atoms%pseudo(itype)%lbeta(ibeta)+1))
     pp_table(1,itype,ibeta) = atoms%pseudo(itype)%lbeta(ibeta)+1               ! l+1
     pp_table(2,itype,ibeta) = 0
     if(atoms%pseudo(itype)%has_so) &
&      pp_table(2,itype,ibeta) = nint(2._SP*atoms%pseudo(itype)%jbeta(ibeta))   ! 2j
   enddo
 enddo
 pp_table(3,:,:)=1                                                              ! pp_spin
 !
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),ID=ID)
 io_err=io_KB_pwscf(ID)
 !
 do ik=1,k%nibz
   !
   call fill_basis(basis_con,struct,ik,ngmax)
   call PP_PWscf_comp(basis_con,atoms)
   !
   ! Write pseudovelocity to disk
   !
   call io_control(ACTION=OP_APP_CL,COM=REP,SEC=(/ik+1/),ID=ID)
   io_err=io_KB_pwscf(ID)
   !
 end do
 !
 ! write NLCC info if needed
 !
 if( any(atoms%pseudo(:)%has_nlcc) ) then
   !
   call PP_nlcc_alloc()
   !
   call fill_basis_rho(basis_con,struct,ng_vec)
   call PP_PWscf_comp_nlcc(basis_con,atoms)
   !
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),ID=ID)
   io_err=io_NLCC_pwscf(ID)
   !
   call PP_nlcc_free()
   !
 endif
 !
 ! write USPP data
 !
 if ( any(atoms%pseudo(:)%is_uspp) ) then
   !
   call qe_pseudo_init()
   call readpp(input_dft)
   !
   call pre_init()
   call allocate_nlpot()
   call init_us_1()
   !
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),ID=ID)
   io_err=io_USPP_pwscf(ID)
   !
!   call PP_uspp_free()
   !
 endif
 !
 ! cleanup
 !
 call PP_free()
 deallocate(pp_table,pp_n_l_comp)
 !
 return
 !
end subroutine PP_PWscf_driver
!
subroutine fill_struct(struct)
 use pw_struct_module
 use pars,                  only : pi
 use R_lattice,             ONLY : b
 use D_lattice,             ONLY : DL_vol, a
 implicit none
 type(pw_struct), intent(out) :: struct
 !
 ! call pw_struct_set  ! probably not needed
 !
 struct%a = transpose(a)  ! should be ok
 struct%b = transpose(b)  ! This is the only source of the b vector array in p2y
 struct%a_omega = DL_vol
 struct%b_omega = 0.0 ! not used
 !
 return
end subroutine fill_struct
!
subroutine fill_atoms(atoms,struct)
 !
 !type pw_atoms
 !  type (pw_struct), pointer :: struct
 !  integer                   :: natoms,ntypes
 !  type (pw_pseudo), pointer :: pseudo(:)
 !  character(100),   pointer :: names(:)
 !  real,             pointer :: positions(:,:)
 !  integer,          pointer :: type_map(:)
 !  real                      :: cutoff_vloc
 !end type pw_atoms
 use pw_data,           ONLY: psfile, nbeta, nmesh, nat_, atm_, tau_, ityp_
 use pw_struct_module
 use pw_atoms_module
 use pw_pseudo_module
 use D_lattice,         ONLY: n_atoms_species_max,n_atomic_species,n_atoms_species, &
&                             atom_pos, Z_species,atomic_number
 implicit none
 type(pw_struct), intent(in) :: struct
 type(pw_atoms) :: atoms
 integer        :: i
 !
 ! Initialize atoms% types
 !
 call pw_atoms_init(atoms,struct,4.0)      
 !
 ! Allocates all atoms% types and Initializes atoms%pseudo types
 !
 call pw_atoms_set_dim(atoms, nat_, n_atomic_species) ! 
 !
 ! Fill the atoms%pseudo type for each type
 !
 do i =1,n_atomic_species
   call fill_a_pseudo(psfile(i),atoms%pseudo(i))
 enddo
 !
 return
 !
end subroutine fill_atoms
!
subroutine fill_a_pseudo(psfile,pseudo)
 !
 ! Allocate and fill a single pseudo
 !
 use pars,             only:SP
 use pw_pseudo_module
 use pw_pseudo_read_m, only: pseudo_read_data
 use pw_data,          only: ecutwfc_,ecutrho_
 implicit none
 type(pw_pseudo), intent(inout) :: pseudo
 character(256),  intent(in)    :: psfile
 
 !
 ! Read data from UPF files, fill pseudo% types
 !
 call pseudo_read_data(psfile,pseudo)
 !
 ! Fill the pseudo%interp table
 ! ecutwfc(Ha) -> cutoff(Ry)
 !
 call pw_pseudo_set_table(pseudo,real(ecutwfc_,SP)*2._SP,real(ecutrho_,SP)*2._SP)
 !
 return
 !
end subroutine fill_a_pseudo
!
subroutine fill_basis(basis,struct,ik,ngmax)
 use pw_struct_module
 use pw_basis_module
 use pw_data,               ONLY : igv_, xk_,alat_
 use wave_func,             ONLY : wf_nc_k, wf_ncx, wf_igk, wf_ng
 implicit none
 type(pw_struct), target :: struct
 type(pw_basis)  :: basis   
 integer, intent(in) :: ik, ngmax
 real :: xk2(3), at(3,3)
 integer :: npw, ic, i
 !
 ! Note: this is not correct, as it is comparing #Gs with #components!
 !
 npw = min(ngmax,wf_nc_k(ik))   ! The number of *components* <= index of max G vectors
 basis%struct => struct
 !
 ! Allocates basis%g(npw) and sets basis%npw
 !
 call pw_basis_set_npw(basis,npw)
 !
 ! wf_igk_ is the index_ array of components for all k-points, and
 ! links a component index to a G-vector in the global G-vector array igv_
 ! Table correspondance G-vec <-> Components: G_ic = wf_igk(ic,ik)
 !
 ! Since yambo converts k to its own basis, and G to a real array,
 ! it is simpler to use the original arrays read from PW
 !
 do ic=1,npw
   basis%g(1:3,ic) =  igv_(1:3,wf_igk(ic,ik))  
 enddo
 basis%r0 = (/ 0.0, 0.0, 0.0 /)  ! not used
 !
 ! Convert kpts: xk_ [cart,2pi/alat] units to basis%k [units of struct%b(RL crystal)]
 !
 at = struct%a/alat_
 do i = 1,3
   xk2(i) = at(1,i)*xk_(1,ik) + at(2,i)*xk_(2,ik) + at(3,i)*xk_(3,ik)
 enddo
 basis%k  = xk2
 ! 
 return
end subroutine fill_basis

subroutine fill_basis_rho(basis,struct,ngmax)
 use pw_struct_module
 use pw_basis_module
 use pw_data,               ONLY : igv_, ngm_
 implicit none
 type(pw_struct), target :: struct
 type(pw_basis)      :: basis   
 integer, intent(in) :: ngmax
 real :: at(3,3)
 integer :: npw, ic, i
 !
 npw = min(ngm_,ngmax)
 basis%struct => struct
 !
 ! Allocates basis%g(npw) and sets basis%npw
 !
 call pw_basis_set_npw(basis,npw)
 !
 do ic=1,npw
   basis%g(1:3,ic) =  igv_(1:3,ic)  
 enddo
 basis%r0 = (/ 0.0, 0.0, 0.0 /)  ! not used
 ! unused
 basis%k  = 0.0
 ! 
 return
end subroutine fill_basis_rho
