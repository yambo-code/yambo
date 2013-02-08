!
! Copyright (C) 2000-2013 C. Hogan and the YAMBO team 
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
subroutine Vnl_driver(en,k)
 use pars,                  ONLY:SP,DP
 use pw_struct_module
 use pw_atoms_module
 use pw_basis_module
 use pw_wfc_module
 use pw_pseudovelocity_module
 use electrons,             ONLY:levels,n_bands,n_spin,n_spinor, n_sp_pol
 use R_lattice,             ONLY:bz_samp
 use wave_func,             ONLY:wf_nc_k, wf_ncx, wf_igk, wf_ng
 use X_m,                   ONLY:X_t
 use pseudo,                ONLY:Vnl
 use com,                   ONLY:msg,warning
 use timing,                ONLY:live_timing
 use memory_m,              ONLY:mem_est
 use IO_m,                  ONLY:io_control,OP_WR_CL,REP
 use mod_com2y,             ONLY:N_G_vectors
 use par_proc_m,            ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m,         ONLY:par_indexes
 implicit none
 !
 type(levels),     intent(in)  :: en     ! Energies 
 type(bz_samp),    intent(in)  :: k      ! K/Q points
 !
 type(pw_pseudovelocity) :: pseudovelocity(n_spinor)
 type(pw_struct), target :: struct
 type(pw_basis), target  :: basis_con  ! Needs to be target for wfc%basis later
 type(pw_atoms)          :: atoms
 type(pw_wfc), pointer   :: wfc_val => null(), wfc_con => null() 
 type(pw_wfc), pointer   :: wfc_val2 => null(), wfc_con2 => null() 
 !
 ! Work space
 !
 complex :: Vnl_cvk(3)
 integer :: ik, ib1, ib2, iv, ic, npwk,ngmax, vb(2), cb(2), i1, ig,io_err,ID
 integer :: i_spin, i_spinor, j_spinor
 type(X_t)         :: X
 type(pp_indexes)  :: px
 integer, external :: io_Vnl
 !
 ! Holds wfc(k) from disk
 !
 complex(SP),allocatable,target  :: wf_disk_SP(:,:,:)

 interface
    subroutine fill_basis(basis,struct,ik,ngmax)
    use pw_struct_module
    use pw_basis_module
    type(pw_struct), target :: struct
    type(pw_basis)  :: basis
    integer, intent(in) :: ik, ngmax
    end subroutine fill_basis
 end interface

 !
 call pp_indexes_reset(px)
 !
 ! Find the Fermi level and occupied/metallic bands
 !
 call k_expand(k)
 call OCCUPATIONS_Fermi(en,k,1)
 !
 vb = (/1,en%nbm/)
 cb = (/en%nbf+1,n_bands/)
 ngmax  = wf_ng
 if (N_G_vectors>0) ngmax=N_G_vectors
 !
 call msg('s',' :: Valence bands         :',vb)
 call msg('s',' :: Conduction bands      :',cb)
 call msg('s',' :: G-vectors             :',ngmax)
 !
 ! Main allocations:
 !
 ! (1) Array storing columns of wfc on disk
 !
 allocate(wf_disk_SP(wf_ncx,n_bands,n_spinor))
 call mem_est("wf_disk_SP",(/size(wf_disk_SP)/),(/SP*2/))
 !
 ! (2) Vnl array to be written to disk. Size depends on input or nelec
 !
 ! Rather than use the minimum size array needed:
 ! allocate(Vnl(3, cb(1):cb(2), vb(1):vb(2), 1:k%nibz, n_spin)) 
 ! we use 1 as the lower band index in both cases for simplicity.
 ! Here vb(2) is always nbm
 !
 allocate(Vnl(3, cb(2), vb(2), k%nibz, n_spin))
 call mem_est("Vnl",(/size(Vnl)/),(/SP*2/))
 Vnl = 0.0
 !
 call msg('s','Import structure data...')
 call fill_struct(struct)
 call msg("l","done")
 !
 call msg('s','Import atoms data...')
 call fill_atoms(atoms,struct)
 call msg("l","done")
 !
 if(n_spinor==2.and.any(.not.atoms%pseudo(:)%psp_has_so))&
&  call warning(' n_spinor=2 but some pseudo does not contain SO')
 !
 call msg('s','Initialize basis data...')
 call pw_basis_init(basis_con,struct)
 call msg("l","done")
 !
 call par_indexes(px,(/k%nibz*n_sp_pol,vb(2)/),low_range=(/1,vb(1)/))
 !
 call live_timing('Pseudovelocity :',px%n_of_elements(myid+1))
 !
 do ik=1,k%nibz
   !
   ! Fill the basis array for this ik, including the basis%k array
   !
   call fill_basis(basis_con,struct,ik,ngmax)
   !
   ! Here the beta functions are moltiplied by spherical armonics
   ! In the case nspinor = 2 I use "spin-angle" and
   ! I've defined pseudovelocity(nspinor).
   ! To include all the SO commutator I should modify
   ! the call to pw_pseudovelocity_braket later and the call to read_wfc...
   call pw_pseudovelocity_init(pseudovelocity,basis_con,atoms)
   !
   ! Read a single wavefunction (ik,isp) from disk:
   ! fill the target wf_disk_SP
   do i_spin=1,n_sp_pol
     !
     call read_wfc_k_sp(ik,i_spin,wf_disk_SP,wf_ncx,n_bands)
     !
     i1=i_spin+(ik-1)*n_sp_pol
     !
     do iv=vb(1),vb(2)  ! Valence
       !
       if(.not.px%element_2D(i1,iv)) cycle
       !
       ! Fill a single valence wavefunction array and basis.
       ! Dimension is fixed by ngmax
       !
       allocate(wfc_val) ! needed for pointer of a pointer, even for %npw
       wfc_val%npw   =   basis_con%npw
       wfc_val%basis => basis_con
       wfc_val%val   => wf_disk_SP(1:wfc_val%npw,iv,1)
       if(n_spinor==2) then
         allocate(wfc_val2) ! needed for pointer of a pointer, even for %npw
         wfc_val2%npw   =   basis_con%npw
         wfc_val2%basis => basis_con
         wfc_val2%val   => wf_disk_SP(1:wfc_val%npw,iv,2)
       endif
       !
       ! Check normalization
       !
       ! I_vk     = pw_wfc_braket(wfc_val,wfc_val) 
       !
       do ic=cb(1),cb(2)  ! Conduction 
         !
         allocate(wfc_con)
         wfc_con%npw   =  basis_con%npw
         wfc_con%basis => basis_con
         wfc_con%val   => wf_disk_SP(1:wfc_con%npw,ic,1)
         if(n_spinor==2) then
           allocate(wfc_con2)
           wfc_con2%npw   =  basis_con%npw
           wfc_con2%basis => basis_con
           wfc_con2%val   => wf_disk_SP(1:wfc_con%npw,ic,2)
         endif
         !
         !== Start consistency tests =====================!
         !
         ! Check normalization and overlap 
         !
         ! I_ck     = pw_wfc_braket(wfc_con,wfc_con) 
         ! O_cvk    = pw_wfc_braket(wfc_val,wfc_con) 
         !
         ! Calculate momentum/velocity moment <v|p|c>
         !
         ! Ev_m_Ec=en%E(iv,ik,i_spin)-en%E(ic,ik,i_spin)
         ! P_cvk    = pw_wfc_p_braket(wfc_val,wfc_con) 
         !
         ! Calculate dipole moment i<v|r|c> = <v|p|c>/(Ev-Ec)
         !
         ! DIP_iR(:,ic,iv,ik,i_spin) = P_cvk(:)/Ev_m_Ec
         !== Stop consistency tests =====================!
         !
         ! Calculate pseudo dipole (velocity) moment <v|[Vnl,r]|c>
         !
         if(n_spinor==2) then
           Vnl_cvk = pw_pseudovelocity_braket(pseudovelocity,&
&                               (/wfc_val,wfc_val2/),(/wfc_con,wfc_con2/))
         else
           Vnl_cvk = pw_pseudovelocity_braket(pseudovelocity,(/wfc_val/),(/wfc_con/))
         endif
         Vnl(:,ic,iv,ik,i_spin) = Vnl_cvk
         !
         if(associated(wfc_con%val)) nullify(wfc_con)
         if(n_spinor==2.and.associated(wfc_con2%val)) nullify(wfc_con2)
       enddo
       if(associated(wfc_val%val)) nullify(wfc_val)
       if(n_spinor==2.and.associated(wfc_val2%val)) nullify(wfc_val2)
       call live_timing(steps=1)
     enddo
   enddo
   do i_spinor=1,n_spinor
     call pw_pseudovelocity_destroy(pseudovelocity(i_spinor))
   enddo
 end do
 !
 call live_timing()
 !
 do i_spin=1,n_sp_pol
   call pp_redux_wait(Vnl(:,:,:,:,i_spin))
 enddo
 !
 ! Write Vnl (pseudovelocity) to disk
 !
 call msg('s','Write Vnl database...')
 X%ib = (/ vb(1),cb(2) /)  ! Full range of bands
 X%ngostnts = ngmax  
 !
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
 io_err=io_Vnl(X,en,ID)
 !
 call msg("l","done")
 !
 return
 !
end subroutine Vnl_driver
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
 use pw_data, only : psfile, nbeta, nmesh, nat_, atm_, tau_, ityp_
 use pw_struct_module
 use pw_atoms_module
 use D_lattice,   ONLY:n_atoms_species_max,n_atomic_species,n_atoms_species, &
&                       atom_pos, Z_species,atomic_number
 implicit none
 type(pw_struct), intent(in) :: struct
 type(pw_atoms) :: atoms
 integer        :: i
 real           :: tmp(3,3),pos(3)
 real,    parameter :: num_2pi     = 2.0 * 3.14159265358979323844
 !
 ! Initialize atoms% types
 !
 call pw_atoms_init(atoms,struct,4.0)      
 !
 ! Allocates all atoms% types and Initializes atoms%pseudo types
 !
 call pw_atoms_set_dim(atoms, nat_, n_atomic_species) ! 
 !
 atoms%names = atm_
 atoms%positions = tau_
 !
 ! Convert from alat to crystal
 !        tmp = atoms%struct%b
 !        pos = num_matmul(transpose(tmp),pos*alat) / num_2pi
 do i=1,nat_
   pos = atoms%positions(:,i)
   tmp = atoms%struct%b
   pos = matmul(transpose(tmp),pos) / num_2pi
   atoms%positions(:,i) = pos
 enddo
 !
 atoms%type_map = ityp_
 !
 ! Fill the atoms%pseudo type for each type
 !
 do i =1,n_atomic_species
   call fill_a_pseudo(atoms%pseudo(i),psfile(i),nbeta(i),nmesh(i))
 enddo
 !
 return
 !
end subroutine fill_atoms
!
subroutine fill_a_pseudo(pseudo,psfile,nbeta, nmesh)
 !
 ! Allocate and fill a single pseudo
 !
 use pw_pseudo_module
 use qexml_module
 use pw_data, only : ecutwfc_
 implicit none
 type(pw_pseudo), intent(out) :: pseudo
 integer, intent(inout)       :: nbeta, nmesh
 character(256), intent(in)   :: psfile
 !
 ! Allocates pseudo% types and Inizializes pseudo%interpolation table
 !
 call pw_pseudo_set_dim(pseudo,nbeta,nmesh)
 !
 ! Read data from UPF files, fill pseudo% types
 !
 call read_pseudo_data(psfile,nmesh,nbeta,pseudo%z,pseudo%mesh,&
&                      pseudo%wmesh,pseudo%vloc,pseudo%lbeta,pseudo%jbeta,&
&                      pseudo%mesh_beta,pseudo%beta,pseudo%d,pseudo%psp_has_so)
 !
 ! Fill the pseudo%interpolation table
 ! ecutwfc(Ha) -> cutoff(Ry)
 !
 call pw_pseudo_set_table(pseudo,real(ecutwfc_)*2.0)
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
 real :: k(3),xk2(3), at(3,3)
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
!
subroutine read_wfc_k_sp(ikibz,i_spin,wf_disk_SP,wf_ncx,n_bands)
 ! Due to the kind type conversion from wf_disk_DP to wfc_val,
 ! cannot use direct pointer assignment => for wfc_val%val
 ! hence create a SP target in read_wfc_k_sp
 use p2y,                 only : verboseIO
 use qexml_module 
 use pars,                only : SP,DP
 use wave_func,           only : wf_nc_k, wf_igk, wf_nb_io_groups,wf_nb_io
 use mod_wf2y,            only : wf_splitter
 use memory_m,            only : mem_est
 use electrons,           only : n_spin,n_sp_pol,n_spinor
 implicit none
 integer     :: npwk, ierr, ib1,ib2, n_b, ib_grp, i_spinor
 integer, intent(in)          :: ikibz,wf_ncx,n_bands,i_spin
 complex(DP), allocatable     :: wf_disk_DP(:,:,:)
 complex(SP),intent(inout)      :: wf_disk_SP(wf_ncx,n_bands,n_spinor)
 !
 wf_nb_io_groups=1
 wf_nb_io=n_bands
 call wf_splitter()
 !
 allocate(wf_disk_DP(wf_ncx, wf_nb_io,n_spinor))
 wf_disk_DP=0._DP
 call mem_est("wf_disk_DP",(/size(wf_disk_DP)/),(/DP*2/))
 !
 npwk = wf_nc_k(ikibz)
 !
 do ib_grp=1,wf_nb_io_groups
   !
   call set_band_block
   !
   if (n_spin==1) call qexml_read_wfc(ibnds=ib1, ibnde=ib2, ik=ikibz, &
&                       wf = wf_disk_DP(1:npwk,1:n_b,1), ierr=ierr)
   if (n_sp_pol==2) call qexml_read_wfc(ibnds=ib1, ibnde=ib2, ik=ikibz, ispin=i_spin, &
&                         wf = wf_disk_DP(1:npwk,1:n_b,1), ierr=ierr)
   if (n_spinor==2) then
     do i_spinor=1,n_spinor
       call qexml_read_wfc(ibnds=ib1, ibnde=ib2, ik=ikibz, ispin=i_spinor, &
&                           wf = wf_disk_DP(1:npwk,1:n_b,i_spinor), ierr=ierr)
     enddo
   endif  
   !
   wf_disk_SP(:,ib1:ib2,:) = wf_disk_DP(:,1:n_b,:)  ! Type conversion
   !
 enddo
 !
 deallocate(wf_disk_DP)
 call mem_est("wf_disk_DP")
 !
 return
 !
contains
!
subroutine set_band_block
 use pars,                ONLY:schlen
 use com,                 ONLY:msg
 character(schlen)       ::sch
 !
 ib1 = (ib_grp-1)*wf_nb_io + 1
 ib2 = min(ib_grp*wf_nb_io,n_bands)
 n_b = ib2-ib1+1
 !
 if(verboseIO) then
   write(sch,'(a,i2,a,i4,a,i4,a)') &
&  "  Filling block ",ib_grp," bands: [",ib1,":",ib2,"]"
   write(*,*) trim(sch)
   call msg('s',trim(sch))
 endif
 end subroutine set_band_block
 !
end subroutine read_wfc_k_sp

