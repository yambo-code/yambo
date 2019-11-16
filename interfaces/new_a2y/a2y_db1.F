!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CH DS HM
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
subroutine a2y_db1(en,k,ncid)
 !
 use netcdf
 use netcdf_data
 use pars,                 only : SP,pi
 use com,                  only : msg, error
 use electrons,            only : levels, n_bands, n_spin,n_sp_pol,n_spinor, l_spin_orbit
 use wave_func,            only : wf_ncx
 use R_lattice,            only : ng_vec, bz_samp, nkibz
 use D_lattice,            only : nsym, n_atoms, n_atomic_species
 use mod_com2y,            only : print_interface_dimensions,  artificial_spin_pol
 !
#include<memory.h>
 !
 type(levels),   intent(inout) :: en
 type(bz_samp),  intent(inout) :: k
 integer,        intent(in)    :: ncid
 ! 
 ! Work Space
 !
 integer                       :: i1,i2,ik,ib,is
 integer                       :: dimid, netcdf_error
 !
 call msg('s','Header/K-points/Energies...')
 !
 ! Directly read dimensions
 !
 n_bands  = netcdf_get_dimension(ncid,"max_number_of_states")
 nkibz    = netcdf_get_dimension(ncid,"number_of_kpoints")
 n_spinor = netcdf_get_dimension(ncid,"number_of_spinor_components")
 n_sp_pol = netcdf_get_dimension(ncid,"number_of_spins")
 n_atoms  = netcdf_get_dimension(ncid,"number_of_atoms")
 wf_ncx   = netcdf_get_dimension(ncid,"max_number_of_coefficients")
 n_atomic_species = netcdf_get_dimension(ncid,"number_of_atom_species")

 if (n_sp_pol==1.and.artificial_spin_pol) n_sp_pol=2

 n_spin  = max(n_spinor, n_sp_pol)
 k%nibz = nkibz
 en%nb  = n_bands
 ng_vec = wf_ncx
 call msg("l","done")
 !
 ! Geometry (must appear first, in order to define alat)
 !
 call msg('s','Cell data...')
 call import_geometry_group
 call msg("l","...done")
 !
 ! Electrons
 !
 call msg('s','Eigenvalues data...')
 call import_electrons_group
 call msg("l","done")
 !
 ! K-points
 !
 call msg('s','K-points mesh...')
 call import_kpoints_group
 call msg('l','done')
 !
 ! PP 
 !
 call msg('s','Pseudopotential...')
 if(trim(NETCDF_kind)=='KSS') call import_gwdata_group_kss
 if(trim(NETCDF_kind)=='WFK') call import_gwdata_group_wfk
 call msg('l','done')
 !
 ! G-vectors grids and indices
 !
 if(trim(NETCDF_kind)=='KSS') call import_basisdata_group_kss
 if(trim(NETCDF_kind)=='WFK') call import_basisdata_group_wfk
 call msg('s','RL vectors...done')
 !
 ! All other data
 !
 call import_miscellaneous_data
 call msg('s','Misc data...done')
 !
 ! Report
 !
 call print_interface_dimensions(en,k)

 return

contains
 
  !
  ! Geometry group subroutines
  !
  subroutine import_geometry_group
    use vec_operate,          only : cross_product
    use R_lattice,            only : b
    use D_lattice,            only : nsym, i_time_rev, i_space_inv, dl_sop, &
&                                    DL_vol, a, alat, &
&                                    n_atomic_species,n_atoms_species,&
&                                    n_atoms_species_max,atom_pos, n_atoms, Z_species
    use mod_com2y,            only : symmetries_check_and_load, alat_mult_factor
    implicit none
    logical                       :: l_identity, l_inversion
    real(SP)                      :: lsop(3,3), cp(3)
    integer                       :: a_nsym, a_species
    integer, allocatable          :: asop(:,:,:)
    character(3), allocatable     :: atom_species_names(:)
    integer,  allocatable, target :: atom_species_(:)
    real(SP), allocatable, target :: reduced_atom_positions_(:,:)

    integer :: varid

    !
    ! Lattice vectors
    !
    varid = netcdf_inquire_varid(ncid, "primitive_vectors")
    netcdf_error = nf90_get_var(ncid, varid, a)
    call netcdf_check(ncid,netcdf_error,varid)

    a(:,:) = transpose(a(:,:))
    alat(1) = alat_mult_factor*maxval( abs( a(1,:) ))
    alat(2) = alat_mult_factor*maxval( abs( a(2,:) ))
    alat(3) = alat_mult_factor*maxval( abs( a(3,:) ))
    cp = cross_product(a(2,:),a(3,:))
    do i1 = 1,3
      DL_vol = DL_vol+a(1,i1)*cp(i1)
    enddo
    b(1,:) = cross_product(a(2,:),a(3,:))*2.0_SP*pi/DL_vol
    b(2,:) = cross_product(a(3,:),a(1,:))*2.0_SP*pi/DL_vol
    b(3,:) = cross_product(a(1,:),a(2,:))*2.0_SP*pi/DL_vol
    !
    ! Atomic position data
    !
    ! n_atoms_max     maximum number of atoms in  single species = maxval(n_atoms)
    ! n_atom_species  number of atomic species
    ! n_atoms         n_atoms(n_atom_species) : number of atoms for each atom species
    ! atom_pos        the positions
    !
    YAMBO_ALLOC( atom_species_, (n_atoms) )
    YAMBO_ALLOC( reduced_atom_positions_, (3,n_atoms ) )
    YAMBO_ALLOC( n_atoms_species, (n_atomic_species) )

    varid = netcdf_inquire_varid(ncid, "atom_species")
    netcdf_error = nf90_get_var(ncid, varid, atom_species_)
    call netcdf_check(ncid,netcdf_error,varid)

    varid = netcdf_inquire_varid(ncid, "reduced_atom_positions")
    netcdf_error = nf90_get_var(ncid, varid, reduced_atom_positions_)
    call netcdf_check(ncid,netcdf_error,varid)

    ! Determine n_atoms_max
    n_atoms_species(:) = 0 
    do i1 = 1, n_atoms
      a_species = atom_species_(i1)
      n_atoms_species( a_species ) = n_atoms_species( a_species ) + 1
    enddo
    n_atoms_species_max = maxval(n_atoms_species)

    ! Reset n_atoms, and fill _natoms and  atom_pos arrays
    n_atoms_species(:) = 0 
    YAMBO_ALLOC( atom_pos, (3, n_atoms_species_max, n_atomic_species) )
    do i1 = 1, n_atoms
      a_species = atom_species_(i1)
      n_atoms_species( a_species ) = n_atoms_species( a_species ) + 1
      atom_pos(:, n_atoms_species( a_species ) , a_species ) = &
&                 matmul( transpose(a), reduced_atom_positions_(:,i1) )
    enddo
    YAMBO_FREE( reduced_atom_positions_ )
    YAMBO_FREE( atom_species_ )

    !
    ! Atomic species data
    !
    YAMBO_ALLOC( Z_species, (n_atomic_species) )
    varid = netcdf_inquire_varid(ncid, "atomic_numbers")
    netcdf_error = nf90_get_var(ncid, varid, Z_species)
    call netcdf_check(ncid,netcdf_error,varid)

    !
    ! Symmetry 
    !
    netcdf_error = nf90_inq_dimid(ncid, "number_of_symmetry_operations", dimid)
    netcdf_error = nf90_inquire_dimension(ncid, dimid, len = a_nsym)

    YAMBO_ALLOC( asop, (3,3,a_nsym) )
    varid = netcdf_inquire_varid(ncid, "reduced_symmetry_matrices")
    netcdf_error = nf90_get_var(ncid, varid, asop)
    call netcdf_check(ncid,netcdf_error,varid)

    call symmetries_check_and_load(asop,a_nsym)

    return
  end subroutine import_geometry_group


  subroutine import_electrons_group
    use electrons,            only : default_nel
    implicit none
    integer :: varid, n_sp_pol_abinit
    real(SP), allocatable, target :: eigenvalues_(:,:,:)

    n_sp_pol_abinit=n_sp_pol
    if (artificial_spin_pol) n_sp_pol_abinit=1

    YAMBO_ALLOC( eigenvalues_, (n_bands, nkibz, 1 ) )

    varid = netcdf_inquire_varid(ncid, "number_of_electrons")
    netcdf_error = nf90_get_var(ncid, varid, default_nel)
    call netcdf_check(ncid,netcdf_error,varid)

    varid = netcdf_inquire_varid(ncid, "eigenvalues")
    netcdf_error = nf90_get_var(ncid, varid, eigenvalues_)
    call netcdf_check(ncid,netcdf_error,varid)

    YAMBO_ALLOC( en%E, (en%nb, k%nibz, n_sp_pol) )

    if (.not. artificial_spin_pol) en%E = eigenvalues_
    if (      artificial_spin_pol) then 
      en%E(:,:,1:1) = eigenvalues_
      en%E(:,:,2:2) = eigenvalues_
    endif

    YAMBO_FREE(eigenvalues_)

    return
  end subroutine import_electrons_group


  subroutine import_basisdata_group_kss
    !
    use vec_operate,          only : sort, iku_v_norm
    use R_lattice,            only : g_vec, b, ng_vec
    use D_lattice,            only : alat, i_space_inv, i_time_rev
    use wave_func,            only : wf_nc_k, wf_igk, wf_ncx, wf_ng
    !
    implicit none
    !
    integer                        :: i1,i2,i3,n1,n2,n3,max_nx,max_ny,max_nz,npw,igv(3),max_size
    integer                        :: g_lim(6), ig_wf, ng_vec_old, ig, ic, ik, g_vec_tmp1(3),varid
    integer,   allocatable         :: igk(:,:), igk_tmp(:,:), istwfk(:), inv_map_k(:,:), map_gamma(:,:,:)
    integer,   allocatable         :: G_mod_indx(:)
    logical                        :: gamma_only,l_new
    logical,   allocatable         :: l_g_mapped(:)
    real(SP)                       :: ecut, norm_2
    real(SP),  allocatable         :: G_mod(:), g_vec_tmp(:,:)
    integer,   allocatable, target :: reduced_coordinates_of_plane_waves(:,:,:)
    !
    YAMBO_ALLOC( istwfk, (k%nibz) )
    YAMBO_ALLOC( wf_nc_k, (k%nibz) )
    allocate( reduced_coordinates_of_plane_waves(3,wf_ncx,k%nibz) )
    YAMBO_ALLOC( wf_igk, (wf_ncx,k%nibz) )
    !
    varid = netcdf_inquire_varid(ncid, "number_of_coefficients")
    netcdf_error = nf90_get_var(ncid, varid, wf_nc_k)
    call netcdf_check(ncid,netcdf_error,varid)
    !
    varid = netcdf_inquire_varid(ncid, "reduced_coordinates_of_plane_waves")
    netcdf_error = nf90_get_var(ncid, varid, reduced_coordinates_of_plane_waves)
    call netcdf_check(ncid,netcdf_error,varid)
    !
    YAMBO_ALLOC( g_vec, (wf_ncx,3) )
    do i1=1,wf_ncx
      g_vec(i1,:)=matmul(transpose(b),reduced_coordinates_of_plane_waves(:,i1,1))*alat(:)/2.0_SP/pi
    enddo
    !
    ng_vec=wf_ncx
    gamma_only=(nkibz==1 .and. all(k%pt(1,:)==0.) )
    if( i_time_rev==1 .or. i_space_inv==1 .and. .not.gamma_only) then
      call msg('s','Closing shells against inversion...')
      call G_rot_grid(-1,'extend_grid')
      call msg('l','done')
      if(ng_vec/=wf_ncx) call msg('s',':: ng_vec was increased to close the G-shells')
    endif
    !
    wf_igk=-1
    do ik=1,nkibz
      do ig=1,wf_nc_k(ik)
        wf_igk(ig,ik)=ig
      enddo
    enddo
    wf_ng=maxval(wf_igk)
    !
    return
    !
  end subroutine import_basisdata_group_kss



  subroutine import_basisdata_group_wfk
    !
    use com,                  only : error
    use vec_operate,          only : sort, iku_v_norm
    use R_lattice,            only : g_vec, b, ng_vec
    use D_lattice,            only : alat, i_space_inv, i_time_rev
    use wave_func,            only : wf_nc_k, wf_igk, wf_ncx, wf_ng
    !
    implicit none
    !
    integer                       :: i1,i2,i3,n1,n2,n3,max_nx,max_ny,max_nz,npw,igv(3),max_size
    integer                       :: g_lim(6), ig_wf, ng_vec_old, ig, ic, ik, g_vec_tmp1(3),varid
    integer,     allocatable      :: igk(:,:), igk_tmp(:,:), istwfk(:), inv_map_k(:,:), map_gamma(:,:,:)
    integer,     allocatable      :: G_mod_indx(:)
    logical                       :: gamma_only,l_new
    real(SP)                      :: ecut, norm_2, b_transpose(3,3)
    real(SP),    allocatable      :: G_mod(:), g_vec_tmp(:,:)
    integer,   allocatable, target :: reduced_coordinates_of_plane_waves(:,:,:)
    !
    YAMBO_ALLOC( istwfk, (k%nibz) )
    YAMBO_ALLOC( wf_nc_k, (k%nibz) )
    allocate( reduced_coordinates_of_plane_waves(3,wf_ncx,k%nibz) )
    !
    varid = netcdf_inquire_varid(ncid, "number_of_coefficients")
    netcdf_error = nf90_get_var(ncid, varid, wf_nc_k)
    call netcdf_check(ncid,netcdf_error,varid)
    !
    wf_ncx = maxval(wf_nc_k) 
    YAMBO_ALLOC( wf_igk, (wf_ncx,k%nibz) )
    !
    varid = netcdf_inquire_varid(ncid, "reduced_coordinates_of_plane_waves")
    netcdf_error = nf90_get_var(ncid, varid, reduced_coordinates_of_plane_waves)
    call netcdf_check(ncid,netcdf_error,varid)
    !
    ! the mapping wf_igk(wf_ncx,k%nibz) brings the WF component at each k-point to
    ! a gamma centered G-vector sphere
    !
    ! the mapping is found by:
    !   1. Generate a G-sphere that accomodates all the G components
    !   2. Put this G-sphere into a big box using the 'gsphere_abinit' 
    !      routine and obtain mapping G-sphere to components
    !      map_gamma :  xyz -> npw
    !   3. Put the G-vectors associated to each K-point in the same big box 
    !      using 'gsphere_abinit' and obtain mapping G-sphere to components
    !      inv_map_k :  npw -> xyz
    !   4. Use the mappings 2 and 3 to generate wf_igk mapping
    !       inv_map_k    map_gamma
    !      npw -> xyz   xyz -> npw
    ! TODO: handle the istwfk /= 1 cases
    !
   
    ! 1.
    ! Get dimensions of sphere
    ecut = 1
   
    max_nx = 0
    max_ny = 0
    max_nz = 0
    do ik=1,k%nibz
      !write(*,*) ik, wf_nc_k(ik)
      max_nx = max(max_nx,maxval(reduced_coordinates_of_plane_waves(1,:,ik)))
      max_ny = max(max_ny,maxval(reduced_coordinates_of_plane_waves(2,:,ik)))
      max_nz = max(max_nz,maxval(reduced_coordinates_of_plane_waves(3,:,ik)))
    end do
    !write(*,*) max_nx, max_ny, max_nz
    n1 = 4*max_nx
    n2 = 4*max_ny
    n3 = 4*max_nz
   
    ! Calculate G-vectors inside the sphere
    b_transpose = transpose(b)
    max_size = (2*n1+1)*(2*n2+1)*(2*n3+1) !max possible number of components
    YAMBO_ALLOC( g_vec_tmp, (max_size,3) )
    YAMBO_ALLOC( igk_tmp, (3,max_size) )
    npw = 0
    do i1=-n1,n1
      do i2=-n2,n2
        do i3=-n3,n3
          igv = [i1,i2,i3]
          g_vec_tmp1 = matmul(b_transpose,igv)/2.0_SP/pi
          norm_2 = g_vec_tmp1(1)**2.0_SP + &
                   g_vec_tmp1(2)**2.0_SP + &
                   g_vec_tmp1(3)**2.0_SP
          if (ecut .gt. 0.25_SP*norm_2) then
              npw = npw+1
              igk_tmp(:,npw) = igv
              g_vec_tmp(npw,:) = matmul(b_transpose,igv)*alat(:)/2.0_SP/pi
          end if
          !this check is not needed anymore in principle
          if (npw > max_size) then
              call error('an error has occurred: npw > max_size')
          end if
        end do
      end do
    end do
    ng_vec = npw
   
    ! can sort the G vectors in whatever way here
    YAMBO_ALLOC(G_mod,(ng_vec))
    YAMBO_ALLOC(G_mod_indx,(ng_vec))
    do ig=1,ng_vec
      G_mod(ig)=iku_v_norm(g_vec_tmp(ig,:))
    enddo
    call sort(G_mod,indx_m1=G_mod_indx)
    YAMBO_FREE(G_mod)
    !
    YAMBO_ALLOC(g_vec,(ng_vec,3))
    YAMBO_ALLOC(igk,(3,ng_vec))
    g_vec(G_mod_indx(:ng_vec),:3)=g_vec_tmp(:ng_vec,:3)
    igk(:3,G_mod_indx(:ng_vec))=igk_tmp(:3,:ng_vec)
    YAMBO_FREE(g_vec_tmp)
    YAMBO_FREE(igk_tmp)
   
    ! 2.
    allocate(map_gamma(n1,n2,n3), inv_map_k(3,npw))
    open(2,file='gs.dat')
    do ic=1,ng_vec
      write(2,*) igk(:,ic)
    end do
    call gsphere_abinit(n1,n2,n3,igk,npw,1,map=map_gamma)
   
    ! 3.
    ! Same thing but for the K-dependent G-spheres     
    ! Get storage type of the wavefunctions
    varid = netcdf_inquire_varid(ncid, "istwfk")
    netcdf_error = nf90_get_var(ncid, varid, istwfk)
    call netcdf_check(ncid,netcdf_error,varid)
    if (.not.all(istwfk==1)) call error('istwfk /= 1 found. Please set istwfk *1 in the Abinit input file and rerun') 
 
    wf_igk = -1
    do ik=1,k%nibz
      npw = wf_nc_k(ik)
      write(2,*)
      write(2,*)
      do ic=1,npw
        write(2,*) reduced_coordinates_of_plane_waves(:,ic,ik)
      end do
      call gsphere_abinit(n1,n2,n3,reduced_coordinates_of_plane_waves(:,:,ik),npw,istwfk(ik),inv_map=inv_map_k)
      ! 4.
      ! Do the wf_igk mapping
      do ic=1,wf_nc_k(ik)
        i1 = inv_map_k(1,ic)
        i2 = inv_map_k(2,ic)
        i3 = inv_map_k(3,ic)
        npw = map_gamma(i1,i2,i3)
        !write(*,*) ic, npw
        if (npw .eq. -1) then
            write(*,*) 'no mapping found', ic, 'to', i1,i2,i3
            call exit(0)
        end if
        wf_igk(ic,ik) = npw
      end do
    end do
    !
    wf_ng=maxval(wf_igk)
    !
    !
    return
    !
  end subroutine import_basisdata_group_wfk


  !
  ! Routine inspired in gsphere routine in
  ! abinit/src/52_fft_mpi_noabirule/m_fftcore.F90
  ! with important input from Matteo Giantomassi
  ! 
  ! Maps G vectors defined in the kg_k to a fftbox of n1,n2,n3
  !
  subroutine gsphere_abinit(n1,n2,n3,kg_k,npw,istwf_k,map,inv_map)

  implicit none

  !Arguments ------------------------------------
  !scalars
  integer,intent(in)  :: istwf_k,n1,n2,n3,npw
  !arrays
  integer,intent(in)  :: kg_k(3,npw)
  integer,optional,intent(out) :: map(n1,n2,n3)
  integer,optional,intent(out) :: inv_map(3,npw)

  !Local variables-------------------------------
  !scalars
  integer :: i1,i1inv,i2,i2inv,i3,i3inv,id1,id2,id3,idat,ipw
  !arrays
  !integer :: i1inver(n1),i2inver(n2),i3inver(n3)

  !In the case of special k-points, invariant under time-reversal,
  !but not Gamma, initialize the inverse coordinates. !Remember indeed that
  !
  !  u_k(G) = u_{k+G0}(G-G0); u_{-k}(G) = u_k(G)^* and therefore:
  !  u_{G0/2}(G) = u_{G0/2}(-G-G0)^*.

  !if (istwf_k>=2) then
  !  if(istwf_k==2 .or. istwf_k==4 .or. istwf_k==6 .or. istwf_k==8)then
  !    i1inver(1)=1
  !    do i1=2,n1
  !      i1inver(i1)=n1+2-i1
  !    end do
  !  else
  !    do i1=1,n1
  !      i1inver(i1)=n1+1-i1
  !    end do
  !  end if
  !  if(istwf_k>=2 .and. istwf_k<=5)then
  !    i2inver(1)=1
  !    do i2=2,n2
  !      i3inver(i2)=n2+2-i2
  !    end do
  !  else
  !    do i2=1,n2
  !      i2inver(i2)=n2+1-i2
  !    end do
  !  end if
  !  if(istwf_k==2 .or. istwf_k==3 .or. istwf_k==6 .or. istwf_k==7)then
  !    i3inver(1)=1
  !    do i3=2,n3
  !      i3inver(i3)=n3+2-i3
  !    end do
  !  else
  !    do i3=1,n3
  !      i3inver(i3)=n3+1-i3
  !    end do
  !  end if
  !end if

  ! TODO: handle the istwfk /= 1 cases
  ! create mapping from G sphere to box
  if (present(map))     map = -1
  if (present(inv_map)) inv_map = -1
  do ipw=1,npw
    i1=kg_k(1,ipw); if(i1<0)i1=i1+n1; i1=i1+1
    i2=kg_k(2,ipw); if(i2<0)i2=i2+n2; i2=i2+1
    i3=kg_k(3,ipw); if(i3<0)i3=i3+n3; i3=i3+1

    if (present(map)    ) map(i1,i2,i3)  = ipw
    if (present(inv_map)) inv_map(:,ipw) = [i1,i2,i3]
  end do

  end subroutine



  !
  ! K-points (convert to new units)
  !===========================================================
  subroutine import_kpoints_group
    use R_lattice,            only : b
    use D_lattice,            only : alat
    implicit none
    integer :: varid
    real(SP), allocatable :: reduced_coordinates_of_kpoints(:,:)
    !
    allocate(reduced_coordinates_of_kpoints(3, k%nibz))
    !
    varid = netcdf_inquire_varid(ncid, "reduced_coordinates_of_kpoints")
    netcdf_error = nf90_get_var(ncid, varid, reduced_coordinates_of_kpoints)
    call netcdf_check(ncid,netcdf_error,varid)
    !
    YAMBO_ALLOC(k%pt,(k%nibz,3))
    do ik = 1,k%nibz
      k%pt(ik,:)=matmul(transpose(b),reduced_coordinates_of_kpoints(:,ik))*alat(:)/2.0_SP/pi
    enddo
    !
    deallocate(reduced_coordinates_of_kpoints)
    return
  end subroutine import_kpoints_group

  !
  ! GW data from KSS file
  !===========================================================
  subroutine import_gwdata_group_kss
    use netcdf_data
    use D_lattice,             only : n_atomic_species
    use pseudo,                only : PP_alloc_abinit, pp_n_l_times_proj_max,&
&                                     pp_n_l_comp, pp_kbs,pp_kb,pp_kbd,&
&                                     pp_table,l_many_proj,pp_n_l_max
#include<memory.h>
    real(SP),allocatable :: pp_kbs_(:,:)
    integer :: max_number_of_angular_momenta,max_number_of_projectors
    integer :: ia, nproj, ip, il, varid, start(3), count(3)
    !
    ! Get kb related dimensions
    !
    max_number_of_angular_momenta = netcdf_get_dimension(ncid,"max_number_of_angular_momenta")
    max_number_of_projectors      = netcdf_get_dimension(ncid,"max_number_of_projectors")
    !
    pp_n_l_max            = max_number_of_angular_momenta
    pp_n_l_times_proj_max = max_number_of_angular_momenta!max_number_of_projectors
    !
    call PP_alloc_abinit()
    YAMBO_ALLOC(pp_n_l_comp,(n_atomic_species))
    YAMBO_ALLOC(pp_table,(3,n_atomic_species,pp_n_l_times_proj_max))
    pp_n_l_comp(:)=pp_n_l_max
    !
    ! [TODO: RECOVER THIS]
    !
    nproj = pp_n_l_times_proj_max/pp_n_l_max
    do ia=1,n_atomic_species
      do il=1,pp_n_l_max ! loop over l
        do ip=1,nproj ! loop over nproj
          pp_table(1,ia,il*(nproj-1)+ip)=il  !  l+1
          pp_table(2,ia,il*(nproj-1)+ip)=ip  !  n_proj
          pp_table(3,ia,il*(nproj-1)+ip)=1   !  i_spin
        enddo
      enddo
    enddo
    !
    ! Form factor signs
    !
    allocate(pp_kbs_( pp_n_l_times_proj_max,  n_atomic_species ))
    start = [1,1,1]
    count = [1,pp_n_l_times_proj_max,n_atomic_species]
    varid = netcdf_inquire_varid(ncid, "kb_formfactor_sign")
    netcdf_error = nf90_get_var(ncid, varid, pp_kbs_, start=start, count=count)
    call netcdf_check(ncid, netcdf_error, varid, "kb_formfactor_sign")
    call netcdf_check(ncid,netcdf_error,varid)
    forall( ia=1:n_atomic_species, ip=1:pp_n_l_times_proj_max ) &
&          pp_kbs(ia,ip) = pp_kbs_(ip,ia)
    YAMBO_FREE(pp_kbs_)
    !
    return
  end subroutine import_gwdata_group_kss


  !
  ! GW data from WFK file
  !===========================================================
  subroutine import_gwdata_group_wfk
    use netcdf_data
    use pars,                  only : SP
    use D_lattice,             only : n_atomic_species
    use wave_func,             only : wf_ncx
    use pseudo,                only : PP_alloc_abinit, pp_n_l_times_proj_max,&
&                                     pp_n_l_comp, pp_kbs,pp_kb,pp_kbd,&
&                                     pp_table,l_many_proj,pp_n_l_max
#include<memory.h>
    real(SP),allocatable :: pp_kbs_(:,:)
    integer :: lmnmax
    integer, allocatable :: indlmn(:,:,:)
    integer :: ia, nproj, ip, varid
    !
    ! Get kb related dimensions
    !
    lmnmax                = netcdf_get_dimension(ncid,"lmnmax")
    pp_n_l_times_proj_max = lmnmax
    !
    ! read abinit indlmn
    !
    allocate(indlmn(6,lmnmax,n_atomic_species))
    varid = netcdf_inquire_varid(ncid, "indlmn")
    netcdf_error = nf90_get_var(ncid, varid, indlmn)
    call netcdf_check(ncid, netcdf_error, varid, "indlmn")
    !
    !calculate max_number of angular momenta
    pp_n_l_max = maxval(indlmn(1,:,:))+1
    !
    call PP_alloc_abinit()
    YAMBO_ALLOC(pp_n_l_comp,(n_atomic_species))
    YAMBO_ALLOC(pp_table,(3,n_atomic_species,pp_n_l_times_proj_max))
    pp_n_l_comp(:)=pp_n_l_max
    !
    do ia=1,n_atomic_species
      do ip=1,pp_n_l_times_proj_max
        pp_table(1,ia,ip)=indlmn(1,ip,ia)+1 !  l+1
        pp_table(2,ia,ip)=indlmn(3,ip,ia)   !  n_proj
        pp_table(3,ia,ip)=indlmn(6,ip,ia)   !  i_spin
      enddo
    enddo
    !
    ! Form factor signs
    !
    allocate(pp_kbs_( pp_n_l_times_proj_max,  n_atomic_species ))
    varid = netcdf_inquire_varid(ncid, "vkbsign")
    netcdf_error = nf90_get_var(ncid, varid, pp_kbs_)
    call netcdf_check(ncid, netcdf_error, varid, "vkbsign")
    forall( ia=1:n_atomic_species, ip=1:pp_n_l_times_proj_max ) &
&          pp_kbs(ia,ip) = pp_kbs_(ip,ia)
    YAMBO_FREE(pp_kbs_)
    !
    return
  end subroutine import_gwdata_group_wfk



  !
  ! Miscellaneous data 
  !===========================================================
  subroutine import_miscellaneous_data
    !
    ! Here read any data not appearing in ETSF specs, or not
    ! yet supported properly
    !
    use D_lattice,            only : input_GS_Tel, n_atomic_species, n_atoms_species
    use electrons,            only : l_spin_orbit, default_nel
    use xc_functionals,       only : GS_xc_KIND,GS_xc_FUNCTIONAL
    use mod_xc2y,             only : XC_yamboID, XC_yamboID2kind
    integer                       :: i1, varid, ixc_
    !
    ! Temperature (Abinit)
    !
    varid = netcdf_inquire_varid(ncid, "tphysel")
    netcdf_error = nf90_get_var(ncid, varid, input_GS_Tel)

    !
    ! Spin orbit splitting (Abinit)
    !
    l_spin_orbit = (n_spin == 2)

    !
    ! XC functional (Abinit)
    !
    varid = netcdf_inquire_varid(ncid, "ixc")
    netcdf_error = nf90_get_var(ncid, varid, ixc_)

    GS_xc_FUNCTIONAL = XC_yamboID('abinit',abinit_func=ixc_)
    GS_xc_KIND = XC_yamboID2kind(GS_xc_FUNCTIONAL)
    return
  end subroutine import_miscellaneous_data

end subroutine a2y_db1
