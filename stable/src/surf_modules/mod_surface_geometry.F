!
! Copyright (C) 2000-2014 C. Hogan  and the YAMBO team 
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
module surface_geometry
  ! 
  ! Tools for dealing with slab/supercell geometries
  !
  use pars,             ONLY : SP
  use com,              ONLY : msg
  use D_lattice,        ONLY : alat, a

  real(SP)                  :: q0x(3), q0y(3), norm(3), az
  integer, allocatable      :: gvecaff(:,:)
  integer, save             :: ix,iy,iz
  real(SP)                  :: nlcell,nlslab,nlbulk
  real(SP)                  :: d_surf, d_vac
  real(SP)                       :: nlayers(3)
  logical                   :: lsymslab
  character(1)              :: qname
  integer                   :: xyz(3)

contains

subroutine init_surface(defs)
  use pars,                  ONLY : SP
  use it_m,                  ONLY : it, initdefs, E_unit,G_unit,T_unit
  implicit none
  type(initdefs), intent(inout)  :: defs

  call it(defs,'Layers',   '[RAS] Atomic layers in cell/slab/bulk', nlayers )
  call it(defs,'q0x'     , '[RAS] [cc] X-polarization direction', q0x )
  call it(defs,'q0y'     , '[RAS] [cc] Y-polarization direction', q0y )

  nlcell = nlayers(1)
  nlslab = nlayers(2)
  nlbulk = nlayers(3)
  return
end subroutine init_surface

  subroutine setup_surface_geometry(lfail)
    use vec_operate,          ONLY : v_norm, cross_product
    use com,                  ONLY : error
    implicit none
    logical, intent(out)          :: lfail

    integer                       :: i1
    real(SP), parameter           :: eps = 1.E-5

    lfail = .false.

    q0x(:)=q0x(:)*1.E-5/v_norm(q0x)
    q0y(:)=q0y(:)*1.E-5/v_norm(q0y)

    norm = cross_product( q0x, q0y )
    norm = norm*1.E-5/v_norm(norm)  ! Required if norm is to be used as a q0

    if ( all( abs(norm).LT.eps) ) then
      lfail = .true.
      call error('Surface normal vector problem 1'//&
&                 'check input vectors q0x and q0y.')
    else if(abs(norm(1)).lt.eps.and.abs(norm(2)).lt.eps) then
      xyz(1) = 1; xyz(2) = 2; xyz(3) = 3
    else if(abs(norm(2)).lt.eps.and.abs(norm(3)).lt.eps) then
      xyz(1) = 2; xyz(2) = 3; xyz(3) = 1
    else if(abs(norm(1)).lt.eps.and.abs(norm(3)).lt.eps) then
      xyz(1) = 3; xyz(2) = 1; xyz(3) = 2
    else
      call error('Surface normal vector problem 2 - check input vectors q0x and q0y.')
      lfail = .true.
    endif

    ! This is a temporary way to do it, needs simple unit vector cell.
    ! For more complicated (non oblique) cells, need to redo everything...
    ix = xyz(1)
    iy = xyz(2)
    iz = xyz(3)

    az = a(iz, iz) ! a is already in a.u.
    !
    ! Layers: run some basic checks
    !
    if(nlcell.lt.nlslab.or.nlbulk.gt.nlslab.or.nlbulk.ge.nlcell) &
&   call msg('nrs','Warning: error in numbers of layers.') 

    return
  end subroutine setup_surface_geometry

  subroutine print_surface_geometry
    implicit none
    real(SP)                   :: fac

      call msg('r','Polarization directions:')
      fac = sqrt(dot_product(q0x,q0x))
      call msg('r','-              X',q0x(1:3)/fac )
      fac = sqrt(dot_product(q0y,q0y))
      call msg('r','-              Y',q0y(1:3)/fac )
      fac = sqrt(dot_product(norm,norm))
      call msg('r','Surface normal :',norm(1:3)/fac )
      call msg('r','Surface vector indices (ix,iy,iz) :',(/ix,iy,iz/) )
      call msg('r','Supercell height az (a.u.) :',az)

      return
    end subroutine print_surface_geometry

    subroutine get_slab_symmetry( loptcut)   
      use D_lattice,             ONLY : i_space_inv, dl_sop, nsym, i_time_rev
      implicit none
      integer                        :: is,i,j
      real(SP)                       :: sym1(3,3)
      real(SP)                       :: plane(3,3), inv(3,3)
      logical,            intent(in) :: loptcut

      ! Here test for symmetry perpendicular to the surface (inversion or plane)
      ! and also in-plane symmetry.

      lsymslab = .false.

      do

!     Test for inversion symmetry, if time reversal not used

        inv = reshape( (/-1, 0, 0, &
&                         0,-1, 0, &
&                         0, 0,-1 /), (/ 3,3 /) )
        do is = 1, nsym ! should be over real syms
          forall(i=1:3,j=1:3) sym1(i,j) = abs(dl_sop(i,j,is)-inv(i,j))
          if((all(sym1.lt.0.001).and.i_time_rev.eq.0).or. &
&             i_space_inv.eq.1) then
            lsymslab = .true.
            call msg('r','Spatial inversion present.')
            exit
          endif
        enddo
        if(lsymslab) exit

!     Test for mirror symmetry perp to surface normal
      
        plane = reshape( (/ merge(-1,1,iz.eq.1), 0, 0, &
&                         0, merge(-1,1,iz.eq.2), 0, &
&                         0, 0, merge(-1,1,iz.eq.3) /), (/ 3,3 /) )
        do is = 1, nsym/(1+i_time_rev) ! Over real syms only
          forall(i=1:3,j=1:3) sym1(i,j) = abs(dl_sop(i,j,is)-plane(i,j))
          if(all(sym1.lt.0.001)) then
            lsymslab = .true.
            call msg('r','Slab symmetries: plane symmetry present...')
            exit
          endif
        enddo
        if(lsymslab) exit

!       No symmetry for RAS present
        call msg('r','Slab symmetries: no half slab symmetry present...')
        return

      enddo

!     Test for cutoff function use: destroys symmetry
      if(loptcut.and.lsymslab) then
         call msg('nr','Note: Real space cutoff function removes slab symmetry.')
         lsymslab = .false.
      endif

      return
  end subroutine get_slab_symmetry

  subroutine setup_gvecaff
    use pars,              ONLY : SP, PI, schlen
    use com,               ONLY : msg
    use R_lattice,         ONLY : ng_vec, g_vec, b
    use matrix_operate,       ONLY : m3inv
    implicit none
    real(SP)                   :: bmod(3),Amat(3,3),Bmat(3)
    character(schlen)          :: sch
! local
    integer                    :: ig, od(6), i

    allocate( gvecaff(ng_vec,3) ) ! this is faster order for ix index searching
    call msg('r','Oblique G-vector array:')
    call m3inv(transpose(b),Amat) ! Amat = (b^T)^(-1)

    do ig = 1, ng_vec
       Bmat = g_vec(ig,:)*2.0_SP*PI/alat(:) ! Convert to a.u.
       Bmat = matmul(Amat,Bmat)             ! n = (b^T)^(-1) G
       gvecaff(ig,:) = nint(Bmat(:))
       write(sch,100) ig,gvecaff(ig,:), g_vec(ig,:)*2.0_SP*PI/alat(:)
! For debugging
!      if(ig.lt.10) call msg('r','First G-vectors:'//trim(sch))
!      if(ig.gt.ng_vec-3) call msg('r','Last  G-vectors:'//trim(sch))
    enddo
100 format(i8,3i4,2x,3f7.3) 
    od(1:6) = (/ minval(gvecaff(:,1)), maxval(gvecaff(:,1)), &
&                minval(gvecaff(:,2)), maxval(gvecaff(:,2)), &
&                minval(gvecaff(:,3)), maxval(gvecaff(:,3)) /)

    call msg('nr','Charge nG limits: ',od(1:6) )

    return
  end subroutine setup_gvecaff

end module surface_geometry
