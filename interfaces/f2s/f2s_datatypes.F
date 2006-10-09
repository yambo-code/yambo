!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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

 module f2s_datatypes
  use pars, ONLY: DP, SP, schlen
!use parser
 implicit none
! define double precision
! integer, parameter :: dp=kind(1.0d0)
!Structures

!----------------------------------------------------------------------
 type finp_type
! Integer
  integer:: ibrav,pgind                 !! lattice type and point group index
  integer:: nsp                         !! number of species
  integer:: nax                         !! auxilary needed for simplicity
  integer,pointer:: na(:)               !! number of atoms per species
  integer,pointer:: lmax(:),lloc(:)     !! max and local l-components of PP
  integer:: ineq_pos(3)                 !! used as dummy variable here
  integer:: nkpt                        !! number of k-points
  integer:: nrot                        !! number of symmetry operations
  integer,pointer:: srot(:,:,:)         !! symmetry operations in lattice coordinates
  integer:: nrho,npos,nthm,nseed        !! not used in SELF
  integer:: nfi_rescale
! Real      
  real(SP):: nel
  real(SP):: ekt
  real(SP):: ecut,ecuti
  real(SP):: T_ion,T_init,Q
  real(SP):: alat                       !! lattice constant
  real(SP):: omega                      !! volume of unit cell
  real(SP):: a(3,3)                     !! lattice vectors
  real(SP):: b(3,3)                     !! reciprocal lattice vectors
  real(SP),pointer::xk(:,:)             !! k-points 
  real(SP),pointer:: zv(:)              !! valence charge per species
  real(SP),pointer:: tau0(:,:,:)        !! atomic coordinates (not transfered to SELF)
  real(SP)::ion_fac,ion_damp,rgauss     !! used as dummy variables here
  real(SP)::spipol                      !! used as dummy variables here
! Logical
  logical:: tpsmesh                     !! pseudopotential tabulated on logarithmic mesh
  logical:: tmetal,tdegen
  logical:: tmold,tband
  logical:: coordwave
  logical:: tinitbasis(3)               !! used as dummy variable here
! Character
  character(10),pointer:: atom(:)       !! name of species
  character(schlen):: file              !! file name
 end type finp_type

 type fgg_type
! Integer
  integer::nr11,nr22,nr33
  integer::ngx,ngwx
  integer,pointer::ngw(:)
  integer,pointer::n1(:),n2(:),n3(:)
! Real
  real(SP)::gcut
  real(SP),pointer::g(:)
  real(SP),pointer::gg(:,:)
 end type fgg_type

type fwvf_type
! Integer
  integer:: nspin, nspin_pt             !! spin polarized mode: future use in SELF
  integer:: ngwx                        !! max number of G-vectors wave function
  integer:: nkptx                       !! max number of k-points
  integer:: nx                          !! max number of bands
  integer :: nnrx                       !! dimension of fft mesh
  integer :: nr1x,nr2x,nr3x             !! 
  integer,pointer :: ngw(:)             !! max number of G-vectors per k-point
                                        !! tables for relating G- and k-vectors
                                        !!
  integer,pointer :: n1(:),n2(:),n3(:)  !!   G-vectors <-> fft-mesh 
  integer,pointer :: igk(:,:)           !!   G-vectors <-> G-vectors(k-point)
                                        !! species and atoms
  integer :: pgind                      !!   point group index (not used in SELF)
  integer :: nsx,nax                    !!   max number of species and atoms/species
  integer,pointer :: na(:)              !!   number of atoms per species
!  Real
  real(dp):: alat,omega                 !! lattice parameter and cell volume
  real(dp),pointer:: tau0(:,:,:)        !! atomic positions in cart.coord. (a.u.)
  real(dp):: ecut                       !! cut-off energy of wave functions
  real(dp),pointer:: xk(:,:),wkpt(:)  !! k-points and wheights
  real(dp):: nel                        !! number of electrons
  real(dp):: efermi                     !! fermi energy
  real(dp),pointer:: eig(:,:),focc(:,:) !! eigenvalues and occupation numbers
  real(SP),pointer:: zeta(:,:,:)        !! spin density (not needed for SELF)
! Complex
  complex(kind((1.0,1.0))),pointer::c0(:)            !! wave functions (ngwx,nx,nkptx)
                                        !!    here only c0(ngwx) will be used
                                        !!    in a loop over nx and nkptx
! Character
  character(schlen)::file               !! file name
  character(10),pointer::atom(:)
end type fwvf_type

type fpp_type
! Integer
  integer:: mmaxx                       !! max number of mesh points
  integer:: lmax                        !! max number of l-components
  integer,pointer:: mmax(:)             !! max number of mesh points
  integer,pointer:: nang(:)             !! number of angular components of PP
  integer,pointer:: iu(:)               !! units to read files from
! Real
  real(SP),pointer:: clog(:)            !! logarithmic mesh constant
  real(SP),pointer:: zval(:)            !! valence charge of PP
  real(SP),pointer:: r(:,:)             !! mesh points per species
  real(SP),pointer:: p(:,:,:)           !! radial pseudo wave function 
                                        !!     per species and l-component
  real(SP),pointer:: vion(:,:,:)        !! l-components of PP per species
  real(SP),pointer:: rhocr(:,:)         !! pseudo core density (not used in SELF)
! Character
      character(schlen),pointer:: file(:)   !! names of the pp-files
 end type fpp_type

 CONTAINS

 subroutine preallocate_finp(finp)
!=================================
 type(finp_type):: finp

 allocate(finp%na(finp%nsp))
 allocate(finp%xk(3,finp%nkpt))

 return
 end subroutine 

 subroutine allocate_finp_coor(finp)
!===================================
 type(finp_type):: finp

 allocate(finp%lmax(finp%nsp))
 allocate(finp%lloc(finp%nsp))
 allocate(finp%zv(finp%nsp))
 allocate(finp%atom(finp%nsp))
 allocate(finp%tau0(3,finp%nax,finp%nsp))

 return
 end subroutine 

 subroutine allocate_finp_rot(finp)
!==================================
 type(finp_type):: finp

 allocate(finp%srot(3,3,finp%nrot))

 return
 end subroutine 

 subroutine deallocate_finp(finp)
!================================
 type(finp_type):: finp

 deallocate(finp%na)
 deallocate(finp%lmax)
 deallocate(finp%lloc)
 deallocate(finp%zv)
 deallocate(finp%atom)
 deallocate(finp%tau0)
 deallocate(finp%xk)
 deallocate(finp%srot)

 return
 end subroutine 

 subroutine allocate_fgg_ngw(nkpt,fgg)
!=====================================
 integer::nkpt
 type(fgg_type):: fgg

 allocate(fgg%ngw(nkpt))

 return
 end subroutine

 subroutine allocate_fgg_g(ngx,fgg)
!==================================
 integer::ngx
 type(fgg_type):: fgg

 allocate(fgg%n1(ngx))
 allocate(fgg%n2(ngx))
 allocate(fgg%n3(ngx))
 allocate(fgg%g(ngx))
 allocate(fgg%gg(ngx,3))

 return
 end subroutine

 subroutine deallocate_fgg(fgg)
!==============================
 type(fgg_type)::fgg

 deallocate(fgg%ngw)
 deallocate(fgg%n1)
 deallocate(fgg%n2)
 deallocate(fgg%n3)
 deallocate(fgg%g)
 deallocate(fgg%gg)

 return
 end subroutine

 
 subroutine allocate_fwvf_wvf(fwvf)
!==================================
 type(fwvf_type):: fwvf

 allocate(fwvf%ngw(fwvf%nkptx))
 allocate(fwvf%n1(8*fwvf%ngwx))
 allocate(fwvf%n2(8*fwvf%ngwx))
 allocate(fwvf%n3(8*fwvf%ngwx))
 allocate(fwvf%igk(fwvf%ngwx,fwvf%nkptx))
 allocate(fwvf%xk(3,fwvf%nkptx))
 allocate(fwvf%wkpt(fwvf%nkptx))
 allocate(fwvf%c0(fwvf%ngwx))

 return
 end subroutine 

 subroutine allocate_fwvf_en(fwvf)
!=================================
 type(fwvf_type):: fwvf

 allocate(fwvf%eig(fwvf%nx,fwvf%nkptx))
 allocate(fwvf%focc(fwvf%nx,fwvf%nkptx))
 allocate(fwvf%zeta(fwvf%nr1x+1,fwvf%nr2x,fwvf%nr3x))

 return
 end subroutine 

 subroutine allocate_fwvf_nsp(fwvf)
!==================================
 type(fwvf_type):: fwvf

 allocate(fwvf%na(fwvf%nax))
 allocate(fwvf%atom(fwvf%nsx))
 allocate(fwvf%tau0(3,fwvf%nax,fwvf%nsx))

 return
 end subroutine 

 subroutine deallocate_fwvf(fwvf)
!================================
 type(fwvf_type):: fwvf

 deallocate(fwvf%ngw)

 deallocate(fwvf%n1)
 deallocate(fwvf%n2)
 deallocate(fwvf%n3)
 deallocate(fwvf%igk)
 deallocate(fwvf%xk)
 deallocate(fwvf%wkpt)
 deallocate(fwvf%eig)
 deallocate(fwvf%focc)
 deallocate(fwvf%zeta)
 deallocate(fwvf%c0)
 deallocate(fwvf%na)
 deallocate(fwvf%atom)
 deallocate(fwvf%tau0)

 return
 end subroutine 

 subroutine preallocate_fpp(nsx,fpp)
!===================================
 integer:: nsx
 type(fpp_type):: fpp

 allocate(fpp%mmax(nsx))
 allocate(fpp%file(nsx))
 allocate(fpp%iu(nsx))
 allocate(fpp%nang(nsx))
 allocate(fpp%clog(nsx))
 allocate(fpp%zval(nsx))

 return
 end subroutine 

 subroutine allocate_fpp(nsx,fpp)
!================================
 integer:: nsx
 type(fpp_type):: fpp

 fpp%mmaxx=maxval(fpp%mmax(:))
 fpp%lmax=maxval(fpp%nang(:))

 allocate(fpp%r(fpp%mmaxx,nsx))
 allocate(fpp%p(fpp%mmaxx,nsx,fpp%lmax))
 allocate(fpp%vion(fpp%mmaxx,nsx,fpp%lmax))
 allocate(fpp%rhocr(fpp%mmaxx,nsx))

 return
 end subroutine 

 subroutine dellocate_fpp(fpp)
!=============================
 type(fpp_type):: fpp

 deallocate(fpp%mmax)
 deallocate(fpp%file)
 deallocate(fpp%iu)
 deallocate(fpp%nang)
 deallocate(fpp%clog)
 deallocate(fpp%zval)
 deallocate(fpp%r)
 deallocate(fpp%p)
 deallocate(fpp%vion)
 deallocate(fpp%rhocr)

 return
 end subroutine 

 end module f2s_datatypes

!!***
