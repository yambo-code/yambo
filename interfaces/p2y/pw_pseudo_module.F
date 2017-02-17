! Self-energies and eXcitations (SaX)
! Copyright (C) 2006 SaX developers team
! Hacked by C. Hogan (2010)
! 
! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License
! as published by the Free Software Foundation; either version 2
! of the License, or (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

!#include "tools_error.h"

module pw_pseudo_module
!use sax_module
!use pw_common_module
!use num_interpolation_module, only : num_interpolation
 use num_interpolation_module
 use numerical_module
implicit none
! Module containing the pw_pseudo type and its methods
private
public :: pw_pseudo,           &
          pw_pseudo_init,      &
          pw_pseudo_destroy,   &
          pw_pseudo_set_table, &
          pw_pseudo_set_dim
        

type pw_pseudo
  real             :: z               ! Ionic charge
  integer          :: nbeta           ! Number of nonlocal projectors
  integer          :: nmesh           ! Number of points in mesh
  real, pointer    :: mesh(:)         ! Mesh [nmesh]
  real, pointer    :: wmesh(:)        ! weights for integrals [nmesh]
  real, pointer    :: vloc(:)         ! Local potential [nmesh]
  integer, pointer :: lbeta(:)
  real, pointer    :: jbeta(:)
  integer, pointer :: mesh_beta(:)
  real, pointer    :: beta(:,:)       ! Non-local projectors [nmesh,nbeta]
  real, pointer    :: d(:)            ! D_ii factors (diagonal for NC) [nbeta]
  real             :: cutoff          ! Maximum useful cutoff (Ry)
  type(num_interpolation), &
           pointer :: interpolation(:)      ! Interpolation table
end type pw_pseudo

interface pw_pseudo_init
  module procedure pw_pseudo_init0
  module procedure pw_pseudo_init1
end interface
interface pw_pseudo_destroy
  module procedure pw_pseudo_destroy0
  module procedure pw_pseudo_destroy1
end interface

contains

subroutine pw_pseudo_init0(pseudo)
  type (pw_pseudo), intent(out) :: pseudo
  pseudo%z    = 0.0
  pseudo%nbeta = 0
  pseudo%nmesh = 0
  allocate(pseudo%mesh(0))
  allocate(pseudo%wmesh(0))
  allocate(pseudo%vloc(0))
  allocate(pseudo%lbeta(0))
  allocate(pseudo%jbeta(0))
  allocate(pseudo%mesh_beta(0))
  allocate(pseudo%beta(0,0))
  allocate(pseudo%d(0))
  allocate(pseudo%interpolation(0))
  pseudo%cutoff = 0.0
end subroutine pw_pseudo_init0

subroutine pw_pseudo_init1(pseudo)
  type (pw_pseudo), intent(out) :: pseudo(:)
  integer ::i
  do i=1,size(pseudo)
    call pw_pseudo_init(pseudo(i))
  end do
end subroutine pw_pseudo_init1

subroutine pw_pseudo_destroy0(pseudo)
  type (pw_pseudo), intent(inout) :: pseudo
  integer ::i
  pseudo%z    = 0.0
  pseudo%nbeta = 0
  pseudo%nmesh = 0
  deallocate(pseudo%mesh)
  deallocate(pseudo%wmesh)
  deallocate(pseudo%vloc)
  deallocate(pseudo%lbeta)
  deallocate(pseudo%jbeta)
  deallocate(pseudo%mesh_beta)
  deallocate(pseudo%beta)
  deallocate(pseudo%d)
  do i=1,size(pseudo%interpolation)
    call num_interpolation_destroy(pseudo%interpolation(i))
  end do
  deallocate(pseudo%interpolation)
  pseudo%cutoff = 0.0
end subroutine pw_pseudo_destroy0

subroutine pw_pseudo_destroy1(pseudo)
  type (pw_pseudo), intent(inout) :: pseudo(:)
  integer ::i
  do i=1,size(pseudo)
    call pw_pseudo_destroy(pseudo(i))
  end do
end subroutine pw_pseudo_destroy1

subroutine pw_pseudo_set_table(pseudo,cutoff)
! use tools_module
! use num_module
  type (pw_pseudo), intent(inout) :: pseudo
  real,             intent(in)    :: cutoff
  integer :: nr
  real    :: q_max,q,delta_q
  integer :: ir,iq,ibeta,l
  real    :: aux(pseudo%nmesh),aux1(pseudo%nmesh)
  

  pseudo%cutoff = cutoff
  q_max = sqrt(2.0*cutoff)
  delta_q = 0.01
  deallocate(pseudo%interpolation)
  allocate(pseudo%interpolation(pseudo%nbeta))
  do ibeta=1,pseudo%nbeta
    call num_interpolation_init(pseudo%interpolation(ibeta),0.0,q_max, &
                                delta_q,parity=+1)
    nr = pseudo%mesh_beta(ibeta)
    if(nr>pseudo%nmesh) call errore("pw_speudo_set_table","nr>mesh",1)
    l = pseudo%lbeta(ibeta)
    do ir=1,nr
      aux(ir) = pseudo%beta(ir,ibeta)*pseudo%wmesh(ir)*pseudo%mesh(ir)**(l+1)
    end do
    do iq=0,pseudo%interpolation(ibeta)%n
      q = pseudo%interpolation(ibeta)%x(iq)
      do ir=1,nr
        aux1(ir) = aux(ir) * num_xmlsphbes(q*pseudo%mesh(ir),l)
      end do
      pseudo%interpolation(ibeta)%y(iq) = &
              num_4pi*num_simpson(aux1(1:nr))
    end do
  end do
end subroutine pw_pseudo_set_table

subroutine pw_pseudo_set_dim(pseudo,nbeta,nmesh)
  type (pw_pseudo), intent(inout) :: pseudo
  integer,          intent(in)    :: nbeta,nmesh
  call pw_pseudo_destroy(pseudo)
  pseudo%nbeta = nbeta
  pseudo%nmesh = nmesh
  allocate(pseudo%mesh(nmesh))
  allocate(pseudo%wmesh(nmesh))
  allocate(pseudo%vloc(nmesh))
  allocate(pseudo%lbeta(nbeta))
  allocate(pseudo%jbeta(nbeta))
  allocate(pseudo%mesh_beta(nbeta))
  allocate(pseudo%beta(nmesh,nbeta))
  allocate(pseudo%d(nbeta))
  allocate(pseudo%interpolation(0))
end subroutine pw_pseudo_set_dim

end module pw_pseudo_module
