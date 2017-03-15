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
 use pars, ONLY:SP
implicit none
! Module containing the pw_pseudo type and its methods
private
public :: pw_pseudo,           &
          pw_pseudo_init,      &
          pw_pseudo_destroy,   &
          pw_pseudo_set_table, &
          pw_pseudo_set_dim
        

type pw_pseudo
  logical           :: psp_has_so
  logical           :: psp_has_nlcc
  real(SP)          :: z               ! Ionic charge
  integer           :: nbeta           ! Number of nonlocal projectors
  integer           :: nmesh           ! Number of points in mesh
  real(SP)          :: cutoff          ! Maximum useful cutoff (Ry)
  real(SP)          :: cutoff_rho      ! Maximum useful cutoff for the density (Ry)
  real(SP), pointer :: mesh(:)         ! Mesh [nmesh]
  real(SP), pointer :: wmesh(:)        ! weights for integrals [nmesh]
  real(SP), pointer :: rho_nlcc(:)     ! nlcc charge density [nmesh]
  real(SP), pointer :: vloc(:)         ! Local potential [nmesh]
  integer,  pointer :: lbeta(:)
  real(SP), pointer :: jbeta(:)
  integer,  pointer :: mesh_beta(:)
  real(SP), pointer :: beta(:,:)       ! Non-local projectors [nmesh,nbeta]
  real(SP), pointer :: d(:)            ! D_ii factors (diagonal for NC) [nbeta]
  type(num_interpolation), &
            pointer :: interpolation(:)    ! Interpolation table beta-proj
  type(num_interpolation)  &
                    :: interpolation_nlcc  ! Interpolation table rho_nlcc
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
  pseudo%cutoff = 0.0
  pseudo%cutoff_rho = 0.0
  allocate(pseudo%mesh(0))
  allocate(pseudo%wmesh(0))
  allocate(pseudo%rho_nlcc(0))
  allocate(pseudo%vloc(0))
  allocate(pseudo%lbeta(0))
  allocate(pseudo%jbeta(0))
  allocate(pseudo%mesh_beta(0))
  allocate(pseudo%beta(0,0))
  allocate(pseudo%d(0))
  allocate(pseudo%interpolation(0))
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
  pseudo%cutoff = 0._SP
  pseudo%cutoff_rho = 0._SP
  deallocate(pseudo%mesh)
  deallocate(pseudo%wmesh)
  deallocate(pseudo%rho_nlcc)
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
  call num_interpolation_destroy(pseudo%interpolation_nlcc)
end subroutine pw_pseudo_destroy0

subroutine pw_pseudo_destroy1(pseudo)
  type (pw_pseudo), intent(inout) :: pseudo(:)
  integer ::i
  do i=1,size(pseudo)
    call pw_pseudo_destroy(pseudo(i))
  end do
end subroutine pw_pseudo_destroy1

subroutine pw_pseudo_set_table(pseudo,cutoff,cutoff_rho)
! use tools_module
! use num_module
  type (pw_pseudo), intent(inout) :: pseudo
  real(SP),         intent(in)    :: cutoff
  real(SP),         intent(in)    :: cutoff_rho
  integer  :: nr
  real(SP) :: q_max,q_max_rho,q,delta_q
  integer  :: ir,iq,ibeta,l
  real(SP) :: aux(pseudo%nmesh),aux1(pseudo%nmesh)
  
  pseudo%cutoff = cutoff
  pseudo%cutoff_rho = cutoff_rho
  q_max = sqrt(2.0*cutoff)
  delta_q = 0.01
  deallocate(pseudo%interpolation)
  allocate(pseudo%interpolation(pseudo%nbeta))
  do ibeta=1,pseudo%nbeta
    call num_interpolation_init(pseudo%interpolation(ibeta),0._SP,q_max, &
                                delta_q,parity=+1)
    nr = pseudo%mesh_beta(ibeta)
    if(nr>pseudo%nmesh) call errore("pw_pseudo_set_table","nr>mesh",1)
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
  !
  ! NLCC
  !
  if (pseudo%psp_has_nlcc) then
    q_max_rho = sqrt(2.0*cutoff_rho)
    call num_interpolation_init(pseudo%interpolation_nlcc,0._SP,q_max_rho, &
                                delta_q,parity=+1)
    nr = pseudo%nmesh
    do ir=1,nr
      aux(ir) = pseudo%rho_nlcc(ir)*pseudo%wmesh(ir)*(pseudo%mesh(ir)**2)
    end do
    do iq=0,pseudo%interpolation_nlcc%n
      q = pseudo%interpolation_nlcc%x(iq)
      do ir=1,nr
        aux1(ir) = aux(ir) * num_xmlsphbes(q*pseudo%mesh(ir),0)
      end do
      pseudo%interpolation_nlcc%y(iq) = num_4pi*num_simpson(aux1(1:nr))
    end do
  endif
  !
end subroutine pw_pseudo_set_table

subroutine pw_pseudo_set_dim(pseudo,nbeta,nmesh)
  type (pw_pseudo), intent(inout) :: pseudo
  integer,          intent(in)    :: nbeta,nmesh
  call pw_pseudo_destroy(pseudo)
  pseudo%nbeta = nbeta
  pseudo%nmesh = nmesh
  allocate(pseudo%mesh(nmesh))
  allocate(pseudo%wmesh(nmesh))
  allocate(pseudo%rho_nlcc(nmesh))
  allocate(pseudo%vloc(nmesh))
  allocate(pseudo%lbeta(nbeta))
  allocate(pseudo%jbeta(nbeta))
  allocate(pseudo%mesh_beta(nbeta))
  allocate(pseudo%beta(nmesh,nbeta))
  allocate(pseudo%d(nbeta))
  allocate(pseudo%interpolation(0))
end subroutine pw_pseudo_set_dim

end module pw_pseudo_module
