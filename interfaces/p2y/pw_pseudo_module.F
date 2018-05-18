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
  logical           :: psp_is_ncpp
  logical           :: psp_is_uspp
  logical           :: psp_is_paw
  real(SP)          :: z               ! Ionic charge
  integer           :: nbeta           ! Number of nonlocal projectors
  integer           :: nmesh           ! Number of points in mesh
  integer           :: kkbeta          ! maxval(psp%mesh_beta(:))
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
  ! Augmentation
  real(SP), pointer :: dmat(:,:)       ! dmat(nbeta,nbeta) atomic D_{mu,nu}
  logical           :: q_with_l        ! if .true. qfunc is pseudized in
                                       ! different ways for different l
  integer           :: nqf             ! number of Q coefficients
  integer           :: nqlc            ! number of angular momenta in Q
  real(SP)          :: qqq_eps         ! qfunc is null if its norm is .lt. qqq_eps
  real(SP), pointer :: rinner(:)       ! rinner(0:2*lmax) r_L
  real(SP), pointer :: qqq(:,:)        ! qqq(nbeta,nbeta) q_{mu,nu}
                                       ! Augmentation without L dependecy
  real(SP), pointer :: qfunc(:,:)      ! qfunc(mesh,nbeta*(nbeta+1)/2)
                                       ! Q_{mu,nu}(|r|) function for |r|> r_L
                                       ! Augmentation depending on L (optional, compulsory for PAW)
  real(SP), pointer :: qfuncl(:,:,:)   !  qfuncl(mesh,nbeta*(nbeta+1)/2,l)
                                       ! Q_{mu,nu}(|r|) function for |r|> r_L
                                       ! Analitycal coeffs cor small r expansion of qfunc (Vanderbilt's code)
  real(SP), pointer :: qfcoef(:,:,:,:) ! qfcoef(nqf,0:2*lmax,nbeta,nbeta)
                                       ! coefficients for Q for |r|<r_L
  ! Radial Interpolation
  type(num_interpolation), pointer :: interp_beta(:)  ! Interpolation table beta-proj
  type(num_interpolation)          :: interp_nlcc     ! Interpolation table rho_nlcc
  type(num_interpolation), pointer :: interp_qfunc(:) ! Interpolation table Q_ij
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
  pseudo%psp_is_ncpp = .false.
  pseudo%psp_is_uspp = .false.
  pseudo%psp_is_paw  = .false.
  pseudo%z    = 0.0
  pseudo%nbeta = 0
  pseudo%kkbeta = 0
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
  !
  pseudo%q_with_l=.false.
  pseudo%nqf=0
  pseudo%nqlc=0
  pseudo%qqq_eps=-1.0
  allocate(pseudo%dmat(0,0))
  allocate(pseudo%rinner(0))
  allocate(pseudo%qqq(0,0))
  allocate(pseudo%qfunc(0,0))
  allocate(pseudo%qfuncl(0,0,0))
  allocate(pseudo%qfcoef(0,0,0,0))
  !
  allocate(pseudo%interp_beta(0))
  allocate(pseudo%interp_qfunc(0))
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
  pseudo%psp_is_ncpp = .false.
  pseudo%psp_is_uspp = .false.
  pseudo%psp_is_paw  = .false.
  pseudo%z    = 0.0
  pseudo%nbeta = 0
  pseudo%kkbeta = 0
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
  !
  pseudo%q_with_l=.false.
  pseudo%nqf=0
  pseudo%nqlc=0
  pseudo%qqq_eps=-1.0
  deallocate(pseudo%dmat)
  deallocate(pseudo%rinner)
  deallocate(pseudo%qqq)
  deallocate(pseudo%qfunc)
  deallocate(pseudo%qfuncl)
  deallocate(pseudo%qfcoef)
  !
  do i=1,size(pseudo%interp_beta)
    call num_interpolation_destroy(pseudo%interp_beta(i))
  enddo
  deallocate(pseudo%interp_beta)
  call num_interpolation_destroy(pseudo%interp_nlcc)
  !
  do i=1,size(pseudo%interp_qfunc)
    call num_interpolation_destroy(pseudo%interp_qfunc(i))
  enddo
  deallocate(pseudo%interp_qfunc)
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
  implicit none
  type (pw_pseudo), intent(inout) :: pseudo
  real(SP),         intent(in)    :: cutoff
  real(SP),         intent(in)    :: cutoff_rho
  integer  :: nr
  real(SP) :: q_max,q_max_rho,q,delta_q
  integer  :: ir,iq,ibeta,jbeta,l,ijv,li,lj,ilast,ndim
  real(SP) :: aux(pseudo%nmesh),aux1(pseudo%nmesh)
  
  pseudo%cutoff = cutoff
  pseudo%cutoff_rho = cutoff_rho
  q_max = sqrt(2.0*cutoff)
  q_max_rho = sqrt(2.0*cutoff_rho)
  delta_q = 0.01
  !
  deallocate(pseudo%interp_beta)
  allocate(pseudo%interp_beta(pseudo%nbeta))
  !
  do ibeta=1,pseudo%nbeta
    call num_interpolation_init(pseudo%interp_beta(ibeta),0._SP,q_max, &
                                delta_q,parity=+1)
    nr = pseudo%mesh_beta(ibeta)
    if(nr>pseudo%nmesh) call errore("pw_pseudo_set_table","nr>mesh",1)
    l = pseudo%lbeta(ibeta)
    do ir=1,nr
      aux(ir) = pseudo%beta(ir,ibeta)*pseudo%wmesh(ir)*pseudo%mesh(ir)**(l+1)
    enddo
    do iq=0,pseudo%interp_beta(ibeta)%n
      q = pseudo%interp_beta(ibeta)%x(iq)
      do ir=1,nr
        aux1(ir) = aux(ir) * num_xmlsphbes(q*pseudo%mesh(ir),l)
      enddo
      pseudo%interp_beta(ibeta)%y(iq) = num_4pi*num_simpson(aux1(1:nr))
    enddo
  enddo
  !
  ! NLCC
  !
  if (pseudo%psp_has_nlcc) then
    call num_interpolation_init(pseudo%interp_nlcc,0._SP,q_max_rho, &
                                delta_q,parity=+1)
    nr = pseudo%nmesh
    do ir=1,nr
      aux(ir) = pseudo%rho_nlcc(ir)*pseudo%wmesh(ir)*(pseudo%mesh(ir)**2)
    enddo
    !
    do iq=0,pseudo%interp_nlcc%n
      q = pseudo%interp_nlcc%x(iq)
      do ir=1,nr
        aux1(ir) = aux(ir) * num_xmlsphbes(q*pseudo%mesh(ir),0)
      enddo
      pseudo%interp_nlcc%y(iq) = num_4pi*num_simpson(aux1(1:nr))
    enddo
  endif
  !
  ! USPP
  !
  if (pseudo%psp_is_uspp) then
    !
    if (pseudo%q_with_l) call errore("pw_pseudo_set_table","q_with_l not implemented",10)
    !
    deallocate(pseudo%interp_qfunc)
    ndim=pseudo%nbeta*(pseudo%nbeta+1)/2
    allocate(pseudo%interp_qfunc(ndim))
    !
    pseudo%kkbeta=maxval(pseudo%mesh_beta(:))
    nr = pseudo%kkbeta
    !
    do l = 0, pseudo%nqlc - 1
      !
      do jbeta=1,pseudo%nbeta
      do ibeta=jbeta,pseudo%nbeta
        !
        lj = pseudo%lbeta(jbeta)
        li = pseudo%lbeta(ibeta)
        !
        ! momentum sum rule
        if (.not.( l>= abs(li-lj) .and. l<=li+lj .and. mod(l+li+lj,2)==0 )) cycle
        !
        ijv = ibeta * (ibeta-1) /2 + jbeta
        !
        call num_interpolation_init(pseudo%interp_qfunc(ijv),0._SP,q_max_rho, &
                                    delta_q,parity=+1)
        !
        do ir = 1, nr
           if ( pseudo%mesh(ir) >= pseudo%rinner(l+1) ) then
              aux(ir) = pseudo%qfunc(ir, ijv)
           else
              ilast = ir
           endif
        enddo
        !
        if ( pseudo%rinner(l+1) > 0.0_SP ) then
           call setqf( pseudo%qfcoef(1, l+1, jbeta, ibeta), aux, pseudo%mesh, &
                       pseudo%nqf,l,ilast)
        endif
        !
        do iq=0,pseudo%interp_qfunc(ijv)%n
          q = pseudo%interp_qfunc(ijv)%x(iq)
          do ir=1,nr
            aux1(ir) = aux(ir) * num_xmlsphbes(q*pseudo%mesh(ir),l)
          enddo
          pseudo%interp_qfunc(ijv)%y(iq) = num_4pi*num_simpson(aux1(1:nr))
        enddo
        !
      enddo
      enddo
      !
    enddo
    !
  endif

  !
  ! PAW
  !
  if (pseudo%psp_is_paw) call errore("pw_pseudo_set_table","PAW not implemented",1)
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
  !
  allocate(pseudo%dmat(nbeta,nbeta))
  allocate(pseudo%qqq(nbeta,nbeta))
  allocate(pseudo%qfunc(nmesh,nbeta*(nbeta+1)/2))
  allocate(pseudo%rinner(0))
  allocate(pseudo%qfuncl(0,0,0))
  allocate(pseudo%qfcoef(0,0,0,0))
  !
  allocate(pseudo%interp_beta(0))
  allocate(pseudo%interp_qfunc(0))
end subroutine pw_pseudo_set_dim

end module pw_pseudo_module
