!
! Copyright (C) 2000-2012 C. Hogan and the YAMBO team 
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
subroutine extract_eps_surf(surf_i, cell_i, npol, eps_b, nw, lbulkerr, lsurferr)
  use pars,                   ONLY : lchlen, schlen, SP
  use units,                  ONLY : HA2EV
  use surface_geometry,       only : d_surf, az, nlcell, nlslab, nlbulk, lsymslab, d_vac
  use optcut,                 ONLY : loptcut, dc
  use com,                    ONLY : msg
  implicit none
  integer,            intent(in)  :: nw, npol
  complex(SP),        intent(in)  :: cell_i(nw,npol) ! 
  complex(SP),        intent(in)  :: eps_b(nw)
  logical,            intent(in)  :: lbulkerr
  complex(SP),        intent(out) :: surf_i(nw,npol)
  logical,            intent(out) :: lsurferr
! ws
  real(SP)                        :: d_bulk, nlsurf, nlvac
  real(SP)                        :: d_layer, d_slab, d_cell
  integer                         :: ie, ipol
  character(lchlen)               :: lch1
  !
  lsurferr = .false.
  d_surf = -1 ! for checks
  !
  ! If no bulk eps, no surface by substraction
  if(.not.loptcut.and.lbulkerr) then
    lsurferr = .true.
    return
  endif
  !
  ! Need some checks on divide by zero.
  if(abs(az).lt.0.001.or.abs(real(nlcell)).lt.0.001) then
    call msg('nrs','Error extracting surface epsilon.')
    lsurferr = .true.
    return
  endif

  d_cell  = az
  d_layer = d_cell/real(nlcell)
  d_slab  = nlslab * d_layer
  d_vac   = d_cell - d_slab
  nlvac   = nlcell - nlslab
  d_bulk  = nlbulk * d_layer
  !
  ! Cutoff used => eps_surf already obtained. Ignore nlbulk.
  ! Renormalization done previously.
  !
  if(loptcut) then
    d_surf = dc
    do ipol = 1, npol
      surf_i(:,ipol) = cell_i(:,ipol)
    enddo
    nlsurf = dc/d_layer
    call msg('nr','Surface layer width defined by cutoff parameter.')
    if(lsymslab) call msg('nr','Note: slab symmetry ignored!')
    call write_cell_info
    return
  endif
  !
  ! Zero bulk layers => Use eps of full slab, d_surf = d_slab(/2)
  !
  if(nlbulk.le.0.001) then
    if(lsymslab) then
      call msg('r',&
&     'Surface dielectric function defined over -half- slab.')
      d_surf = d_slab * 0.5_SP
      nlsurf = nlslab * 0.5_SP
      do ipol = 1, npol
        surf_i(:,ipol) = cell_i(:,ipol)*d_cell/(d_surf*2.0_SP)
      enddo
    else
      ! This does not yield anything physically useful.
      call msg('nr',&
&     'Surface dielectric function defined over -full- slab.')
      d_surf = d_slab
      nlsurf = nlslab
      do ipol = 1, npol
        surf_i(:,ipol) = cell_i(:,ipol)*d_cell/d_surf
      enddo
    endif
    do ipol = 1, npol
      if(aimag(surf_i(1,ipol)).lt.0.001) &
&          surf_i(1,ipol) = cmplx( real(surf_i(1,ipol)),0.0_SP,SP)
    enddo
    call write_cell_info
    return
  endif
  !
  ! Non-zero bulk layers => Subtract from slab_eps
  !
  if(abs(nlslab-nlbulk).le.0.001) then
    call msg('nrs','Error: surface layer zero width! Exiting.')
    lsurferr = .true.
    return
  endif
  !
  if(lsurferr) return
  !
  ! Symmetric slab case: subtract bulk and divide by 2
  !
  if(lsymslab) then
    nlsurf = (nlslab - nlbulk )/2.0_SP
    d_surf = (d_slab - d_bulk )/2.0_SP  ! width of single surface
    do ie=1, nw
      do ipol = 1, npol
        surf_i(ie,ipol)=(d_cell*cell_i(ie,ipol)- d_bulk*eps_b(ie))/(d_surf*2.0_SP)
      enddo
    enddo
    call msg('nr',&
&   'Symmetric slab: dielectric function for one surface extracted.')
  !
  ! Asymmetric slab case: subtract bulk
  !
  else
    nlsurf = (nlslab - nlbulk )
    d_surf =  d_slab - d_bulk 
    do ie=1, nw
      do ipol = 1, npol
        surf_i(ie,ipol)=(d_cell*cell_i(ie,ipol)- d_bulk*eps_b(ie))/(d_surf*1.0_SP)
      enddo
    enddo
    call msg('nr','Non symmetric slab without cutoff:'// &
&      ' bulk dielectric function subtracted. Result is unphysical!.')
  endif
  call write_cell_info

  return

contains

  subroutine write_cell_info
    implicit none

    write(lch1,'(a14,1x,5a9,a10)') &
&   "Supercell:","Layer","Cell","Vacuum","Slab","Bulk","Surf"
    if(lsymslab) write(lch1,'(a14,1x,5a9,a10)') &
&   "Supercell:","Layer","Cell","Vacuum","Slab","Bulk","2 x Surf"

    call msg('nr',lch1)

    write(lch1,'(a14,1x,5f9.2,f10.2)') "No. of layers:",&
&               1.0,nlcell, nlvac, nlslab, nlbulk, nlsurf
    call msg('r',lch1)

    write(lch1,'(a14,1x,5f9.4,f10.2)') "Atomic units:",&
&               d_layer,d_cell, d_vac, d_slab, d_bulk, d_surf
    call msg('r',lch1)
    return
  end subroutine write_cell_info
end subroutine extract_eps_surf
