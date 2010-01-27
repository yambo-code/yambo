!
! Copyright (C) 2000-2010 C. Hogan and the YAMBO team 
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
subroutine calc_loss_bulk(lossbq, hw, eps_b, nwp ) ! for this q and hwgrid
  use pars,                  ONLY : SP
  use eels_integration,      ONLY : dv, dsurf
  use eels_scattering
  implicit none
  integer,            intent(in) :: nwp
  real(SP),           intent(in) :: hw(nwp)
  complex(SP),        intent(in) :: eps_b(nwp)
  real(SP),          intent(out) :: lossbq(nwp)
! ws
  real(SP)                       :: dvac, hwin, qparm, bpar, bqfac, bq2
  real(SP)                       :: fac, ebfac
  complex(SP)                    :: ebulk
  integer                        :: iw

  dvac = abs(dv - dsurf) ! Check

  do iw = 1, nwp
    hwin = hw(iw)
    ebulk = eps_b(iw)
    !
    ! Check that scattering set up is preserved...need to det integrate again!
    !
    call calc_pre_qpar( hwin, bqfac, bq2, qparm, bpar )
    !
    fac   = qparm*cos(bpar*dvac) - bpar*sin(bpar*dvac) ! units?
    ebfac = aimag( (ebulk-1.0_SP)/(ebulk+1.0_SP) )
    !
    lossbq(iw) = bqfac * bq2 * ebfac * exp(-qparm*dvac ) * fac
    !
  enddo

  return
end subroutine calc_loss_bulk

