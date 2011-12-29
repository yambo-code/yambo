!
! Copyright (C) 2000-2012 C. Hogan  and the YAMBO team 
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
module analyse_peaks
  !
  ! Under development (Conor)
  !
  use pars,                   ONLY : schlen, SP, pi
! Here will put input data for windows, etc.
  logical                         :: lAnalysePeaks
  save
  private

  public :: print_analyse_peaks, init_analyse_peaks
  public :: lAnalysePeaks

contains

subroutine init_analyse_peaks(defs)
  !
  use it_m,                  ONLY : it, initdefs, E_unit,G_unit,T_unit, V_general
  use parser_m,      only : parser
  implicit none
  type(initdefs), intent(inout)  :: defs

  call it('f',defs,'PkAnlyse', '[RAS] Peak analysis mode (Uncomment to use)', &
&       verb_level=V_general )
  call parser('PkAnlyse',lAnalysePeaks)

  return
end subroutine init_analyse_peaks

    subroutine print_analyse_peaks
      use com,   ONLY:msg
      implicit none

      if(lAnalysePeaks) call msg('r','Transition analysis requested.')
 
      return
    end subroutine print_analyse_peaks

end module analyse_peaks
