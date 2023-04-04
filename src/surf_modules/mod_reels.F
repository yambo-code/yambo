!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CH
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
module reels_module
  use pars,                 ONLY : SP
  integer                       :: npol, nfull
  logical                       :: lepsmix
  logical                       :: lreels
  complex(SP), allocatable,save :: epsm1_avA(:,:,:)
  real(SP), allocatable         :: loss(:,:,:)

contains

subroutine init_reels(defs)
  use pars,                  ONLY : SP, schlen
  use it_tools,              ONLY : it
  use it_m,                  ONLY : initdefs, V_general
  implicit none
  type(initdefs), intent(inout)  :: defs

  call it('f',defs,'EpsMix'  , '[REELS] DF mixing (`on`;`off`)', verb_level=V_general )
  call it(defs,'NumPol'  , '[REELS] Number of polarizations (`1`,`2`)', npol )
  call it(defs,'ETStpsLs'  , '[REELS] Total Energy Steps in Loss function', nfull, verb_level=V_general )

  return
end subroutine init_reels

end module reels_module
