!
! Copyright (C) 2000-2013 C. Hogan and the YAMBO team 
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
module ras_module
  use pars,                   ONLY : SP
  implicit none
  
  logical                         :: lras, lreels, ltrans
  !
  ! Variables for the Localization routine
  !
  logical                         :: lloc
  character(len=2)                :: loctype
  logical, allocatable            :: transloc(:,:,:)
  real(SP)                        :: locfac, upperlim, lowerlim

contains

subroutine CalcRAS(hw, eps_x, eps_y, eps_b, nw, d_surf,label)
  use pars,                   ONLY : schlen, SP
  use units,                  ONLY : HA2EV
  use com,                    ONLY : msg, of_open_close
  implicit none
  integer,             intent(in) :: nw
  real(SP),            intent(in) :: hw(nw), d_surf
  complex(SP),         intent(in) :: eps_x(nw),eps_y(nw),eps_b(nw)
  character(4), intent(in)       :: label
! Local
  real(SP)                        :: Aw(nw), Bw(nw) 
  real(SP)                        :: Rdelta(nw), Idelta(nw)
  real(SP)                        :: RAS(nw),SDR(nw)
  real(SP)                        :: denom, od(9), cost
  integer                         :: i
  character(schlen)               :: of_name
  character(10)                   :: headings(9)
  
  
  cost = 4.0_SP/137.03604_SP

  do i = 1, nw
     denom     = (1.0_SP - real( eps_b(i) ) )**2 + (AIMAG( eps_b(i)) )**2
     Aw(i)      = ( real( eps_b(i) ) - 1.0_SP ) / denom
     Bw(i)      =  AIMAG( eps_b(i) )            / denom
     Rdelta(i) =   real( eps_x(i) ) -  real( eps_y(i) )   
     Idelta(i) =  AIMAG( eps_x(i) ) - AIMAG( eps_y(i) )  
     RAS(i)    = cost * hw(i) * d_surf * ( Aw(i) * Idelta(i) - Bw(i) * Rdelta(i) )
     SDR(i)    = -1.0_SP * cost * hw(i) * d_surf * &
&           AIMAG( (1.0_SP - 0.5*(eps_x(i)+eps_y(i)))/(1.0_SP - eps_b(i)) )
  enddo

  write (of_name,'(a,a)') 'ras'//label
  call of_open_close(of_name,'ot')

! Output file titles
!       1     2       3         4      5   6     7         8
  headings(1:9) = (/' 1',' 2',' 3',' 4',' 5',' 6',' 7',' 8',' 9'/)
  call msg('o ras','#',headings(1:8),INDENT=0,USE_TABS=.true.)
  headings(1:9) = (/'hw (eV)','    RAS','e1(x-y)','e2(x-y)', &
 &                    '      A','      B',' A x e2',' B x e1','    SDR'/)
  call msg('o ras','#',headings(1:9),INDENT=0,USE_TABS=.true.)

! Write data to file
  do i = 1, nw
     od = (/ hw(i)*HA2EV, RAS(i), Rdelta(i), Idelta(i), Aw(i), Bw(i), &
&             Aw(i)*Idelta(i), Bw(i)*Rdelta(i), SDR(i) /)
     call msg('o ras','',od,INDENT=0,USE_TABS=.true.)
  enddo 
  call of_open_close(of_name)

  return
end subroutine CalcRAS


end module ras_module
