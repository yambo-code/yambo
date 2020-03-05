!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
module YPP_interfaces
 !
 interface
   !
   subroutine excitons_sort_and_report(iq,BS_R,BS_E,BS_E_SOC_corr,EXC_spin)
     use pars,          ONLY:SP
     use BS,            ONLY:BS_H_dim
     use YPPm,          ONLY:EXCITON_spin
     implicit none
     integer                      :: iq
     complex(SP)                  :: BS_R(BS_H_dim),BS_E(BS_H_dim)
     real(SP),   optional,pointer :: BS_E_SOC_corr(:,:)
     type(EXCITON_spin),optional  :: EXC_spin
   end subroutine
   !
 end interface
 !
end module YPP_interfaces
