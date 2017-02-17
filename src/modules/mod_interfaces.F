!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
module interfaces
 !
 implicit none
 !
 interface
   !
   subroutine OCCUPATIONS_gaps(E,E_g_dir,E_g_ind,Nbf,Nbm)
     use pars, ONLY: SP
     use electrons,     ONLY:levels,n_sp_pol
     type(levels)       :: E
     real(SP), optional :: E_g_dir(1+(n_sp_pol-1)*2,2) ! min - max
     real(SP), optional :: E_g_ind(1+(n_sp_pol-1)*2,2) ! min - max
     integer , optional :: Nbf(n_sp_pol)
     integer , optional :: Nbm(n_sp_pol)
   end subroutine 
   !
 end interface
 !
end module
