!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
module hamiltonian
 !
 use pars,       ONLY:SP,schlen
 !
 integer           :: H_ref_bands(2)
 !
 character(schlen) :: H_potential
 !
 ! Possible Kinds, functionals and external perturbations
 !
 integer           :: H_kind
 integer           :: H_xc_functional
 integer           :: H_perturbation
 !
 complex(SP), allocatable :: Ho(:,:,:,:)
 !
 contains
   !
   integer function B_mat_index(ib1,ib2,nb)
     integer :: ib1,ib2,nb(2)
     !
     B_mat_index=(ib1-nb(1))*(nb(2)-nb(1)+1)+ib2-nb(1)+1
     !
   end function
   !
end module
