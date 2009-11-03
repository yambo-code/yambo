!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
module magnetic
 !
 use pars, ONLY:SP,schlen
 !
 character(schlen) :: MAG_hamiltonian_type
 character(schlen) :: MAG_gauge               ! 'SYMM' 'X_ASYMM' 'Y_ASYMM' 'Z_ASYMM'
 logical           :: MAG_pauli
 logical           :: MAG_landau
 !
 ! Magnetic field in polar coordinates. 
 ! Theta angle with z axis; psi angle with x axis
 !
 real(SP)          :: MAG_B
 real(SP)          :: MAG_theta
 real(SP)          :: MAG_psi
 !
 ! Magnetic field in cartesian coordinates
 !
 real(SP)          :: Bx
 real(SP)          :: By
 real(SP)          :: Bz
 !
 real(SP)          :: MAG_radius              
 integer           :: gauge_factors(2,3)
 !
 real(SP),    allocatable :: A_magn_x(:)
 real(SP),    allocatable :: A_magn_y(:)
 real(SP),    allocatable :: A_magn_z(:)
 complex(SP), allocatable :: wf_x(:,:)
 complex(SP), allocatable :: wf_y(:,:)
 complex(SP), allocatable :: wf_z(:,:)
 real(SP),    allocatable :: x(:)
 real(SP),    allocatable :: y(:)
 real(SP),    allocatable :: z(:)
 logical,     allocatable :: ir_table(:)
 !
end module
