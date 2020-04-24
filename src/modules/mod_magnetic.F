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
module magnetic
 !
 use pars, ONLY:SP,schlen
 implicit none
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
 ! Magnetic field in cartesian coordinates & gauge factors
 !
 real(SP)          :: Bx
 real(SP)          :: By
 real(SP)          :: Bz
 !
 ! Aharonov Bohm variables
 !
 real(SP)                 :: MAG_radius              
 logical,     allocatable :: ir_table(:)
 !
 ! Vector potential & gauge factors
 !
 real(SP),    allocatable :: A_magn_x(:)
 real(SP),    allocatable :: A_magn_y(:)
 real(SP),    allocatable :: A_magn_z(:)
 integer                  :: gauge_factors(2,3)
 !
 ! Phase trick phasctors
 !
 logical                  :: phase_trick
 real(SP), parameter      :: phase_radius=0.01
 integer                  :: n_loops
 !
end module
