!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
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
