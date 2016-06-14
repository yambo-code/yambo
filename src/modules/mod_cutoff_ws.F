!
! Copyright (C) 2002-2008 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Written by Giovanni Bussi
! Adapted to QE    by Andrea Ferretti & Layla Martin Samos
! Adapted to Yambo by Andrea Ferretti & Daniele Varsano
!
!----------------------------------
  MODULE cutoff_ws_m
  !----------------------------------
  !
  use pars, only : DP,PI
  IMPLICIT NONE

  !
  ! general purpose parameters
  !
  REAL(DP), PARAMETER :: e2     = 2.0_DP
  REAL(DP), PARAMETER :: toll   = 1.0E-6_DP
 
  !
  ! definitions
  ! 
  TYPE vcut_type
      REAL(DP)          :: a(3,3)
      REAL(DP)          :: b(3,3)
      REAL(DP)          :: a_omega
      REAL(DP)          :: b_omega
      REAL(DP), POINTER :: corrected(:,:,:)
      REAL(DP)          :: cutoff
      LOGICAL           :: orthorombic
  END TYPE vcut_type

  !
  ! interfaces
  !
  interface
    !
    function num_inverse(a) result(inv)
       use pars
       implicit none
       real(dp)              :: inv(0:2,0:2)
       real(dp), intent(in)  :: a(0:2,0:2)
    end function
    !
    function vcut_minimal_image(a,b,r1,orthorombic) result(res)
       use pars
       implicit none
       real(dp), intent(in) :: a(3,3)
       real(dp), intent(in) :: b(3,3)
       real(dp), intent(in) :: r1(3)
       logical,  intent(in) :: orthorombic
       real(dp)             :: res(3)
    end function
    !
  end interface

END MODULE cutoff_ws_m

