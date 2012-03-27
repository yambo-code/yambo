!
! Copyright (C) 2002-2004 quantum-ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!------------------------------------------------------------------------------!
    MODULE kinds
!------------------------------------------------------------------------------!

      IMPLICIT NONE
      SAVE
! ... kind definitions
      INTEGER, PARAMETER :: DP = selected_real_kind(14,200)
#if defined _DOUBLE
      INTEGER, PARAMETER :: SP = selected_real_kind(14,200)
#else
      INTEGER, PARAMETER :: SP = selected_real_kind(6,30)
#endif
      INTEGER, PARAMETER :: i4b = selected_int_kind(9)
      PUBLIC :: SP, DP, i4b

    END MODULE kinds
