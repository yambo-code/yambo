!
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------
SUBROUTINE init_vloc()
  !----------------------------------------------------------------------
  !! This routine computes the fourier coefficient of the local
  !! potential vloc(ig,it) for each type of atom.
  !
  USE atom,           ONLY : msh, rgrid
  !USE m_gth,          ONLY : vloc_gth
  USE kinds,          ONLY : DP
  USE uspp_param,     ONLY : upf
  USE ions_base,      ONLY : ntyp => nsp
  USE cell_base,      ONLY : omega, tpiba ! [FP] tpiba2 not in module
  USE vlocal,         ONLY : vloc
  USE gvect,          ONLY : ngl, gl, gshells
  !USE Coul_cut_2D,    ONLY : do_cutoff_2D, cutoff_lr_Vloc
  !
  IMPLICIT NONE
  !
  INTEGER :: nt
  ! counter on atomic types
  !
  vloc(:,:) = 0._DP
  !
  do nt = 1, ntyp
     !
     ! compute V_loc(G) for a given type of atom
     !
     ! [FP] We only consider the normal case. Check the original
     !      QE subroutine of the same name for more options
     !
     ! normal case
     !
     call vloc_of_g( rgrid(nt)%mesh, msh(nt), rgrid(nt)%rab, rgrid(nt)%r, &
                     upf(nt)%vloc(1), upf(nt)%zp, tpiba**2._DP, ngl, gl, omega, vloc(1,nt) )
     !
  enddo
  !
  ! [FP] QE cutoff_2D goes here. Not supported so far
  !
END SUBROUTINE init_vloc

