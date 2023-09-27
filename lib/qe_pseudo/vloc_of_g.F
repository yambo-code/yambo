!
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------
subroutine vloc_of_g (mesh, msh, rab, r, vloc_at, zp, tpiba2, ngl, &
     gl, omega, vloc)
  !----------------------------------------------------------------------
  !
  !    This routine computes the Fourier transform of the local
  !    part of an atomic pseudopotential, given in numerical form.
  !    A term erf(r)/r is subtracted in real space (thus making the
  !    function short-ramged) and added again in G space (for G<>0)
  !    The G=0 term contains \int (V_loc(r)+ Ze^2/r) 4pi r^2 dr.
  !    This is the "alpha" in the so-called "alpha Z" term of the energy.
  !    Atomic Ry units everywhere.
  !
  !   [FP] Unit check (Ry or Ha for yambo)
  !
  USE kinds
  USE constants, ONLY : pi, fpi, e2, eps8
  !USE esm, ONLY : do_comp_esm, esm_bc
  !USE Coul_cut_2D, ONLY: do_cutoff_2D, lz 
  implicit none
  !
  !    first the dummy variables
  !
  integer, intent(in) :: ngl, mesh, msh
  ! ngl : the number of shells of G vectors
  ! mesh: number of grid points in the radial grid
  ! msh : as above, used for radial integration
  !
  real(DP), intent(in) :: zp, rab (mesh), r (mesh), vloc_at (mesh), tpiba2, &
                          omega, gl (ngl)
  ! zp : valence pseudocharge
  ! rab: the derivative of mesh points
  ! r  : the mesh points
  ! vloc_at: local part of the atomic pseudopotential on the radial mesh
  ! tpiba2 : 2 pi / alat
  ! omega  : the volume of the unit cell
  ! gl     : the moduli of g vectors for each shell
  !
  real(DP), intent(out):: vloc (ngl)
  !
  ! vloc: the fourier transform of the potential
  !
  !    local variables
  !
  real(DP) :: vlcp, fac, gx
  real(DP), allocatable :: aux (:), aux1 (:)
  integer :: igl, igl0, ir
  ! igl :counter on g shells vectors
  ! igl0:first shell with g != 0
  ! ir  :counter on mesh points
  !
  real(DP), external :: qe_erf
  !
  allocate ( aux(msh), aux1(msh) )
  if (gl (1) < eps8) then
     !
     ! first the G=0 term
     !
     ! [FP] QE esm and 2D_cutoff for G=0 go here. So far not supported.
     !      Check the original subroutine of the same name in QE.
     !
     do ir = 1, msh
        aux (ir) = r (ir) * (r (ir) * vloc_at (ir) + zp * e2)
     enddo
     !
     call simpson (msh, aux, rab, vlcp)
     vloc (1) = vlcp        
     igl0 = 2
  else
     igl0 = 1
  endif
  !
  !   here the G<>0 terms, we first compute the part of the integrand 
  !   function independent of |G| in real space
  !
  do ir = 1, msh
     aux1 (ir) = r (ir) * vloc_at (ir) + zp * e2 * qe_erf (r (ir) )
  enddo
  fac = zp * e2 / tpiba2
  !
  !    and here we perform the integral, after multiplying for the |G|
  !    dependent part
  !
  do igl = igl0, ngl
     gx = sqrt (gl (igl) * tpiba2)
     do ir = 1, msh
        aux (ir) = aux1 (ir) * sin (gx * r (ir) ) / gx
     enddo
     call simpson (msh, aux, rab, vlcp)
     !
     ! [FP] QE esm and cutoff_2D for G<>0 go here.
     !
     vlcp = vlcp - fac * exp ( - gl (igl) * tpiba2 * 0.25d0) / gl (igl)
     vloc (igl) = vlcp
  enddo
  vloc (:) = vloc(:) * fpi / omega
  deallocate (aux, aux1)

!return
end subroutine vloc_of_g
