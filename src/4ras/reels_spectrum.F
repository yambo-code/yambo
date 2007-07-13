!
! Copyright (C) 2000-2005 C. Hogan and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
subroutine CalcREELS(lossf, hw, surf_i, eps_b, nw, qpard)
  use pars,                 ONLY : schlen, SP, HARTREE, PI
  use surface_geometry
  use eels_kinematics
  use eels_detector
  use model_loss_function
  implicit none
  integer,      intent(in) :: nw
  real(SP),     intent(in) :: hw(nw)
  complex(SP),  intent(in) :: eps_b(nw)
  real(SP),     intent(out):: qpard(2)
  !
  ! surf_i(nw,1) = q_x, etc on input.
  !
  complex(SP),  intent(in) :: surf_i(nw,3) ! MUST have npol = 3
  real(SP),     intent(out):: lossf(nw,2)
! Local
  integer                  :: iw, idet
  real(SP)                 :: costakk, kpmag, qpard_(nw)
  real(SP)                 :: qparm, qtot2, qpar(2)
  real(SP)                 :: Akk, loss_qpar(2)
  real(SP)                 :: tmpImg, tmpImg2   ! debug
  complex(SP)              :: eps_s(3)
  !
  ! Energy independent stuff. Other input in calling routine
  !
  call setup_det
  !
  costakk=2.0_SP/PI/PI/costheta0

  do iw = 1, nw
    kpmag = kpmagf( hw(iw) )
    write(40,*) hw(iw)*HARTREE,kpmag

    loss_qpar(:) = 0
    !
    ! Integrate scattering cross section over detector window
    !
    do idet = 1, ndetpt
      !
      ! Get Q_|| for this set of angles.
      !
      call get_qpar_det(qpar, kpmag, idet )
!   write(40,*) hw(iw)*HARTREE,kpmag,idet,qpar
      !
      ! Reset trig factors for this detector setting
      !
      call reset_trig_det( theta_det(idet), 0.0_SP )
      !
      ! Kinematic prefactor
      !
      qparm  = sqrt( qpar(1)**2 + qpar(2)**2 )
      if(idet.eq.1) qpard_(iw) = qparm*d_surf
      qtot2 = kmag**2 + kpmag**2 - 2.0_SP * kmag * kpmag * costhetad
      Akk = costakk*(kpmag/kmag)*(qparm/qtot2**2) * w_det(idet)
      !
      ! Parallel case 
      !
      if(lparallel) Akk = 2.0_SP/PI/kmag**2  
      !
      ! Loss function for q || q_x
      !
      eps_s(:) = (/ surf_i(iw,1), surf_i(iw,2), surf_i(iw,3) /)
      tmpImg = model_Img(lossfm, eps_b(iw),qpar=qpar, esurf = eps_s, dsurf = d_surf, dvac = bimp)
      loss_qpar(1) = loss_qpar(1) + Akk * w_det(idet) * &
&         model_Img(lossfm, eps_b(iw), qpar=qpar,esurf=eps_s,dsurf=d_surf,dvac=bimp)
!     write(38,*) loss_qpar(1),Akk,w_det(idet),
      !
      ! Loss function for q || q_y
      !
      eps_s(:) = (/ surf_i(iw,2), surf_i(iw,1), surf_i(iw,3) /)
      tmpImg2 = model_Img(lossfm,eps_b(iw), qpar=qpar, esurf = eps_s, dsurf = d_surf, dvac = bimp)
      loss_qpar(2) = loss_qpar(2) + Akk * w_det(idet) * & 
&         model_Img(lossfm,eps_b(iw),qpar=qpar,esurf=eps_s,dsurf=d_surf,dvac=bimp)
      !
    enddo

    lossf(iw,:) = loss_qpar(:) ! other factors?

!<-- DEBUG
!    write(88,107) hw(iw)*HARTREE,qparm,kpmag,qtot2,Akk,tmpImg,tmpImg2,lossf(iw,1),lossf(iw,2),real(surf_i(iw,1)),real(surf_i(iw,2)),aimag(surf_i(iw,1)),aimag(surf_i(iw,2)),qpar(1),qpar(2)
!   write(23,107) hw(iw)*HARTREE, qpard_(iw), lossf(iw,1), lossf(iw,2), Akk, qparm,qtot2,qpar,kpmag
    write(24,107) hw(iw)*HARTREE, lossf(iw,1), lossf(iw,2), Akk, qparm,qtot2,qpar,kpmag
107 format(f8.3,20e16.7)
!--> DEBUG
  enddo

  qpard(1) = minval(qpard_(:))
  qpard(2) = maxval(qpard_(:))

  return
end subroutine CalcREELS


