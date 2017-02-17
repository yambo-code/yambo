!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CH
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
  use pars,                 ONLY : schlen, SP,PI
  use units,                ONLY : HA2EV
  use surface_geometry
  use eels_kinematics
  use eels_detector
  use model_loss_function
  use LIVE_t,              ONLY:live_timing
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
  integer                  :: iw, idet, pb=13
  real(SP)                 :: costakk, kpmag, qpard_(nw)
  real(SP)                 :: qparm, qtot2, qpar(2)
  real(SP)                 :: Akk, loss_qpar(2)
  real(SP)                 :: Img_x, Img_y 
  complex(SP)              :: eps_s(3)
  !
  ! Energy independent stuff. Other input in calling routine
  !
  call setup_det
  !
  costakk=2.0_SP/PI/PI/costheta0

  call live_timing('Energy loss:',nw, SERIAL=.true.)
  do iw = 1, nw
    kpmag = kpmagf( hw(iw) )

    loss_qpar(1) = 0.0_SP
    loss_qpar(2) = 0.0_SP
    !
    ! Integrate scattering cross section over detector window
    !
    do idet = 1, n_det_pt
      
      !
      ! Reset trig factors for this detector setting
      !
      call reset_trig_det( theta_det(idet), phi_det(idet) )
      !
      ! Get Q_|| for this set of angles.
      !
      call get_qpar_det(qpar, kpmag, idet )
      !
      ! Kinematic prefactor
      !
      qparm  = sqrt( qpar(1)**2 + qpar(2)**2 )
      if(idet.eq.1) qpard_(iw) = qparm*d_surf
      qtot2 = kmag**2 + kpmag**2 - 2.0_SP * kmag * kpmag * costhetad
      if(abs(costhetad).gt.0.999999) then
         qtot2 = kmag**2 + kpmag**2 - 2.0_SP * kmag * kpmag * 0.999999 ! fpz
      endif
      Akk = costakk*(kpmag/kmag)*(qparm/qtot2**2) 

      !
      ! Parallel case 
      !
      if(lparallel) Akk = 2.0_SP/PI/kmag**2  
      !
      ! Loss function for q || q_x
      !
      eps_s(:) = (/ surf_i(iw,1), surf_i(iw,2), surf_i(iw,3) /)
      Img_x = model_Img(lossfm, eps_b(iw),qpar=qpar, esurf = eps_s, dsurf = d_surf, dvac = bimp)
      if(pendepth.gt.0.001) Img_x = Img_x + model_Img(pb, eps_b(iw), qpar=qpar, pendepth=pendepth)
      loss_qpar(1) = loss_qpar(1) + Akk * Img_x * w_det(idet)
      !
      ! Loss function for q || q_y
      !
      eps_s(:) = (/ surf_i(iw,2), surf_i(iw,1), surf_i(iw,3) /)
      Img_y = model_Img(lossfm,eps_b(iw), qpar=qpar, esurf = eps_s, dsurf = d_surf, dvac = bimp)
      if(pendepth.gt.0.001) Img_y = Img_y + model_Img(pb, eps_b(iw), qpar=qpar, pendepth=pendepth)
      loss_qpar(2) = loss_qpar(2) + Akk * Img_y * w_det(idet)
      !
    enddo

    lossf(iw,:) = loss_qpar(:) ! other factors?

!DEBUG>
!    if(iw.eq.1) write(24,108) "####hw  ","lossx","lossy","Akk","|qpar|","|Q|^2","Q_||(x)","Q_||(y)",& ! DEBUG
!&   "|k'|","Img_x","Img_y"   ! DEBUG
!    write(24,107) hw(iw)*HARTREE, lossf(iw,1), lossf(iw,2), Akk, qparm,qtot2,qpar,kpmag,Img_x,Img_y ! DEBUG
!DEBUG<
107 format(f8.3,20e16.7)
108 format(a8,20a16)
    call live_timing(steps=1)
  enddo
  call live_timing

  qpard(1) = minval(qpard_(:))
  qpard(2) = maxval(qpard_(:))

  call eels_report
  return

contains

  subroutine eels_report
    use com,                    ONLY : msg
    call msg('nrs',"REELS set up report")
    call msg('rs',"|qpar| x d_surf =",qpard)
    call msg('rs',"d_surf =",d_surf)
    call msg('rs',"Loss function type ="//lossformlg(lossfm) )
    call print_eels_kin('s')
    call print_eels_det('s')
    return
  end subroutine eels_report

end subroutine CalcREELS


