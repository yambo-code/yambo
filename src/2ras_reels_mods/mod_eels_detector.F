!
! Copyright (C) 2000-2005 C. Hogan  and the SELF team 
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
module eels_detector
  !
  ! Integration of detector
  !
  use pars,                   ONLY : schlen, SP, pi, lchlen
  use eels_kinematics

  integer                          :: detintm, ndet, ndetpt
  character(len=2)                 :: detint  ! av(erage)/no(ne)/fu(ll)
  real(SP), allocatable            :: theta_det(:), phi_det(:), w_det(:)
  real(SP)                         :: thetadet
  real(SP)                  :: costhetad, sinthetad
  real(SP)                  :: cosphid, sinphid
  logical                   :: ldetector
  save
  private

  public :: thetadet, detint, detintm
  public :: init_detector
  public :: ldetector, ndetpt, theta_det, ndet, phi_det
  public :: costhetad, w_det
  public :: setup_eels_det, print_eels_det, check_eels_det, setup_det
  public :: get_qpar_det, reset_trig_det, q_det

contains
 
subroutine init_detector(defs)
  use pars,                  ONLY : SP
  use it_m,                  ONLY : it, initdefs, E_unit,G_unit,T_unit, V_more
  implicit none
  type(initdefs), intent(inout)  :: defs

  call it(defs,'DetAngle', '[REELS] Detector window  (deg)', thetadet )
  call it(defs,'DetIntMd', '[REELS] Detector integration (`av`,`nu`,`no`,`an`)', detint )
  call it(defs,'NumIntPt', '[REELS] Number of points for integral', detintm, verb_level=V_more )

! check here...

  return
end subroutine init_detector

  subroutine setup_eels_det( lfail )
    implicit none
    logical, intent(out)           :: lfail

    lfail = .false.

    thetadet = thetadet*pi/180.0_SP
    ndet = (2*detintm)+1
    ndetpt = (ndet-1)**2 + 1
    if(detint.eq."av") ndetpt = 1
    if(detint.eq."no") ndetpt = 1

    return
  end subroutine setup_eels_det

  subroutine check_eels_det(lfail)
    use com,   ONLY:msg
    implicit none
    real(SP), parameter    :: zero = 1.0_SP
    character(lchlen)      :: errlist(10)
    integer                :: nerr, i
    logical, intent(out)   :: lfail
    ! 
    ! Perform various checks on the input angles.
    ! 
    lfail=.false.
    nerr = 0
    !
    ! 1 if av/pr/an then thetadet > 0 
    !
    if( (detint.eq."av".or.detint.eq."pr".or.&
&        detint.eq."an".or.detint.eq."fu").and.abs(thetadet).lt.zero) then
       nerr = nerr + 1
       errlist(nerr) = 'Detector size (DetAngle) must be nonzero when'//&
&                      ' integrating over detector (DetIntMd='//detint//')'
    endif
    !
    ! 2 if av then thetap = 0 (specular) - to be extended for general case
    !
    if( detint.eq."av".and.abs(thetap).gt.zero) then
       nerr = nerr + 1
       errlist(nerr) = 'Detector averaging (DetIntMd="av") only allowed with specular scattering.'
    endif
    !
    ! 3 if no then thetap > 0 (else kinematic -> inf!)
    !
    if( detint.eq."no".and.abs(thetap).lt.zero) then
       nerr = nerr + 1
       errlist(nerr) = 'Deflection angle (Thetap) must be nonzero if detector size is ignored.'
    endif
    !
    ! if thetap = 0 and thetadet = 0 then fail (already accounted for)
    !
    ! 4 if thetap = 0 and fu and ndet = 1 then fail (kinematic -> inf)
    !
    if( abs(thetap).lt.zero.and.detint.eq."fu".and.ndetpt.eq.1) then
       nerr = nerr + 1
       errlist(nerr) = 'More points needed (NumIntPt>1) for full integration with specular scattering.'
    endif
    if(nerr.ne.0) then
      lfail = .true.
 call msg('nrs','WARNING: Detector integration setup failed for the following reason(s):')
      do i=1,nerr
 call msg('rs',errlist(i))
      enddo
    endif

  end subroutine check_eels_det 
  
  subroutine print_eels_det
    use com,   ONLY:msg
    implicit none

    ldetector = .false.
    call msg('nr','Detector integration factors:')
    call msg('r','Detector   angle (deg) :',thetadet*180.0_SP/pi )
    if(detint.eq."av") call msg('r','Simple averaging over detector window.' )
    if(detint.eq."no") call msg('r','No integration over detector window.' )
    if(detint.eq."fu") then 
       call msg('r','Numerical integration over detector window.' )
       call msg('r','Number of integration shells :',detintm )
       ldetector = .true.
    endif
    if(detint.eq."pr") then 
       call msg('r','Numerical integration over detector window (prefactors only).' )
       call msg('r','Number of integration shells :',detintm )
       ldetector = .true.
    endif
    if(detint.eq."an") then 
       call msg('r','Analytical integration over detector window (prefactors only).' )
       call msg('r','Circular geometry assumed.' )
    endif
    return
  end subroutine print_eels_det

    function q_det(kpmag)
      !
      !   For numerical integration across detector, wiSPh thetad, azimuth phid
      !
      real(SP)             :: q_det(3)
      real(SP),intent(in)  :: kpmag
      real(SP)             :: fac1, fac2, fac3, qtmp(3)

      
      fac1 = costhetap*cosphid*sinthetad + costhetad*sinthetap
      fac2 =           sinphid*sinthetad
      fac3 = sinthetap*cosphid*sinthetad - costhetad*costhetap
      write(*,*) fac1,fac2,fac3

! The cosphi is not defined anywhere -> need to fix
!     qtmp(1) = costheta0*cosphi*fac1 + costheta0*sinphi*fac2 - sintheta0*fac3
!     qtmp(2) =          -sinphi*fac1 +           cosphi*fac2
!     qtmp(3) = sintheta0*cosphi*fac1 + sintheta0*sinphi*fac2 + costheta0*fac3

      q_det(1) =   kmag * sintheta0 - &
&                kpmag * qtmp(1)
      q_det(2) = -kpmag * qtmp(2)
      q_det(3) =  -kmag * costheta0 - &
&                kpmag * qtmp(3)

    end function q_det

!=--------------------------------------------------------------------=

  subroutine reset_trig_det( thetad, phid )
    implicit none
    real(SP), intent(in)      :: thetad, phid
    costhetad = cos(thetad)
    sinthetad = sin(thetad)
    cosphid   = cos(phid)
    sinphid   = sin(phid)
    return
  end subroutine reset_trig_det

  subroutine setup_det
    implicit none
    integer                         :: i, j, ii
    
    allocate(theta_det(ndetpt), phi_det(ndetpt), w_det(ndetpt) )
    if(detint.eq."av".or.detint.eq."no") then
      !
      ! Set to detector *angular aperture* angle. 
      ! If "no", theta/phi det are not used.
      ! If "av", phi_det not used (integrated over).
      !
      theta_det(1) = thetadet  
      phi_det(1)   = 0.0_SP 
      w_det(1)     = 1.0_SP
    else if(detint.eq."fu".or.detint.eq."pr") then
      !
      ! Set up numerical integration points
      !
      if(ndetpt.ne.1) then
        ii = 0
        do i = 1, ndet
        do j = 1, ndet-1
          ii = ii + 1
          theta_det(ii) = -thetadet+(2.0_SP*thetadet)/real(ndet-1)*real(i-1) 
          phi_det(ii)    = 0.0_SP+pi/real(ndet-1)*real(j-1) ! phid [0:pi]
          w_det(ii) = 1.0_SP/real(ndetpt)
!--> DEBUG
!         write(*,100) ii,i,j,theta_det(ii)*180.d0/pi,phi_det(ii)*180.d0/pi
!100 format(3i3,f8.3,f8.3)
!<-- DEBUG
          if(i.eq.(ndet+1)/2) exit
        enddo
        enddo
      else
        theta_det(1) = 0.0_SP
        phi_det(1) = 0.0_SP
      endif
    else if(detint.eq."no") then
      !
      ! Set up numerical integration points
      !
      theta_det(1) = 0.0_SP
      phi_det(1) = 0.0_SP
    endif
    return
  end subroutine setup_det

!=--------------------------------------------------------------------=
  real(SP) function qy_av(kpmag)
    !
    !   Used for calculating the average, in a specular scattering case
    !   Note: XZ is plane of incidence, not YZ (earlier codes, Del Sole paper)
    !
    real(SP),intent(in)  :: kpmag
    real(SP)             :: qy2

    qy2 =  0.5_SP * (kpmag**2) * (sinthetad**2)
    qy_av = sqrt(qy2)

  end function qy_av

  real(SP) function qx_av(kpmag)
    real(SP),intent(in)  :: kpmag
    real(SP)             :: qx2

    qx2 =  (kmag**2) * (sintheta0**2) &
&        + 0.5_SP * (kpmag**2) * (sinthetad**2) * (costheta0**2) &
&        +          (kpmag**2) * (costhetad**2) * (sintheta0**2) &
&        - 2.0_SP * kmag * kpmag * (sintheta0**2) * costhetad
    qx_av = sqrt(qx2)

  end function qx_av

  subroutine get_qpar_det( qpar, kpmag, idet )
    implicit none
    integer, intent(in), optional      :: idet
    real(SP), intent(in)               :: kpmag
    real(SP), intent(out)              :: qpar(2)
    real(SP)                           :: qpax, qpay, qtmp(3), phid, thetad

    if(detint=="av") then
      !
      ! Take single average value of Q_|| 
      !
      qpax = qx_av( kpmag )
      qpay = qy_av( kpmag )
      qpar = (/ qpax, qpay /)
      !
    else if(detint=="no") then 
      !
      ! Select single exact Q_|| inside detector 
      ! 
      qpax = q_x(kpmag)
      qpay = q_y(kpmag)
      qpar = (/ qpax, qpay /)
      ! 
    else ! fu
      !
      ! Select {Q_||} values inside detector window
      !
      if(lplanar) then
        phi    = phi_det(idet)
        thetap = theta_det(idet)
        call reset_trig
        qpax = q_x(kpmag)
        qpay = q_y(kpmag)
        qpar = (/ qpax, qpay /)
      else
        phid   = phi_det(idet)    
        thetad = theta_det(idet)
        call reset_trig_det( thetad, phid )
        qtmp(:) = q_det(kpmag)
        qpar = (/ qtmp(1), qtmp(2) /)
      endif
      !
    endif
    return
  end subroutine get_qpar_det

end module eels_detector
