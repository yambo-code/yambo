!
! Copyright (C) 2000-2008 C. Hogan  and the YAMBO team 
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
module eels_detector
  !
  ! Integration of detector
  !
  use pars,                   ONLY : schlen, SP, pi, lchlen
  use eels_kinematics

  integer                          :: det_int_typem, n_det_pt
  character(len=2)                 :: det_int_type  ! av(erage)/no(ne)/nu(merical)
  real(SP), allocatable            :: theta_det(:), phi_det(:), w_det(:)
  real(SP)                         :: thetadet
  real(SP)                  :: costhetad, sinthetad
  real(SP)                  :: cosphid, sinphid
  logical                   :: ldetector
  save
  private

  public :: thetadet, det_int_type, det_int_typem
  public :: init_detector
  public :: ldetector, n_det_pt, theta_det, phi_det
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
  call it(defs,'DetIntMd', '[REELS] Detector integration (`av`,`no`,`nu`)', det_int_type )
#if defined _YPP_RAS 
  call it(defs,'NumIntPt', '[REELS] Number of points for integral', n_det_pt )
#else
  call it(defs,'NumIntPt', '[REELS] Number of points for integral', n_det_pt, verb_level=V_more )
#endif
! check here...

  return
end subroutine init_detector

  subroutine setup_eels_det( lfail )
    use pars,                only  : radian
    implicit none
    logical, intent(out)           :: lfail

    lfail = .false.

    thetadet = thetadet*radian
    if(det_int_type.eq."av") n_det_pt = 1
    if(det_int_type.eq."no") n_det_pt = 1

    return
  end subroutine setup_eels_det

  subroutine check_eels_det(lfail)
    use com,   ONLY:msg
    implicit none
    real(SP), parameter    :: zero = 1.0e-4_SP
    character(lchlen)      :: errlist(10)
    integer                :: nerr, i
    logical, intent(out)   :: lfail
    ! 
    ! Perform various checks on the input angles.
    ! 
    lfail=.false.
    nerr = 0
    !
    ! 1 if av/nu then thetadet > 0 
    !
    if( (det_int_type.eq."av".or.det_int_type.eq."nu").and.abs(thetadet).lt.zero) then
       nerr = nerr + 1
       errlist(nerr) = 'Detector size (DetAngle) must be nonzero when'//&
&                      ' integrating over detector (DetIntMd='//det_int_type//')'
    endif
    !
    ! 2 if av then phi = 0 (planar only)
    !
    if( det_int_type.eq."av".and.abs(phi).gt.zero) then
       nerr = nerr + 1
       errlist(nerr) = 'Detector averaging (DetIntMd="av") only allowed with planar scattering.'
    endif
    !
    ! 3 if no then thetap > 0 (else kinematic -> inf!)
    !
    if( det_int_type.eq."no".and.abs(thetap).lt.zero) then
       nerr = nerr + 1
       errlist(nerr) = 'Deflection angle (Thetap) must be nonzero if detector size is ignored.'
    endif
    !
    ! Accounted for:
    ! if thetap = 0 and thetadet = 0 then fail (already accounted for)
    ! if thetap = 0 and nu and ndet = 1 then fail (kinematic -> inf)
    !
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
    use pars,  only : radian
    implicit none

    ldetector = .false.
    call msg('nr','Detector integration factors:')
    call msg('r','Detector   angle (deg) :',thetadet/radian )
    if(det_int_type.eq."av") call msg('r','Simple averaging over detector window.' )
    if(det_int_type.eq."no") call msg('r','No integration over detector window.' )
    if(det_int_type.eq."nu") then 
       call msg('r','Numerical integration over detector window.' )
       call msg('r','Number of integration points :',n_det_pt )
       ldetector = .true.
    endif
!   if(det_int_type.eq."an") then 
!      call msg('r','Analytical integration over detector window (prefactors only).' )
!      call msg('r','Circular geometry assumed.' )
!   endif
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
    use pars,                   only :  radian
    implicit none
    integer                         :: i, j, ii
    !
    ! Random integration parameters
    !
    integer                         :: n_rand
    real(SP)                        :: x,y,r,a,b, Rdet, phi_tmp, area
    
    allocate(theta_det(n_det_pt), phi_det(n_det_pt), w_det(n_det_pt) )
    !
    ! The default values
    !
    theta_det(1) = 0.0_SP
    phi_det(1)   = 0.0_SP
    w_det(1)     = 1.0_SP

    if(det_int_type.eq."av".or.det_int_type.eq."no") then
      !
      ! Set to detector *angular aperture* angle. 
      ! If "no", theta/phi det are not used.
      ! If "av", phi_det not used (integrated over).
      !
      theta_det(1) = thetadet  

    else if(det_int_type.eq."nu") then
      !
      ! Set up numerical integration points
      !
      call random_seed
      Rdet = kmag * sin(thetadet)
      area = pi*(kmag*sin(thetadet))**2
      n_rand = 0
      do while(.true.)
        call random_number(a)
        call random_number(b)
        x = -Rdet + 2.0*Rdet*a
        y = -Rdet + 2.0*Rdet*b
        r = sqrt(x**2 + y**2)
        if(r.gt.Rdet) cycle
        n_rand = n_rand + 1
        call cart_to_polar(x,y,phi_tmp)
        !
        ! |r| = |k'| sin(thetap)
        !
        theta_det(n_rand) = asin(r/kmag) ! [0:thetad]
        phi_det(n_rand)   = phi_tmp
        w_det(n_rand)     = 1.0_SP/real(n_det_pt)

!<-- DEBUG
!       write(30,100) n_rand, x,y,r, phi_det(n_rand)/radian, theta_det(n_rand)/radian
100 format(i4,2f7.3,3x,f6.3,f9.3,f9.3)
!<-- DEBUG
        if(n_rand.eq.n_det_pt) exit
      enddo

    endif
    return
  end subroutine setup_det

  subroutine cart_to_polar(x,y,phi)
    implicit none
    real(SP), intent(in) :: x,y ! radians!
    real(SP), intent(out) :: phi

    phi = atan(-x/y)
    if(phi.lt.0) phi = phi+2.0*pi
    if(y.ge.0) return
    if(x.ge.0) then
      phi = phi + pi
    else if(x.lt.0) then
      phi = phi - pi
    endif

    return
  end subroutine cart_to_polar

!=--------------------------------------------------------------------=
  real(SP) function qy_av(kpmag)
    !
    !   Used for calculating the average, in a specular scattering case
    !   Note: XZ is plane of incidence, not YZ (earlier codes, Del Sole paper)
    !   Check this is ok also for a PLANAR case. (should be)
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

    if(det_int_type=="av") then
      !
      ! Take single average value of Q_|| 
      !
      qpax = qx_av( kpmag )
      qpay = qy_av( kpmag )
      qpar = (/ qpax, qpay /)
      !
    else if(det_int_type=="no") then 
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
