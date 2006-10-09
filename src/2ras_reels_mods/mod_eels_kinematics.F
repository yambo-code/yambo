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
module eels_kinematics

  use pars,                   ONLY : schlen, SP, pi, HARTREE

  real(SP)                  :: theta0_in, thetap_in, thetas_in, phi_in
  real(SP)                  :: theta0, thetap, thetas, phi
  real(SP)                  :: costheta0, sintheta0
! real(SP)                  :: costhetas, sinthetas
  real(SP)                  :: costhetap, sinthetap
  real(SP)                  :: cosphi, sinphi
  real(SP)                  :: E0, kmag
  real(SP)                  :: bimp ! impact factor in 3 layer case
  ! Depend just on the angles
  logical                  :: lplanar, lspecular, lparallel, lgrazing 
! logical                  :: lfixed

  save
  private

  public :: init_kinematics
  public :: print_eels_impact, print_eels_kin, setup_eels_kin, print_eels_geometry
  public :: q_x, q_y, q_z, qy_av_, qx_av_, hwq, kpmagf
  public :: thetap, lplanar, phi, lparallel, bimp
  public :: costheta0, sintheta0, costhetap, sinthetap, kmag, reset_trig
  public :: E0, theta0_in, thetap_in, phi_in

contains

subroutine init_kinematics(defs) ! only those common to RAS and REELS runlevels
  use pars,                  ONLY : SP, HARTREE
  use it_m,                  ONLY : it, initdefs, E_unit,G_unit,T_unit, V_more
  implicit none
  type(initdefs), intent(inout)  :: defs

  call it(defs,'E0'      , '[REELS] Incident energy', E0, E_unit )
  call it(defs,'Theta0'  , '[REELS] Incident angle   (deg)', theta0_in )
  call it(defs,'Thetap'  , '[REELS] Deflection angle (deg)', thetap_in )
  call it(defs,'Phi'     , '[REELS] Azimuthal angle  (deg)', phi_in, verb_level=V_more )
  call it(defs,'ImpactFt', '[REELS] Impact Factor (a.u.)', bimp,verb_level=V_more )

  return
end subroutine init_kinematics

  subroutine print_eels_kin
    use com,               ONLY : msg
    implicit none
    real(SP)                   :: fac = 180.0_SP/pi

    call msg('nr','Kinematic settings for EELS calculation:')
    call msg('r','Incident energy (eV) :',E0*HARTREE )
    call msg('r','Incident   angle (deg) :',theta0*fac )
    call msg('r','Deflection angle (deg) :',thetap*fac )
    call msg('r','Scattering angle (deg) :',thetas*fac )
    call msg('r','Azimuthal  angle (deg) :',phi*fac )
    return
  end subroutine print_eels_kin
 
  subroutine setup_eels_kin( lfail )
    implicit none
    logical, intent(out)      :: lfail
    real(SP)           :: rad
    rad = pi/180.0_SP

    lfail = .false.

    thetas   = theta0 + thetap

    lplanar = .false.
    if(abs(phi).le.0.001) lplanar = .true.
    lparallel = .false.
    if(abs(theta0-90.0_SP).le.0.001) lparallel = .true.
    lgrazing = .false.
    if(theta0.gt.80.0_SP) lgrazing = .true.
    lspecular = .false.
    if(abs(thetap).le.0.0001) lspecular = .true.
!   Balzarotti fixed angle (theta0 + thetas = 90) (not used)
!   lfixed = .false.
!   if(abs(thetas + theta0 - 90.0_SP).le.0.0001) lfixed = .true.

    E0       = E0/HARTREE        ! E0(eV) -> E0(au)
    kmag     = sqrt(2.0_SP*E0) ! kmag   -> au
    thetas    = thetas*rad
    theta0    = theta0*rad
    thetap    = thetap*rad
    phi       = phi*rad

    thetas_in = thetas
    theta0_in = theta0
    thetap_in = thetap
    phi_in    = phi

    sintheta0 = sin(theta0)
    costheta0 = cos(theta0)

    call reset_trig ! 
  
    return
  end subroutine setup_eels_kin
  
  subroutine print_eels_geometry
    use com,   ONLY:msg
    implicit none
      call msg('nrs','REELS scattering geometry:')
      if(lparallel) then
        call msg('rs','... Non reflecting electron scattering setup (Batson/Howie/Kociak theory).')
        call msg('rs','... Impact factor (a.u.) :',bimp )
      else
        call msg('nrs','... Regular electron scattering setup (Mills'' theory).')
        if(lspecular) call msg('rs','... Specular scattering.')
        if(.not.lspecular.and.lplanar) call msg('rs','... Planar scattering.')
        if(.not.lspecular.and..not.lplanar) call msg('rs','... Non-planar scattering.')
        if(lgrazing) call msg('nrs','... Warning: approaching grazing incidence! Theory breaking down....')
      endif
    return
  end subroutine print_eels_geometry

  subroutine print_eels_impact
    use com,   ONLY:msg
    implicit none

    if(lparallel) then
      call msg('nrs','Non reflecting electron scattering setup (Batson/Howie/Kociak theory).')
      call msg('rs','Impact factor (a.u.) :',bimp )
    else
      call msg('nrs','Regular electron scattering setup (Mills'' theory).')
    endif
    if(lgrazing) call msg('nrs','Warning: incident angle > 80 deg! Theory breaking down....')
    return
  end subroutine print_eels_impact


  subroutine reset_trig
    !
    ! Detector/integration scheme-dependent kinematic factors
    !
    implicit none
    sintheta0 = sin(theta0)
    costheta0 = cos(theta0)
    sinthetap = sin(thetap)
    costhetap = cos(thetap)
!   sinthetas = sin(thetas)
!   costhetas = cos(thetas)
    sinphi    = sin(phi)
    cosphi    = cos(phi)
    return
  end subroutine reset_trig
  subroutine reset_angles
    !
    ! Reset kinematic set up to input values
    !
    implicit none
    thetap = thetap_in
    theta0 = theta0_in
    thetas = thetas_in
    phi    = phi_in
    return
  end subroutine reset_angles
  
  !
  ! Transferred momenta: For z > 0 defining the slab
  ! q =           k^s                -    k'
  !                     |k'| = |k| sqrt(1-hw/E0) 
  !
  ! qx    [ sintheta0 ]     [ sinthetap x cosphi x costheta0 + costhetap x sintheta0 ]
  ! qy=|k|[     0     ]-|k'|[ sinthetap x sinphi                                     ]
  ! qz    [-costheta0 ]     [ sinthetap x cosphi x sintheta0 - costhetap x costheta0 ]
  !
  real(SP) function q_z(kpmag)
    real(SP),intent(in)  :: kpmag

    q_z = -kmag * costheta0 - &
&        kpmag * (sinthetap*cosphi*sintheta0 - costhetap*costheta0)
  end function q_z
 
  real(SP) function q_y(kpmag)
    real(SP),intent(in)  :: kpmag
       
    q_y = -kpmag * (sinthetap*sinphi)
       
  end function q_y
 
  real(SP) function q_x(kpmag)
    real(SP),intent(in)  :: kpmag

    q_x =  kmag * sintheta0 - &
&        kpmag * (sinthetap*cosphi*costheta0 + costhetap*sintheta0)

  end function q_x

  !
  !   Used for calculating the average, in a specular scattering case
  !   Note: XZ is plane of incidence, not YZ (earlier codes, Del Sole paper)
  !
  real(SP) function qy_av_(kpmag)
    real(SP),intent(in)  :: kpmag
    real(SP)             :: qy2

    qy2 =  0.5_SP * (kpmag**2) * (sinthetap**2)
    qy_av_ = sqrt(qy2)

  end function qy_av_

  real(SP) function qx_av_(kpmag)
    real(SP),intent(in)  :: kpmag
    real(SP)             :: qx2

    qx2 =  (kmag**2) * (sintheta0**2) &
&        + 0.5_SP * (kpmag**2) * (sinthetap**2) * (costheta0**2) &
&        +          (kpmag**2) * (costhetap**2) * (sintheta0**2) &
&        - 2.0_SP * kmag * kpmag * (sintheta0**2) * costhetap
    qx_av_ = sqrt(qx2)

  end function qx_av_
 
  real(SP) function hwq(q_x)
    real(SP),intent(in) :: q_x
    real(SP)            :: num, den

    num = sintheta0 - q_x/kmag
    den = (sinthetap*cosphi*costheta0 + costhetap*sintheta0)
    hwq = E0 - E0*(num/den)**2

  end function hwq
 
  real(SP) function kpmagf( hw )
    real(SP),intent(in) :: hw
 
    kpmagf = kmag * sqrt(1.0_SP - hw/E0) 
  
  end function kpmagf

end module eels_kinematics
