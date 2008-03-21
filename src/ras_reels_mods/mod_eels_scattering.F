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
module eels_scattering
  ! 
  ! Setting up electron scattering off surface, prefactors, etc
  !
  use com,             ONLY : msg
  Use pars,            only : HARTREE, SP, PI
  use surface_geometry
  use eels_kinematics

  real(SP)                 :: xaxis(3), yaxis(3), zaxis(3)
  real(SP)                 :: vparmag, vzmag, vpar(3), vz(3)
  real(SP), allocatable    :: bpar_w(:),bqfac_w(:),qparm_w(:), kpmag_w(:)
  save
  private

  public :: xaxis,  bpar_w, qparm_w, bqfac_w
  public :: scattering_frame, electron_velocity, init_prefactors, end_prefactors
  public :: calc_pre_qpar, calc_prefactors, print_prefactors

contains

  subroutine scattering_frame( q0 )
    use vec_operate,      only : cross_product
    implicit none
    real(SP),  intent(in)    :: q0(3)
    real(SP)                 :: fac

    fac = sqrt(dot_product(q0,q0))

    xaxis(1) = q0(1)/fac
    xaxis(2) = q0(2)/fac
    xaxis(3) = q0(3)/fac

    ! Care...z axis is opposite to EELS frame.

    fac = sqrt(dot_product(norm,norm))
    zaxis(1) = -norm(1)/fac
    zaxis(2) = -norm(2)/fac
    zaxis(3) = -norm(3)/fac

    yaxis = cross_product(zaxis,xaxis)

    call msg('r','New EELS reference frame:')
    call msg('r','EELS X-axis: (v||) ',xaxis(1:3))
    call msg('r','EELS Y-axis:       ',yaxis(1:3))
    call msg('r','EELS Z-axis: (v_z) ',zaxis(1:3))

    return
  end subroutine scattering_frame

  subroutine electron_velocity
    implicit none
    real(SP)                     :: od(3)

    vparmag  = kmag*sintheta0
    vzmag    = kmag*costheta0
    vpar(1) = vparmag*xaxis(1)
    vpar(2) = vparmag*xaxis(2)
    vpar(3) = vparmag*xaxis(3)

    vz(1) = vzmag*zaxis(1)
    vz(2) = vzmag*zaxis(2)
    vz(3) = vzmag*zaxis(3)

    ! Electron is defined to be along x in EELS ref. frame.
    ! The vpar array contains the x and y components of the vector vpar.
    ! This is always positive.
    ! vz is z component of incident electron, which is
    ! also positive for a surface defined with (z-zsurf) > 0.

    od(1:3) = (/ kmag*sintheta0,0.0,0.0 /)
    call msg('nr','v_|| [EELS frame]  ',od(1:3))
    od(1:3) = (/ 0.0,0.0,kmag*costheta0 /)
    call msg('r','v_z  [EELS frame]  ',od(1:3))

    call msg('nr','v_|| [Xstal frame] ',vpar(1:3))
    call msg('r','v_z  [Xstal frame] ',vz(1:3))

    return
  end subroutine electron_velocity

  subroutine init_prefactors
    use eels_wgrid,    ONLY   : nwtot
    implicit none

    allocate( bpar_w(nwtot), bqfac_w(nwtot) )
    allocate( qparm_w(nwtot), kpmag_w(nwtot) )

    return
  end subroutine init_prefactors

  subroutine end_prefactors
    implicit none

    deallocate( bpar_w, bqfac_w )
    deallocate( qparm_w, kpmag_w )

    return
  end subroutine end_prefactors

  subroutine calc_pre_qpar( hw, bqfac, bq2, qparm, bpar )
    use eels_detector
    implicit none
    integer                  :: iw, iq
    real(SP), parameter      :: eps = 0.001
    real(SP)                 :: q(3), qz(3)
    real(SP), intent(in)     :: hw
    real(SP), intent(out)    :: qparm, bpar, bqfac, bq2
    real(SP)                 :: kpmag, qzm, qtot2, qpar(3), qtmp(3)

    kpmag = kpmagf( hw )                 !  <= | k'(hw) |
    !   Get q_par, q_z (select appropriate vector)
    if(ldetector) then
      qtmp(1) = q_x(kpmag)
      qtmp(2) = q_y(kpmag)
      qtmp(3) = q_z(kpmag)
    else
      qtmp(:) = q_det(kpmag)
    endif
    qpar(:) = qtmp(1)*xaxis(:) + &       !  <=  Q_par(r)  ! in Xstal frame
&             qtmp(2)*yaxis(:)
    qparm = sqrt(dot_product(qpar,qpar)) !  <= |Q_par|

    bpar = hw - &
&          dot_product(qpar,vpar)/vzmag !  <= |b_par|

    qz(:)   = qtmp(3)*zaxis(:)           !  <=  Q_z(r)
    qzm = sqrt(dot_product(qz,qz))       !  <= |Q_z| 


    if(qparm.lt.eps.and.bpar.lt.eps) then
!      Specular |q=0| case diverges...
       bq2 = 0.0_SP
    else
       bq2 = 1.0_SP/ ( bpar**2 + qparm**2 ) !  <=  1/( b^2 + q^2 )
    endif

    qtot2 = kmag**2 + kpmag**2 - &
&           2.0_SP*kmag*kpmag*costhetap !  <= |Q_z(r) + Q_par(r)|^2 ?

!--> DEBUG
!     bqfac = -8.d0*pi/vzmag**2*bq2        ! for cross section wrt qpar
!<-- DEBUG
    bqfac = -1.0_SP/pi/pi/costheta0*bq2  ! for cross section wrt solid angle

    return
  end subroutine calc_pre_qpar

  subroutine calc_prefactors
    !
    ! For a full set of hw with fixed kinematics (zero detector size)
    !
    use eels_wgrid,    ONLY   : hwgrid, nwtot
    implicit none
    integer                  :: iw, iq
    real(SP), parameter      :: eps = 0.001
    real(SP)                 :: q(3), qpar(3), qz(3)
    real(SP)                 :: hw, kpmag, qparm, bpar, qzm, bq2, qtot2, bqfac

    bqfac_w = 0.0; bpar_w = 0.0; qparm_w = 0.0; kpmag_w = 0.0
    do iw = 1, nwtot
      hw = hwgrid( iw )
      !
      ! Don't calculate prefactors outside the physical range.
      !
      if(hw.ge.E0.or.hw.lt.0.0) then 
        cycle
      endif

      kpmag = kpmagf( hw )                 !  <= | k'(hw) |

      qpar(:) = q_x(kpmag)*xaxis(:) + &
&               q_y(kpmag)*yaxis(:)        !  <=  Q_par(r)
      qparm = sqrt(dot_product(qpar,qpar)) !  <= |Q_par|

      bpar = hw - &
&             dot_product(qpar,vpar)/vzmag !  <= |b_par|

      qz(:)   = q_z( kpmag)*zaxis(:)       !  <=  Q_z(r)
      qzm = sqrt(dot_product(qz,qz))       !  <= |Q_z| 

      if(qparm.lt.eps.and.bpar.lt.eps) then
!        Specular |q=0| case diverges...
         bq2 = 0.0_SP
      else
         bq2 = 1.0_SP/ ( bpar**2 + qparm**2 ) !  <=  1/( b^2 + q^2 )
      endif

      qtot2 = kmag**2 + kpmag**2 - &
&              2.0_SP*kmag*kpmag*costhetap !  <= |Q_z(r) + Q_par(r)|^2 ?

!<-- DEBUG
!     bqfac = -8.d0*pi/vzmag**2*bq2        ! for cross section wrt qpar
!--> DEBUG
      bqfac = -1.0_SP/pi/pi/costheta0*bq2  ! for cross section wrt solid angle

!     Fill external arrays (suffix "_w")
      bqfac_w(iw) = bqfac
      bpar_w(iw)  = bpar
      qparm_w(iw) = qparm
      kpmag_w(iw) = kpmag
    enddo

    return
  end subroutine calc_prefactors

  subroutine print_prefactors
    use eels_wgrid,    ONLY   : hwgrid, nwtot
    implicit none
    integer                   :: iw

    ! Write case where specular (thetap = 0) and E >> hw,
    ! Q_par  = |k| hw sintheta0 / (2E0)

    open(unit=95,file="prefactors.dat")
    write(95,100) "# iw","hw","|k'|","|qpar|","bpar  ","bqfac"
    do iw = 1, nwtot
      write(95,101)  iw,hwgrid(iw)*HARTREE,kpmag_w(iw),qparm_w(iw),&
&     bpar_w(iw), bqfac_w(iw)
    enddo
    close(95)
    return
100 format(a4,a8  ,3a11,  a16)
101 format(i4,f8.3,3f11.5,f16.3)
  end subroutine print_prefactors

 
end module eels_scattering
