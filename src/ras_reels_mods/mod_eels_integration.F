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
module eels_integration
  !
  ! Integration of epsilon-1
  !
  use pars,             only : schlen, SP, pi

  real(SP)                  :: dc, dv, dsurf
  real(SP)                       :: limits(3)
  real(SP), parameter       :: zero = 1.0e-5_SP
  character(schlen)              :: inttype
  logical                   :: lanalytical
  save
  private

  public :: init_reels_integration
  public :: print_eels_int, calc_Img_qpar, lanalytical
  public :: calc_loss_analytical, calc_zk
 
  public :: dc, dv, dsurf ! only for loss_bulk.F
  public :: limits, inttype

contains

subroutine init_reels_integration(defs)
  use it_m,                  only : it, initdefs, E_unit,G_unit,T_unit
  implicit none
  type(initdefs), intent(inout)  :: defs

  call it(defs,'EELIntMd', &
&      '[REELS] EELS integration method (`analytical`,`numerical`)', inttype )
  call it(defs,'IntLimit', '[REELS] Integration limits vac/surf/slab (a.u.)', limits )

  lanalytical = merge (.true., .false., trim(inttype).ne."numerical")
  dv       = limits(1)
  dsurf    = limits(2)
  dc       = limits(3)

  return
end subroutine init_reels_integration

  subroutine print_eels_int
    use com,   only:msg
    implicit none
    real(SP)    :: fac = 180.0_SP/pi

    if(lanalytical) &
&     call msg('nr','Analytical integration of inverse epsilon.')
    if(.not.lanalytical) &
&     call msg('nr','Numerical integration of inverse epsilon.')
    call msg('r','Integration limits:')
    call msg('r','Vacuum  limit (a.u.) :',dv )
    call msg('r','Surface limit (a.u.) :',dsurf )
    call msg('r','Slab    limit (a.u.) :',dc )
    return
  end subroutine print_eels_int

  complex(SP) function Hzintd(z0,a,b,d1,d2)
    !
    !
    !
    real(SP)           :: b,d1,d2, z0 
    complex(SP)        :: a,ctmp0,ctmp1,ctmp2
    !
    ! Filter case where q_z + G_z = 0
    !
    if(abs(a).lt.zero) then
       Hzintd = cmplx( b*( (d2-z0) - (d1-z0) ) , 0.0_SP)
       return
    endif

    ctmp0 = 1.d0/(b**2 + a**2)
    ctmp1 = exp(a*d1)*(a*cos(b*(d1-z0)) + b*sin(b*(d1-z0)))
    ctmp2 = exp(a*d2)*(a*cos(b*(d2-z0)) + b*sin(b*(d2-z0)))
    Hzintd = (ctmp2-ctmp1)*ctmp0
    return
  end function Hzintd

  complex(SP) function Fzintd(z0,a,b,d1,d2)
    real b,d1,d2, z0
    complex a,ctmp0,ctmp1,ctmp2
    !
    ! Filter case where ctmp0 = 0
    !
    if(abs(a+b).lt.zero) then
       Fzintd = cmplx( 0.0_SP , 0.0_SP)
       return
    endif

    ctmp0 = (b + a)
    ctmp1 = exp(ctmp0*d2 - b*z0) -  exp(ctmp0*d1 - b*z0)
    Fzintd = ctmp1/ctmp0

  end function Fzintd

  subroutine calc_loss_numerical
  end subroutine calc_loss_numerical
 
  subroutine calc_loss_analytical(lossqw, iq, epsm1)
    !
    ! Calculate loss spectrum for this polarization and iqpar
    !
    use eels_scattering
    use eels_gvec,              only : iGz0indx
    use surface_geometry,       only : iz
    use R_lattice,              only : g_vec
    use eels_wgrid,             only : wqtab
    implicit none
    complex(SP), intent(in)         :: epsm1(:,:,:) 
    real(SP), intent(out)           :: lossqw(:,:) ! different components
    integer, intent(in)             :: iq
!
    integer                         :: ig, ig_1, iw, ngblk, nwsec, iwtot
    complex(SP)                     :: iq0Gz(size(epsm1,1)) ! ngblk
    real(SP)                        :: g1(3), q0z, lossq(4)
    complex(SP), parameter          :: ci = cmplx(0.0_SP, 1.0_SP)

    ngblk = size(epsm1,1)
    nwsec = size(epsm1,3)

! What is nw here for the epsmix cases?

    if(nwsec.ne.size(lossqw,2)) then
       write(*,*) "ERROR: inconsitenta arrays loss analytical ",nwsec,size(lossqw,2)
    endif
    !
    !  Energy independent arrays
    !
    q0z = 0.0_SP ! not used for now
    do ig = 1, ngblk
      ig_1  = iGz0indx(ig) ! the real index of G
      g1(:) = g_vec(ig_1,:) 
      iq0Gz(ig) = ci * ( g1(iz) + q0z ) 
    enddo
    !
    !  Loop over energy: iw is on nwsec mesh; iwtot is on nwtot mesh for prefactors
    !
    do iw = 1, nwsec
      iwtot = wqtab(iw,iq)
      call integrate_loss( lossq, iwtot, iq0Gz, epsm1(1:ngblk,1:ngblk,iw), ngblk )
      lossqw(1:4,iw) = lossq(1:4)
    enddo

    return
  end subroutine calc_loss_analytical

  subroutine integrate_loss( lossq, iw, iq0Gz, epsm1_w, ngblk )
    !
    ! Calculates the loss for a single energy (and iq), including
    ! detector integration
    !
    use eels_scattering,        only : qparm_w, bpar_w, bqfac_w
    use eels_detector
    use eels_wgrid,             only : hwgrid
    implicit none
    integer,     intent(in)         :: ngblk, iw
    complex(SP), intent(in)         :: iq0Gz(ngblk), epsm1_w(ngblk,ngblk)
    real(SP),    intent(out)        :: lossq(4)
!   ws
    integer                         :: idet
    real(SP)                        :: lossg(3), hw
    real(SP)                        :: bpar, qparm, bqfac, bq2
    real(SP)                        :: phid, thetad




    !   Select detector integration

    if(detint.eq."no") then      ! exact

      qparm = qparm_w(iw)      
      bpar  = bpar_w(iw)      
      call calc_Img_qpar( lossg, iq0Gz, qparm, bpar, epsm1_w, ngblk )

    else if(detint.eq."av") then ! average

      qparm = qparm_w(iw)      
      bpar  = bpar_w(iw)      
      call calc_Img_qpar( lossg, iq0Gz, qparm, bpar, epsm1_w, ngblk )

!DISABLED
!   else if(detint.eq."pr") then ! exact + integration over kinematic factor?
! Integration over kinematic factor could be numerical or analytical

!     qparm = qparm_w(iw)      
!     bpar  = bpar_w(iw)      
!     call calc_Img_qpar( lossg, iq0Gz, qparm, bpar, epsm1_w, ngblk )
!     do idet = 1, ndet 
!       phid   = phi_det(idet)
!       thetad = theta_det(idet)
!     enddo

!DISABLED
!   else if(detint.eq."fu") then ! full numerical integration

!     ! Numerically integrate over spherical surface region
!     do idet = 1, ndet 
!       phid   = phi_det(idet)
!       thetad = theta_det(idet)
!       !
!       call reset_trig_det( thetad, phid ) ! get cos/sin for thetad/phid
!       !
!       hw = hwgrid(iw)
!       !
!       call calc_pre_qpar( hw, bqfac, bq2, qparm, bpar )
!       !
!       ! Call the bulk here on a requested grid.
!
!       !
!       call calc_Img_qpar( lossg, iq0Gz, qparm, bpar, epsm1_w, ngblk )
!     enddo

    else 
      lossg = 0
    endif
    !
    ! Since want small possibility in future to integrate prefactors over detector,
    ! (even just analytically), include the prefactors here.
    ! qparm_w and bqfac should be on same energy mesh.
    !
    lossq(1:3) = lossg(1:3) * bqfac_w(iw)
    lossq(4)   = lossg(1)
!<-- DEBUG 
!   loss(:,1) = loss(:,1) + aimag( bqfac_w(:) * eHint(:)*eFint(:)/az ) ! Total 
!   loss(:,2) = loss(:,2) + aimag( bqfac_w(:) * eHint(:)*eFint(:)/az )! No bulk
!   loss(:,3) = loss(:,3) + aimag(             eHint(:)*eFint(:)/az  )! No kin
!   loss(:,4) = loss(:,4) + aimag(             eHint(:)/az           ) ! eHint
!   loss(:,5) = loss(:,5) + aimag(                       eFint(:)/az )! eFint
!--> DEBUG 
    return
  end subroutine integrate_loss
 
  subroutine calc_Img_qpar(lossg, iq0Gz, qparm, bpar, epsm1, ngblk )
    implicit none
    integer,     intent(in)         :: ngblk
    complex(SP), intent(in)         :: epsm1(ngblk,ngblk) ! order?
    real(SP),    intent(in)         :: bpar, qparm
    complex(SP), intent(in)         :: iq0Gz(ngblk)
    real(SP),    intent(out)        :: lossg(3) ! different components
!
    integer                         :: ig1, ig2
    complex(SP)                     :: iq0Gz1, iq0Gz2
    complex(SP)                     :: eHint, eFint
    real(SP)                        :: az = 1.0 ! THIS IS A QUICK FIX...
 

    write(*,*) 
    write(*,*)  "Check the integration"
    write(*,*) 
    write(*,*) ngblk,dc, dsurf,dv
    write(*,*) qparm, bpar

    lossg(:) = 0.0_SP
    do ig1 = 1, ngblk ! loop over G = Gz

      iq0Gz1 = iq0Gz(ig1)

      eHint = Hzintd(dsurf, iq0Gz1, bpar, dv, dsurf)

!     Sum over G' = G'z
      eFint = cmplx(0.0_SP,0.0_SP)
      do ig2 = 1, ngblk
        iq0Gz2 = iq0Gz(ig2)
        eFint = eFint + epsm1(ig1,ig2) *  &
&                 ( Fzintd(dsurf, -iq0Gz2, -qparm, dsurf, dc) - &
&                   Fzintd(dsurf, -iq0Gz2, +qparm, dv, dsurf) + &
&          2.0_SP * Hzintd(dsurf, -iq0Gz2,  bpar,  dv, dsurf) )
!<-- DEBUG
!! Testing: eps part only
!!               eFint(iw) = eFint(iw) + epsm1(ig1,ig2,iw)
!! Testing: integral part only
!!                eFint(iw) = eFint(iw) + (1.d0,0.d0) *  &
!--> DEBUG
      enddo
    write(*,*) "iG",ig1,eHint,eFint

      lossg(1) = lossg(1) + aimag( eHint*eFint/az ) ! Total 
      lossg(2) = lossg(2) + aimag( eHint/az       ) ! eHint 
!     lossg(2) = lossg(2) + real( eHint/az       ) ! eHint is real if G=0
      lossg(3) = lossg(3) + aimag(       eFint/az ) ! eFint

    enddo ! end loop over G
    write(*,*) 
    return
  end subroutine calc_Img_qpar

  subroutine calc_zk
  end subroutine calc_zk
  subroutine intsetup
  end subroutine intsetup

end module eels_integration
