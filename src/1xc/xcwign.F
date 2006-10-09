!{\src2tex{textfont=tt}}
!!****f* ABINIT/xcwign
!! NAME
!! xcwign
!!
!! FUNCTION
!! Returns exc, vxc, and eventually d(vxc)/d($\rho$) from input $\rho$.
!! Wigner exchange and correlation (xc)--see e.g. David Pines,
!! Elementary Excitations in Solids, p. 94, NY 1964.
!! Expression is exc=-(0.44)/(rs+7.8)-efac/rs (hartree), efac below.
!! rs = $(3/(4\pi))^{1/3}* \rho (r)^{-1/3}$.
!!
!! COPYRIGHT
!! Copyright (C) 1998-2005 ABINIT group (DCA, XG, GMR)
!! This file is distributed under the terms of the
!! GNU General Public License, see ~ABINIT/Infos/copyright
!! or http://www.gnu.org/copyleft/gpl.txt .
!! For the initials of contributors, see ~ABINIT/Infos/contributors .
!!
!! INPUTS
!!  npt=number of real space points on which density is provided
!!  order=gives the maximal derivative of Exc computed.
!!  rhor(npt)=electron number density (bohr^-3)
!!  rspts(npt)=corresponding Wigner-Seitz radii, precomputed
!!
!! OUTPUT
!!  exc(npt)=exchange-correlation energy density (hartree)
!!  vxc(npt)=xc potential (d($\rho$*exc)/d($\rho$)) (hartree)
!!  if(order>1) dvxc(npt)=derivative d(vxc)/d($\rho$) (hartree*bohr^3)
!!
!! PARENTS
!!      drivexc
!!
!! CHILDREN
!!      leave_new,wrtout
!!
!! SOURCE

 subroutine xcwign(dvxc,exc,npt,order,rhor,rspts,vxc)

 use pars,            ONLY : DP,pi
 use xc_constants
 
 implicit none
 
!Arguments ------------------------------------
!scalars
 integer,intent(in) :: npt,order
!arrays
 real(DP),intent(in) :: rhor(npt),rspts(npt)
 real(DP),intent(out) :: dvxc(npt),exc(npt),vxc(npt)
 
!Local variables-------------------------------
!c1 and c2 are the Wigner parameters in hartree and bohr resp.
!scalars
 integer :: ipt
 real(DP),parameter :: c1=0.44_DP,c2=7.8_DP,c4_3=4.0_DP/3.0_DP,c8_27=8.0_DP/27.0_DP
 real(DP) :: dfac,efac,rs,rsc2m1,rsm1,vfac,vxcnum
 character(len=500) :: message
 
! *************************************************************************

!Checks the values of order
! if(order<0 .or. order>2)then
!  write(message, '(a,a,a,a,a,a,i3,a)' )ch10,&
!&  ' xcwign : BUG -',ch10,&
!&  '  With Wigner xc functional, the only',ch10,&
!&  '  allowed values for order are 0, 1 or 2, while it is found to be',&
!&       order,'.'
!  call wrtout(6,message,'COLL')
!  call leave_new('COLL')
! end if

!Compute vfac=(3/(2*Pi))^(2/3)
 vfac=(1.5_DP/pi)**(2.0_DP/3.0_DP)
!Compute efac=(3/4)*vfac
 efac=0.75_DP*vfac
!Compute dfac=(4*Pi/9)*vfac
 dfac=(4.0_DP*pi/9.0_DP)*vfac

!Loop over grid points
 do ipt=1,npt
  rs=rspts(ipt)
  rsm1=1.0_DP/rs
  rsc2m1=1.0_DP/(rs+c2)
! compute energy density (hartree)
  exc(ipt)=-c1*rsc2m1-efac*rsm1
  vxcnum=-(c4_3*rs+c2)*c1
! compute potential (hartree)
  vxc(ipt)=vxcnum*rsc2m1**2-vfac*rsm1
! compute d(vxc)/d(rho) (hartree*bohr^3)
  if(order>1)dvxc(ipt)=-(c8_27*pi)*(c1*rs**4)*(rs+rs+c2)*rsc2m1**3-dfac*rs**2
 end do
!
 end subroutine xcwign
!!***
