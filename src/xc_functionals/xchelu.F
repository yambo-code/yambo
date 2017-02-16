!{\src2tex{textfont=tt}}
!!****f* ABINIT/xchelu
!! NAME
!! xchelu
!!
!! FUNCTION
!! Returns exc, vxc, and eventually d(vxc)/d($\rho$) from input rho.
!!
!! NOTES
!! Hedin-Lundqvist exchange and correlation (xc)--
!! L. Hedin and B.I. Lundqvist, J. Phys. C. 4, 2064 (1971).
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
!!  rspts(npt)=Wigner-Seitz radii at each point
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

 subroutine xchelu(dvxc,exc,npt,order,rspts,vxc)

 use pars,            ONLY : DP,pi
 use xc_constants
 
 implicit none
 
!Arguments ------------------------------------
!scalars
 integer,intent(in) :: npt,order
!arrays
 real(DP),intent(in) :: rspts(npt)
 real(DP),intent(out) :: dvxc(npt),exc(npt),vxc(npt)
 
!Local variables-------------------------------
!aa and cc are H-L fitting parameters A and C (C in hartree)
!rs = (3/(4 Pi))**(1/3) * rho(r)**(-1/3).
!scalars
 integer :: ipt
 real(DP),parameter :: aa=21.0_DP,c1_21=1.0_DP/21.0_DP,c4_9=4.0_DP/9.0_DP
 real(DP),parameter :: cc=0.0225_DP
 real(DP) :: dfac,efac,rs,rsm1,vfac,xx
 character(len=500) :: message
 
! *************************************************************************

!!Checks the values of order
! if(order<0 .or. order>2)then
!  write(message, '(a,a,a,a,a,a,i3,a)' )ch10,&
!&  ' xchelu : BUG -',ch10,&
!&  '  With Hedin-Lundqvist xc functional, the only',ch10,&
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
! compute energy density exc (hartree)
  xx=rs*c1_21
  exc(ipt)=-cc*((1._DP+xx**3)*log(1._DP+1._DP/xx)+&
&  0.5_DP*xx-xx*xx-third) - efac*rsm1
! compute xc potential d(rho*exc)/d(rho) (hartree)
  vxc(ipt)=-cc*log(1._DP+aa*rsm1)-vfac*rsm1
! compute d(vxc)/d(rho) (hartree*bohr^3)
  if(order>1) dvxc(ipt)=-(rs**2)*((c4_9*pi)*cc*rs/(1._DP+xx) + dfac)
 end do
!
 end subroutine xchelu
!!***
