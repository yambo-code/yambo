!{\src2tex{textfont=tt}}
!!****f* ABINIT/xcpzca
!! NAME
!! xcpzca
!!
!! FUNCTION
!! Returns exc, vxc, and d(vxc)/d($\rho$) from input rho.
!!
!! NOTE
!! Perdew-Zunger parameterization of Ceperly-Alder electron gas
!! energy data--
!! J. Perdew and A. Zunger, Phys. Rev. B 23, 5048 (1981).
!! D.M. Ceperly and B.J. Alder, Phys. Rev. Lett. 45, 566 (1980).
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

 subroutine xcpzca(dvxc,exc,npt,order,rhor,rspts,vxc)

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
!Perdew-Zunger parameters a, b, b1, b2, c, d, gamma
!scalars
 integer :: ipt
 real(DP),parameter :: aa=0.0311_DP,b1=1.0529_DP,b2=0.3334_DP,bb=-0.048_DP
 real(DP),parameter :: c4_3=4.0_DP/3.0_DP,c7_6=7.0_DP/6.0_DP,cc=0.0020_DP
 real(DP),parameter :: dd=-0.0116_DP,ga=-0.1423_DP
 real(DP) :: den,den3,dfac,efac,logrs,rs,rsm1,t1,t2,vfac
 character(len=500) :: message
 
! *************************************************************************

!Compute vfac=(3/(2*Pi))^(2/3)
 vfac=(1.5_DP/pi)**(2.0_DP/3.0_DP)
!Compute efac=(3/4)*vfac
 efac=0.75_DP*vfac
!Compute dfac=(4*Pi/9)*vfac
 dfac=(4.0_DP*pi/9.0_DP)*vfac

!Checks the values of order
! if(order<0 .or. order>2)then
!  write(message, '(a,a,a,a,a,a,i3,a)' )ch10,&
!&  ' xcpzca : BUG -',ch10,&
!&  '  With Perdew-Zunger Ceperley-Alder xc functional, the only',ch10,&
!&  '  allowed values for order are 0, 1 or 2, while it is found to be',&
!&       order,'.'
!  call wrtout(6,message,'COLL')
!  call leave_new('COLL')
! end if

!Loop over grid points
 do ipt=1,npt
  rs=rspts(ipt)
  rsm1=1.0_DP/rs
! Consider two regimes: rs<1 or rs>=0
  if (rs<1._DP) then
   logrs=log(rs)
!  compute energy density exc (hartree)
!================
! -efac*rsm1=ex
! -vfac*rsm1=vx
!================
   exc(ipt)=(aa+cc*rs)*logrs+dd*rs+bb-efac*rsm1
!  compute potential vxc=d(rho*exc)/d(rho) (hartree)
   vxc(ipt)=(aa+two_thirds*cc*rs)*logrs+(dd+dd-cc)*rs*third+&
&        (bb-aa*third)-vfac*rsm1
!  compute d(vxc)/d(rho) (hartree*bohr^3)
   if(order>1)dvxc(ipt)=-(3._DP*aa+(cc+dd+dd)*rs+2._DP*cc*rs*logrs)&
&   /(9._DP*rhor(ipt))-dfac*rs**2
  else
   t1=b1*sqrt(rs)
   t2=b2*rs
   den=1._DP/(1._DP+t1+t2)
   exc(ipt)=ga*den-efac*rsm1
   vxc(ipt)=ga*(1._DP+c7_6*t1+c4_3*t2)*den**2-vfac*rsm1
   if(order>1)then
    den3=den**3
    dvxc(ipt)=(ga*den3/(36._DP*rhor(ipt)))*(5._DP*t1+8._DP*t2+&
&     7._DP*t1**2+16._DP*t2**2+21._DP*t1*t2)-dfac*rs**2
   end if
  end if
 end do
!
 end subroutine xcpzca
!!***
