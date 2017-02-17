!{\src2tex{textfont=tt}}
!!****f* ABINIT/xcxalp
!! NAME
!! xcxalp
!!
!! FUNCTION
!! Returns exc, vxc, and eventually d(vxc)/d($\rho$) from input $\rho$.
!! "X$\alpha$" method is used in this subroutine:
!! Expression is exc=-alpha*efac/rs (hartree), efac below.
!! rs = $(3/(4\pi))^{1/3}* \rho (r)^{-1/3}$.
!
! Note that you recover LDAX when   \alpha=1 
!                       SLATER when \alpha=3/2
!
! this is following PW notation
!
!!
!! COPYRIGHT
!! Copyright (C) 1998-2005 ABINIT group (DCA, XG, GMR)
!! This file is distributed under the terms of the
!! GNU General Public License, see ~ABINIT/Infos/copyright
!! or http://www.gnu.org/copyleft/gpl.txt .
!! For the initials of contributors, see ~ABINIT/Infos/contributors .
!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
! 
! Added by A. Marini spin support (November 2008) 
! 
!!
!! INPUTS
!!  npt=number of real space points on which density is provided
!!  order=gives the maximal derivative of Exc computed.
!!  rspts(npt)=Wigner-Seitz radii, at each point
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

 subroutine xcxalp(alpha,exc,npt,n_spin,order,rspts,zeta,vxc,dvxc)  ! dvxc is optional

 use pars,            ONLY : DP,pi
 use xc_constants
 
 implicit none
 
!Arguments ------------------------------------
!scalars
 integer,intent(in) :: npt,order,n_spin
!arrays
 real(DP),intent(in) :: rspts(npt),alpha,zeta(npt)
 real(DP),intent(out):: exc(npt),vxc(npt,n_spin)
 real(DP),intent(out),optional :: dvxc(npt,3)
 
!Local variables-------------------------------
!scalars
 integer :: ipt
 real(DP) :: dfac,efac,rs_up,rsm1_up,rs_dn,rsm1_dn,vfac,rs,rsm1
 character(len=500) :: message
 
! *************************************************************************

!!Checks the values of order
! if(order<0 .or. order>2)then
!  write(message, '(a,a,a,a,a,a,i3,a)' )ch10,&
!&  ' xcxalp : BUG -',ch10,&
!&  '  With X-alpha xc functional, the only',ch10,&
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
 !
 if (n_spin==1) then
   do ipt=1,npt
     rs=rspts(ipt)
     rsm1=1.0_DP/rs
!    compute energy density (hartree)
     exc(ipt)=-alpha*efac*rsm1
!    compute potential (hartree)
     vxc(ipt,1)=-alpha*vfac*rsm1
!    compute d(vxc)/d(rho) (hartree*bohr^3)
     if(order>1) dvxc(ipt,1)=-alpha*dfac*rs**2
   end do
 else
!  Loop over grid points
   do ipt=1,npt
     rs_up=rspts(ipt)*(2._DP/(1._DP+zeta(ipt)))**(1._DP/3._DP)
     rsm1_up=1.0_DP/rs_up
     rs_dn=rspts(ipt)*(2._DP/(1._DP-zeta(ipt)))**(1._DP/3._DP)
     rsm1_dn=1.0_DP/rs_dn
!    compute energy density (hartree) ! -alpha (9/4/pi)**2/3 
     exc(ipt)=-alpha*efac*(rsm1_up+rsm1_dn)
!   compute potential (hartree)
     vxc(ipt,1)=-alpha*vfac*rsm1_up
     vxc(ipt,2)=-alpha*vfac*rsm1_dn
!
! compute d(vxc)/d(rho) (hartree*bohr^3)
! if(nspden=2): dvxc(:,1)=dvxc($\uparrow$)/d$\rho(\uparrow)$,
! dvxc(:,2)=dvxc($\uparrow$)/d$\rho(\downarrow)$, dvxc(:,3)=dvxc($\downarrow$)/d$\rho(\downarrow)$
!
     if(order>1) then 
       dvxc(ipt,1)=-alpha*dfac*rs_up**2
       dvxc(ipt,2)=0._DP
       dvxc(ipt,3)=-alpha*dfac*rs_dn**2
     endif
   end do
 endif
 !
 end subroutine xcxalp
!!***
