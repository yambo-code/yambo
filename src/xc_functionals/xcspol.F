!{\src2tex{textfont=tt}}
!!****f* ABINIT/xcspol
!! NAME
!! xcspol
!!
!! FUNCTION
!! Spin-polarized exchange and correlation, parameterized by Mike Teter
!! of Corning Incorporated.
!!
!! COPYRIGHT
!! Copyright (C) 1998-2005 ABINIT group (DCA, XG, GMR)
!! This file is distributed under the terms of the
!! GNU General Public License, see ~ABINIT/Infos/copyright
!! or http://www.gnu.org/copyleft/gpl.txt .
!! For the initials of contributors, see ~ABINIT/Infos/contributors .
!!
!! INPUTS
!!  nspden=number of spin-density components
!!  npts= number of points to be computed
!!  order=its absolute value gives the maximal derivative of Exc to be computed.
!!  rspts(npts)=Seitz electron radius (bohr)
!!  zeta(npts)=$(\rho\uparrow-\rho\downarrow)/(\rho\uparrow+\rho\downarrow)$=degree of polarization
!!  (ignored if nspden=1, in which case zeta should be 0)
!!
!! OUTPUT
!!  if(abs(order)>1) dvxc(npts,1+nspden)=              (Hartree*bohr^3)
!!   if(nspden=1 .and. order==2): dvxc(:,1)=dvxc/d$\rho$ , dvxc(:,2) empty
!!   if(nspden=1 .and. order==-2): also compute dvxc(:,2)=dvxc($\uparrow$)/d$\rho(\downarrow)$
!!   if(nspden=2): dvxc(:,1)=dvxc($\uparrow$)/d$\rho(\uparrow)$,
!!       dvxc(:,2)=dvxc($\uparrow$)/d$\rho(\downarrow)$, dvxc(:,3)=dvxc($\downarrow$)/d$\rho(\downarrow)$
!!
!!  exc(npts)=exchange-correlation energy density (hartree)
!!  vxc(npts,nspden)=xc potent. (d($\rho$*exc)/d($\rho\uparrow$)) and d/d($\rho\downarrow$) (ha)
!!  (only overall potential d($\rho$*exc)/d($\rho$) returned in vxc(1) for nspden=1)
!!
!! Normalization: Exc=$\int(exc(r)*\rho(r) d^3 r)$ for $\rho$(r)=electron density.
!!
!! TODO
!! To be added later
!!  d2vxc=derivative $d^2 (Vxc)/d(rho)^2$ (hartree*bohr^6)
!!
!! NOTES
!! This form is based on Mike Teter s rational polynomial
!! exc=-(a0+a1*rs+a2*rs**2+a3*rs**3)/(b1*rs+b2*rs**2+b3*rs**3+b4*rs**4)
!! where the parameters are fit to reproduce
!! (in this case) the Perdew-Wang parameterization of the correlation
!! energy given in Phys. Rev. B 45, 13244-13249 (1992).
!!
!! Each parameter is interpolated between zeta=0 and 1 by
!! a_i(zeta)=a_i(0)+(a_i(1)-a_i(0))*f_x(zeta) and
!! f_x(zeta)=[(1+zeta)$^{4/3}$+(1-zeta)$^{4/3}$-2]/(2*(2$^{1/3}$-1)).
!!
!! Beware : in this expression, zeta is actually replaced by zeta*alpha_zeta,
!! where alpha_zeta is very close to 1, but slightly lower.
!! This is to remove the singularity in the derivatives when abs(zeta) is 1
!! Below,  a_i(1)-a_i(0) is called "da" for delta a, same for b s.
!!
!! rs = $(3/(4\pi))^{1/3} * \rho(r)^{-1/3}$
!! zeta = $(\rho\uparrow-\rho\downarrow)/(\rho\uparrow+\rho\downarrow)$
!! b1 must be 1 and a0 must be $(3/4)(3/(2\pi))^{2/3}$.
!!
!! PARENTS
!!      drivexc
!!
!! CHILDREN
!!      leave_new,wrtout
!!
!! SOURCE

 subroutine xcspol(dvxc,exc,npts,nspden,order,rspts,vxc,zeta)

 use pars,            ONLY : DP,pi
 use xc_constants
 
 implicit none
 
!Arguments ------------------------------------
!scalars
 integer,intent(in) :: npts,nspden,order
!arrays
 real(DP),intent(in) :: rspts(npts),zeta(npts)
 real(DP),intent(out) :: dvxc(npts,3),exc(npts),vxc(npts,nspden)
 
!Local variables-------------------------------
!The generation of density from rs needs rsfac and rsfac^(-3) :
!rsfac=(3/(4 Pi))^(1/3) ; rsfacm3=4pi/3
!Mike Teter s parameters of 8 April 1993.
!New parameters which accomodate spin polarization (fit to P-W)
!Paramagnetic limit:a0p,...b4p
!(a0=(3/4)(3/(2 Pi))^(2/3)) (note that b1=1 is fixed)
!Differences, ferromagnetic - paramagnetic (delta params):da1,da2,da3,db1,db2,db3,db4
!scalars
 integer :: debug,ipts
 real(DP),parameter :: a0p=.4581652932831429_DP,a1p=2.217058676663745_DP
 real(DP),parameter :: a2p=0.7405551735357053_DP,a3p=0.01968227878617998_DP
 real(DP),parameter :: alpha_zeta=1.0_DP-1.0d-6,b1p=1.0_DP
 real(DP),parameter :: b2p=4.504130959426697_DP,b3p=1.110667363742916_DP
 real(DP),parameter :: b4p=0.02359291751427506_DP,da0=.119086804055547_DP
 real(DP),parameter :: da1=0.6157402568883345_DP,da2=0.1574201515892867_DP
 real(DP),parameter :: da3=0.003532336663397157_DP,db1=0.0_DP
 real(DP),parameter :: db2=0.2673612973836267_DP,db3=0.2052004607777787_DP
 real(DP),parameter :: db4=0.004200005045691381_DP,ft=4._DP/3._DP
 real(DP),parameter :: rsfac=0.6203504908994000_DP,rsfacm3=rsfac**(-3)
 real(DP) :: a0,a1,a2,a3,b1,b2,b3,b4,d1,d1m1,d2d1drs2,d2d1drsdf,d2excdf2
 real(DP) :: d2excdrs2,d2excdrsdf,d2excdz2,d2fxcdz2,d2n1drs2,d2n1drsdf,dd1df
 real(DP) :: dd1drs,delta,dexcdf,dexcdrs,dexcdz,dfxcdz,dn1df,dn1drs,dvxcdrs
 real(DP) :: dvxcpdrho,dvxcpdz,excipt,excunp,fact,fxc,n1,rho,rho_dn,rho_dnm
 real(DP) :: rho_dnp,rho_up,rho_upm,rho_upp,rhom1,rs,rsl,vxcp,vxcunp,zet
 real(DP) :: zeta_mean,zetm,zetm_third,zetp,zetp_third
 character(len=500) :: message
!no_abirules
!Set a minimum rho below which terms are 0
 real(DP),parameter :: rhotol=1.d-28

! *************************************************************************

!Although fact is parameter value, some compilers are not able to evaluate
!it at compile time.
 fact=one/(two**(four*third)-two)

!DEBUG
!Finite-difference debugging, do not take away
! debug=1
! zeta_mean=0.1d0
! delta=0.0001
! if(debug==1)then
!  do ipts=1,npts,5
!   rho=ipts*0.01d0
!   rho_up=rho*(1.0d0+zeta_mean)*0.5d0
!   rho_dn=rho*(1.0d0-zeta_mean)*0.5d0
!   rho_upp=rho_up+delta
!   rho_upm=rho_up-delta
!   rho_dnp=rho_dn+delta
!   rho_dnm=rho_dn-delta
!   First possibility : vary rho up , and then rho down
!   zeta(ipts  )=(rho_up -rho_dn )/(rho_up +rho_dn )
!   zeta(ipts+1)=(rho_upp-rho_dn )/(rho_upp+rho_dn )
!   zeta(ipts+2)=(rho_upm-rho_dn )/(rho_upm+rho_dn )
!   zeta(ipts+3)=(rho_up -rho_dnp)/(rho_up +rho_dnp)
!   zeta(ipts+4)=(rho_up -rho_dnm)/(rho_up +rho_dnm)
!   rspts(ipts  )=rsfac*(rho_up +rho_dn )**(-third)
!   rspts(ipts+1)=rsfac*(rho_upp+rho_dn )**(-third)
!   rspts(ipts+2)=rsfac*(rho_upm+rho_dn )**(-third)
!   rspts(ipts+3)=rsfac*(rho_up +rho_dnp)**(-third)
!   rspts(ipts+4)=rsfac*(rho_up +rho_dnm)**(-third)
!DEBUGBUG : another possibility : vary rho and zeta
!   zeta(ipts+1)=zeta(ipts  )
!   zeta(ipts+2)=zeta(ipts  )
!   zeta(ipts+3)=zeta(ipts  )+delta
!   zeta(ipts+4)=zeta(ipts  )-delta
!   rspts(ipts+1)=rsfac*(rho+delta)**(-third)
!   rspts(ipts+2)=rsfac*(rho-delta )**(-third)
!   rspts(ipts+3)=rspts(ipts  )
!   rspts(ipts+4)=rspts(ipts  )
!ENDDEBUGBUG
!  end do
! end if
! nspden=2
! order=2
!ENDDEBUG

 if (nspden==1) then
!DEBUG
!Do not take away : uncomment, to test the computation of some nspden=1
!quantities with the nspden=2 code
!if (.false.) then
!ENDDEBUG

! No spin-polarization so skip steps related to zeta not 0

  do ipts=1,npts

   rs=rspts(ipts)
   n1=a0p+rs*(a1p+rs*(a2p+rs*a3p))
   d1=rs*(b1p+rs*(b2p+rs*(b3p+rs*b4p)))
   d1m1=1.0_DP/d1

!  Exchange-correlation energy
   excipt=-n1*d1m1
   exc(ipts)=excipt

!  Exchange-correlation potential
   dn1drs=a1p+rs*(2._DP*a2p+rs*(3._DP*a3p))
   dd1drs=b1p+rs*(2._DP*b2p+rs*(3._DP*b3p+rs*(4._DP*b4p)))

!  dexcdrs is d(exc)/d(rs)
   dexcdrs=-(dn1drs+excipt*dd1drs)*d1m1
   vxc(ipts,1)=excipt-third*rs*dexcdrs

!  If the exchange-correlation kernel is needed
   if ( order**2 >1 ) then

    d2n1drs2=2._DP*a2p+rs*(6._DP*a3p)
    d2d1drs2=2._DP*b2p+rs*(6._DP*b3p+rs*(12._DP*b4p))
!   d2excdrs2 is d2(exc)/d(rs)2
    d2excdrs2=-(d2n1drs2+2._DP*dexcdrs*dd1drs+excipt*d2d1drs2)*d1m1
    dvxcdrs=third*(2.0_DP*dexcdrs-rs*d2excdrs2)
!   And d(vxc)/d(rho)=(-rs/(3*rho))*d(vxc)/d(rs)
    dvxc(ipts,1)= -rs**4*rsfacm3*third*dvxcdrs

    if(order==-2)then
!    dn1df=d(n1)/d(fxc) and dd1df=d(d1)/d(fxc)
     dn1df=da0+rs*(da1+rs*(da2+rs*da3))
     dd1df=rs*(db1+rs*(db2+rs*(db3+rs*db4)))
     dexcdf=-(dn1df+excipt*dd1df)*d1m1
!    d2(fxc)/d(zeta)2
     d2fxcdz2=ft*third*(alpha_zeta**2)*2._DP*fact
!    d2(exc)/d(zeta)2
     d2excdz2=d2fxcdz2*dexcdf
     rhom1=rsfacm3*rs**3
     dvxc(ipts,2)= dvxc(ipts,1) - d2excdz2*rhom1
    end if

   end if

  end do

!Allows for nspden==1, in the case of testing nspden=1 against nspden=2
 else if (nspden<=2) then

!  DEBUG
!  do not take away : allows to compare nspden=1 and nspden=2 coding
!  if (nspden==1)then
!   zeta(:)=zero
!  end if
!  ENDDEBUG

! Allow for spin polarization. This part could be optimized for speed.
  do ipts=1,npts

   rs=rspts(ipts)
   zet=zeta(ipts)
   zetp=1._DP+zet*alpha_zeta
   zetm=1._DP-zet*alpha_zeta
   zetp_third=zetp**third
   zetm_third=zetm**third
!  Exchange energy spin interpolation function f(zeta)
   fxc=( zetp*zetp_third + zetm*zetm_third - two ) *fact

   a0=a0p+fxc*da0
   a1=a1p+fxc*da1
   a2=a2p+fxc*da2
   a3=a3p+fxc*da3
   b1=b1p+fxc*db1
   b2=b2p+fxc*db2
   b3=b3p+fxc*db3
   b4=b4p+fxc*db4

   n1= a0+rs*(a1+rs*(a2+rs*a3))
   d1=rs*(b1+rs*(b2+rs*(b3+rs*b4)))
   d1m1=one/d1

!  Exchange-correlation energy
   excipt=-n1*d1m1
   exc(ipts)=excipt

!  Exchange-correlation potential
   dn1drs=a1+rs*(2._DP*a2+rs*(3._DP*a3))
   dd1drs=b1+rs*(2._DP*b2+rs*(3._DP*b3+rs*(4._DP*b4)))
!  dexcdrs is d(exc)/d(rs)
   dexcdrs=-(dn1drs+excipt*dd1drs)*d1m1

!  Only vxcp contributes when paramagnetic
   vxcp=excipt-third*rs*dexcdrs

!  d(fxc)/d(zeta)  (which is 0 at zeta=0)
   dfxcdz=ft*alpha_zeta*(zetp_third-zetm_third)*fact

!  dn1df=d(n1)/d(fxc) and dd1df=d(d1)/d(fxc)
   dn1df=da0+rs*(da1+rs*(da2+rs*da3))
   dd1df=rs*(db1+rs*(db2+rs*(db3+rs*db4)))

!  dexcdz is d(exc)/d(zeta)
   dexcdf=-(dn1df+excipt*dd1df)*d1m1
   dexcdz=dfxcdz*dexcdf

!  Compute Vxc for both spin channels

   vxc(ipts,1)=vxcp - (zet-one)*dexcdz
   vxc(ipts,2)=vxcp - (zet+one)*dexcdz

!DEBUG Allow to check the variation of rho and zeta
!  vxc(ipts,1)=vxcp
!  vxc(ipts,2)=dexcdz
!ENDDEBUG

   if(abs(order)>1)then

!   Compute second derivative with respect to rho
    d2n1drs2=2._DP*a2+rs*(6._DP*a3)
    d2d1drs2=2._DP*b2+rs*(6._DP*b3+rs*(12._DP*b4))
!   d2excdrs2 is d2(exc)/d(rs)2
    d2excdrs2=-(d2n1drs2+two*dexcdrs*dd1drs+excipt*d2d1drs2)*d1m1
    dvxcdrs=third*(two*dexcdrs-rs*d2excdrs2)
!   And d(vxc)/d(rho) paramagnetic =(-rs/(3*rho))*d(vxcp)/d(rs)
!   remember : 1/rho=(4pi/3)*rs**3=rsfacm3*rs**3
    rhom1=rsfacm3*rs**3
    dvxcpdrho= -rs*rhom1*third * dvxcdrs

!   Compute mixed second derivative with respect to rho and zeta
    d2n1drsdf=da1+rs*(2._DP*da2+rs*(3._DP*da3))
    d2d1drsdf=db1+rs*(2._DP*db2+rs*(3._DP*db3+rs*(4._DP*db4)))
!   d2excdrsdf is d2(exc)/d(rs)df
    d2excdrsdf=-(d2n1drsdf+dexcdrs*dd1df+dexcdf*dd1drs+excipt*d2d1drsdf)*d1m1
!   d(vxc)/d(zeta) paramagnetic
    dvxcpdz=dexcdz-third*rs*dfxcdz*d2excdrsdf

!   Compute second derivative with respect to zeta
!   the second derivative of n1 and d1 wrt f vanishes
    d2excdf2=-(two*dexcdf*dd1df)*d1m1
!   d2(fxc)/d(zeta)2
    d2fxcdz2=ft*third*(alpha_zeta**2)*(zetp_third**(-2)+zetm_third**(-2))*fact
!   d2(exc)/d(zeta)2
    d2excdz2=d2fxcdz2*dexcdf+dfxcdz**2*d2excdf2

!   Compute now the three second derivatives of the Exc energy with respect
!   to : wrt twice spin-up ; wrt spin-up and spin-dn ; wrt twice spin-down
    dvxc(ipts,1)= dvxcpdrho   &
&                +two*rhom1*( one-zet)*(dvxcpdz-dexcdz) &
&                +d2excdz2*rhom1*(one-zet)**2
    dvxc(ipts,2)= dvxcpdrho   &
&                +two*rhom1*(    -zet)*(dvxcpdz-dexcdz) &
&                +d2excdz2*rhom1*(one-zet)*(-one-zet)
    if(nspden==2)then
     dvxc(ipts,3)= dvxcpdrho   &
&                +two*rhom1*(-one-zet)*(dvxcpdz-dexcdz) &
&                +d2excdz2*rhom1*(-one-zet)**2
    else
!    For testing purposes, need the spin-averaged quantity
     dvxc(ipts,1)= ( dvxc(ipts,1) + dvxc(ipts,2) ) * half
    end if

!DEBUG Allow to check the variation of rho and zeta
!   dvxc(ipts,1)=dvxcpdrho
!   dvxc(ipts,2)=d2excdz2
!   dvxc(ipts,3)=dvxcpdz
!ENDDEBUG

   end if

  end do

! else
!
! Disallowed value for nspden
!  write(message, '(a,a,a,a,a,a,i12,a)' ) ch10,&
!&   ' xcspol: BUG -',ch10,&
!&   '  Argument nspden must be 1 or 2; ',ch10,&
!&   '  Value provided as argument was ',nspden,'.'
!  call wrtout(06,message,'COLL')
!  call leave_new('COLL')
 end if

!DEBUG
!Finite-difference debugging, do not take away
!if(debug==1)then
! write(6,*)' delta =',delta
! do ipts=1,npts,5
!  rho=(rspts(ipts)/rsfac)**(-3)
!  write(6, '(a,i5,a,2es16.8)' ) ' Point number',ipts,' with rho,zeta=',rho,zeta(ipts)
!  write(6, '(3es16.8)' )exc(ipts)*rho,vxc(ipts,1),vxc(ipts,2)
!  write(6, '(3es16.8)' )dvxc(ipts,1),dvxc(ipts,3),dvxc(ipts,2)
!  write(6, '(3es16.8)' )exc(ipts)*rho,&
!&      ( exc(ipts+1)*(rho+delta) - exc(ipts+2)*(rho-delta) )/2.d0/delta,&
!&      ( exc(ipts+3)*(rho+delta) - exc(ipts+4)*(rho-delta) )/2.d0/delta
!   write(6, '(4es16.8)' )&
!&    ( vxc(ipts+1,1) - vxc(ipts+2,1) )/2.d0/delta,&
!&    ( vxc(ipts+3,2) - vxc(ipts+4,2) )/2.d0/delta,&
!&    ( vxc(ipts+3,1) - vxc(ipts+4,1) )/2.d0/delta,&
!&    ( vxc(ipts+1,2) - vxc(ipts+2,2) )/2.d0/delta
!  end do
!  stop
! end if
!ENDDEBUG

!DEBUG
!if(order==-2)then
! write(6,*)' xcspol : ipts,npts ',ipts,npts
! write(6,*)dvxcdrs,d2excdz2,d2fxcdz2,dexcdf
! write(6,*)rhom1
! write(6,*)dvxc(1000,1),dvxc(1000,2)
!stop
!end if
!ENDDEBUG

 end subroutine xcspol
!!***
