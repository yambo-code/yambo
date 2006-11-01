  case('Cylinder')
!-----------------------------------------------------------------
! CYLINDER SURFACE
!-----------------------------------------------------------------
! Vc=int[0,zc]cos(q_z*t)F(t)dt
! F(t)=int[0,_rc]dsG(s,q_xy,t)
! G(s,q_xy,t)=s*J0(q_xy*s)/sqrt(s^+t^2)
! if maindir is "infinite" some integrals are treated analitically
! Define grid in real space (r,z):
!
   call section(2,'Cutoffed Coulomb potential','f')
   if (ncpu.gt.1) psection=.true.
   call imsg('b','=> G-vecs [NGDIAG] :',ngdiag)
   
   if(axis(1).eq.1.and.axis(2).eq.1) then
    call errmsg('set cilynder axis along ONE cartesian direction')
   endif
   if(axis(1).eq.1.and.axis(3).eq.1) then
    call errmsg('set cilynder axis along ONE cartesian direction')
   endif
   if(axis(3).eq.1.and.axis(2).eq.1) then
    call errmsg('set cilynder axis along ONE cartesian direction')
   endif
   if(axis(1).eq.1) then
    ipar=1
    iort1=2
    iort2=3
   elseif(axis(2).eq.1) then
    ipar=2
    iort1=1
    iort2=3
   elseif(axis(3).eq.1) then
    ipar=3
    iort1=2
    iort2=1
   endif
   if (index(maindir,'infinite').ne.0) then
    hcyl=abs(1/qpt(2,ipar))-1
    write (ch1,'(a,f5.1)') 'max lenght allowed for hcyl ->',hcyl
    call capo('b')
    call wrmsg('b',trim(ch1))
    call capo('b')
   else
    hcyl=dhcyl
   endif
   zcut=hcyl
   zcut2=zcut**2.
   rcut2=rcut**2.
   do iq=1,nq
    q(:)=qpt(iq,:)*2*pi/alat
    do ig=1+myid,ngdiag,ncpu
     write (ch1,'(a,i3.3,a,i6.6,a,i6.6)') '  [p.',myid,'] Q/G-vector ',iq,'/',ig         
     call wrrep(ch1)
     g1(:)=q(:)+gvec(ig,:)*2*pi/alat
     qz=g1(ipar)
     qr=sqrt((g1(iort1)**2+g1(iort2)**2))
     Vcutoff(iq,ig)=0.

     if (.not.lcylinf) then  !finite cylinder
      if (qr.eq.0.and.qz.ne.0) then
       const=1./qz**2-cos(qz*zcut)/qz**2-zcut*sin(qz*zcut)/qz
       const2=sin(qz*zcut)*sqrt(rcut**2+zcut**2)
       call qgausseps(F3,0,zcut,Vd) 
!
! QGAUS4 subroutine QGAUS of Num. Rec. modified, Gauss-Legendre integration 
! performed with 20 points 
! the original one is 10 points and are not enough for this calculation.
! nodes and weight tabulated in QGAUS4 have been calculated with the subroutine 
! gaussq.f from netlib
! For strong oscillating functions I call gausseps, it integrates functions via 
! Gauss-Legendre integration, adding poitns until a ceirtain convergence is reached.         
!
       Vcutoff(iq,ig)=const+(const2-Vd)/qz
      else if (qz.eq.0) then
       call qgaus4(F4,0,rcut,Vd)
       Vcutoff(iq,ig)=Vd
      else
       call qgausseps(F2,0,zcut,Vd)
       Vcutoff(iq,ig)=Vd
      endif

     else !infinite
      
      if (qz.eq.0.and.qr.ne.0) then
       call qgaus4(F5,0,rcut,Vd)
       Vcutoff(iq,ig)=-Vd+rcut*log(2*zcut)*bessj1(qr*rcut)/qr
      else if (qz.ne.0) then
       Vcutoff(iq,ig)=F6(qr,abs(qz))/(qz**2+qr**2)  
      endif
     endif
     enddo
    enddo
   
    if (myid.eq.0) then 
!     Vcutoff(1,1)=-1./4.*rcut2*(2*log(rcut)-1)
!   if (.not.lcylinf) Vcutoff(1,1)=0.5*(-zcut2+zcut*sqrt(zcut2+rcut2)+&
!&                 rcut2*log((zcut+sqrt(zcut2+rcut2))/rcut))
     Vcutoff(1,1)=0.5*(-zcut2+zcut*sqrt(zcut2+rcut2)+&
&                 rcut2*log((zcut+sqrt(zcut2+rcut2))/rcut))
    endif
    deallocate(nqstar,star)
    goto 101

 end subroutine

 real function F1(s)
 include 'dimen.par'
 include 'selfdimen.par'
 real s,qr,qz,z,arg,bessj0,zz
 common /ftoint/ qr,qz,zz
 arg=qr*s
 if (zz.eq.0) then
  F1= bessj0(arg)
 else
  F1= bessj0(arg)*s/sqrt(s**2+zz**2)
 endif
 end function F1

 real function F2(z)
 include 'dimen.par'
 include 'selfdimen.par'
 real z,G,qr,qz,zz,F1
 external F1
 common /ftoint/ qr,qz,zz
 zz=z
 call qgaus4(F1,0,rcut,G)
 F2= G*cos(qz*z)      
 end function F2

 real function F3(z)
 include 'dimen.par'
 include 'selfdimen.par'
 real z,qr,qz,zz,arg
 common /ftoint/ qr,qz,zz
 arg=qz*z
 F3=z*sin(arg)/sqrt(rcut**2+z**2)
 end function F3

 real function F4(z)
 include 'dimen.par'
 include 'selfdimen.par'
 real z,qr,qz,zz,arg,bessj0
 common /ftoint/ qr,qz,zz
 if (z.eq.0) then
  F4=0.
 else
  arg=qr*z
  F4=bessj0(arg)*z*log((zcut+sqrt(zcut**2+z**2))/z)
 endif
 end function F4

 real function F5(z)
 include 'dimen.par'
 include 'selfdimen.par'
 real z,qr,qz,zz,arg,bessj0
 common /ftoint/ qr,qz,zz
 if (z.eq.0) then
  F5=0.
 else
  arg=qr*z
  F5=bessj0(arg)*z*log(z)
 endif
 end function F5

 real function F6(x,y)
 include 'dimen.par'
 include 'selfdimen.par'
 real x,y,arg1,arg2,bessj0,bessj1,bessk0,bessk1
 arg1=x*rcut
 arg2=y*rcut
 F6=x*rcut*bessj1(arg1)*bessk0(arg2)-y*rcut*bessj0(arg1)*bessk1(arg2)
 F6=F6+1
 end function F6
!
