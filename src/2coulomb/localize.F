
!SOURCE  : localize.F
!SUBJECT : Routines to handle localized systems
!
!AUTHOR  : Daniele Varsano <wabvavad@scsx03.sc.ehu.es> (2004)
!
! CONTAINS:
!
! - subroutine cutoff
!   :: real function F1
!   :: real function F2
!   :: real function F3
!   :: real function F4
!   :: real function F5
!   :: real function F6
!   :: real function lsin
! - subroutine cutofftitle
! - subroutine zdensity
! - subroutine checkgeosym
! - subroutine coltest
!
!###########################################################
 subroutine cutoff(qpg,qpt,gvec,symmop,a1,a2,a3,b1,b2,b3,&
&                  grot,syminv,q0,qpgm2int,lcoltest)
!###########################################################
!
! In this subroutine I calculate the FT of a cutoffed coulomb
! interaction, zeroed outside a given cylinder/box/sphere geometry.
!
! The cutoffed potential in G-space is given by
!
! CYLINDER:
! ---------
!
! Vc(q)=4pi*Int[0,Rc]Int[0,zc]drdz cos(q_z*z)*J0(q_r*r)/sqrt(r**2+z**2)
! q_r=sqrt(qx**2+qy**2)
!
! BOX:
! ----
!
! Vc(q,G)=1/(Vdl Nq)\sum_{G'} V(q'+G') F(q'+G',q+G)
!
! Note that q\in BZ and
!
! F(v,w)=8 \prod_i sin[(v_i-w_i)L_i/2]/(v_i-w_i)
!
!------------------------------------------------------------------
!
 use parser
 include 'dimen.par'
include 'selfdimen.par'
#if defined MPI
 include 'mpif.h'
#endif
!
 integer syminv(nsym),grot(nsym,ngwmax)
 real qpt(nq,3),symmop(3,3,nsym),gvec(ngvec,3),&
&     a1(3),a2(3),a3(3),b1(3),b2(3),b3(3),q0(3),qpgm2int(nq,ngqpgint,ngqpgint)
 complex qpg(nq,ngwmax)
!
!Stars of the q-points:
!
 integer, allocatable :: star(:,:),nqstar(:)
 real, allocatable :: weight(:)
!
! Work Space
!
 integer comp,iqg
 real F1,F2,F3,F4,F5,F6,q(3),rdot,g1(3),rdum,q1(3),&
&     qz,qr,G,z,Vd,zz,xx(5),ww(5),volrl,qc(3),qs(3),q2(3),lsin,bessj1
 real*8 drcut,dhcyl,rcut2,zcut2,const,const2,dboxside(3)
 complex Vcutoff(nq,ngdiag),cdotu,F(nq*ngwmax),Vc(nq*ngwmax)
#if defined MPI
 complex Vcutoffp(nq,ngdiag)
#endif
 character*3 npdirs,cyldir
 logical lcoltest,lcylinf
!
 external F1,F2,F3,F4,F5,F6
 common /ftoint/ qr,qz,zz
 
!-------------------------------------------------------------
 lcoltest=.true.
 call section(2,'Cutoff Geometry','f')
 hcyl=0.
 lcoltest=.true.
 call iparse_double(C_string('R_cutoff'),dble(0.),drcut)
 call iparse_double(C_string('Xboxside'),dble(0.),dboxside(1))
 call iparse_double(C_string('Yboxside'),dble(0.),dboxside(2))
 call iparse_double(C_string('Zboxside'),dble(0.),dboxside(3))
 call iparse_str(C_string('NoPerDir'),'',npdirs)
 call iparse_str(C_string('Cyldir'),'',cyldir)
 call iparse_str(C_string('Maindir'),'',maindir)
 if (index(maindir,'infinite').eq.0)  then
  call iparse_double(C_string('ClLength'),dble(0.),dhcyl)
  hcyl=dhcyl
 endif
 rcut=drcut
 boxside(:)=dboxside(:)
 cutoffmode=''
 if (rcut.gt.0..and.hcyl.eq.0.) then
  cutoffmode='Sphere'
  if (index(maindir,'infinite').ne.0) cutoffmode='Cylinder'
  goto 11
 endif
 if (rcut.gt.0..and.hcyl.gt.0.) then
  cutoffmode='Cylinder' 
  goto 11
 endif
 if (boxside(1).gt.0..and.boxside(2).gt.0..and.boxside(3).gt.0.) then
  cutoffmode='Box'
  goto 11
 endif
11 if (trim(cutoffmode)=='') call errmsg(&
& 'Unable to identify the geometry. At least one of the Geometry parameters must be set')
 if (index(npdirs,'X').eq.0.and.index(npdirs,'Y').eq.0.and.index(npdirs,'Z').eq.0) then
  call errmsg('The "NoPerDir" entry in the input file must contain only X,Y,Z characters')
 endif

 lcylinf=.false.
 axis(:)=0
 if (index(cyldir,'X').ne.0) axis(1)=1
 if (index(cyldir,'Y').ne.0) axis(2)=1
 if (index(cyldir,'Z').ne.0) axis(3)=1 
 if  (index(maindir,'infinite').ne.0) lcylinf=.true.
 call cutofftitle(0,'Coulomb potential')
 


 if (trim(cutoffmode).ne.'Sphere') then
!-----------------------------------------------------------------
! I need to expand the Q points in the BZ
!
  call section(3,'Q-points IBZ -> BZ','f')
  allocate(weight(nq),nqstar(nq),star(nq,nsym))
  call stars(qpt,nq,symmop,b1,b2,b3,weight,star,nqstar)
  nqbz=sum(nqstar(1:nq))
  deallocate(weight)
!-----------------------------------------------------------------
! Then I check that the geometry choosen does not brake any simmetry.
! In the same routine I calculate the "Effective Volume" epsvol
!
  call checkgeosym(a1,a2,a3,symmop,npdirs)
 endif

 select case(trim(cutoffmode))
  case('Sphere')
!-----------------------------------------------------------------
! SPHERICAL SURFACE
!-------------------
   do iq = 1, nq
    ig1=1
    if (iq.eq.1) ig1=2
    do ig = ig1,ngwmax
     rdum=sqrt(1.0-cos(qpg(iq,ig)*rcut))
     if (abs(rdum).lt.1.E-5) rdum=1.E-5
     qpg(iq,ig) = qpg(iq,ig)/rdum
    enddo
   enddo
   qpg(1,1) = sqrt(2.0)/rcut
   epsvol=vol
   lcoltest=.false.
   goto 102
  case('Box')
!-----------------------------------------------------------------
! BOX  WITH K SAMPLING
!----------------------
! I define the uncutoffed Coulomb interaction
! (common to the three surfaces)
!
   volrl=8.*pi**3./vol
   do iq=1,nq
    qpt(1,:)=q0(:)
    q(:)=qpt(iq,:)*2*pi/alat
    do ig=1,ngwmax
     iqg=(iq-1)*ngwmax+ig
     g1(:)=q(:)+gvec(ig,:)*2*pi/alat
     rdum=rdot(g1,g1)
     if (rdum.lt.1.E-5) then
      Vc(iqg)=7.7956*(volrl/float(nqbz))**(-2./3.)
     else
      Vc(iqg)=1./rdot(g1,g1)
     endif
    enddo
    if (sfile('data.qpgint')) then
     do ig=1,ngqpgint
      Vc((iq-1)*ngwmax+ig)=qpgm2int(iq,ig,ig)*vol*float(nqbz)/2.
     enddo
    endif
   enddo
   call section(2,'Cutoffed Coulomb potential','f')
   if (ncpu.gt.1) psection=.true.
   call imsg('b','=> G-vecs [NGDIAG] :',ngdiag)
   Vcutoff(:,:)=(0.,0.)
   do iq1=1,nq
    do ig1=1+myid,ngdiag,ncpu
     write (ch1,'(a,i3.3,a,i6.6,a,i6.6)') '  [p.',myid,'] Q/G-vector ',iq1,'/',ig1
     call wrrep(ch1)
     q1(:)=qpt(iq1,:)+gvec(ig1,:)
     do iq2=1,nq
      qc(:)=qpt(iq2,:)
      do ig2=1,ngwmax
       iqg=(iq2-1)*ngwmax+ig2
       F(iqg)=0.d0
       do i=1,nqstar(iq2)
        is=star(iq2,i)
        call symmact(symmop,is,qc,qs)
        q2(:)=qs(:)+gvec(grot(is,ig2),:)
        F(iqg)=F(iqg)+lsin(q1(1),q2(1),boxside(1))*&
&            lsin(q1(2),q2(2),boxside(2))*lsin(q1(3),q2(3),boxside(3))
       enddo
      enddo
     enddo
     Vcutoff(iq1,ig1)=cdotu(ngwmax*nq,Vc,1,F,1)/(vol*float(nqbz))
    enddo
   enddo
   deallocate(nqstar,star)
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

  end select

101 continue
      
#if defined MPI
 psection=.false.
 call mpi_barrier(mpi_comm_world,ierr)
 call mpi_allreduce(Vcutoff(1,1),Vcutoffp(1,1),nq*ngdiag,&
&                   mpi_complex,mpi_sum,mpi_comm_world,ierr)
 Vcutoff(:,:)=Vcutoffp(:,:)
#endif
 
 qpg(:,1:ngdiag)=sqrt(1./Vcutoff(:,1:ngdiag))
 if (myid.ne.0) return
102 continue
 call openunit(unit1,'data.cutoff','u')
 write (unit1) cutoffmode,maindir
 write (unit1) ngdiag,rcut,hcyl,boxside,epsvol,nq,axis(1),axis(2),axis(3)
 if (sfile('data.qpgint')) call cutofftitle(unit1,'Coulomb potential <+RIM>')
 if (.not.sfile('data.qpgint')) call cutofftitle(unit1,'Coulomb potential')
 write (ch2,'(a,f12.5)') ':: Area of the DL cell cut  [a.u.] :',epsvol
 write (unit1) ch2
 do iq=1,nq
  write (unit1) qpg(iq,1:ngdiag)
 enddo
 call closeunit('data.cutoff')

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
 real function lsin(g1,g2,side)
 include 'dimen.par'
 include 'selfdimen.par'
 real g1,g2,side,rfac
 rfac=(g1-g2)*2.*pi/alat
 if (abs(rfac).le.1.E-5) then
  lsin=side
 else
  lsin=2.*sin((rfac)*side/2.)/rfac
 endif
 end function
!
!####################################
 subroutine cutofftitle(lunit,ctitle)
!####################################
!
 include 'dimen.par'
 include 'selfdimen.par'
!
 integer lunit
 character*(*) ctitle
!
 write (ch1,'(4a)') '=> ',ctitle,' zeroed outside a ',trim(cutoffmode)
 if (lunit.eq.0) then
  call capo('b')
  call wrmsg('b',trim(ch1))
  call capo('b')
 else
  write (lunit) ch1
 endif
 select case(trim(cutoffmode))
  case('Sphere')
   write (ch1,'(a,f10.4)') ':: Sphere radius [a.u.]:',rcut
   if (lunit.eq.0) call wrmsg('b',trim(ch1))
   if (lunit.ne.0) write (lunit) ch1
   return
  case('Box')
   do i=1,3
    if (boxside(i).ne.0) then
     write (ch1,'(a,i3.3,a,f10.4)') ':: Box side(',i,') [a.u.]:',boxside(i)
     if (lunit.eq.0) call wrmsg('b',trim(ch1))
     if (lunit.ne.0) write (lunit) ch1
    endif
   enddo
  case('Cylinder')
   write (ch1,'(a,f10.4)') ':: Sphere radius [a.u.]:',rcut
   if (lunit.eq.0) call wrmsg('b',trim(ch1))
   if (lunit.ne.0) write (lunit) ch1
   if (index(maindir,'infinite').eq.0)  then
    write (ch1,'(a,f10.4)') ':: Cylinder half length [a.u.]:',hcyl
    if (lunit.eq.0) call wrmsg('b',trim(ch1))
    if (lunit.ne.0) write (lunit) ch1
   endif
   write (ch1,'(a,3(x,i1))') ':: Cylinder axis direction',axis(1),axis(2),axis(3)
   if (lunit.eq.0) call wrmsg('b',trim(ch1))
   if (lunit.ne.0) write (lunit) ch1
   write (ch1,'(2a)') ':: Main direction:',trim(maindir)
   if (lunit.eq.0) call wrmsg('b',trim(ch1))
   if (lunit.ne.0) write (lunit) ch1
   return
 end select

 end subroutine
!
!#################################
 subroutine zdensity(a1,a2,a3,rho)
!#################################
!
! Here the density is zeroed outside the cutoff region specified
! for the columbian.
!
!--------------------------------------------------------------------
!
 include 'dimen.par'
 include 'selfdimen.par'
!
 real a1(3),a2(3),a3(3),rho(nr)
!
! Work space
!
 real rvec(3),center(3)
 logical fcut
!
!--------------------------------------------------------------------
 call section(3,'[Localized System] => Density cut-off included','f')

 call cutofftitle(0,'Electronic density')
 center(:)=0.5*a1(:)+0.5*a2(:)+0.5*a3(:)
 call r3msg('b',':: Cell`s "center":',center)

 ir = 0
 do ir1 = 0, nfft1-1
  do ir2 = 0, nfft2-1
   do ir3 = 0, nfft3-1
    ir = 1 + ir1 + ir2*nfft1 + ir3*nfft1*nfft2
    rvec(1) = ir1*a1(1)/nfft1 + ir2*a2(1)/nfft2 + ir3*a3(1)/nfft3
    rvec(2) = ir1*a1(2)/nfft1 + ir2*a2(2)/nfft2 + ir3*a3(2)/nfft3
    rvec(3) = ir1*a1(3)/nfft1 + ir2*a2(3)/nfft2 + ir3*a3(3)/nfft3
    rvec(:)=rvec(:)-center(:)
    if (fcut(rvec)) rho(ir)=0.0
   end do
  end do
 end do

 end subroutine

 logical function fcut(v)
 include 'dimen.par'
 real v(3)
 select case(trim(cutoffmode))
  case('Sphere')
   fcut=(sqrt(v(1)**2.+v(2)**2.+v(3)**2.).gt.rcut)
  case('Cylinder')
  if  (index(maindir,'infinite').ne.0) then
   fcut=(sqrt(v(iort1)**2.+v(iort2)**2.).gt.rcut)
  else
   fcut=(abs(v(ipar)).gt.hcyl.or.&
&        sqrt(v(iort1)**2.+v(iort2)**2.).gt.rcut)
  endif
 end select
 end function
!
!
!##############################################
 subroutine checkgeosym(a1,a2,a3,symmop,npdirs)
!##############################################
!
 include 'dimen.par'
 include 'selfdimen.par'
!
 real symmop(3,3,nsym),a1(3),a2(3),a3(3)
 character*3 npdirs
!
! Work Space
 logical lsym,lanp(3)
 integer ncorner,comp,ndirs,pl1,pl2,ldir,napl
 real vgeo(8,3),vt(3)
!
!------------------------------------------------------
 call section(2,'Geometry simmetries','f')
!
 lanp(:)=.false.
 if (index(npdirs,'X').ne.0) lanp(1)=.true.
 if (index(npdirs,'Y').ne.0) lanp(2)=.true.
 if (index(npdirs,'Z').ne.0) lanp(3)=.true.
 pl1=0
 pl2=0
 ldir=0
 ndirs=0
 do i1=1,3
  if (lanp(i1)) ndirs=ndirs+1
 enddo
 if (ndirs.eq.3) then
  call wrmsg('b',':: Non-periodic cut of the DL unit cell is the cell itself')
 else  if (ndirs.eq.2) then
  call wrmsg('b',':: Non-periodic cut of the DL unit cell is a plane')
  do i1=1,3
   if (.not.lanp(i1)) ldir=i1
   if (lanp(i1).and.pl1.eq.0) then
    pl1=i1
    cycle
   endif
   if (lanp(i1).and.pl2.eq.0) then
    pl2=i1
    cycle
   endif
  enddo
 else  if (ndirs.eq.1) then
  call wrmsg('b',':: Non-periodic cut of the DL unit cell is a line')
  do i1=1,3
   if (lanp(i1)) ldir=i1
   if ((.not.lanp(i1)).and.pl1.eq.0) then
    pl1=i1
    cycle
   endif
   if ((.not.lanp(i1)).and.pl2.eq.0) then
    pl2=i1
    cycle
   endif
  enddo
 endif
!
 epsvol=0.
 napl=0
 if (a1(ldir).eq.0.) napl=napl+1
 if (a2(ldir).eq.0.) napl=napl+1
 if (a3(ldir).eq.0.) napl=napl+1
 if (napl.ne.2.and.ndirs.le.2) then
  call wrmsg('b',':: The above geometry defines a "Cutoff plane"')
  if (ndirs.eq.1) call wrmsg('b','   perpendicular to the non-periodic direction')
  if (ndirs.eq.2) call wrmsg('b','   corresponding to the non-periodic plane')
  call errmsg('No (or too many) DL unit cell vectors in this plane')
 endif
 if (a1(pl1).eq.0..and.a1(pl2).eq.0.) epsvol=abs(a1(ldir))
 if (a2(pl1).eq.0..and.a2(pl2).eq.0.) epsvol=abs(a2(ldir))
 if (a3(pl1).eq.0..and.a3(pl2).eq.0.) epsvol=abs(a3(ldir))
 if (ndirs.eq.1) &
& write (ch2,'(a,f10.5)') ':: Length of the DL cell along the Cutoff line [a.u.] :',epsvol
 if (ndirs.eq.2) then
  epsvol=vol/epsvol
  write (ch2,'(a,f10.5)') ':: Area of the DL cell cut along the Cutoff plane [a.u.] :',epsvol
 endif
 if (ndirs.eq.3) then
  epsvol=vol
  ch2=':: DL volume renormalized to 1'
 endif

 if (trim(cutoffmode).ne.'Box') return
 vgeo(:,:)=0.
 ncorner=8
 do i=1,8
  vgeo(i,:)=boxside(:)/2.
 enddo
 vgeo(2,1)=-vgeo(2,1)
 vgeo(3,2)=-vgeo(3,2)
 vgeo(4,3)=-vgeo(4,3)
 vgeo(5,1)=-vgeo(5,1)
 vgeo(5,2)=-vgeo(5,2)
 vgeo(6,1)=-vgeo(6,1)
 vgeo(6,3)=-vgeo(6,3)
 vgeo(7,2)=-vgeo(7,2)
 vgeo(7,3)=-vgeo(7,3)
 vgeo(8,:)=-vgeo(8,:)

 lsym=.true.
 do is=1,nsym
  do i1=1,ncorner
   call symmact(symmop,is,vgeo(i1,:),vt)
   do i2=1,ncorner
    if (comp(vt,vgeo(i2,:),compacc).eq.1) goto 2
   enddo
   lsym=.false.
   goto 3
2 enddo
 enddo

3 if (lsym) write (ch1,'(3a)') ':: The ',trim(cutoffmode),' is as simmetric as the lattice'
 if (.not.lsym) write (ch1,'(3a)') ':: Warning! The ',trim(cutoffmode),&
&                                ' is LESS simmetric than the lattice'
 call wrmsg('b',trim(ch1))
 call wrmsg('b',trim(ch2))

 end subroutine
!
!#####################################################################
 subroutine coltest(qpg,qpt,gvec,symmop,b1,b2,b3,a1,a2,a3,grot,&
&                   syminv,lcoltest)
!#####################################################################
!
 use parser
 include 'dimen.par'
 include 'selfdimen.par'
#if defined MPI
 include 'mpif.h'
#endif
!
 integer syminv(nsym),grot(nsym,ngwmax)
 real qpt(nq,3),symmop(3,3,nsym),gvec(ngvec,3),a1(3),a2(3),a3(3),&
&     b1(3),b2(3),b3(3),a4(3)
 complex qpg(nq,ngwmax)
!
! work space
! 
 integer pl1,pl2,ldir,nrsteps
 parameter(nrsteps=500)
 real rvec(4,nrsteps+1,3),center(3),potential(4,nrsteps+1),rdot,rmod,lmod(4),&
&     lvec(4,3),lq(3)
 complex cutpotential(4,nrsteps+1),pcutpotential(4,nrsteps+1)
 character*200 lch1
 logical fcut,lcoltest
!
!Stars of the q-points:
!
 integer, allocatable :: star(:,:),nqstar(:)
 real, allocatable :: weight(:)
!
 common/cutoffgeo/pl1,pl2,ldir
!----------------------------------------------------------------

 center(:)=0.5*a1(:)+0.5*a2(:)+0.5*a3(:)
 a4(1)=a1(1)
 a4(2)=a2(2)
 a4(3)=a3(3)
 call section(2,'Testing the cutoffed Coulomb Potential','t')
 call r3msg('b',':: Cell`s "center":',center)
!
!-----------------------------------------------------------------
! I need to expand the Q-points in the BZ
!
 call section(3,'Q-points IBZ -> BZ','f')
!
 allocate(weight(nq),nqstar(nq),star(nq,nsym))
 call stars(qpt,nq,symmop,b1,b2,b3,weight,star,nqstar)
 nqbz=sum(nqstar(1:nq))
 deallocate(weight)
!
!-----------------------------------------------------------------
 potential(:,:)=0.
 cutpotential(:,:)=(0.,0.)
 pcutpotential(:,:)=(0.,0.)
!
! Along A1, A2 ,A3
!-----------------
 do ir=1,nrsteps+1
  rvec(1,ir,:) = 4*((ir-1)*a1(:)/nrsteps + 0.5*a2(:)+ 0.5*a3(:)-center(:))
  rvec(2,ir,:) = 4*((ir-1)*a2(:)/nrsteps + 0.5*a3(:)+ 0.5*a1(:)-center(:))
  rvec(3,ir,:) = 4*((ir-1)*a3(:)/nrsteps + 0.5*a1(:)+ 0.5*a2(:)-center(:))
  rvec(4,ir,:) = rvec(1,ir,:)+rvec(2,ir,:)+rvec(3,ir,:)
  do i1=1,4
   rmod=sqrt(rdot(rvec(i1,ir,:),rvec(i1,ir,:)))
   if (rmod.gt.1.E-5)  potential(i1,ir)=1./rmod
   if (fcut(rvec(i1,ir,:))) potential(i1,ir)=0.
  enddo
 enddo
!
 do ir=myid+1,nrsteps+1,ncpu
  do iq=1,nq
   do i1=1,nqstar(iq)
    is=syminv(star(iq,i1))
    do i2=1,4
     call symmact(symmop,is,rvec(i2,ir,:),lvec(i2,:))
    enddo
    do ig=1,ngdiag
     ig1=grot(is,ig)
     lq(:)=(qpt(iq,:)+gvec(ig1,:))*2*pi/alat
     lmod(1)=rdot(lq,lvec(1,:))
     lmod(2)=rdot(lq,lvec(2,:))
     lmod(3)=rdot(lq,lvec(3,:))
     lmod(4)=rdot(lq,lvec(4,:))
     cutpotential(:,ir)=cutpotential(:,ir)+Exp((0.,1.)*lmod(:))*4.*pi/(qpg(iq,ig1)**2.)
    enddo
   enddo
  enddo
  cutpotential(:,ir)=cutpotential(:,ir)/(vol*float(nqbz))
 enddo
 deallocate(nqstar,star)

#if defined MPI
 call mpi_barrier(mpi_comm_world,ierr)
 call mpi_allreduce(cutpotential(1,1),pcutpotential(1,1),4*(nrsteps+1),&
&                   mpi_complex,mpi_sum,mpi_comm_world,ierr)
 cutpotential(:,:)=pcutpotential(:,:)
 if (myid.ne.0) return
#endif

 outfile(5)='out.cutoff-test'
 call openunit(unit1,trim(outfile(5)),'f')
 call wroutitle(unit1,'Section of the Cut-offed Coulomb Potential along A1/A2/A3')
 call wrmsgout(unit1,'Comparison of the calculated (Vc) and exact (Ve) potential.')
 call wrmsgout(unit1,'')
 lfilemsg=.false.
 if (lcoltest) then
 call openunit(unit2,'data.cutoff','u')
 read (unit2) cutoffmode
 read (unit2) ngdiagrd,rcut,hcyl,boxside,epsvol,nqrd
 read (unit2) ch1
 write (ch2,'(a,i5)') '=> G-vecs read [NGDIAG] :',ngdiagrd
 call wrmsgout(unit1,trim(ch2))
 call wrmsgout(unit1,trim(ch1))
 call wrmsgout(unit1,'')
 read (unit2) ch1
 call wrmsgout(unit1,trim(ch1))
 read (unit2) ch1
 call wrmsgout(unit1,trim(ch1))
 read (unit2) ch1
 call wrmsgout(unit1,trim(ch1))
 read (unit2) ch1
 call wrmsgout(unit1,trim(ch1))
 call closeunit('data.cutoff')
 endif
 lfilemsg=.true.
 call wroutitle(unit1,'Section of the Cut-offed Coulomb  &
      &       Potential along A1/A2/A3')
 call wrmsgout(unit1,'Comparison of the calculated and exact potential.')
 call wrmsgout(unit1,'')
 write (lch1,'(3(a,7x))') '<r|a1>      Ve r//a1       Vc r//a1',&
&                 '<r|a2>      Ve r//a2       Vc r//a2',&
&                 '<r|a3>      Ve r//a3       Vc r//a3'
 call wrmsgout(unit1,trim(lch1))
 do ir=1,nrsteps+1
  lmod(1)=rdot(rvec(1,ir,:),a1)/sqrt(rdot(a1,a1))
  lmod(2)=rdot(rvec(2,ir,:),a2)/sqrt(rdot(a2,a2))
  lmod(3)=rdot(rvec(3,ir,:),a3)/sqrt(rdot(a3,a3))
  lmod(4)=rdot(rvec(4,ir,:),a4)/sqrt(rdot(a4,a4))
  write (unit1,1001) lmod(1),potential(1,ir),real(cutpotential(1,ir)),&
&                    lmod(2),potential(2,ir),real(cutpotential(2,ir)),&
&                    lmod(3),potential(3,ir),real(cutpotential(3,ir))
 enddo
 call closeunit(trim(outfile(5)))
!
1001    format(10(g12.5,2x))
 
 end subroutine

