!
!SOURCE  : mbvxc.F
!SUBJECT : Exact Exchange,Static Screened Exchange, Random Phase Approximation
!          (quasi particle limits, plasmon pole approximation) 
!          and corresponding approximated epotential   
!          scf driver/noscf driver for bands/vxc(r) plotting 
! 
!AUTHOR  : Myrta Gruening 
!
!CONTAINS:
!
! SCF (drivers and utilities):
!
! subroutine dftscf
! subroutine subvhartr
! subroutine plotvr
! subroutine rchecksym
! subroutine noscf
! subroutine readvxc
! subroutine reportenergies
! subroutine writeselfs
!
! MB-VXC:
!
! subroutine mbvxc
! subroutine subsig
! subroutine subceda
! subroutine discontinuity
!
!#################################################################
 subroutine dftscf(rrotm1,igfft,wfr0,grot,syminv,sigindex,en0,occ,&
&            qpg,qpgm2int,qpgm2intocc,bzvolocc,qk,qksym,qg0,&
&            nqkibz,nqksym,nqk,rhoq0,symmop,weight,b1,b2,b3,gvec,&
&            ngw,igk)
!#################################################################
!
! scf driver: Solves iteratively the set of KS equations. 
!             The one-electron matrix is reconstructed from the 
!             KS eigenvalues and eigenvectors given as input 
!             (en0, wfr0) 
!
!---------------------------------------------------------------
 use parser
 use a2sm
 use selfm
 implicit none
#if defined MPI
 include 'mpif.h'
#endif 
!
 integer(1) qksym(nq,2,nqkibzmax,nqksymax)
 integer(2) qg0(nq,nqkibzmax,nqksymax),qk(nq,2,nqkibzmax),nqksym(nq,nqkibzmax),&
&          sigindex(nq,nsym,nkibz,3)
 integer rrotm1(nsym,nr),igfft(ngwmax,g0dim),nqkibz(nq),syminv(nsym),nqk(nq),&
&        grot(nsym,ngwmax),igk(ngvec,nkibz),ngw(nkibz)
 real en0(nbn,nkibz),occ(nbn,nkibz),qpgm2int(nq,ngqpgint,ngqpgint),&
&     qpgm2intocc(nbmetal,nq,nsym,ksig2,metngqpgint,metngqpgint),&
&     bzvolocc(nbmetal,nq,nsym,ksig2)
 complex rhoq0(rhoq0dim,nbnchi,nkibz,nsym),wfr0(nbn,nkibz,nr),qpg(nq,ngwmax)
 real symmop(3,3,nsym),weight(nkibz),b1(3),b2(3),b3(3),gvec(ngvec,3)
!
! Work Space
!------------
!
! density, xc and hartree potential/ energies
!
 real, external :: vxcca, vxnr, vcjas, ecjas, expw
 real vxc(nr), vxclda(nr), exlda, eclda, eh, exx, ex, ec, e1el, ekin, &
&     rho(nr),  rhonew(nr), rholda(nr), qptrd(nq,3),kpg(3)
 complex vxcnew(nr), vhr(nr),  vhrlda(nr), vcoh(nr), sigx, sigc,Wdum, wvec
 logical lcorr, lldac, lexx, lssx, lrpaxc, lmbxc, laprx, lkli
 complex, allocatable :: wfc(:,:,:), wfctmp(:,:)
 real(8) efermi
 real rdot
 character(chlen) timech
 character(chlen) fname
 common/fermienergy/efermi
!
! dieletric matrices
!
 complex, allocatable :: res(:,:,:),resrd(:,:),pole(:,:,:),polerd(:,:),&
&         epsm1(:,:,:), epsm1diag(:,:), epsm1rd(:,:), epsm1diagrd(:) 
!
! Abinit header
!
 type(abhdr_type) :: abhdr
!
! KS: matrices and solutions
!
 complex wfr(nbn,nkibz,nr), hksmat(nbn,nbn), h1mat(nbn,nbn,nkibz),&
&        vxchmat,kinmat(nbn,nbn,nkibz)
 real  en(nbn,nkibz), eshift, fac
!
! scf parameters
!
 real tester
 integer niter
!
! kpoints
!
 real kpt(ksig2,3)
!
! store/restart 
!
 integer rnbn, rnkibz, rngdiag, rngblk, rnr, rngwfc
 complex, allocatable :: rstmat(:,:,:)
 logical lstore, lrestart, lstartvxc
 character(10) xctyprd ,xcapprxrd
!
! checks
!
 logical lsym, trflag
!
! dummies
!
 integer iq,ik,i,ir,ib,ig,ib1,ib2,i1,n,j
 real(8) ltcpu1,ltcpu2
 complex, allocatable :: cdum(:)
 real, allocatable :: rdum(:)

#if defined MPI
 complex, allocatable :: prstmat(:,:,:)
 complex ph1mat(nbn,nbn,nkibz), pwfr(nbn,nkibz,nr)
 real pen(nbn,nkibz), pe1el, pekin
#endif

!===============================================================================
!
! Input logicals:
!
 lcorr=.true.    ! abinit calculations with lda correlation
!
! mb based functionals:  
!
 lexx = (trim(xctyp).eq."exx".or.trim(xctyp).eq."exxc")
 lssx = (trim(xctyp).eq."ssx")
 lrpaxc = (trim(xctyp).eq."rpa")
 lmbxc= (lexx.or.lssx.or.lrpaxc)
!
! add lda correlation to exx
!
 lldac=(trim(xctyp).eq."exxc") 
!
! use the common energy denominator/kli approximations
!
 laprx=(trim(xcapprx).eq."ceda".or.trim(xcapprx).eq."kli")
 lkli=(trim(xcapprx).eq."kli")
!
! Default restart logicals
!
 lstore=.true.    ! store data on file data.ks_out
 lrestart=.true.  ! restart job reading it from data.ks_in
 lstartvxc=.true. ! read the starting potential from file
!
! look for restart files and set up the associated logicals
!
 if (.not.sfile('data.ks_in')) lrestart=.false.
 if (.not.sfile('data.vxc_in')) lstartvxc=.false.
 if (lrestart.and.lstartvxc) then
  call wrmsg('b','Cannot use  in the same run both data.ks_in and data.vxc_in')
  lstartvxc=.false.
 end if
!
! write the parameters on report
!
 call wrmsg('b','-----------------------------------------------------------')
 call wrmsg('b','Main setup:')
 call wrmsg('b','-----------------------------------------------------------')
 abhdr%invxctyp='unknown'
 abhdr%ixc=-1
 call abgsreport(abhdr,.true.,.false.)
 write (ch1,'(2a)') '- Input KS states: ',trim(abhdr%invxctyp)
 call wrmsg('b',trim(ch1))
 write (ch1,'(a)') '- KS potential   :'
 ch2=ch1
 if (lexx) write (ch2,'(2a)') trim(ch1),' EXact eXchange'
 if (lssx) write (ch2,'(2a)') trim(ch1),' Static Screened eXchange'
 if (lrpaxc) write (ch2,'(2a)') trim(ch1),&
   'Random Phase Approximation (quasiparticle limit, see PRA 51, 2005)'
 ch1=ch2
 if (.not.lmbxc) write (ch2,'(2a)') trim(ch1),' LDA-XC CA/PZ'
 ch1=ch2
 if (lldac) write (ch2,'(2a)') trim(ch1),' + LDA-C CA/PZ'
 call wrmsg('b',trim(ch2))
 write (ch1,'(a)') '- Approximations :'
 ch2=ch1
 if (laprx.and.(.not.lkli)) write (ch2,'(2a)') trim(ch1),&
&                                 ' Common Energy Denominator (CEDA)'
 ch1=ch2
 if (laprx.and.lkli) write (ch2,'(2a)') trim(ch1),' Krieger Li Iafrate (KLI)'
 ch1=ch2
 if (.not.laprx) write (ch2,'(2a)') trim(ch1),' None'
 ch1=ch2
 call wrmsg('b',trim(ch2))
 write (ch1,'(a,i3.3)') '- Requested Iterations :',niterscf
 call wrmsg('b',trim(ch1))
 call capo('b')
 call wrmsg('b','- Driver options:')
 if (lstore) call wrmsg('b','  :: KS matrix stored on disk')
 if (lrestart) call wrmsg('b','  :: Starting from KS matrix of an old calculation')
 if (lstartvxc) call wrmsg('b','  :: Reading starting potential from file')
 call wrmsg('b','-----------------------------------------------------------')
 call capo('b')
!===============================================================================
 if (abhdr%ixc.eq.2) then
  lcorr=.true.
 else if ((abhdr%ixc.eq.8).or.(abhdr%ixc.ge.50)) then
  lcorr=.false.
 else if (abhdr%ixc==-1) then
  call capo('b')
  call wrmsg('b','xc potential unknown! SELF won`t stop but remember that the')
  call wrmsg('b','local KS solution is implemented only for Perdew-Zunger-Ceperley-Alder')
  call wrmsg('b','or exchange only input KS states (ixc 2 and 8 of Abinit 4.3.3)')
 else 
  call capo('b')
  call wrmsg('b','Local DFT solution implemented only for Perdew-Zunger-Ceperley-Alder')
  call wrmsg('b','or exchange only input KS states (ixc 2 and 8 of Abinit 4.3.3)')
  call errmsg('Wrong input xc potential')
 endif
!
! in the case of correlated mb potentials read the inverse dielectric function 
! from the data files (existence already checked in self.F)
!
! Only exchange case (default,initialization):
 ngscdg=1
 ngppdg=1
 ngscbl=1
 ngppbl=1
 allocate(epsm1diag(1,nq))
 allocate(epsm1(1,1,nq))
 epsm1diag(:,:)=(0.,0.)
 epsm1(:,:,:)=(0.,0.)
 allocate(res(1,1,nq))
 allocate(pole(1,1,nq))
 res(:,:,:)=(0.,0.)
 pole(:,:,:)=(0.,0.) 
! Static screened exchange (data.epsm1s):
 if (lssx) then
  allocate(epsm1diagrd(1),epsm1rd(1,1))
  call rdepsm1fordft(epsm1diagrd,epsm1rd,1)
  ngscbl =  ngblkrd
  ngscdg =  ngdiagrd
  deallocate(epsm1diag,epsm1,epsm1diagrd,epsm1rd)
  allocate(epsm1diag(ngscdg,nq),epsm1diagrd(ngscdg))
  allocate(epsm1(ngscbl,ngscbl,nq),epsm1rd(ngscbl,ngscbl))
  do iq=1,nq 
   call rdepsm1fordft(epsm1diagrd,epsm1rd,iq)
   epsm1diag(:,iq) = epsm1diagrd(:)
   epsm1(:,:,iq) = epsm1rd(:,:)
  end do
  call closeunit('data.epsm1s')
  deallocate(epsm1diagrd,epsm1rd)
! RPA in the PP approximation (data.pp):
 elseif (lrpaxc) then 
  allocate(polerd(1,1),resrd(1,1))
  call rdplasmonpolefordft(polerd,resrd,1)
  deallocate(res,resrd,pole,polerd)
  ngppdg = ngdiagrd
  ngppbl = ngblkrd
  allocate(pole(ngppdg,ngppdg,nq),polerd(ngppdg,ngppdg))
  allocate(res(ngppdg,ngppdg,nq),resrd(ngppdg,ngppdg))
  do iq=1,nq 
   call rdplasmonpolefordft(polerd,resrd,iq)
   pole(:,:,iq) = polerd(:,:)
   res(:,:,iq) = resrd(:,:)
  end do
  call closeunit('data.pp')
  deallocate(polerd,resrd)
 end if
!===============================================================================
 call section (2,'Input density and KS hamiltonian','f')

 do ik=1,nkibz 
  read(unit5) (kpt(ik,i),i=1,3)
 enddo
 rewind(unit5)
!
! Starting density and potentials/energies
!
 call density(rho,wfr0,weight,occ,rrotm1,.false.)
 rholda(:)=rho(:)
 call subvhartr(vhr,rho,qpg,igfft)
 exlda=0.
 eclda=0.
 eh=0.
 do ir=1, nr
  if ((sxctyp=='lda').or.(sxctyp=='')) vxclda(ir) =  vxnr(rho(ir)*real(nfft1*nfft2*nfft3)/vol)
  if (lcorr) vxclda(ir) = vxclda(ir)+ vcjas(rho(ir)*real(nfft1*nfft2*nfft3)/vol)
  exlda=exlda + rho(ir)*(expw(rho(ir)*real(nfft1*nfft2*nfft3)/vol))
  if (lcorr) eclda = &
&  eclda + rho(ir)*(ecjas(rho(ir)*real(nfft1*nfft2*nfft3)/vol))
  eh=eh+rho(ir)*real(vhr(ir))
 end do

 if ((sxctyp.ne.'lda').and.(sxctyp.ne.'')) then
  allocate(cdum(nr),rdum(nr))
  call readvxc(vxclda,cdum,rdum,rdum) !NB: the last three arguments are dummies
  lstartvxc = .false.
  deallocate(cdum,rdum)
 end if
!
!===========================================================================
! read the coefficient of the wfs for the calculation of the kinetic energy
!
 allocate(wfc(nbnin,nkibz,ngwfc),wfctmp(nbnin,ngvec))
 call openunit(unit2,'self.wfc','u')
 wfctmp(:,:)=(0.,0.)
 wfc(:,:,:)=(0.,0.)
 do ik=1,nkibz
  read (unit2) ((wfctmp(ib,i),ib=1,nbnin),i=1,ngw(ik))
  do ib=1,nbnin
   do ig=1,ngw(ik)
    if (igk(ig,ik).le.ngwfc) wfc(ib,ik,igk(ig,ik))=wfctmp(ib,ig)
   enddo
  enddo
 enddo
 call closeunit('self.wfc')
 deallocate(wfctmp)
!
!===========================================================================
! Calculate the one-electron hamiltonian 
!
 h1mat(:,:,:)=(0.,0.)
 kinmat(:,:,:)=(0.,0.)
!
#if defined MPI
 ph1mat(:,:,:)=(0.,0.)
 pe1el=0.
 pekin = 0.
 psection=.true.
#endif
!
 e1el = 0.
 ekin = 0.
 call pclock(999,'')
 do ik=1+myid,nkibz,ncpu
  do ib1=1,nbn
   do ib2=1,ib1
    if (ib2.eq.ib1) h1mat(ib1,ib2,ik)=en0(ib1,ik)+efermi
    vxchmat=(0.,0.)
    do ig = 1,ngwfc
     kpg(:)=(kpt(ik,:)+gvec(ig,:))*2*pi/alat
     kinmat(ib1,ib2,ik) = kinmat(ib1,ib2,ik)+ &
&     rdot(kpg,kpg)*conjg(wfc(ib1,ik,ig))*wfc(ib2,ik,ig)
    end do
    do ir=1,nr
     vxchmat = vxchmat + &
&     conjg(wfr0(ib1,ik,ir))*wfr0(ib2,ik,ir)*(vxclda(ir)+real(vhr(ir)))
    enddo
    h1mat(ib1,ib2,ik)=h1mat(ib1,ib2,ik)-vxchmat
    if ((ib2.le.nbfull).and.(ib2.eq.ib1)) then
     e1el = e1el + weight(ik)*real(h1mat(ib1,ib2,ik))
     ekin = ekin + weight(ik)*real(kinmat(ib1,ib2,ik))
    end if    
   end do
  enddo
 end do
 call pclock(116,'1-el Hamiltonian loop')
 psection=.false.

#if defined MPI
 call mpi_barrier(mpi_comm_world,ierr)
 call mpi_allreduce(h1mat(1,1,1),ph1mat(1,1,1),nbn**2*nkibz,&
& mpi_complex,mpi_sum,mpi_comm_world,ierr)
 call mpi_allreduce(e1el,pe1el,1,mpi_real,mpi_sum,mpi_comm_world,ierr)
 call mpi_allreduce(ekin,pekin,1,mpi_real,mpi_sum,mpi_comm_world,ierr)
 h1mat(:,:,:)=ph1mat(:,:,:)
 ekin = pekin
 e1el = pe1el
#endif
 deallocate(wfc)
!
! Starting energies report:
!
 call reportenergies('r','Energies from the LDA calculations:',&
& ekin,e1el,eh,exlda,eclda,exlda+eclda)
!
 vhrlda(:)=vhr(:)
 vxc(:)=vxclda(:)
!
! read the starting potentials/densities from file
!
 if (lstartvxc) then
  call readvxc(vxc,vhr,rho,vxcnew) !vxcnew here is dummy, will not be used.
  vhr(:)=vhr(:)+vhrlda(:)
 end if
!
!===========================================================================
 call section (2,'KS Self-consistent loop','f')
 call capo('b')
!
! Start SCF iterations
!
!
! COMMENT: to do-> rationalize when to calculate the potential and 
!          when to solve the KS eqs. 
!
 do i=1,niterscf
  if ((lstore).or.(lrestart)) then 
   allocate(rstmat(nbn,nbn,nkibz)) 
   rstmat(:,:,:)=(0.,0.)
#if defined MPI
   allocate(prstmat(nbn,nbn,nkibz)) 
   prstmat(:,:,:)=(0.,0.)
#endif  
  end if
!
! Read from restart file the KS matrix elements
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 
  if (lrestart) then
   call wrmsg('b','Reading KS matrix elements/density from file "data.ks_in"')
   call openunit(unit2,'data.ks_in','u')
#if !defined NOHEADER
   call rdheader(unit2)   
#endif
!
! COMMENT: to change ->
! I would like to start numbering the iterations from where it stopped...
!
   read (unit2)  i1, rnkibz, rnbn, rngdiag, rngblk
   read (unit2)  xctyprd, xcapprxrd
   call i2msg('b', '   :: K-points range :', 1,rnkibz)
   call i2msg('b', '   :: Bands range    :', 1,rnbn)
   call  imsg('b', '   :: G-vectors      :', rngdiag)
   call  imsg('b', '   :: Iterations     :', i1)
   call  amsg('b', '   :: XC-pot(type)   :', xctyprd)
   call  amsg('b', '   :: XC-pot(appr.)  :', xcapprxrd)
   call icheck('Scanning <data.ks_in>',rnkibz,'read Kpts',nkibz,'given Kpts','eq')
   call icheck('Scanning <data.ks_in>',rnbn,'read Bands',nbn,'given bands','eq')
   call icheck('Scanning <data.ks_in>',rngdiag,'read Gvecs',ngdiag,'given Gvecs','eq')
   read (unit2) (rho(ir),ir=1,nr)
   read (unit2) (((rstmat(ib1,ib2,ik),ib1=1,nbn), ib2=1,nbn),ik=1,nkibz)
   call capo('b')
   call closeunit('data.ks_in')
  end if
!
  write (ch1,'(a,i3.3)') '=> Iteration ',i
  call wrmsg('b','=================================')
  call wrrep(trim(ch1))
  call wrmsg('b',trim(ch1))
  call wrmsg('b','=================================')
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  call rchecksym(rrotm1,vxc,nr,lsym)
  if (.not.lsym) then
   call capo('s')
   call  wrmsg('s',' [WARNING] Something is wrong with the vxc symmetry.')
   nwarnings=nwarnings+1
   call capo('s')
  end if
  call section(3,'KS Hamiltonian','f')

#if defined MPI
  psection=.true.
#endif

  wfr(:,:,:)=(0.,0.)
  en(:,:)=0.
  e1el = 0.
  ekin = 0.
  
#if defined MPI
  pwfr(:,:,:)=(0.,0.)
  pen(:,:)=0.
  pe1el=0.
  pekin=0.
#endif
!
  call pclock(999,'')
  do ik=1+myid,nkibz,ncpu 
!
! Calculate KS matrix (or read it from file)
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
   call lcputime(ltcpu1)
   if (lrestart) then 
    hksmat(:,:)=rstmat(:,:,ik)
   else
    hksmat(:,:)=h1mat(:,:,ik)
    do ib1=1,nbn
     do ib2=1,ib1
      vxchmat=(0.,0.)
      do ir=1,nr 
       vxchmat = vxchmat + &
&       conjg(wfr0(ib1,ik,ir))*wfr0(ib2,ik,ir)*(vxc(ir)+real(vhr(ir)))
      enddo
      hksmat(ib1,ib2)=hksmat(ib1,ib2)+vxchmat
     enddo
    enddo
   end if
   if ((lstore).and.(.not.lrestart)) rstmat(:,:,ik)=hksmat(:,:)
!
! Diagonalize KS matrix
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   call matdiag(hksmat,en(1,ik),nbn)
!
! New wave functions  (normalized already)
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   do ib1=1,nbn
    do ib2=1,nbn
     wfr(ib1,ik,:)=wfr(ib1,ik,:)+wfr0(ib2,ik,:)*hksmat(ib2,ib1)
    end do
   end do
!
! 1-electron and KS kinetic energy
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   do ib1=1,nbn
    do ib2=1,ib1
     fac = 2.
     if (ib1.eq.ib2) fac = 1.
     do ib=1,nbfull
      ekin = ekin + fac*weight(ik)*&
&      real(conjg(hksmat(ib1,ib))*hksmat(ib2,ib)*kinmat(ib1,ib2,ik))
      e1el = e1el + fac*weight(ik)*&
&      real(conjg(hksmat(ib1,ib))*hksmat(ib2,ib)*h1mat(ib1,ib2,ik))
     end do
    end do
   end do
!
! Print few KS orbital energies
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
   write (*,'(7x,a,i3.3,a,10(f8.3,1x))') &
&   'Kpt ',ik,' few levels [unshifted] :',(en(n,ik)*HARTREE,n=nbfull-2,nbfull+2)
   call lcputime(ltcpu2)
   ch3=timech(ltcpu2-ltcpu1)
   write (ch4,'(3x,a,i3.3,a,i3.3,2a)') &
&   '[p.',myid,' K.',ik,'] KS hamiltonian solution :',trim(ch3)
   call wrrep(trim(ch4))
  end do
!
! End K-point loop
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  write (ch2,'(2a)') trim(ch1),' - KS solution'
  call pclock(6,trim(ch2))
  psection=.false.
#if defined MPI
  call mpi_barrier(mpi_comm_world,ierr)
  call mpi_allreduce(wfr(1,1,1),pwfr(1,1,1),nbn*nkibz*nr,&
&                     mpi_complex,mpi_sum,mpi_comm_world,ierr)
  wfr(:,:,:)=pwfr(:,:,:)
  call mpi_allreduce(en(1,1),pen(1,1),nbn*nkibz,&
&                     mpi_real,mpi_sum,mpi_comm_world,ierr)
  en(:,:)=pen(:,:)
  call mpi_allreduce(e1el,pe1el,1,mpi_real,mpi_sum,mpi_comm_world,ierr)
  e1el=pe1el
  call mpi_allreduce(ekin,pekin,1,mpi_real,mpi_sum,mpi_comm_world,ierr)
  ekin=pekin
  if (lstore.or.(lrestart))  then 
   call mpi_allreduce(rstmat(1,1,1),prstmat(1,1,1),nbn*nbn*nkibz,&
 &                     mpi_complex,mpi_sum,mpi_comm_world,ierr)
   rstmat(:,:,:)=prstmat(:,:,:)
  end if
#endif
  if ((lrestart).and.(.not.lstore)) deallocate(rstmat)
#if defined MPI
  if (lstore.or.(lrestart))   deallocate(prstmat)
#endif

 call capo('r')
 call wrmsg('r','Bands (unshifted)')
 call capo('r')
 if (myid.eq.0) then
  do ik=1,nkibz
   write (11,1) ik,(kpt(ik,j),j=1,3)
   do j=1,int(nbn/10)
    write (11,'(3x,10(f8.3,1x))') (en(n,ik)*HARTREE,n=10*(j-1)+1,10*j)
   enddo
   write (11,'(3x,10(f8.3,1x))') (en(n,ik)*HARTREE,n=10*int(nbn/10)+1,nbn)
  enddo
 endif
1 format (4x,'kpt(',i3,')=',1x,3(f6.3,1x))
!
! store the KS matrix element and the density in 'data.ks_xctyp-xcapprx' 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if ((lstore).and.(myid.eq.0)) then
  write (fname,'(4a)') 'data.ks_',trim(xctyp),'-',trim(xcapprx)
  if  (xcapprx=='') write (fname,'(4a)') 'data.ks_',trim(xctyp)
  call openunit(unit2,trim(fname),'u')
#if !defined NOHEADER
   call wrheader(unit2)   
#endif
   write (unit2)  i, nkibz, nbn, ngdiag, ngblk
   write (unit2)  xctyp, xcapprx 
   write (unit2) (rho(ir),ir=1,nr)
   write (unit2) (((rstmat(ib1,ib2,ik),ib1=1,nbn),ib2=1,nbn),ik=1,nkibz)
   call closeunit(trim(fname))
  end if
  if (lstore)   deallocate(rstmat)
!
! Calculate new density 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  call density(rhonew,wfr,weight,occ,rrotm1,.false.)
!
! Calculate the tester: \int |rho(r)-rhonew(r)| dr
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  tester=0.
  do ir=1,nr
   tester=tester+abs(rho(ir)-rhonew(ir))/vol
  enddo
!
! New potentials/energies
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if (lmbxc) then
   if (laprx) then
    call subceda(vxcnew,wfr,occ,rrotm1,igfft,grot,syminv,qpg,qpgm2int,&
&                  qpgm2intocc,bzvolocc,sigindex,kpt,b1,b2,b3,symmop,&
&                  rhoq0,epsm1,epsm1diag,res,pole,en,lkli)
   else
    call mbvxc(rrotm1,igfft,wfr,grot,syminv,sigindex,en,occ,qpg,qpgm2int,&
&                qpgm2intocc,bzvolocc,qk,qksym,qg0, nqkibz,nqksym,nqk,rhoq0,&
&                symmop,weight,kpt,b1,b2,b3,epsm1,epsm1diag,res,pole,vxcnew)
   endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!    
! linear mixing of the xc potential (this kind of potentials are not affected 
! from the mixing of the density) 
!
   if (lldac) then
    do ir=1,nr
     vxcnew(ir)=vxcnew(ir)+vcjas(rhonew(ir)*real(nfft1*nfft2*nfft3)/vol)
    end do
   end if
   if ((i.ne.1).and.(.not.lrestart)) then
    vxc(:)=(.7)*vxc(:)+.3*real(vxcnew(:))
   else 
    vxc(:)=real(vxcnew(:))
   end if
  end if
!
! Linear mixing of the new density with the old one 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if (i.ne.1) then
   rho(:)=(.7)*rho(:)+.3*rhonew(:)
  else 
   rho(:)=rhonew(:)
  end if
!
! Hartree potential
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  call subvhartr(vhr,rho,qpg,igfft)
!
  lrestart=.false.

  call section(3,'Exchange self-energy','f')
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  exlda=0.
  eclda=0.
  eh = 0.  
  exx = 0.
  if (lmbxc) then
   nw = 1 
   wvec = (0.,0.) 
   do ib=1,nbfull
    do ik=1,nkibz
     call subsig(sigx,sigc,ib,ib,ik,rrotm1,igfft,wfr,grot,sigindex,en,&
&                 occ,qpg,qpgm2int,qpgm2intocc, bzvolocc,epsm1,epsm1diag,&
&                  res,pole,wvec,.true.)
     exx=exx+weight(ik)*real(sigx)
    end do
   end do
  end if
  do ir=1, nr
   if (.not.lmbxc) then
    vxclda(ir) =  vxnr(rho(ir)*real(nfft1*nfft2*nfft3)/vol)+&
&    vcjas(rho(ir)*real(nfft1*nfft2*nfft3)/vol)
    exlda = exlda + rho(ir)*expw(rho(ir)*real(nfft1*nfft2*nfft3)/vol)   
    vxc(ir) = vxclda(ir)
   end if
   if ((.not.lmbxc).or.lldac) eclda = eclda + &
&   rho(ir)*ecjas(rho(ir)*real(nfft1*nfft2*nfft3)/vol)
   eh=eh+rho(ir)*real(vhr(ir))
  end do
  if (lmbxc) then
   ex = exx
   ec = 0.
   if (lldac) ec = eclda
  else 
   ex = exlda
   ec = eclda
  end if
  call reportenergies('s',&
&                      'Energies after this iteration',ekin,e1el,eh,ex,ec,ex+ec)
  call capo('r')
  write (ch1,'(2(a,g10.3),a)') &
&  '>> Convergence tester [should be smaller than ',convscf,']:',tester,' <<'
  call wrmsg('b',trim(ch1))
  call capo('b')
  niter=i
  if ((tester.lt.convscf).and.(i.gt.1)) goto 100
! 
! Clean exit from SCF cycle:
!
  if (sfile('stopscf')) then
   if (myid.eq.0) then
    call capo('b')
    call imsg('b', 'File stopscf found. Exiting the SCF at iteration',i)
    call wrmsg('b', 'Remember to remove this file')
    call capo('b')
   end if
   goto 200
  end if
  call capo('b')
 end do
!
! Exit the SCF loop, write the report
!
 write (ch1,'(a,i3,a)') 'SC loop NOT converged after',niter,' iterations'
 call wrmsg('b','========================================================')
 call wrmsg('b',trim(ch1))
 call wrmsg('b','========================================================')
 goto 200

100 write (ch1,'(a,i3,a)') 'SC loop succesfully converged after',niter,' iterations'
 call wrmsg('b','========================================================')
 call wrmsg('b',trim(ch1))
 call wrmsg('b','========================================================')
!
! calculate the exchange (corr) energy, the derivative discontinuity
!
200 if (myid.eq.0) then
  write (fname,'(4a)') 'data.vxc_',trim(xctyp),'-',trim(xcapprx)
  if  (xcapprx=='') write (fname,'(4a)') 'data.vxc_',trim(xctyp)
  call openunit(unit2,trim(fname),'u')
#if !defined NOHEADER
  call wrheader(unit2)   
#endif
  write (unit2)  nr, nkibz, ngwfc, nbn
  write (unit2)  xctyp, xcapprx
  write (unit2) (vxc(ir),ir=1,nr)
  write (unit2) (vhr(ir)-vhrlda(ir),ir=1,nr)
  write (unit2) (rho(ir),ir=1,nr)
  write (unit2) (vxclda(ir),ir=1,nr)
  call closeunit(trim(fname))
 end if
!
! write the new energies/wave function in self.wfc/self.gen files 
!
 call writeselfs(wfr,en,tester,niter,abhdr,igfft) 
!
 if (lmbxc) then 
  call discontinuity(rrotm1,igfft,wfr,grot,sigindex,kpt,en,&
&                    occ,qpg,qpgm2int,qpgm2intocc,bzvolocc,vxc,&
&                    epsm1,epsm1diag,res,pole)
!
  exx = 0.
  do ib=1,nbfull
   do ik=1,nkibz
    call subsig(sigx,sigc,ib,ib,ik,rrotm1,igfft,wfr,grot,sigindex,en,&
&                occ,qpg,qpgm2int,qpgm2intocc, bzvolocc,epsm1,epsm1diag,&
&                  res,pole,wvec,.true.)
    exx=exx+weight(ik)*real(sigx)
   end do
  end do
 end if

 if (lmbxc) then
  ex = exx
  ec = 0.
  if (lldac) ec = eclda
 else 
  ex = exlda
  ec = eclda
 end if
 deallocate(epsm1diag,epsm1,res,pole)
 call capo('r')
 call reportenergies('r','Energy Report',ekin,e1el,eh,ex,ec,ex+ec)
 write (ch1,'(2(a,g10.3),a)') &
& '>> Convergence tester [should be smaller than ',convscf,']:',tester,' <<'
 call capo('b')
 call wrmsg('b',trim(ch1))
 if (myid.eq.0) then 
  eshift=-1000
  do ik=1,nkibz
   if (en(nbfull,ik).gt.eshift) eshift=en(nbfull,ik)
  end do
  call capo('r')
  call rmsg('r','Bands from diagonalization of KS Hamiltonian [eV] shifted by',&
&            eshift*HARTREE) 
  call capo('r')
   do ik=1,nkibz
    write (11,1) ik,(kpt(ik,j),j=1,3)
    do j=1,int(nbn/10)
     write (11,'(3x,10(f8.3,1x))') ((en(n,ik)-eshift)*HARTREE,n=10*(j-1)+1,10*j)
    enddo
    write (11,'(3x,10(f8.3,1x))') ((en(n,ik)-eshift)*HARTREE,n=10*int(nbn/10)+1,nbn)
   enddo
  end if
!
! write out files:
!
  if (myid.eq.0) then 
   outfile(7)='out.dft-ksnrg'
   call efile(outfile(7))
   call openunit(unit1,trim(outfile(7)),'f')
   call wroutitle(unit1,'KS eigenvalues from diagonalization of KS Hamiltonian [eV]')
   call wrmsgout(unit1,'')
   write(unit1,11) '=> XC-pot(type) :', xctyp
   write(unit1,11) '=> XC-pot(appr.):', xcapprx
   call imsgout(unit1,'=> number iterations:', niter)
   call rmsgout(unit1,'=> convergence tester', tester)
   call rmsgout(unit1,'=> bands shifted by', eshift*HARTREE)
   call wrmsgout(unit1,'')
   do ik=1,nkibz
    write (unit1,13) ik,(kpt(ik,j),j=1,3)
    do j=1,int(nbn/10)
     write (unit1,'(3x,10(f8.3,1x))') ((en(n,ik)-eshift)*HARTREE,n=10*(j-1)+1,10*j)
    enddo
    write (unit1,'(3x,10(f8.3,1x))') ((en(n,ik)-eshift)*HARTREE,n=10*int(nbn/10)+1,nbn)
   enddo
   call closeunit(trim(outfile(7)))
 
   outfile(8)='out.dft-TOTnrg'
   call efile(outfile(8))
   call openunit(unit1,trim(outfile(8)),'f')
   call wroutitle(unit1,'KS Energies (from self-consistent solution of KS-equation [a.u.]')
   write(unit1,11) '=> XC-pot(type) :', xctyp
   write(unit1,11) '=> XC-pot(appr.):', xcapprx
   call imsgout(unit1,'=> number iterations:', niter)
   call rmsgout(unit1,'=> convergence tester', tester)
   call wrmsgout(unit1,' kinetic      1-electron   Hartree      Exchange     Correlation')
   call wrmsgout(unit1,'')
   write (unit1,12) ekin, 2.*e1el,  .5*eh, ex, ec
   call closeunit(trim(outfile(8)))
 endif
11 format('# ',a,a)
12 format(5(f9.5,4x))
13 format ('# ',2x,'kpt(',i3,')=',1x,3(f6.3,1x))
!
 end subroutine dftscf
!   
!#################################################################
 subroutine mbvxc(rrotm1,igfft,wfr,grot,syminv,sigindex,en,occ,&
&                  qpg,qpgm2int,qpgm2intocc,bzvolocc,qk,qksym,qg0,&
&                  nqkibz,nqksym,nqk,rhoq0,symmop,weight,kpt,b1,b2,b3,&
&                  epsm1,epsm1diag,res,pole,vxcr)
!#################################################################
!
! calculate the exact exchange potential (vxcr)
!
!---------------------------------------------------------------
 use parser
 use selfm
 implicit none
#if defined MPI
 include 'mpif.h'
#endif
!
 integer(1) qksym(nq,2,nqkibzmax,nqksymax)
 integer(2) qg0(nq,nqkibzmax,nqksymax),qk(nq,2,nqkibzmax),nqksym(nq,nqkibzmax),&
&          sigindex(nq,nsym,nkibz,3)
 integer rrotm1(nsym,nr),igfft(ngwmax,g0dim),nqkibz(nq),syminv(nsym),nqk(nq),&
&        grot(nsym,ngwmax)
 real en(nbn,nkibz),occ(nbn,nkibz),qpgm2int(nq,ngqpgint,ngqpgint),&
&     qpgm2intocc(nbmetal,nq,nsym,ksig2,metngqpgint,metngqpgint),&
&     bzvolocc(nbmetal,nq,nsym,ksig2),b1(3),b2(3),b3(3),symmop(3,3,nsym),&
&     weight(nkibz),kpt(ksig2,3)
 complex rhoq0(rhoq0dim,nbnchi,nkibz,nsym),wfr(nbn,nkibz,nr),qpg(nq,ngwmax),vxcr(nr),&
&        epsm1diag(ngscdg,nq), epsm1(ngscbl,ngscbl,nq), res(ngppdg,ngppdg,nq), &
&        pole(ngppdg,ngppdg,nq)
 character(chlen) timech
!
! Work Space
!------------
!
! chi0:
! 
 complex, allocatable :: wvec(:)
 complex chi0(ngblk,ngblk),chi0diag(ngdiag),chi0t(ngblk-1,ngblk-1)
!
! oscillators:
!
 complex rhow(ngdiag,2),rhotmp(ngdiag)
!
! sigmaxc:
! 
 complex sigx, sigc
!
! stars of kpoints
! 
 real, allocatable :: kweight(:)
 integer, allocatable :: kstar(:,:),nk(:)
!
! rhoxc, vxc
!
 complex rhoxc(ngdiag),vxc(ngdiag)

 integer ig,ib2,ib1,ik,ik1,is
 real(8) ltcpu1,ltcpu2
 logical trflag
!
#if defined MPI
 complex prhoxc(ngdiag)
#endif
!---------------------------------------------------------------
!
 call section(3,'Updated xc-Potential','f')
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
 nw=1
 allocate(wvec(1))
!
 wvec(1)=0.
!
 call subchi(1,rrotm1,igfft,wfr,grot,qpg,syminv,qk,qksym,qg0,&
 &            nqkibz,nqksym,nqk,en,occ,wvec,rhoq0,chi0,chi0diag,.true.)
!
! inverse of chi0 (no head and wings):
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
 do ig=2,ngblk
  chi0t(ig-1,1:ngdiag-1)=chi0(ig,2:ngdiag)
 end do
!
 if (ngdiag.le.1) then 
  call errmsg('ngdiag should be greater than 1')
 endif
 if (ngdiag.eq.ngblk) then
  call lapackinv(chi0t,ngdiag-1)
 else 
  call errmsg ('ngdiag != ngblk not implemented')
 end if
 rhoxc(:)=(0.,0.)
#if defined MPI
 prhoxc(:)=(0.,0.)
#endif
!
! stars for k integration:
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
 allocate(kweight(nkibz),nk(nkibz),kstar(nkibz,nsym))
 call stars(kpt,nkibz,symmop,b1,b2,b3,kweight,kstar,nk)
!
!
 call wrmsg('b','=> Vxc Bands loop [detailed timing in status file]')
#if defined MPI
 psection=.true.
#endif
!
 call pclock(999,'')
 do ib2=nbfull+1+myid,nbn,ncpu  
  call lcputime(ltcpu1)
  do ib1=1,nbfull 
   do ik=1,nkibz !sum on k points of the irr. BZ  
! SUM_{R_k} rhow_{ib1,ib2}(R_k, q=0, G): 
    rhow(1:ngdiag,1:2)=(0.,0.)
    do ik1=1,nk(ik) 
     is=syminv(kstar(ik,ik1))
     call subrhotwforlife(1,is,ib1,ik,ib2,ik,1,1,&
&                          rrotm1,igfft,wfr,grot,rhotmp,ngdiag)
     rhotmp(1)=rhoq0(ib1,ib2,ik,is)
     if (.not.trflag(is)) rhow(:,1)=rhow(:,1)+conjg(rhotmp(:))
     if (trflag(is)) then
      rhow(1,2)=rhow(1,2)+conjg(rhotmp(1))
      rhow(2:ngdiag,2)=rhow(2:ngdiag,2)+rhotmp(2:ngdiag)
     end if
    enddo
! <ib1,ik| Sig_x(c)|ib2, ik>:
    if (trim(xctyp).eq."rpa") wvec(1) = en(ib1,ik)+ (0.,1.)*zero
    call subsig(sigx,sigc,ib1,ib2,ik,rrotm1,igfft,wfr,grot,sigindex,en,occ,&
&               qpg,qpgm2int,qpgm2intocc,bzvolocc,epsm1,epsm1diag,&
&               res,pole,wvec,.false.)
!
! Due to TR symmetry introduced a factor 2 (only the half of rhow has been 
! calculated).
! 
    rhoxc(:) = rhoxc(:)+ occ(ib1,ik)*(2.-occ(ib2,ik))*  &
&               (rhow(:,1)*(sigx+sigc)+rhow(:,2)*conjg(sigx+sigc))/  &
&               (en(ib1,ik)-en(ib2,ik))
   enddo
  enddo
  call lcputime(ltcpu2)
  ch2=timech(ltcpu2-ltcpu1)
  write (ch1,'(3x,a,i3.3,a,i3.3,2a)') &
&  '[p.',myid,' B.',ib2,'] Single band loop component :',trim(ch2)
  call wrrep(trim(ch1))
 enddo
 call pclock(6,'Vxc bands loop')
 psection=.false.
!
#if defined MPI
 call mpi_barrier(mpi_comm_world,ierr)
 call mpi_allreduce(rhoxc(1),prhoxc(1),ngdiag,&
&                    mpi_complex,mpi_sum,mpi_comm_world,ierr)
 rhoxc(:)=prhoxc(:)
#endif
!
 deallocate(kweight,nk,kstar,wvec)
!
! VXC(G)= inv(CHI0(G,G'))RHOXC(G'): 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
 vxc(:)=(0.,0.)
 do ig=2,ngdiag
  vxc(2:ngdiag)=vxc(2:ngdiag)+chi0t(1:ngdiag-1,ig-1)*rhoxc(ig)
 end do
!
 vxc(:)=vxc(:)/(vol*float(nkbz)) 
 vxcr(:)=(0.,0.)
 vxcr(igfft(1:ngdiag,1))=vxc(1:ngdiag)
 call c3dfft(vxcr,1)
!
 end subroutine mbvxc
!
!#################################################################
 subroutine subvhartr(vhr,rho,qpg,igfft)
!#################################################################
!
! calculate the hartree potential (vhr): 
! sum_G 4pi*rho(G)/|G|²  with G!=0, for G=0 vhr(0)=0
!
!---------------------------------------------------------------
!
 use parser
 use selfm
 implicit none
!
 real rho(nr)
 complex qpg(nq,ngwmax),vhr(nr)
 integer igfft(ngwmax,g0dim) 
!
! Work space
!
 complex rhog(nr), vh(ngwmax)
!
 call section(3,'Hartree Potential','f')
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 vh(1:ngwmax)=0.
 rhog(:)=rho(:)
 call c3dfft(rhog,-1)
 vh(2:ngwmax)=((4.*pi/vol)*(rhog(igfft(2:ngwmax,1))))&
&              /qpg(1,2:ngwmax)**2.
 !
 vhr(:)=(0.,0.)
 vhr(igfft(1:ngwmax,1))=vh(1:ngwmax)
 call c3dfft(vhr,1)
!
 end subroutine subvhartr
!
!#################################################################
 subroutine plotvr(vin,a1,a2,a3,igfft,gvec,vname)
!#################################################################
!
! plot the potential (vin) or on the original fft grid or on an 
! external grid read from the file "points"
! 
!---------------------------------------------------------------
!
 use parser
 use selfm
 implicit none
!
 real a1(3),a2(3),a3(3),vin(nr),gvec(ngvec,3)
 integer igfft(ngwmax,g0dim)
 character(chlen) vname
!
! Work space
!
 integer nrrd,ir,ig,ir1,ir2,ir3
 real rvec(3)
 complex cdum(nr), vg(ngwmax),vdum
!
 if (myid.eq.0) then
  call openunit(unit3,trim(vname),'f')
  call wroutitle(unit3,'Potential in real space')
  if (sfile('points')) then
   call wrmsgout(unit3,'')
   call wrmsgout(unit3,'Grid read from file "points"')
   call wrmsgout(unit3,'       x          y          z            v')
   call openunit(unit2,'points','f')
   read (unit2,*) nrrd
   call capo('b')
   call wrmsg('b',' Plotting the potential using an external grid')
   call imsg('b', ' Number of points in the external grid:',nrrd)
   vg(1:ngwmax)=0.
   cdum(:)=vin(:)
   call c3dfft(cdum,-1)
   vg(2:ngwmax)=cdum((igfft(2:ngwmax,1)))/fftnorm**2
   do ir=1,nrrd
    vdum=(0.,0.)
    read (unit2,*) rvec(1),rvec(2),rvec(3)
    do ig=1,ngwmax
     vdum=vdum+vg(ig)*exp((0.,1.)*dot_product(2*pi*gvec(ig,:)/alat,rvec))
    end do
    write(unit3,2) rvec(1),rvec(2),rvec(3), real(vdum)*HARTREE
   end do
  else
   call wrmsgout(unit3,'')
   call  wrmsgout(unit3,'FFT Grid used')
   call wrmsgout(unit3,'       x          y          z            v')
   call capo('b')
   call wrmsg('b',' Plotting the potential using the FFT grid')
   ir = 0
   do ir1 = 0, nfft1-1
    do ir2 = 0, nfft2-1
     do ir3 = 0, nfft3-1
      ir = 1 + ir1 + ir2*nfft1 + ir3*nfft1*nfft2
      rvec(1) = ir1*a1(1)/nfft1 + ir2*a2(1)/nfft2 + ir3*a3(1)/nfft3
      rvec(2) = ir1*a1(2)/nfft1 + ir2*a2(2)/nfft2 + ir3*a3(2)/nfft3
      rvec(3) = ir1*a1(3)/nfft1 + ir2*a2(3)/nfft2 + ir3*a3(3)/nfft3
      write(unit3,2) rvec(1),rvec(2),rvec(3), vin(ir)*HARTREE
     end do
    end do
   end do
  end if

  call closeunit(trim(vname))
!
 end if

2 format(4f12.6)
!
 end subroutine plotvr

!#################################################################
 subroutine rchecksym(trans,pot,ndim,lsym)
!#################################################################
!
! checks that the real vector "pot" of dimension "ndim" 
! has the total symmetry under the transformation matrix "trans" 
! (dimension ndim*nsym). 
! Returns "lsym": true    if pass the check
!                 false   if do NOT pass the check
!
!---------------------------------------------------------------
 use parser
 use selfm
 implicit none
!
 integer ndim,trans(nsym,ndim)
 real pot(ndim)
 logical  lsym
!
!work space
!
 integer ii,ij,is
 logical trflag
!
 lsym=.true.
 do ii=1,ndim
  do is=1,nsym
   ij=trans(is,ii)
   if ((.not.trflag(is)).and.(abs(pot(ii)-pot(ij)).ge.1.E-4)) lsym=.false.
  end do
 end do
!
 end subroutine rchecksym

!######################################################################
 subroutine noscf(wfr0,en0,weight,occ,rrotm1,igfft,gvec,a1,a2,a3)
!######################################################################
!
! calculate the bands structure given the potential (read from data.vxc_in) 
! and plot it in r-space
!
!--------------------------------------------------------------------------
 use parser
 use selfm
 implicit none
#if defined MPI
 include 'mpif.h'
#endif
!
 complex wfr0(nbn,nkibz,nr)
 real weight(nkibz), en0(nbn,nkibz),occ(nbn,nkibz),gvec(ngvec,3),a1(3),a2(3),a3(3)
 integer rrotm1(nsym,nr),igfft(ngwmax,g0dim)
!
! Workspace
!
 integer ik,ib1,ib2,ir,i,n
 real, external :: vxnr, vcjas
 real vxc(nr),vxclda(nr), rho(nr), rhoin(nr), en(nbn,nkibz),kpt(ksig2,3),tester, eshift
 complex  vdif(nr),wfr(nbn,nkibz,nr), hksmat(nbn,nbn), vxchmat
 logical lcorr
#if defined MPI
 complex pwfr(nbn,nkibz,nr)
 real pen(nbn,nkibz)
#endif
!
!===========================================================================
 call section (2,'Not Self-consistent solution of the KS equation','f')
!
 if (.not.sfile('data.vxc_in')) &
& call errmsg('File data.vxc_in not found. Copy data.vxc_out in data.vxc_in')
 call readvxc(vxc,vdif,rhoin,vxclda)
 outfile(4)='out.band'
 outfile(5)='out.vxc'
 call efile(outfile(4))
 call efile(outfile(5))
 wfr(:,:,:)=(0.,0.)
 en(:,:)=0.
#if defined MPI
 psection=.true.
 pwfr(:,:,:)=(0.,0.)
 pen(:,:)=0.
#endif
!
 call capo('b')
!
! Calculate the KS matrix from the information read in data.vxc_in 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 do ik=1+myid,nkibz,ncpu
  hksmat(:,:)=(0.,0.)
  do ib1=1,nbn
   do ib2=1,ib1
    if (ib2.eq.ib1) hksmat(ib1,ib2)=en0(ib1,ik)
    vxchmat=(0.,0.)
    do ir=1,nr
     vxchmat = vxchmat + conjg(wfr0(ib1,ik,ir))&
&     *wfr0(ib2,ik,ir)*((vxc(ir)-vxclda(ir))+real(vdif(ir)))
    enddo
    hksmat(ib1,ib2)=hksmat(ib1,ib2)+vxchmat
   enddo
  enddo
!
! Diagonalize the matrix
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  call section (3,'Diagonalize the KS matrix and density','f')
  call matdiag(hksmat,en(1,ik),nbn)
!
! Wave functions (only for calculating the 'new' density)
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  do ib1=1,nbn
   do ib2=1,nbn
    wfr(ib1,ik,:)=wfr(ib1,ik,:)+wfr0(ib2,ik,:)*hksmat(ib2,ib1)
   end do
  end do
 end do
 psection=.false.

#if defined MPI
 call mpi_barrier(mpi_comm_world,ierr)
 call mpi_allreduce(wfr(1,1,1),pwfr(1,1,1),nbn*nkibz*nr,&
&                    mpi_complex,mpi_sum,mpi_comm_world,ierr)
 wfr(:,:,:)=pwfr(:,:,:)
 call mpi_allreduce(en(1,1),pen(1,1),nbn*nkibz,&
&                    mpi_real,mpi_sum,mpi_comm_world,ierr)
 en(:,:)=pen(:,:)
#endif
!
! compare the input density with the one calculated from the 
! new wave functions 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
 call density(rho,wfr,weight,occ,rrotm1,.false.)  
 tester=0.
 do ir=1,nr
  tester=tester+abs(rhoin(ir)-rho(ir))/vol
 enddo
 call rmsg('b','Difference between input and output density',tester)
!
! Bands shifted: 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
 eshift=-1000
 do ik=1,nkibz
  if (en(nbfull,ik).gt.eshift) eshift=en(nbfull,ik)
  read(unit5) (kpt(ik,i),i=1,3)
 end do
 rewind(unit5)

 call openunit(unit2,trim(outfile(4)),'f')
 call wroutitle(unit2,'Band structure')
 call  wrmsgout(unit2,'')
 call  wrmsgout(unit2,'              Kpoints                             Bands')
 call  wrmsgout(unit2,'     K_x       K_y       K_z')
 do ik=1,nkibz
  write(unit2,'(11f10.3)') kpt(ik,:), ((en(n,ik)-eshift)*HARTREE, n=1,nbfull+2)
 end do
 call closeunit(trim(outfile(4)))
!
! plot the potential
!
 call section (3,'Plot the vxc potential','f')
 call capo('b')
 ch9=trim(outfile(5))
 call plotvr(vxc,a1,a2,a3,igfft,gvec,ch9)

 end subroutine noscf
!
!#################################################################
 subroutine readvxc(vxc,vhr,rho,vxclda)
!#################################################################
!
! read from the data.vxc_in file: 
! -vxc    : the xc potential, 
! -vhr    : the difference between the Hartree potentials and the 
!       Hartree potentials corresponding to the original basis set 
! -rho    : the density (rho)
! -vxclda : the xc potential corresponding to the original basis set
!
!---------------------------------------------------------------
 use parser
 use selfm
 implicit none
!
 real vxc(nr),rho(nr),vxclda(nr)
 complex vhr(nr)
!workspace
 integer  rnr, rnkibz, rngdiag, rnbn,ir
 character(10) xctyprd ,xcapprxrd

 if (.not.sfile('data.vxc_in')) then
  write (ch1,'(3a)')' Detected ',trim(sxctyp),' input DFT states'
  call wrmsg('b',trim(ch1))
  call wrmsg('b',' A QP calculation based on non-LDA wavefunctions')
  call wrmsg('b',' does need the corresponding consistent DFT potential')
  call wrmsg('b',' provided in the "data.vxc_in" file') 
  call errmsg('Impossible to proceed without the "data.vxc_in" file')
 endif

 call capo('b')
 call wrmsg('b','Reading DFT potentials from file "data.vxc_in"')
 call capo('b')
 call openunit(unit2,'data.vxc_in','u')
#if !defined NOHEADER
 call rdheader(unit2)   
#endif
 read (unit2)  rnr, rnkibz, rngdiag, rnbn
 read (unit2)  xctyprd, xcapprxrd
 call i2msg('b', '   :: K-points range          :', 1,rnkibz)
 call i2msg('b', '   :: Bands range             :', 1,rnbn)
 call  imsg('b', '   :: NGDIAG                  :', rngdiag)
 call  amsg('b', '   :: XC-potential(type)      :', xctyprd)
 call  amsg('b', '   :: XC-potential(appr.)     :', xcapprxrd)
 if (rnr.ne.nr) then
  call wrmsg('b','The mesh of the potential is different from the one')
  call wrmsg('b','used in the present calculation: the dimension of the')
  call wrmsg('b','mesh depends on ngdiag (for -sigx run on ngwsigx')
  call errmsg('nr in "data.vxc_in" differs from nr in "self.in"')
 end if 
 read (unit2) (vxc(ir),ir=1,nr)
 read (unit2) (vhr(ir),ir=1,nr)
 read (unit2) (rho(ir),ir=1,nr)
 read (unit2) (vxclda(ir),ir=1,nr)
 call closeunit('data.vxc_in')

 end subroutine readvxc

!######################################################################
 subroutine subsig(sigx,sigc,nb1,nb2,nk3,rrotm1,igfft,wfr,grot,sigindex,en,&
&                  occ,qpg,qpgm2int,qpgm2intocc,bzvolocc,epsm1,epsm1diag,&
&                  res,pole,wvec,lOnlyX)
!######################################################################
!
! calculate the matrix element <nb1,nk3|Sigma_x|nb2,nk3> (sigx)
!                              <nb1,nk3|Sigma_c|nb2,nk3> (sigc)
! Sigma_c:
! -static screened exchange 
! -rpa (plasmon pole apprx.)
!
!---------------------------------------------------------------
 use parser
 use selfm
 implicit none
#if defined MPI
 include 'mpif.h'
#endif
!
 integer(2) sigindex(nq,nsym,nkibz,3)
 integer rrotm1(nsym,nr),igfft(ngwmax,g0dim), grot(nsym,ngwmax),nb1,nb2,nk3
 complex wfr(nbn,nkibz,nr), qpg(nq,ngwmax),sigx,sigc,wvec,&
&     epsm1diag(ngscdg,nq), epsm1(ngscbl,ngscbl,nq), &
&     res(ngppdg,ngppdg,nq), pole(ngppdg,ngppdg,nq)
 real occ(nbn,nkibz),qpgm2int(nq,ngqpgint,ngqpgint),&
&     qpgm2intocc(nbmetal,nq,nsym,ksig2,metngqpgint,metngqpgint),&
&     bzvolocc(nbmetal,nq,nsym,ksig2), en(nbn,nkibz)
 logical lOnlyX
!
!Work Space
!
 integer ngdiago, ngblock, nbnchiminrd, nbnsig,ns2, nk2, ng0, ng1, ng2,&
&        iq,is,ib3,ig,ig1,ig2
 logical lssx, lrpaxc
 complex rhotmp1(ngwsigx), rhotmp2(ngwsigx)  
 complex, allocatable :: qpgm2intmph(:,:),qpgm2intmpe(:,:) 
!--------------------------------------------------------------------------------
 
 nbnsig = nbmetal
 ngblock = 1
 ngdiago = ngwsigx
 lssx =((trim(xctyp).eq."ssx").and.(.not.lOnlyX))
 lrpaxc = ((trim(xctyp).eq."rpa").and.(.not.lOnlyX))  
 if (lssx)   ngblock = ngscbl
 if (lrpaxc) then
  ngblock = ngppbl
  nbnsig = nbn
 endif
!
 allocate(qpgm2intmph(ngblock,ngwsigx),qpgm2intmpe(ngblock,ngwsigx))
!
 sigx=(0.,0.)
 sigc=(0.,0.)
 do iq=1,nq
  do is=1,nsym
   if (sigindex(iq,is,nk3,1).ne.0) then
    nk2=sigindex(iq,is,nk3,1)
    ns2=sigindex(iq,is,nk3,2)
    ng0=sigindex(iq,is,nk3,3)
    do ib3=1,nbnsig
     call subrhotwforlife(iq,is,nb1,nk3,ib3,nk2,ns2,ng0,&
&                          rrotm1,igfft,wfr,grot,rhotmp1,ngdiago)
     if (nb1.ne.nb2) then
      call subrhotwforlife(iq,is,nb2,nk3,ib3,nk2,ns2,ng0,&
&                           rrotm1,igfft,wfr,grot,rhotmp2,ngdiago)
     else
      rhotmp2(:)=rhotmp1(:)
     end if
     call giveqpgm2int(qpgm2intmpe,qpgm2intmph,ib3,iq,is,nk3,nk2,ngblock,&
&                       ngwsigx,qpg,qpgm2int,qpgm2intocc,bzvolocc,grot,occ,&
&                       .false.)
! exchange:
     if (ib3.le.nbmetal) then 
      do ig = 1,ngwsigx
       ig1=1
       if ((ngblock.gt.1).and.(ig.le.ngblock)) ig1=ig
       sigx=sigx-&
&       2.*pi*rhotmp1(ig)*conjg(rhotmp2(ig))*qpgm2intmpe(ig1,ig)
      end do
     end if
! correlation, screened exchange:
     if (lssx) then
      do ig1 = 1,ngscbl
       do ig2 = 1,ngscbl
        sigc=sigc-&
&        2.*pi*rhotmp1(ig1)*conjg(rhotmp2(ig2))*qpgm2intmpe(ig1,ig2)*epsm1(ig1,ig2,iq)
       end do
      end do
      do ig = ngscbl+1,ngscdg
       sigc=sigc-&
&       2.*pi*rhotmp1(ig)*conjg(rhotmp2(ig))*qpgm2intmpe(1,ig)*epsm1diag(ig,iq)
      end do
     elseif (lrpaxc) then
       do ig1 = 1,ngppbl
        do ig2 = 1,ngppbl
         sigc=sigc+&
&         2.*pi*rhotmp1(ig1)*conjg(rhotmp2(ig2))*res(ig1,ig2,iq)*&
&         (qpgm2intmpe(ig1,ig2)/(conjg(wvec-en(ib3,nk2))+pole(ig1,ig2,iq))+&
&         qpgm2intmph(ig1,ig2)/(wvec-en(ib3,nk2)-pole(ig1,ig2,iq)))
       end do
      end do
      do ig =ngppbl+1,ngppdg
       sigc=sigc+&
&       2.*pi*rhotmp1(ig)*conjg(rhotmp2(ig))*res(ig,ig,iq)*&
&       (qpgm2intmpe(1,ig)/(conjg(wvec-en(ib3,nk2))+pole(ig,ig,iq))+&
&       qpgm2intmph(1,ig)/(wvec-en(ib3,nk2)-pole(ig,ig,iq)))
      end do
     end if
    end do
   end if
  end do
 end do

 deallocate(qpgm2intmph,qpgm2intmpe)

 end subroutine subsig

!####################################################################
 subroutine subceda(vxc,wfr,occ,rrotm1,igfft,grot,syminv,qpg,qpgm2int,&
&                     qpgm2intocc,bzvolocc,sigindex,kpt,b1,b2,b3,symmop,&
&                     rhoq0,epsm1,epsm1diag,res,pole,en,lkli)
!####################################################################
!
! Common Energy Denominator Approximation (and KLI) 
! for the exact exchange, screened exchange and rpa potential 
! for the latter see M.E. Casida PRA 51, 2005 (eqs. 3.12-3.14)
!
!--------------------------------------------------------------------
!
 use parser
 use selfm
 implicit none
#if defined MPI
 include 'mpif.h'
#endif
!
 complex vxc(nr),wfr(nbn,nkibz,nr),qpg(nq,ngwmax),rhoq0(rhoq0dim,nbnchi,nkibz,nsym),&
&        epsm1diag(ngscdg,nq), epsm1(ngscbl,ngscbl,nq), res(ngppdg,ngppdg,nq), &
&        pole(ngppdg,ngppdg,nq)
 integer(2) sigindex(nq,nsym,nkibz,3)
 integer rrotm1(nsym,nr),igfft(ngwmax,g0dim),grot(nsym,ngwmax),syminv(nsym)
 real occ(nbn,nkibz),qpgm2int(nq,ngqpgint,ngqpgint),weight(nkibz),&
&     bzvolocc(nbmetal,nq,nsym,ksig2), en(nbn,nkibz),&
&     qpgm2intocc(nbmetal,nq,nsym,ksig2,metngqpgint,metngqpgint),&
&     kpt(ksig2,3),symmop(3,3,nsym),b1(3),b2(3),b3(3)
 logical lkli
 character(chlen) timech
!
! Work Space
!------------
!
 real rho(nr), fac
 real(8) ltcpu1,ltcpu2
 complex chi0(ngdiag-1,ngdiag-1),vxcg(ngdiag),wxcg(ngdiag),rhoxc(ngdiag),&
&        rhotwr(nr),rhotw1(ngdiag),rhotwr1(nr),&
&        f(ngwsigx),fr(nr),&
&        vxcsym(nr),sigx,sigc,wvec,vxctmp(nr,nkibz),rhow(ngdiag,2),rhotmp(ngwsigx),&
&        rhotw(ngdiag),rhowsq(ngdiag,ngdiag)
 integer nk2, ns2, ng0,ibmin, nbnsig, ngblock,ik,iq,is,ib1,ib2,ig,ig1,ig2,ik1,ir
 logical trflag, lssx, lrpaxc
 real, allocatable :: kweight(:)
 integer, allocatable :: kstar(:,:), nk(:)
 complex, allocatable :: qpgm2intmph(:,:),qpgm2intmpe(:,:)
#if defined MPI
 complex prhoxc(ngdiag),pvxc(nr),prhowsq(ngdiag,ngdiag)
#endif
!
!--------------------------------------------------------------------------
 if (lkli) call section(3,'Updated KLI x(c)-Potential','f')
 if (.not.lkli) call section(3,'Updated CEDA x(c)-Potential','f')
!
 allocate(kweight(nkibz),nk(nkibz),kstar(nkibz,nsym))
 call stars(kpt,nkibz,symmop,b1,b2,b3,kweight,kstar,nk)

 call density(rho,wfr,kweight,occ,rrotm1,.false.) 
!
! initialize / defaults:
!
 vxctmp(:,:)=(0.,0.)
 vxc(:)=(0.,0.)
 rhowsq(:,:)=(0.,0.)
 rhoxc(:)=(0.,0.)
 ibmin=1
 ngblock = 1
 nbnsig = nbmetal
 
 lssx =(trim(xctyp).eq."ssx") 
 lrpaxc = (trim(xctyp).eq."rpa")  
 if (lssx)   ngblock = ngscbl
 if (lrpaxc) then
  ngblock = ngppbl
  nbnsig = nbn
 endif
 allocate(qpgm2intmph(ngblock,ngwsigx),qpgm2intmpe(ngblock,ngwsigx))
 call wrmsg('b','=> Vxc kpoints loop [detailed timing in status file]')
#if defined MPI
 prhoxc(:)=(0.,0.)
 prhowsq(:,:)=(0.,0.)
 pvxc(:) = (0.,0.)
 psection=.true.
#endif
 call pclock(999,'')
!
!loop on the k points (reduced volume) - parallelized
!
 do ik=1+myid,nkibz,ncpu 
!
! 'hole' part of the potential:
!
  call lcputime(ltcpu1)
  do iq=1,nq
   do is=1,nsym
    if (sigindex(iq,is,ik,1).ne.0) then
     nk2=sigindex(iq,is,ik,1)
     ns2=sigindex(iq,is,ik,2)
     ng0=sigindex(iq,is,ik,3)
     do ib1=1,nbfull
      do ib2=1,nbnsig
       call subrhotwforlife(iq,is,ib1,ik,ib2,nk2,ns2,ng0,&
&                            rrotm1,igfft,wfr,grot,rhotmp,ngwsigx)
       call giveqpgm2int(qpgm2intmpe,qpgm2intmph,ib2,iq,is,ik,nk2,ngblock,ngwsigx,& 
&                         qpg,qpgm2int,qpgm2intocc,bzvolocc,grot,occ,.false.)
       f(:)=(0.,0.)
!  Exchange:
       if (ib2.le.nbmetal) then 
        do ig=1,ngwsigx
         ig1=1
         if ((ngblock.gt.1).and.(ig.le.ngblock)) ig1=ig
         f(ig)=-2.*pi*conjg(rhotmp(ig))*qpgm2intmpe(ig1,ig) 
        end do
       end if
! Correlation, Static screening:
       if (lssx) then
        do ig1 = 1,ngscbl
         do ig2 = 1,ngscbl
          f(ig1)= f(ig1)-2.*pi*conjg(rhotmp(ig2))*qpgm2intmpe(ig1,ig2)*epsm1(ig1,ig2,iq)
         end do
        end do
        do ig= ngscbl+1,ngscdg
         f(ig)= f(ig)-2.*pi*conjg(rhotmp(ig))*qpgm2intmpe(1,ig)*epsm1diag(ig,iq)
        end do
! Correlation, RPA (quasiparticle limit, with PP):
       elseif (lrpaxc) then
        wvec = en(ib1,ik)+(0.,1.)*zero
        do ig1 = 1,ngppbl
         do ig2 = 1,ngppbl
          f(ig1)=f(ig1)+2.*pi*conjg(rhotmp(ig2))*res(ig1,ig2,iq)*&
          &(qpgm2intmpe(ig1,ig2)*1./(conjg(wvec-en(ib2,nk2))+pole(ig1,ig2,iq))+&
          &qpgm2intmph(ig1,ig2)*1./(wvec-en(ib2,nk2)-pole(ig1,ig2,iq)))
         end do
        end do
        do ig=ngppbl+1,ngppdg
         f(ig)=f(ig)+2.*pi*conjg(rhotmp(ig))*res(ig,ig,iq)*&
         &(qpgm2intmpe(1,ig)*1./(conjg(wvec-en(ib2,nk2))+pole(ig,ig,iq))+&
         &qpgm2intmph(1,ig)*1./(wvec-en(ib2,nk2)-pole(ig,ig,iq)))
        end do
       end if
       fr(:)=(0.,0.)
       fr(igfft(1:ngwsigx,grot(syminv(is),ng0))) = f(1:ngwsigx)
       call c3dfft(fr,1)
       fr(:)= fr(rrotm1(is,:))
       if (trflag(ns2)) vxctmp(:,ik)=vxctmp(:,ik)+occ(ib1,ik)*&
&       fr(:)*conjg(wfr(ib1,ik,:)*wfr(ib2,nk2,rrotm1(ns2-nsym/(inv+1),:)))
       if (.not.trflag(ns2)) vxctmp(:,ik)=vxctmp(:,ik)+occ(ib1,ik)*&
&       fr(:)*conjg(wfr(ib1,ik,:))*wfr(ib2,nk2,rrotm1(ns2,:))
      end do
     end do
    end if
   end do
  end do
  do ik1=1,nk(ik) 
   is=kstar(ik,ik1)
   do ir=1,nr
    if (.not.trflag(is)) vxc(ir) = vxc(ir) + vxctmp(rrotm1(is,ir),ik)
    if (trflag(is)) vxc(ir) = vxc(ir) + &
&    conjg(vxctmp(rrotm1(is-nsym/(inv+1),ir),ik))
   enddo
  enddo
!
! orbital products/densities part:  
!
  do ib1=1,nbfull
   if (lkli) ibmin=ib1
   do ib2=ibmin,ib1
    rhow(1:ngdiag,1:2)=(0.,0.)
    fac=.5
    if (ib1.ne.ib2) fac=1. 
    do ik1=1,nk(ik) 
     is=kstar(ik,ik1)
     if (.not.trflag(is)) rhotwr(:) = &
&     conjg(wfr(ib1,ik,rrotm1(is,:)))*wfr(ib2,ik,rrotm1(is,:))
     if (trflag(is)) rhotwr(:)= conjg(wfr(ib2,ik,rrotm1(is-nsym/(inv+1),:)))&
&     *wfr(ib1,ik,rrotm1(is-nsym/(inv+1),:))
     rhotwr1(:)=rhotwr(:)/(rho(:)*float(nfft1*nfft2*nfft3))
     rhotw(1:ngdiag)=(0.,0.)
     rhotw1(1:ngdiag)=(0.,0.)
     call c3dfft(rhotwr,1)
     rhotwr(igfft(1,1))=(0.,0.)
     if (ib1.eq.ib2) rhotwr(igfft(1,1))=(1.,0.)
     rhotw(1:ngdiag) = rhotwr(igfft(1:ngdiag,1))
     call c3dfft(rhotwr1,1)
     rhotw1(1:ngdiag) = rhotwr1(igfft(1:ngdiag,1))
     if (.not.trflag(is)) rhow(:,1) = rhow(:,1) + conjg(rhotw1(:))
     if (trflag(is)) rhow(:,2) = rhow(:,2) + conjg(rhotw1(:))
     do ig=1,ngdiag
      rhowsq(:,ig)=rhowsq(:,ig)+fac*occ(ib1,ik)*occ(ib2,ik)*&
&      conjg(rhotw1(:))*rhotw(ig)
     end do
    end do
    if (lrpaxc) wvec =  en(ib1,ik)+(0.,1.)*zero
    call subsig(sigx,sigc,ib1,ib2,ik,rrotm1,igfft,wfr,grot,sigindex,en,occ,qpg,&
&                qpgm2int,qpgm2intocc,bzvolocc,epsm1,epsm1diag,res,pole,wvec,&
&                .false.)
    rhoxc(:) = rhoxc(:) + fac*occ(ib1,ik)*occ(ib2,ik)*&
&              rhow(:,1)*(sigx+sigc)+rhow(:,2)*conjg(sigx+sigc)   
   end do
  end do
!
! Timing of this loop:
!
  call lcputime(ltcpu2)
  ch2=timech(ltcpu2-ltcpu1)
  write (ch1,'(3x,a,i3.3,a,i3.3,2a)') &
&  '[p.',myid,' K.',ik,'] Single kpoint loop component :',trim(ch2)
  call wrrep(trim(ch1))
 end do 
!
! close loop on the k points
!
#if defined MPI
 call mpi_barrier(mpi_comm_world,ierr)
 call mpi_allreduce(rhoxc(1),prhoxc(1),ngdiag,&
&                    mpi_complex,mpi_sum,mpi_comm_world,ierr)
 rhoxc(:)=prhoxc(:)
 call mpi_allreduce(vxc(1),pvxc(1),nr,&
&                    mpi_complex,mpi_sum,mpi_comm_world,ierr)
 vxc(:)=pvxc(:)
 call mpi_allreduce(rhowsq(1,1),prhowsq(1,1),ngdiag*ngdiag,&
&                    mpi_complex,mpi_sum,mpi_comm_world,ierr)
 rhowsq(:,:)=prhowsq(:,:)
#endif
 call pclock(6,'Vxc Kpoint loop')
 psection=.false.
!
 vxc(:)= vxc(:)/(rho(:)*float(nkbz))
!
 deallocate(qpgm2intmpe,qpgm2intmph,kweight,nk,kstar)
!
! FT and subtracting off-diagonal term: 
! 
 wxcg(:)=(0.,0.)
 call c3dfft(vxc,-1)
 wxcg(2:ngdiag)=vxc(igfft(2:ngdiag,1))/float(nfft1*nfft2*nfft3)
 wxcg(2:ngdiag)=wxcg(2:ngdiag)-rhoxc(2:ngdiag)/float(nkbz)
!  
! approximate chi0:
!
 chi0(:,:)=(0.,0.)
 do ig1=1,ngdiag-1
  chi0(ig1,ig1)=(1.,0.)
  chi0(ig1,1:ngdiag-1)=chi0(ig1,1:ngdiag-1)-rhowsq(ig1+1,2:ngdiag)/float(nkbz)
 end do
!
 call lapackinv(chi0,ngdiag-1)
!
! vx potential:
!
 vxcg(:)=(0.,0.)
 do ig=2,ngdiag
  vxcg(2:ngdiag)=vxcg(2:ngdiag)+chi0(1:ngdiag-1,ig-1)*wxcg(ig)
 end do

 vxc(:)=(0.,0.)
 vxc(igfft(2:ngdiag,1))=vxcg(2:ngdiag)
 call c3dfft(vxc,1)

 end subroutine subceda
!
!#################################################################
 subroutine discontinuity(rrotm1,igfft,wfr,grot,sigindex,kpt,en,&
&                         occ,qpg,qpgm2int,qpgm2intocc,bzvolocc,vxc,&
&                         epsm1,epsm1diag,res,pole)
!#################################################################
!
! Estimates the derivative discontinuity:
! Delta_xc = gap -gap_KS = (I-e_c) - (A -e_v)
! with -A = e_c +<c|Sig_xc - v_xc|c>; -I = e_v + <v|Sig_xc - v_xc|v> 
! (refs. preprint Niquet, Gonze; PRB ----- and refs therein) 
!
!---------------------------------------------------------------
 use parser
 use selfm
 implicit none
#if defined MPI
 include 'mpif.h'
#endif
!
 integer(2) sigindex(nq,nsym,nkibz,3)
 integer rrotm1(nsym,nr),igfft(ngwmax,g0dim), grot(nsym,ngwmax)
 complex wfr(nbn,nkibz,nr), qpg(nq,ngwmax),res(ngppdg,ngppdg,nq),&
&        pole(ngppdg,ngppdg,nq),epsm1(ngscbl,ngscbl,nq),&
&        epsm1diag(ngscdg,nq)
 real occ(nbn,nkibz),en(nbn,nkibz),qpgm2int(nq,ngqpgint,ngqpgint),&
& qpgm2intocc(nbmetal,nq,nsym,ksig2,metngqpgint,metngqpgint),&
& bzvolocc(nbmetal,nq,nsym,ksig2),kpt(ksig2,3), vxc(nr)
!
! Work Space
!------------
!
 integer ik,ir
 complex sig, sigcor,sigx, wvec
 real sigv(nkibz), sigc(nkibz), vxcc(nkibz), vxcv(nkibz)
#if defined MPI
 real psigv(nkibz), psigc(nkibz), pvxcc(nkibz), pvxcv(nkibz)
#endif
!--------------------------------------------------------------------

 call section(2,'Vxc discontinuity','f')
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 vxcv(:) = 0.
 vxcc(:) = 0.
 sigv(:) = 0.
 sigc(:) = 0.
#if defined MPI
 pvxcv(:) = 0.
 pvxcc(:) = 0.
 psigv(:) = 0.
 psigc(:) = 0.
 psection=.true.
#endif

 do ik=1+myid,nkibz,ncpu
  wvec=en(nbfull,ik)+(0.,1.)*zero
  call subsig(sigx,sigcor,nbfull,nbfull,ik,rrotm1,igfft,wfr,grot,sigindex,en,&
&             occ,qpg,qpgm2int,qpgm2intocc,bzvolocc,epsm1,epsm1diag,&
&                  res,pole,wvec,.false.)
  sig=sigx+sigcor
  sigv(ik)=real(sig)
  wvec=en(nbfull+1,ik)+(0.,1.)*zero
  call subsig(sigx,sigcor,nbfull+1,nbfull+1,ik,rrotm1,igfft,wfr,grot,sigindex,en,&
&             occ,qpg,qpgm2int,qpgm2intocc,bzvolocc,epsm1,epsm1diag,&
&             res,pole,wvec,.false.)
  sig=sigx+sigcor
  sigc(ik)=real(sig)
  do ir=1,nr 
   vxcv(ik) = vxcv(ik)+conjg(wfr(nbfull,ik,ir))*wfr(nbfull,ik,ir)*vxc(ir)
   vxcc(ik) = vxcc(ik)+conjg(wfr(nbfull+1,ik,ir))*wfr(nbfull+1,ik,ir)*vxc(ir)
  enddo
 end do
 psection=.false.
!
#if defined MPI
 call mpi_barrier(mpi_comm_world,ierr)
 call mpi_allreduce(vxcv(1),pvxcv(1),nkibz,mpi_complex,&
&                    mpi_sum,mpi_comm_world,ierr)
 vxcv(:)=pvxcv(:)
 call mpi_allreduce(vxcc(1),pvxcc(1),nkibz,mpi_complex,&
&                    mpi_sum,mpi_comm_world,ierr)
 vxcc(:)=pvxcc(:)
 call mpi_allreduce(sigc(1),psigc(1),nkibz,mpi_complex,&
&                    mpi_sum,mpi_comm_world,ierr)
 sigc(:)=psigc(:)
 call mpi_allreduce(sigv(1),psigv(1),nkibz,mpi_complex,&
&                    mpi_sum,mpi_comm_world,ierr)
 sigv(:)=psigv(:)
#endif

 if (myid.eq.0) then
  call capo ('r')
  call wrmsg('r','Estimate of the derivative discontinuity following the prescription')
  call wrmsg('r','in the preprint Niquet-Gonze. For EXX see also Gorling-Levy(to add)')
  call wrmsg('r','Definitions:')
  call wrmsg('r','             I = -(e_v + <v|Sig_xc - v_xc|v>),    Ionization energy')
  call wrmsg('r','             A = -(e_c + <c|Sig_xc - v_xc|c>),  Electronic affinity')
  call wrmsg('r','             Gap = I - A                 Estimate of the energy gap')
  call wrmsg('r','             KS gap = e_c - e_v        KS orbital energy difference')
  call wrmsg('r','             Delta = Gap - KS gap,         Derivative discontinuity')
  call wrmsg('r','-------------------------------------------------------------------')
  call wrmsg('r','Kpoint     -I          -A          KS gap       Gap          Delta ')
  call wrmsg('r','-------------------------------------------------------------------')
  do ik =1,nkibz
   write(11,2) &
&   ik,(en(nbfull,ik)+(sigv(ik)-vxcv(ik)))*HARTREE,&
&   (en(nbfull+1,ik)+(sigc(ik)-vxcc(ik)))*HARTREE,&
&   (en(nbfull+1,ik)-en(nbfull,ik))*HARTREE,&
&   (en(nbfull+1,ik)-en(nbfull,ik)+(sigc(ik)-vxcc(ik))-&
&   (sigv(ik)-vxcv(ik)))*HARTREE,&
&   ((sigc(ik)-vxcc(ik))-(sigv(ik)-vxcv(ik)))*HARTREE
  end do
  call wrmsg('r','-------------------------------------------------------------------')
 end if

2 format (3x,i3,f12.4,1x,f12.4,1x,f12.4,1x,f12.4,1x,f12.4)

 end subroutine discontinuity
!
!##########################################################
 subroutine reportenergies(mode,msg,ekin,e1el,eh,ex,ec,exc)
!##########################################################
!
! writes the energy report
! 
!---------------------------------------------------------------
! 
 use selfm
 implicit none
!
 real ekin,e1el,eh,ex,ec, exc
 character(1) mode
 character msg*(*)
!
 if (myid.gt.0.and.(.not.psection)) return
!
 call capo(mode)
 call wrmsg(mode,'----------------------------------------------------')
 write (ch1,'(2a)') trim(msg),' [a.u.]:'
 call wrmsg(mode,trim(ch1))
 call wrmsg(mode,'----------------------------------------------------')
 call rmsg(mode,                       'KS kinetic Energy            :', ekin)
 call rmsg(mode,                       'Ion-electron energy          :', 2.*e1el-ekin)
 call rmsg(mode,                       'KS 1-electron energy         :', 2.*e1el)
 call rmsg(mode,                       'Hartree Energy               :', .5*eh)
 if (abs(ex).gt.1.e-4)  call rmsg(mode,'Exchange Energy              :', ex)
 if (abs(ec).gt.1.e-4)  call rmsg(mode,'Correlation Energy           :', ec)
 if (abs(exc).gt.1.e-4) call rmsg(mode,'Exchange-Correlation Energy  :', exc)
 call rmsg(mode,                       'Total electronic energy      :', 2.*e1el+.5*eh+ex+ec)
 call wrmsg(mode,'----------------------------------------------------')
 call capo(mode)
 
 end subroutine reportenergies
!
!###################################################################
 subroutine rdepsm1fordft(epsm1diag,epsm1,iq)
!###################################################################!
! Read epsilon-1 file (static):
!
! If ngscdg (dimen.par) is 1 read only the dimensions and pass them 
! into ngblkrd,ngdiagrd. Else read the matrix elements and pass them
! into epsm1diag,epsm1 (for a given iq)	
!
!-------------------------------------------------------------
 use selfm
 implicit none
#if defined MPI
 include 'mpif.h'
#endif 
!
 complex epsm1diag(ngscdg), epsm1(ngscbl,ngscbl)
!
!Work Space
!
 integer  nbnchiminrd,iq,ig
 complex  Wdum
 real qptrd(nq,3)
!
 if (ngscdg.eq.1) then 
  call openunit(unit1,'data.epsm1s','u')
#if !defined NOHEADER
  call rdheader(unit1)   
#endif 
  read (unit1) ngblkrd,ngdiagrd,ngwfcrd,nbnchiminrd,nbnchird,nqrd,nwWrd,&
  &              Wdum,qptrd(:,:),nkbzgrid
 ch7='Parameters Static Inverse Dielectric Matrix'
 call menu8(11,2)
 call menu8(6,2)
 return
!
!Dimensions compatibility (to add!)
!
 else
  ! Pay attention it could be that the file is not open at this point!!! to fix.
  read (unit1) epsm1diag(1:ngscdg)
  do ig=1,ngscbl
   read(unit1) epsm1(ig,1:ngscbl)
  end do
 end if
 end subroutine rdepsm1fordft
!
!###############################################################
 subroutine rdplasmonpolefordft(pole,res,iq)
!###############################################################
!
!Read epsilon-1 file (plasmon-pole parametrisation):
!
! If ngppdg (dimen.par) is 1 read only the dimensions and pass them 
! into ngblkrd,ngdiagrd. Else read the matrix elements and pass them
! into epsm1diag,epsm1 (for a given iq)	
!
!-------------------------------------------------------------
 use selfm
 implicit none
#if defined MPI
 include 'mpif.h'
#endif 
!
 complex res(ngppdg,ngppdg),pole(ngppdg,ngppdg)
!
!Work Space
!
 real qptrd(nq,3)
 integer check1,check2,check3,PPflag,Wrepflag,iq,ig1,ig2,ig
 complex w1,w2,cdum
!
 if (ngppdg.eq.1) then
  call openunit(unit2,'data.pp','u')
#if !defined NOHEADER
  call rdheader(unit2)   
#endif
  read (unit2) nqrd,ngblkrd,ngdiagrd,ngwfcrd,nbminrd,nbnchird,qptrd(:,:),ehchimin,ehchimax
  read (unit2) w1
  read (unit2) w2
  call menu6(11,4,w1,w2)
  call menu6(6,4,w1,w2)
  return
!
!Dimensions compatibility (to add!)
!
 else
! Pay attention it could be that the file is not open at this point!!! to fix.
  read(unit2) &
&  ((pole(ig1,ig2),ig2=1,ngppbl),ig1=1,ngppbl),(pole(ig,ig),ig=ngppbl+1,ngppdg)
  read(unit2) &
&  ((res(ig1,ig2),ig2=1,ngppbl),ig1=1,ngppbl),(res(ig,ig),ig=ngppbl+1,ngppdg)
 endif

!
 end subroutine rdplasmonpolefordft
!
!###############################################################
 subroutine writeselfs(wfr,en,tester,niter,abhdr,igfft)
!###############################################################
!
! write the new wave function, energies in self.wfc, self.gen
!
!---------------------------------------------------------------
 use a2sm
 use selfm
 implicit none
!
#if defined MPI
 include 'mpif.h'
#endif 
!
 complex wfr(nbn,nkibz,nr),wftmp(nr)
 real en(nbn,nkibz),tester
 integer igfft(ngwmax,g0dim),niter
 type(abhdr_type) :: abhdr
!
! Work space
!
 integer ik,ib,i1,i2,i3,i,j,ig,is
 complex wfc(nbn,nkibz,nr)
 character(chlen) fname
!
! Temporary self.gen variables
!
 real a1(3),a2(3),a3(3),symmop(3,3,nsym),gvec(ngvec,3),kpt(nkibz,3)
 integer igk(ngvec,nkibz),ngw(nkibz),lnsym
!
! Date
!
 integer clocksecnum(3),walldate(2,6)
 character(chlen) chdate(2)

 common/clocksec/clocksecnum,chdate,walldate
!
!---------------------------------------------------------------
 call section(2,'Writing new SELF input files.','f')
!---------------------------------------------------------------
!
 call section(3,'FFT: real 2 reciprocal space transformation.','f')
!##################################################################
 do ik=1,nkibz
  do ib=1,nbn
   wftmp(:)=wfr(ib,ik,:)/fftnorm
   call c3dfft(wftmp,-1)
   wfc(ib,ik,1:ngwfc)=wftmp(igfft(1:ngwfc,1))
  enddo
 enddo

 if (xcapprx=='') write (fname,'(4a)') 'self.gen_',trim(xctyp)
 if (xcapprx/='') write (fname,'(4a)') 'self.gen_',trim(xctyp),'-',trim(xcapprx)
 write (ch1,'(3a)') 'New ',trim(fname),' [self.gen format] file'
 call section(3,trim(ch1),'t')

!##################################################################

 call openunit(unit1,'self.gen','u')
 lnsym=nsym/(inv+1)
 read(unit1)
 read(unit1) alat
 read(unit1) a1
 read(unit1) a2
 read(unit1) a3
 read(unit1)
 read(unit1)(((symmop(i1,i2,i3),i1=1,3),i2=1,3),i3=1,lnsym)
 read(unit1) ((gvec(i,j),j=1,3),i=1,ngvec)
 do ik=1,nkibz
  read (unit1) (kpt(ik,j),j=1,3)
  read (unit1) ngw(ik)
  read (unit1) (igk(ig,ik),ig=1,ngw(ik)) 
  read (unit1) 
  read (unit1) 
 enddo
 call closeunit('self.gen')
 call openunit(unit1,trim(fname),'u')
#if defined A2SNOTEMP
 write (unit1) nkibz,ngvec,lnsym,int(nel)
#else
 write (unit1) nkibz,ngvec,lnsym,int(nel),nbn,Tel
#endif
 write (unit1) alat
 write (unit1) a1(:)
 write (unit1) a2(:)
 write (unit1) a3(:)
 write (unit1) inv
 write (unit1) (((symmop(i1,i2,is),i1=1,3),i2=1,3),is=1,lnsym)
 write (unit1) ((gvec(i1,i2),i2=1,3),i1=1,ngvec)
 do ik=1,nkibz
  write (unit1) kpt(ik,:)
  write (unit1) ngw(ik)
  write (unit1) igk(1:ngw(ik),ik)
  write (unit1) nbn
  write (unit1) en(:,ik)
 enddo

!##################################################################
! Taken from abgsrep.F [A2S]:
!
! SELF xc-potential types.
!
! ixctlt(50+1)='Exact-Exchange.'
! ixctlt(51+1)='Exact-Exchange + LDA correlation.'
! ixctlt(52+1)='Statically screened exchange.'
! ixctlt(53+1)='Dynamically screened exchange.'
! ixctlt(54+1)='Exact-Exchange. Krieger Li Iafrate approximation.'
! ixctlt(55+1)='Exact-Exchange + LDA correlation. Krieger Li Iafrate approximation.'
! ixctlt(56+1)='Statically screened exchange. Krieger Li Iafrate approximation.'
! ixctlt(57+1)='Dynamically screened exchange. Krieger Li Iafrate approximation.'
! ixctlt(58+1)='Exact-Exchange. Common Energy Denominator Approximation.'
! ixctlt(59+1)='Exact-Exchange + LDA correlation. Common Energy Denominator Approximation.'
! ixctlt(60+1)='Statically screened exchange. Common Energy Denominator Approximation.'
! ixctlt(61+1)='Dynamically screened exchange. Common Energy Denominator Approximation.'
!=====================================================================================
 if (abhdr%ixc==-1) goto 1
 if (trim(xctyp).eq."exx") abhdr%ixc=50
 if (trim(xctyp).eq."exxc") abhdr%ixc=51
 if (trim(xctyp).eq."ssx") abhdr%ixc=52
 if (trim(xctyp).eq."rpa") abhdr%ixc=53
 if (trim(xcapprx).eq."kli") abhdr%ixc=abhdr%ixc+4
 if (trim(xcapprx).eq."ceda") abhdr%ixc=abhdr%ixc+8

 abhdr%date=walldate(1,3)*10000+walldate(1,2)*100+walldate(1,1)

 abhdr%tester=tester

 abhdr%niter=niter

 abhdr%codvsn=chver(9:14)

 abhdr%ngfft(1)=nfft1
 abhdr%ngfft(2)=nfft2
 abhdr%ngfft(3)=nfft3

 write (unit1) 'GS vars'
 write (unit1) abhdr%codvsn,abhdr%date,abhdr%natom,abhdr%ngfft(1:3),&
&              abhdr%nspden,abhdr%nspinor,abhdr%ecut,abhdr%ntypat
 write (unit1) abhdr%ixc,abhdr%npsp,abhdr%tester,abhdr%niter
 do i1=1, abhdr%npsp
  write (unit1) abhdr%title(i1)
  write (unit1) abhdr%znuclpsp(i1)
  write (unit1) abhdr%zionpsp(i1)
  write (unit1) abhdr%pspso(i1)
  write (unit1) abhdr%pspdat(i1)
  write (unit1) abhdr%pspcod(i1)
  write (unit1) abhdr%pspxc(i1)
 enddo
1 call closeunit(trim(fname))

 if (xcapprx=='') write (fname,'(4a)') 'self.wfc_',trim(xctyp)
 if (xcapprx/='') write (fname,'(4a)') 'self.wfc_',trim(xctyp),'-',trim(xcapprx)
 write (ch1,'(3a)') 'New ',trim(fname),' [self.wfc format] file'
 call section(3,trim(ch1),'t')
!##################################################################
 call openunit(unit1,trim(fname),'u')
 do ik=1,nkibz
  write (unit1) (( wfc(ib,ik,ig),ib=1,nbn),ig=1,ngw(ik))
 enddo
 call closeunit(trim(fname))
!
#if defined MPI
 call mpi_barrier(mpi_comm_world,ierr)
#endif
!
 end subroutine writeselfs
