!
! Copyright (C) 2000-2012 C. Attaccalite and the YAMBO team 
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
subroutine bz_interp_coeff(Xk,R1D,engre,ndim)
  ! Interpolation scheme (PRB 38 p2721)
  ! Aug2002 Georg Madsen : First version based on subroutine from D.Singh
  ! Apr2004 Georg Madsen : blocked dgemm version
  ! Code take from BoltzTrap
  ! http://www.icams.de/content/departments/ams/madsen/boltztrap.html
  !
  use pars,           ONLY:SP,pi
  use interpolate,    ONLY:nwave,lattice_vectors,lpfac,stern1,int_sop
  use D_lattice,      ONLY:nsym,a
  use R_lattice,      ONLY:bz_samp
  use stderr,         ONLY:intc
  implicit none
  !
  integer,parameter         :: iblock=128
  real(sp),parameter        :: C1=0.75_SP,C2=0.75_SP
  integer,intent(in)        :: ndim
  type(bz_samp), intent(in) :: xk
  real(sp), intent(in)      :: r1d(ndim,xk%nibz)
  real(sp), intent(inout)   :: engre(nwave,ndim)
  !
  ! Work Space
  !
  integer                  :: i,k,nw,inf,ibb,inw,idimk,i1
  integer                  :: nst,nkpt
  real(SP)                 :: metten1_dir(3,3)
  real(SP)                 :: rnot2,xx2
  real(SP)                 :: spw
  real(SP)                 :: stg(3,nsym),rdum(3)
  real(SP), allocatable    :: spwn(:)
  real(SP), allocatable    :: spw1(:,:), spw2(:,:)
  real(SP), allocatable    :: lambda(:,:), hmat(:,:)
  real(SP), allocatable    :: rho(:), ipiv(:)
  real(SP), parameter      :: twopi=2._sp*pi
  !
  nkpt=Xk%nibz
  !
  allocate(ipiv(nkpt-1))
  allocate(rho(nwave))
  allocate(hmat(nkpt-1,nkpt-1))
  allocate(lambda(nkpt-1,ndim))
  allocate(spw1(nkpt,IBLOCK))
  allocate(spw2(IBLOCK,nkpt))
  allocate(spwn(nwave))
  !
  metten1_dir=matmul(a,transpose(a))
  !
  !  prepare vector of energies in complex array 
  forall(i=1:ndim,k=1:nkpt-1) lambda(k,i)=R1D(i,k)-R1D(i,nkpt)
  !
  !  prepare array of factors rho for smoothing 
  rho(1)= 0._SP
  rnot2 = 1._SP/dot_product(lattice_vectors(1:3,2),matmul(lattice_vectors(1:3,2),metten1_dir(1:3,1:3)))
  rho(2)= 1._SP/((1._SP-C1)**2 + C2)
  !
  do nw=3,nwave 
     xx2 = rnot2*dot_product(lattice_vectors(1:3,nw),matmul(lattice_vectors(1:3,nw),metten1_dir(1:3,1:3)))
     rho(nw)=1._SP/((1._SP-C1*xx2)**2 + C2*xx2**3)
  enddo
  !  prepare matrix H(I,J) 
  hmat=0._SP
  do ibb=0,(nwave-1)/iblock
     spw1=0._SP
     idimk=min(iblock,nwave-iblock*ibb)
     do inw=1,idimk
        nw=inw+iblock*ibb
        Rdum(1:3)=lattice_vectors(1:3,nw)
        call stern1(rdum,nsym,int_sop,nst,stg)
        do i=1,nst
          forall(k=1:nkpt) spw1(k,inw)=spw1(k,inw)+cos(twopi*dot_product(Xk%pt(k,1:3),stg(1:3,i)))
        enddo
        spw1(:,inw)=spw1(:,inw)/nst
     enddo
     !
     forall(k=1:nkpt-1,inw=1:idimk) spw1(k,inw)=spw1(k,inw)-spw1(nkpt,inw)
     !
     do inw=1,idimk
        do k=1,nkpt-1
           nw=inw+iblock*ibb
           spw2(inw,k)=spw1(k,inw)*rho(nw)
        enddo
     enddo
#if defined _DOUBLE
     call dgemm('N','N',nkpt-1,nkpt-1,idimk,1._SP,spw1,nkpt,spw2,iblock,1._SP,hmat,nkpt-1)
#else
     call sgemm('N','N',nkpt-1,nkpt-1,idimk,1._SP,spw1,nkpt,spw2,iblock,1._SP,hmat,nkpt-1)
#endif
     !                   M       N     K        A    LDA  B    LDB        C    LDC
  enddo     
  !
#if defined _DOUBLE
  call dgetrf(nkpt-1,nkpt-1,hmat,nkpt-1,ipiv,inf)
#else
  call sgetrf(nkpt-1,nkpt-1,hmat,nkpt-1,ipiv,inf)
#endif
  !
  if(inf/=0) stop 'error in factorization '
  !
  ! Solve all the equations for all the bands at once (eq. 10 of Ref 1 = PRB 38 2721)
  !
#if defined _DOUBLE
  call dgetrs('N',nkpt-1,ndim,hmat,nkpt-1,ipiv,lambda,nkpt-1,inf)
#else
  call sgetrs('N',nkpt-1,ndim,hmat,nkpt-1,ipiv,lambda,nkpt-1,inf)
#endif
  !
  if(inf/=0) stop 'error in diagonalization '
  !	  
  engre=0._SP
  !
  do nw=1,nwave
     Rdum(1:3)=lattice_vectors(1:3,nw)
     call stern1(Rdum,nsym,int_sop,nst,stg)
     spwn(nw)=sum(cos(twopi*(Xk%pt(nkpt,1)*stg(1,1:nst)+Xk%pt(nkpt,2)*stg(2,1:nst) &
&                           +Xk%pt(nkpt,3)*stg(3,1:nst))))
     spwn(nw)=spwn(nw)/nst
  enddo
  !
  do nw=2,nwave
     Rdum(1:3)=lattice_vectors(1:3,nw)
     call stern1(Rdum,nsym,int_sop,nst,stg)
     do k=1,nkpt-1
        spw=sum(cos(twopi*(Xk%pt(k,1)*stg(1,1:nst)+Xk%pt(k,2)*stg(2,1:nst)+Xk%pt(k,3)*stg(3,1:nst))))
        spw=spw/nst
        forall(i=1:ndim) engre(nw,i)=engre(nw,i)+ lambda(k,i)*(spw-spwn(nw))*rho(nw)
     enddo
  enddo
  !
  do i=1,ndim
    engre(1,i)=engre(1,i)+sum(engre(2:nwave,i)*spwn(2:nwave))
    engre(1,i)=R1D(i,nkpt)-engre(1,i)
  enddo
  !
  call kcomp(Xk,R1D,engre,ndim)
  !
  deallocate(ipiv)
  deallocate(rho)
  deallocate(hmat)
  deallocate(lambda)
  deallocate(spw1)
  deallocate(spw2)
  deallocate(spwn)

end subroutine bz_interp_coeff


subroutine kcomp(Xk,R1D,engre,ndim)
  use units,          ONLY:HA2EV
  use pars,           ONLY:SP
  use R_lattice,      ONLY:bz_samp
  use interpolate,    ONLY:nwave
  !
  implicit none
  type(bz_samp), intent(in) :: Xk
  integer,intent(in)        :: ndim
  real(SP),intent(in)       :: engre(nwave,ndim)
  real(SP),intent(in)       :: R1D(ndim,Xk%nibz)
  !
  ! Work Space
  !
  integer                  :: ikpt,iband
  real(SP), allocatable    :: ene(:,:)
  !
  allocate (ene(ndim,Xk%nibz))
  !
  call fourier_interpolation(Xk,ene,engre,ndim)
  !
  do ikpt=1,Xk%nibz
     do iband=1,ndim
       if(abs(ene(iband,ikpt)-R1D(iband,ikpt))>.0001) then
           write(6,*) 'WARNING'              
       write(6,'(A,3e14.4,A,i4,2(A,f14.6))') 'K=',Xk%pt(ikpt,1:3),' band:',iband, &
&                       ' ~e:',ene(iband,ikpt)*HA2EV,' e',R1D(iband,ikpt)*HA2EV
           write(6,*) ' Increase LPFAC'
        endif
     enddo
  enddo
  deallocate (ene)
end subroutine kcomp
