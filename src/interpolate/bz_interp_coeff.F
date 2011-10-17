!
! Copyright (C) 2000-2010 C. Attaccalite and the YAMBO team 
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
  IMPLICIT NONE
  !
  INTEGER,PARAMETER         :: IBLOCK=128
  REAL(SP),PARAMETER        :: C1=0.75_SP,C2=0.75_SP
  INTEGER,INTENT(IN)        :: ndim
  type(bz_samp), intent(in) :: Xk
  real(SP), intent(in)      :: R1D(ndim,Xk%nibz)
  real(SP), intent(inout)   :: engre(nwave,ndim)

 ! local vars
  INTEGER                 :: i,k,nw,inf,ibb,inw,idimk
  INTEGER                 :: nst,nkpt
  REAL(SP)                 :: metten1_dir(3,3)
  REAL(SP)                 :: rnot2,xx2
  REAL(SP)                 :: spw
  REAL(SP)                 :: stg(3,nsym),Rdum(3)
  REAL(SP), allocatable    :: spwn(:)
  REAL(SP), allocatable    :: spw1(:,:), spw2(:,:)
  REAL(SP), allocatable    :: lambda(:,:), hmat(:,:)
  REAL(SP), allocatable    :: rho(:), ipiv(:)
  real(SP), parameter      :: twopi=2._SP*pi
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
  metten1_dir=MATMUL(a,TRANSPOSE(a))
  !
  !  prepare vector of energies in complex array 
  forall(i=1:ndim,k=1:nkpt-1) lambda(k,i)=R1D(i,k)-R1D(i,nkpt)
  !
  !  prepare array of factors rho for smoothing 
  rho(1) = 0._SP
  rnot2 = 1._SP/DOT_PRODUCT(lattice_vectors(1:3,2),MATMUL(lattice_vectors(1:3,2),metten1_dir(1:3,1:3)))
  rho(2)= 1._SP/((1._SP-C1)**2 + C2)
  !
  DO nw=3,nwave 
     xx2 = rnot2*DOT_PRODUCT(lattice_vectors(1:3,nw),MATMUL(lattice_vectors(1:3,nw),metten1_dir(1:3,1:3)))
     rho(nw)=1._SP/((1._SP-C1*xx2)**2 + C2*xx2**3)
  ENDDO
  !  prepare matrix H(I,J) 
  hmat=0._SP
  DO ibb=0,(nwave-1)/iblock
     spw1=0._SP
     idimk=MIN(iblock,nwave-iblock*ibb)
     DO inw=1,idimk
        nw=inw+iblock*ibb
        Rdum(1:3)=lattice_vectors(1:3,nw)
        CALL stern1(Rdum,nsym,int_sop,nst,stg)
        DO i=1,nst
          forall(k=1:nkpt) spw1(k,inw)=spw1(k,inw)+COS(TWOPI*DOT_PRODUCT(Xk%pt(k,1:3),stg(1:3,i)))
        ENDDO
        spw1(:,inw)=spw1(:,inw)/nst
     ENDDO
     !
     forall(k=1:nkpt-1,inw=1:idimk) spw1(k,inw)=spw1(k,inw)-spw1(nkpt,inw)
     !
     DO inw=1,idimk
        DO k=1,nkpt-1
           nw=inw+iblock*ibb
           spw2(inw,k)=spw1(k,inw)*rho(nw)
        ENDDO
     ENDDO
#if defined _DOUBLE
     CALL dgemm('N','N',nkpt-1,nkpt-1,idimk,1._SP,spw1,nkpt,spw2,IBLOCK,1._SP,hmat,nkpt-1)
#else
     CALL sgemm('N','N',nkpt-1,nkpt-1,idimk,1._SP,spw1,nkpt,spw2,IBLOCK,1._SP,hmat,nkpt-1)
#endif
     !                   M       N     K        A    LDA  B    LDB        C    LDC
  ENDDO     

!  print*,'xx',SUM(hmat)
!  stop
#if defined _DOUBLE
  CALL DGETRF(nkpt-1,nkpt-1,hmat,nkpt-1,ipiv,inf)
#else
  CALL SGETRF(nkpt-1,nkpt-1,hmat,nkpt-1,ipiv,inf)
#endif
  !
  if(inf/=0) stop 'error in factorization'
  !
  ! Solve all the equations for all the bands at once (eq. 10 of Ref 1 = PRB 38 2721)
  !
#if defined _DOUBLE
  CALL DGETRS('N',nkpt-1,ndim,hmat,nkpt-1,ipiv,lambda,nkpt-1,inf)
#else
  CALL SGETRS('N',nkpt-1,ndim,hmat,nkpt-1,ipiv,lambda,nkpt-1,inf)
#endif
  !
  engre=0._SP
  !
  DO nw=1,nwave
     Rdum(1:3)=lattice_vectors(1:3,nw)
     CALL stern1(Rdum,nsym,int_sop,nst,stg)
     spwn(nw)=sum(cos(TWOPI*(Xk%pt(k,1)*stg(1,1:nst)+Xk%pt(k,2)*stg(2,1:nst)+Xk%pt(k,3)*stg(3,1:nst))))
     spwn(nw)=spwn(nw)/nst
  ENDDO
  !
  DO nw=2,nwave
     Rdum(1:3)=lattice_vectors(1:3,nw)
     CALL stern1(Rdum,nsym,int_sop,nst,stg)
     DO K=1,nkpt-1
        spw=sum(cos(TWOPI*(Xk%pt(k,1)*stg(1,1:nst)+Xk%pt(k,2)*stg(2,1:nst)+Xk%pt(k,3)*stg(3,1:nst))))
        spw=spw/nst
        forall(i=1:ndim) engre(nw,i)=engre(nw,i)+ lambda(k,i)*(spw-spwn(nw))*rho(nw)
     ENDDO
  ENDDO
  !
  DO i=1,ndim
    engre(1,i)=engre(1,i)+sum(engre(2:nwave,i)*spwn(2:nwave))
    engre(1,i)=R1D(i,nkpt)-engre(1,i)
  ENDDO
  !
  CALL kcomp(Xk,R1D,engre,ndim)
  !
  deallocate(ipiv)
  deallocate(rho)
  deallocate(hmat)
  deallocate(lambda)
  deallocate(spw1)
  deallocate(spw2)
  deallocate(spwn)

end subroutine bz_interp_coeff


SUBROUTINE kcomp(Xk,R1D,engre,ndim)
  use units,          ONLY:HA2EV
  use pars,           ONLY:SP
  use R_lattice,      ONLY:bz_samp
  use interpolate,    ONLY:lattice_vectors,nwave,STERN1,int_sop
  !
  IMPLICIT NONE
  type(bz_samp), intent(in) :: Xk
  INTEGER,INTENT(IN)      :: ndim
  REAL(SP),INTENT(IN)     :: engre(nwave,ndim)
  real(SP),intent(in)     :: R1D(ndim,Xk%nibz)
  !
  ! Work Space
  !
  INTEGER                  :: ikpt,iband
  REAL(SP), allocatable    :: ene(:,:)
  !
  allocate (ene(ndim,Xk%nibz))
  !
  call fourier_interpolation(Xk,ene,engre,ndim)
  !
  DO ikpt=1,Xk%nibz
     DO iband=1,ndim
       IF(ABS(ene(iband,ikpt)-R1D(iband,ikpt))>.0001) THEN
           WRITE(6,*) 'WARNING'              
       WRITE(6,'(A,3e14.4,A,i4,2(A,f14.6))') 'K=',Xk%pt(ikpt,1:3),' band:',iband, &
&                       ' ~e:',ene(iband,ikpt)*HA2EV,' e',R1D(iband,ikpt)*HA2EV
           WRITE(6,*) ' Increase LPFAC'
        ENDIF
     ENDDO
  ENDDO
  deallocate (ene)
end subroutine kcomp
