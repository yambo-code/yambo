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
subroutine bz_interp_setup(Xk)
 !
 ! Code taken from BolzTraP
 ! http://www.icams.de/content/departments/ams/madsen/boltztrap.html
 !
 use pars,           ONLY:SP,pi
 use interpolate,    ONLY:nwave,lattice_vectors,lpfac,STERN1,int_sop
 use D_lattice,      ONLY:nsym,DL_vol,a,dl_sop
 use R_lattice,      ONLY:bz_samp,b
 use vec_operate,    ONLY:sort
 use matrix_operate, ONLY:m3inv
 !
 implicit none
 type(bz_samp) :: Xk
 !
 !Work Space
 !
 real(SP)                  :: sphere_radius,metten(3,3),a_inv(3,3),b_inv(3,3)
 real(SP), POINTER         :: absk(:)
 real(SP)                  :: absm,m2(3),kdum(3),stg(3,nsym)
 integer                   :: i,j,i1,i2,i3,is
 integer                   :: ind,nradm,irad,istart,islut
 integer                   :: kmax1(3)
 integer                   :: m(3),nst
 real(SP), allocatable     :: radii(:),tmp_vec(:,:)
 integer,  allocatable     :: icnt(:),ihkl1(:,:),ihkl2(:,:)
 logical                   :: addvec,kdelta
 !
 sphere_radius=(Xk%nibz*nsym*lpfac*3.*DL_vol/4./pi)**(1./3.)
 !
 allocate(int_sop(3,3,nsym))
 !
 do i1=1,3
   kmax1(i1)=sphere_radius*sqrt(dot_product(a(i1,:),a(i1,:)))+1
 enddo
 !
 call m3inv(transpose(a),a_inv)
 call m3inv(b,b_inv)
 !
 do is=1,nsym
   !
   int_sop(:,:,is)=dl_sop(:,:,is)*(2._SP*pi)
   int_sop(:,:,is)=matmul(a_inv,int_sop(:,:,is))
   int_sop(:,:,is)=matmul(int_sop(:,:,is),b_inv)
   !
 enddo
!     WRITE(6,*) ' KXMAX,KYMAX,KZMAX',kmax1(1:3)
!     WRITE(6,*) ' Sphere_radius',sphere_radius
 !
 metten=matmul(a,transpose(a))
 nradm=PRODUCT(2*kmax1+1)
 ALLOCATE(icnt(nradm),radii(nradm),ihkl1(3,nradm))
 irad=0
 DO I3=-kmax1(3),kmax1(3)
   M(3)=I3
   DO I2=-kmax1(2),kmax1(2)
     M(2)=I2
     DO I1=-kmax1(1),kmax1(1)
       M(1)=I1
       m2=m
       absm=DOT_PRODUCT(m2(1:3),MATMUL(m2(1:3),metten(1:3,1:3)))
       absm=SQRT(absm)
       if(absm>sphere_radius) CYCLE
       irad=irad+1
       icnt(irad)=irad
       radii(irad)=absm
       ihkl1(1:3,irad)=m(1:3)
     ENDDO
   ENDDO
 ENDDO
 ! 
 ! Quicksort over radii
 call sort(arrin=radii(1:irad),indx=icnt(1:irad))
 !     Now re-order equal radii
 !     Copy a sorted hkl list from ihkl1 to ihkl2
 !
! do i1=1,irad
!   write(*,*) ' radi ',i1,icnt(i1),radii(i1)
! enddo
 !
 allocate (ihkl2(3,irad))
 do j=1,irad
   i=icnt(j)
   ihkl2(1:3,j)=ihkl1(1:3,i)
!   write(*,*) ' l vec ',j,matmul(ihkl2(1:3,j),a(1:3,1:3))
 enddo
 deallocate (ihkl1)
 !
 allocate(lattice_vectors(3,irad), absk(irad))
 absk=0._SP
 NWAVE=1
 m2(1:3)=ihkl2(1:3,1)
 lattice_vectors(1:3,nwave)=ihkl2(1:3,1)
 absk(nwave)=radii(1)
 DO j=2,irad      
   m2(1:3)=ihkl2(1:3,j)
   absm=radii(j)
   addvec=.FALSE.
!  NEW vector of diffent length
   IF(ABSM-ABSK(nwave)>0.0001) THEN
     istart=nwave+1
     addvec=.true.
   ENDIF
! .... NEW VECTOR EQU. OLD ONE
   IF(.NOT.addvec) THEN
     CALL STERN1(m2,nsym,int_sop,nst,stg)
     DO I=istart,islut
       kdum(1:3)=lattice_vectors(1:3,I)
       IF(kdelta(kdum,nst,stg)) GOTO 1
     ENDDO
     addvec=.TRUE.
1  CONTINUE
   ENDIF
! .... PUT NEW VECTOR IN LIST
   IF(addvec) THEN
     nwave=nwave+1
     lattice_vectors(1:3,nwave)=m2
     absk(nwave)=absm
     islut=nwave
   ENDIF
 ENDDO
!    
! WRITE(6,*) 'Nwave' ,nwave
!  do i1=1,nwave
!    write(*,*) ' lattice_vectors ',lattice_vectors(:,i1),absk(i1)
!  enddo

 deallocate(absk,icnt,radii,ihkl2)
 !
 allocate(tmp_vec(3,nwave))
 tmp_vec(1:3,1:nwave)=lattice_vectors(1:3,1:nwave)
 deallocate(lattice_vectors)
 allocate(lattice_vectors(3,nwave))
 lattice_vectors(1:3,1:nwave)=tmp_vec(1:3,1:nwave)
 deallocate(tmp_vec)
 !
end subroutine

LOGICAL FUNCTION kdelta(k,nst,stg)
  !.... TEST, IF K IS IN STAR OF G  (GENERATED IN STERN)                  
  USE pars,         ONLY:SP
  USE vec_operate,  ONLY:v_is_zero
  IMPLICIT NONE
  INTEGER          :: nst
  REAL(SP)         :: k(3),stg(3,nst)
  INTEGER          :: i
  !---------------------------------------------------------------------  
  KDELTA=.FALSE.
  DO I=1,NST
     IF(v_is_zero(stg(:,i)-k(:))) kdelta=.TRUE.
  ENDDO
END FUNCTION kdelta
