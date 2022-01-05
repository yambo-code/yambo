
subroutine build_spin_sop()
 !
 use pars,           ONLY:SP,pi,cI,cONE,cZERO
 use matrix_operate, ONLY:m3det
 use D_lattice,      ONLY:dl_sop,spin_sop,nsym,i_time_rev,&
&                         sigma_0,sigma_x,sigma_y,sigma_z
 use electrons,      ONLY:n_spinor
 !
 !
#include<memory.h>
 !
 real(SP) :: s(3,3)
 complex(SP) :: u(2,2)
 real(SP), parameter :: eps=1.d-5
 real(SP)  :: det, saux(3,3), ax(3), angle, cosa, sina
 integer  :: is, i1, i2
 !
 !
 YAMBO_FREE(spin_sop)
 !
 YAMBO_ALLOC(spin_sop,(n_spinor,n_spinor,nsym))
 !
 if(n_spinor==1) then
   spin_sop(1,1,:)=cONE
   return
 endif
 !
 do is=1,nsym
   !
   s=dl_sop(:,:,is)
   !
   det = s(1,1) * ( s(2,2) * s(3,3) - s(3,2) * s(2,3) )-   &
         s(1,2) * ( s(2,1) * s(3,3) - s(3,1) * s(2,3) )+   &
         s(1,3) * ( s(2,1) * s(3,2) - s(3,1) * s(2,2) )
   !
   !  inversion has no effect in spin space, so improper rotations are 
   !  multiplied by inversion
   !
   IF (ABS(det+1.d0) < eps) THEN
     saux=-s
   ELSE
     saux=s
   ENDIF
   !
   ! Check for identity or inversion
   !
   IF ((ABS(saux(1,1)-1.d0) < eps).AND. &
       (ABS(saux(2,2)-1.d0) < eps).AND. &
       (ABS(saux(3,3)-1.d0) < eps).AND. &
       (ABS(saux(1,2)) < eps).AND.(ABS(saux(2,1)) < eps) &
  .AND.(ABS(saux(2,3)) < eps).AND. &
       (ABS(saux(3,2)) < eps).AND.(ABS(saux(1,3)) < eps) &
  .AND.(ABS(saux(3,1)) < eps)) THEN
     u(1,1)=(1.d0,0.d0)
     u(1,2)=(0.d0,0.d0)
     u(2,1)=(0.d0,0.d0)
     u(2,2)=(1.d0,0.d0)
     spin_sop(:,:,is)=u
     cycle
  ENDIF
  !
  !   Find the rotation axis and the rotation angle
  !
  call versor(saux,ax)
  angle=angle_rot(saux)
  angle=0.5d0*angle*pi/180.d0
  cosa=COS(angle)
  sina=SIN(angle)
  !
  !  set the spin space rotation matrix elements
  !
  u(1,1)=CMPLX(cosa,-ax(3)*sina,kind=SP)
  u(1,2)=CMPLX(-ax(2)*sina, -ax(1)*sina,kind=SP)
  u(2,1)=-CONJG(u(1,2))
  u(2,2)=CONJG(u(1,1))
  !
  !  To each 3x3 rotation one can associate two 2x2 rotation matrices in spin
  !  space. This function returns the U matrix with positive cosa term
  !
  IF (cosa < -eps ) u=-u
  !
  spin_sop(:,:,is)=u
  !
 enddo
 !
 do is=1,nsym
   write(*,*) is
   !
   do i1=1,2
     do i2=1,2
       if(abs(aimag(spin_sop(i1,i2,is)))<1.E-5_SP) spin_sop(i1,i2,is)=cmplx( real(spin_sop(i1,i2,is)),0._SP)
       if(abs(real(spin_sop(i1,i2,is))) <1.E-5_SP) spin_sop(i1,i2,is)=cmplx(0._SP,aimag(spin_sop(i1,i2,is)))
     enddo
   enddo
   !
   do i1=1,2
     write(*,*) i1,spin_sop(:,i1,is)
   enddo
 enddo
 !
 contains
   !
 SUBROUTINE versor(smat,ax)
!-----------------------------------------------------------------------------
!
!  This subroutine receives a rotation matrix and determines the rotation
!  axis. The orientation of the axis is with the tip in the hemisphere
!  z>=0. In the xy plane the axis is in the x>0 region and the positive
!  y axis is taken for z=0 and x=0.
!
USE pars, ONLY : SP
IMPLICIT NONE

REAL(SP) :: smat(3,3), ax(3)
REAL(SP), PARAMETER  ::  eps=1.d-7
REAL(SP) :: a1(3), norm
INTEGER :: ipol, jpol, ts
!
!  Check if it is a 180 rotation
!
ts=tipo_sym(smat)
IF (ts/=3.and.ts/=4.and.ts/=6) &
     call errore('versor','called in the wrong case',1)
IF (ts==4) THEN
!
!   First the case where the axis is parallel to a coordinate axis
!
   ax=0.d0
   DO ipol=1,3
      IF (ABS(smat(ipol,ipol)-1.d0) < eps ) ax(ipol)=1.d0
   END DO
   norm=sqrt(ax(1)**2+ax(2)**2+ax(3)**2)
   IF (ABS(norm)>eps) RETURN
!
!   then the general case
!
   DO ipol=1,3
      a1(ipol)=sqrt(ABS(smat(ipol,ipol)+1.d0)/2.d0)
   END DO

   DO ipol=1,3
      DO jpol=ipol+1,3
         IF (ABS(a1(ipol)*a1(jpol))>eps) THEN
            a1(ipol)=0.5d0*smat(ipol,jpol)/a1(jpol)
         END IF
      END DO
   END DO

ELSE
!
!  It is not a 180 rotation: compute the rotation axis
!
   a1(1) =-smat(2,3)+smat(3,2)
   a1(2) =-smat(3,1)+smat(1,3)
   a1(3) =-smat(1,2)+smat(2,1)

END IF
!
!  The direction of the axis is arbitrarily chosen, with positive z. In the
!  xy plane with positive x, and along y with positive y.
!
IF (a1(3) < -eps ) THEN
   a1=-a1
ELSEIF (abs(a1(3))<eps .and. a1(1) < -eps ) THEN
   a1=-a1
ELSEIF (abs(a1(3))<eps .and. abs(a1(1))<eps.and.a1(2) < -eps ) THEN
   a1=-a1
ENDIF

norm=sqrt(a1(1)**2+a1(2)**2+a1(3)**2)
!IF (norm<eps) call errore('versor','problem with the matrix',1)
ax=a1/norm

RETURN
END SUBROUTINE versor
 !
FUNCTION angle_rot(smat)
!-----------------------------------------------------------------------------
!
!  This subroutine receives a rotation matrix and determine the 
!  rotation angle. 
!
USE pars, ONLY : SP
IMPLICIT NONE

REAL(SP), PARAMETER  ::  eps=1.d-7

REAL(SP) :: smat(3,3)

REAL(SP) :: a1(3), ax(3)
REAL(SP) :: angle_rot, angle_rot1, pi, sint, cost
 
!
!  Check if it is a 180 rotation
!
IF (tipo_sym(smat)==4) THEN
   angle_rot=180.d0
   RETURN
END IF
pi=4.d0*atan(1.d0)
!
!  Compute the axis
!
a1(1) =-smat(2,3)+smat(3,2)
a1(2) =-smat(3,1)+smat(1,3)
a1(3) =-smat(1,2)+smat(2,1)

sint=0.5d0*sqrt(a1(1)**2+a1(2)**2+a1(3)**2)
IF (sint<eps) CALL errore('angle_rot','problem with the matrix',1)
IF (ABS(sint)> 1.0_SP+eps) CALL errore('angle_rot','problem with sint',1)
!
!  small rounding errors that make |sint|>1.0 produce NaN in the next ASIN
!  function, so we remove them
!
IF (ABS(sint) > 1.0_SP) sint=SIGN(1.0_SP, sint)
!
!  The direction of the axis is chosen in such a way that a1(3) is always
!  positive if non zero. Otherwise a1(2) is positive, or a1(1) respectively
!
ax=a1
IF (ax(3) < -eps ) THEN
   ax=-ax
ELSEIF (abs(ax(3))<eps .and. ax(2) < -eps ) THEN
   ax=-ax
ELSEIF (abs(ax(3))<eps .and. abs(ax(2))<eps.and.ax(1) < -eps ) THEN
   ax=-ax
ENDIF
IF (ABS(a1(1))>eps) THEN
   sint=SIGN(sint,a1(1)/ax(1)) 
ELSEIF (ABS(a1(2))>eps) THEN
   sint=SIGN(sint,a1(2)/ax(2)) 
ELSEIF (ABS(a1(3))>eps) THEN
   sint=SIGN(sint,a1(3)/ax(3)) 
END IF
!
!  Compute the cos of the angle
!
ax=a1/(2.d0*sint)
IF (ABS(ax(1)**2-1.d0)>eps) THEN
   cost=(smat(1,1)-ax(1)**2)/(1.d0-ax(1)**2)
ELSE IF (ABS(ax(2)**2-1.d0)>eps) THEN
   cost=(smat(2,2)-ax(2)**2)/(1.d0-ax(2)**2)
ELSE IF (ABS(ax(3)**2-1.d0)>eps) THEN
   cost=(smat(3,3)-ax(3)**2)/(1.d0-ax(3)**2)
END IF

IF (ABS(sint**2+cost**2-1.d0) > eps ) &
       CALL errore('angle_rot','problem with the matrix',1)
angle_rot1=ASIN(sint)*180.d0/pi
IF (angle_rot1 < 0.d0) THEN
   IF (cost < 0.d0) THEN
      angle_rot1=-angle_rot1+180.d0
   ELSE
      angle_rot1=360.d0+angle_rot1
   ENDIF
ELSE
   IF (cost < 0.d0) angle_rot1=-angle_rot1+180.d0
ENDIF

angle_rot=angle_rot1

RETURN
END FUNCTION angle_rot
 !
FUNCTION tipo_sym(s)
!--------------------------------------------------------------------------
! This function receives a 3x3 orthogonal matrix which is a symmetry 
! operation of the point group of the crystal written in cartesian 
! coordinates and gives as output a code according to the following:
!
!  1   Identity
!  2   Inversion
!  3   Proper rotation of an angle <> 180 degrees
!  4   Proper rotation of 180 degrees
!  5   Mirror symmetry
!  6   Improper rotation
!
USE pars, ONLY : SP

IMPLICIT NONE

REAL(SP), PARAMETER :: eps=1.d-7
REAL(SP) :: s(3,3), det, det1
INTEGER  :: tipo_sym
!
! Check for identity
!
IF ((ABS(s(1,1)-1.d0) < eps).AND. &
    (ABS(s(2,2)-1.d0) < eps).AND. &
    (ABS(s(3,3)-1.d0) < eps).AND. &
    (ABS(s(1,2)) < eps).AND.(ABS(s(2,1)) < eps).AND.(ABS(s(2,3)) < eps).AND. &
    (ABS(s(3,2)) < eps).AND.(ABS(s(1,3)) < eps).AND.(ABS(s(3,1)) < eps)) THEN
   tipo_sym=1
   RETURN
ENDIF
!
! Check for inversion
!
IF ((ABS(s(1,1)+1.d0) < eps).AND. &
    (ABS(s(2,2)+1.d0) < eps).AND. &
    (ABS(s(3,3)+1.d0) < eps).AND. &
    (ABS(s(1,2)) < eps).AND.(ABS(s(2,1)) < eps).AND.(ABS(s(2,3)) < eps).AND. &
    (ABS(s(3,2)) < eps).AND.(ABS(s(1,3)) < eps).AND.(ABS(s(3,1)) < eps)) THEN
   tipo_sym=2
   RETURN
ENDIF
!
! compute the determinant
!
det = s(1,1) * ( s(2,2) * s(3,3) - s(3,2) * s(2,3) )-   &
      s(1,2) * ( s(2,1) * s(3,3) - s(3,1) * s(2,3) )+   &
      s(1,3) * ( s(2,1) * s(3,2) - s(3,1) * s(2,2) ) 
!
! Determinant equal to 1: proper rotation
!
IF (abs(det-1.d0) < eps) THEN
!
!  check if an eigenvalue is equal to -1.d0 (180 rotation)
!
   det1=(s(1,1)+1.d0)*((s(2,2)+1.d0)*(s(3,3)+1.d0)-s(3,2)*s(2,3))-   &
         s(1,2)*       (s(2,1)*      (s(3,3)+1.d0)-s(3,1)*s(2,3))+   &
         s(1,3)*       (s(2,1)*s(3,2)             -s(3,1)*(s(2,2)+1.d0)) 

   IF (abs(det1) < eps) THEN
      tipo_sym=4     ! 180 proper rotation
   ELSE
      tipo_sym=3     ! proper rotation <> 180
   ENDIF
   RETURN
ENDIF
!
! Determinant equal to -1: mirror symmetry or improper rotation
!
IF (abs(det+1.d0) < eps) THEN
!
!  check if an eigenvalue is equal to 1.d0 (mirror symmetry)
!
   det1=(s(1,1)-1.d0)*((s(2,2)-1.d0)*(s(3,3)-1.d0)-s(3,2)*s(2,3))-   &
         s(1,2)*       (s(2,1)*      (s(3,3)-1.d0)-s(3,1)*s(2,3))+   &
         s(1,3)*       (s(2,1)*s(3,2)             -s(3,1)*(s(2,2)-1.d0)) 

   IF (abs(det1) < eps) THEN
      tipo_sym=5   ! mirror symmetry
   ELSE
      tipo_sym=6   ! improper rotation
   ENDIF
   RETURN
ELSE
   !call errore('tipo_sym','symmetry not recognized',1)
ENDIF
END FUNCTION tipo_sym
 !
end subroutine build_spin_sop
