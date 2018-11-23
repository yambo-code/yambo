!
! Copyright (C) 2002-2008 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Written by Giovanni Bussi
! Adapted to QE by Andrea Ferretti & Layla Martin Samos
! Adapted to Y  by Andrea Ferretti & Daniele Varsano
!
!------------------------------------------
  SUBROUTINE vcut_init(vcut,a,cutoff)
  !------------------------------------------
  !
  USE cutoff_ws_m
  use com,           ONLY:error,msg
  use LIVE_t,        ONLY:live_timing
  use openmp,        ONLY:OPENMP_update,n_threads,master_thread
  use parallel_m,    ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
  use interfaces,    ONLY:PARALLEL_index
  implicit none
  type(vcut_type),   intent(out) :: vcut
  real(DP),          intent(in)  :: a(3,3)
  real(DP),          intent(in)  :: cutoff

  integer      :: n1,n2,n3
  integer      :: i1,i2,i3
  integer      :: ierr
  real(DP)     :: q(3)
  !character(9) :: subname='vcut_init'     
  real(DP)     :: mod2a(3)
  type(PP_indexes) ::px
  !
  real(DP), external :: vcut_formula
  real(DP), external :: num_determinant

  vcut%cutoff=cutoff

  vcut%a=a
  vcut%b= 2.0d0* PI * transpose(num_inverse(vcut%a))
  vcut%b_omega=num_determinant(vcut%b)
  vcut%a_omega=num_determinant(vcut%a)
  !
  ! automatically finds whether the cell is orthorombic or not
  !
  vcut%orthorombic=.false.
  mod2a=sqrt(sum(vcut%a**2,1))
  if(abs(sum(vcut%a(:,1)*vcut%a(:,2)))/(mod2a(1)*mod2a(2))<toll .and. &
     abs(sum(vcut%a(:,2)*vcut%a(:,3)))/(mod2a(2)*mod2a(3))<toll .and. &
     abs(sum(vcut%a(:,3)*vcut%a(:,1)))/(mod2a(3)*mod2a(1))<toll) vcut%orthorombic=.true.
  !
  if (.not.vcut%orthorombic) call msg('nr','WARNING non-orthorombic case untested')
  !
  n1=ceiling(vcut%cutoff*sqrt(sum(vcut%a(1,:)**2))/(2.0*pi))
  n2=ceiling(vcut%cutoff*sqrt(sum(vcut%a(2,:)**2))/(2.0*pi))
  n3=ceiling(vcut%cutoff*sqrt(sum(vcut%a(3,:)**2))/(2.0*pi))

  allocate(vcut%corrected(-n1:n1,-n2:n2,-n3:n3), STAT=ierr)
  if ( ierr/=0 ) CALL error('allocating cvut%corrected')

  !
  ! define the Fourier component of the modified Coulomb potential
  !
  vcut%corrected=0.0
  !
  call PP_indexes_reset(px)
  call PARALLEL_index(px,(/2*n2+1,2*n3+1/))
  !
  call live_timing('WS setup',px%n_of_elements(myid+1)/n_threads)
  !
  !$omp parallel default(shared), private(i3,i2,i1,q)
  !
  call OPENMP_update(master_thread)
  !
  !$omp do collapse(2), schedule(dynamic)
  do i3=-n3,n3
    do i2=-n2,n2
      !
      if (master_thread) call live_timing(steps=1)
      if (.not.px%element_2D(i2+n2+1,i3+n3+1)) cycle
      !
      do i1=-n1,n1
        !
        q = matmul(vcut%b,(/i1,i2,i3/)) 
        if( sum(q**2) > vcut%cutoff**2 ) cycle
        !
        vcut%corrected(i1,i2,i3) = &
&            vcut_formula(q,vcut%a,vcut%b,vcut%a_omega,vcut%orthorombic)
        !
      enddo
    enddo
    !
  enddo
  !$omp end do
  !$omp end parallel
  !
  call live_timing()
  !
  call PP_redux_wait(vcut%corrected)
  !
END SUBROUTINE vcut_init

!------------------------------------------
  SUBROUTINE vcut_info(vcut)
  !------------------------------------------
  !
  USE cutoff_ws_m
  use com,           ONLY:msg
  use pars,          ONLY:SP,DP,schlen
  use D_lattice,     ONLY:alat
  use stderr,        ONLY:real2ch
  implicit none
  !
  type(vcut_type), intent(in) :: vcut
  !
  real(SP) :: dummy
  integer  :: i, n(3)
  character(schlen) :: dumb_ch1
  !
  if ( associated( vcut%corrected ) ) then
     !
     do i = 1, 3
        n(i) = ( size( vcut%corrected, i) -1 ) / 2
     enddo
     dummy=real(vcut%cutoff,SP)
     call msg('r','Cutoff:',dummy)
     call msg('r','n grid:',n)
     call msg('nr','WS Direct Lattice(DL) unit cell [iru  /  cc(a.u.)]')
     dumb_ch1='A1 ='//trim(real2ch(real(vcut%a(1,1),SP)/alat(1)))//' '//&
&                 trim(real2ch(real(vcut%a(2,1),SP)/alat(2)))//' '//&
&                 trim(real2ch(real(vcut%a(3,1),SP)/alat(3)))//'     '//&
&                 trim(real2ch(real(vcut%a(1,1),SP)))//' '//& 
&                 trim(real2ch(real(vcut%a(2,1),SP)))//' '//trim(real2ch(real(vcut%a(3,1),SP)))
     call msg('r',dumb_ch1) 
     dumb_ch1='A2 ='//trim(real2ch(real(vcut%a(1,2),SP)/alat(1)))//' '//&
&                 trim(real2ch(real(vcut%a(2,2),SP)/alat(2)))//' '//&
&                 trim(real2ch(real(vcut%a(3,2),SP)/alat(3)))//'     '//&
&                 trim(real2ch(real(vcut%a(1,2),SP)))//' '//&
&                 trim(real2ch(real(vcut%a(2,2),SP)))//' '//trim(real2ch(real(vcut%a(3,2),SP)))
     call msg('r',dumb_ch1) 
     dumb_ch1='A3 ='//trim(real2ch(real(vcut%a(1,3),SP)/alat(1)))//' '//&
&                 trim(real2ch(real(vcut%a(2,3),SP)/alat(2)))//' '//&
&                 trim(real2ch(real(vcut%a(3,3),SP)/alat(3)))//'     '//&
&                 trim(real2ch(real(vcut%a(1,3),SP)))//' '//trim(real2ch(real(vcut%a(2,3),SP)))//' '//&
&                 trim(real2ch(real(vcut%a(3,3),SP)))
     call msg('rn',dumb_ch1)
!
!     call msg('nr','See if it works, next some info has to be dumped to the database')     
!      write(*,*) "this should be put in the report using msg interface"
!      WRITE(*, "(  2x,'Cutoff: ',f6.2,4x,'  n grid: ',3i4,/)") vcut%cutoff, n(:)
     !
  ENDIF
  !
END SUBROUTINE vcut_info

!------------------------------------------
  SUBROUTINE vcut_destroy(vcut)
  !------------------------------------------
  !
  USE cutoff_ws_m
  use com,           ONLY:error
  IMPLICIT NONE
  !
  TYPE(vcut_type), INTENT(INOUT) :: vcut
  INTEGER :: ierr
  !
  DEALLOCATE(vcut%corrected, STAT=ierr)
  IF ( ierr/=0 ) CALL error('deallocating vcut')
  !
END SUBROUTINE vcut_destroy

!------------------------------------------
  FUNCTION vcut_get(vcut,q) RESULT(res)
  !------------------------------------------
  !
  USE cutoff_ws_m
  use com,           ONLY:error
  IMPLICIT NONE
  !
  TYPE(vcut_type), INTENT(IN) :: vcut
  REAL(DP),        INTENT(IN) :: q(3)
  REAL(DP)                    :: res
  !
  REAL(DP)     :: i_real(3)
  INTEGER      :: i(3)
  !CHARACTER(8) :: subname='vcut_get'
  !
  i_real=(MATMUL(TRANSPOSE(vcut%a),q))/ (2.0d0*PI)
  i=NINT(i_real)
  !
  ! internal check
  IF( SUM( (i-i_real)**2 ) > toll ) &
     CALL error('q vector out of the grid')
  !
  IF( SUM(q**2) > vcut%cutoff**2 ) THEN
     !
     ! usual form of Coulomb potential
     res = 4.0d0*PI * e2 / SUM(q**2) 
     !
  ELSE
     !
     IF( i(1)>ubound(vcut%corrected,1) .OR. i(1)<lbound(vcut%corrected,1) .OR. &
         i(2)>ubound(vcut%corrected,2) .OR. i(2)<lbound(vcut%corrected,2) .OR. &
         i(3)>ubound(vcut%corrected,3) .OR. i(3)<lbound(vcut%corrected,3)) THEN
         CALL error('index out of bound') 
     ENDIF
     !
     res=vcut%corrected(i(1),i(2),i(3))
     !
  ENDIF
  !
END FUNCTION vcut_get

!------------------------------------------
  FUNCTION vcut_spheric_get(vcut,q) RESULT(res)
  !------------------------------------------
  !
  USE cutoff_ws_m
  IMPLICIT NONE
  !
  TYPE(vcut_type), INTENT(IN) :: vcut
  REAL(DP),        INTENT(IN) :: q(3)
  REAL(DP)                    :: res 
  !
  REAl(DP) :: a(3,3), Rcut, kg2 
  LOGICAL  :: limit
  !
  !
  a = vcut%a
  !
  Rcut=0.5d0*minval(sqrt(sum(a**2,1)))
  Rcut=Rcut-Rcut/50.0d0
  limit=.false.
  kg2=sum(q**2)
  if(kg2<toll) then
    limit=.true.
  endif
  if(.not.limit) then
    res=4.0d0*PI*e2/kg2*(1.0d0-cos(Rcut*sqrt(kg2)))
  else
    res=4.0d0*PI*e2*Rcut**2/2.0d0
  endif
  !
END FUNCTION vcut_spheric_get

!---------------------------------------------------------
  FUNCTION vcut_formula(q,a,b,a_omega,orthorombic) result(res)
  !---------------------------------------------------------
  !
  ! Define the FT of the Coulomb potential according to the
  ! current lattice.
  !
  use cutoff_ws_m, only : DP, PI
  IMPLICIT NONE
  !
  REAL(DP), INTENT(IN) :: q(3)
  REAL(DP), INTENT(IN) :: a(3,3)
  REAL(DP), INTENT(IN) :: b(3,3)
  REAL(DP), INTENT(IN) :: a_omega
  LOGICAL,  INTENT(IN) :: orthorombic
  REAL(DP)             :: res
  !
  real(dp), external :: vcut_formula_longrange
  real(dp), external :: vcut_formula_shortrange
  !
  real(dp) :: rwigner
  real(dp) :: sigma
  !
  rwigner=0.5d0*sqrt(1.0d0/maxval(sum(b**2,1)))*2.0d0*pi

  !
  ! 3.0 is set to give a short range contribution inside the WS cell
  !
  sigma=3.0d0/rwigner

  ! compute longrange and shortrange contributions
  res=vcut_formula_longrange(q,a,b,a_omega,sigma,6.0D0,orthorombic) &
     +vcut_formula_shortrange(q,sigma)
END FUNCTION vcut_formula

!---------------------------------------------------------
  FUNCTION vcut_formula_longrange(q,a,b,a_omega,sigma,security,orthorombic) result(res)
  !---------------------------------------------------------
  ! compute the longrange contribution
  use cutoff_ws_m
  IMPLICIT NONE
  !
  real(dp), intent(in) :: q(3)
  real(dp), intent(in) :: a(3,3)
  real(dp), intent(in) :: b(3,3)
  real(dp), intent(in) :: a_omega
  real(dp), intent(in) :: sigma
  real(dp), intent(in) :: security ! it determines the grid for the real-space sum; a reasonable value is 4.0
  logical,  intent(in) :: orthorombic
  real(dp)             :: res
  integer  :: n1,n2,n3
  integer  :: i1,i2,i3
  real(dp) :: d1,d2,d3,weight,factor
  real(dp) :: r(3),r1(3),r2,modr
  logical  :: n1_is_even,n1_is_odd
  real(dp) :: tmp
  logical, parameter :: shifted=.false.
  integer  :: n1max
  real(dp) :: i1_real,i2_real,i3_real
  real(DP), external :: qe_erf
  !real(DP), dimension=3, external :: vcut_minimal_image
 

  n1=security*sqrt(sum(a(:,1)**2))*sigma
  n2=security*sqrt(sum(a(:,2)**2))*sigma
  n3=security*sqrt(sum(a(:,3)**2))*sigma

  n1_is_even=(n1/2)*2==n1
  n1_is_odd=.not.n1_is_even

  d1=1.0d0/real(n1,dp)
  d2=1.0d0/real(n2,dp)
  d3=1.0d0/real(n3,dp)
  res=0.0d0
  weight=a_omega*d1*d2*d3
! the only symmetry which can be used for any value of q is inversion
! NON-SHIFTED:
!  if n1 is even: loop between 0 and n1/2, with weight=2.0 for all points except 0 and n1max
!  if n2 is odd:  loop between 0 and (n1+1)/2, with weight=2.0 for all points except 0
! SHIFTED:
!  if n1 is even: loop between 0 and n1/2-1, with weight=2.0 for all points
!  if n2 is odd:  loop between 0 and (n1+1)/2, with weight=2.0 for all points except n1max

  if(shifted)then
    if(n1_is_even) n1max=n1/2-1
    if(n1_is_odd)  n1max=(n1+1)/2-1
  else
    if(n1_is_even) n1max=n1/2
    if(n1_is_odd)  n1max=(n1+1)/2
  end if
  do i1=0,n1max
    factor=2.0d0
    if(shifted) then
      if(n1_is_odd .and. i1==n1max) factor=1.0d0
    else
      if(i1==0) factor=1.0d0
      if(n1_is_even .and. i1==n1max) factor=1.0d0
    end if
    i1_real=i1
    if(shifted) i1_real=i1_real+0.5d0
    do i2=0,n2-1
      i2_real=i2
      if(shifted) i2_real=i2_real+0.5d0
      do i3=0,n3-1
        i3_real=i3
        if(shifted) i3_real=i3_real+0.5d0
        !
        r1=matmul(a,(/i1_real*d1,i2_real*d2,i3_real*d3/))
        r=vcut_minimal_image(a,b,r1,orthorombic)
        r2=sum(r**2)
        modr=sqrt(r2)
        !
        if(modr*sigma<toll) then
          tmp=e2*sqrt(2.0d0/pi)*sigma
        else
          tmp=e2*qe_erf(sigma*sqrt(0.5d0)*modr)/modr
        end if
        res=res+weight*factor*tmp*cos(sum(r*q))
      end do
    end do
  end do
 END FUNCTION vcut_formula_longrange

!---------------------------------------------------------
FUNCTION vcut_formula_shortrange(q,sigma) result(res)
  !---------------------------------------------------------
  use cutoff_ws_m
  implicit none
  real(dp), intent(in) :: q(3)
  real(dp), intent(in) :: sigma
  real(dp)             :: res
  if(sum(q**2/(sigma*sigma))<toll) then
! analytic limit for small q
    res=e2*2.0d0*pi/(sigma*sigma)
  else
    res=e2*4.0d0*pi/sum(q**2)*(1.d0-exp(-0.5d0*sum(q**2)/(sigma*sigma)))
  end if
END FUNCTION vcut_formula_shortrange

!---------------------------------------------------------
FUNCTION vcut_minimal_image(a,b,r1,orthorombic) result(res)
  !---------------------------------------------------------
  use pars
  implicit none
  real(dp), intent(in) :: a(3,3)
  real(dp), intent(in) :: b(3,3)
  real(dp), intent(in) :: r1(3)
  logical,  intent(in) :: orthorombic
  real(dp)             :: res(3)
  real(dp) :: r_minimal(3)
  real(dp) :: r2_minimal
  real(dp) :: r_try(3)
  real(dp) :: r2_try
  real(dp) :: r_components(3)
  integer :: i1,i2,i3
  integer, parameter :: max_displacement=1
!
  if(orthorombic) then
! NINT ALGORITHM FOR ORTHOROMBIC CELL
    r_components=(matmul(transpose(b),r1))/(2.0*pi)
    r_components=r_components-nint(r_components)
    r_minimal=matmul(a,r_components)
  else
! POOR MAN ALGORITHM FOR GENERIC CELL
    r_minimal=r1
    r2_minimal=sum(r_minimal**2)
! loop over the possible neighbours
    do i1=-max_displacement,max_displacement
      do i2=-max_displacement,max_displacement
        do i3=-max_displacement,max_displacement
          if(i1==0 .and. i2==0 .and. i3==0) cycle
            r_try=r1+matmul(a,(/i1,i2,i3/))
            r2_try=sum(r_try**2)
            if(r2_try<r2_minimal) then
              r2_minimal=r2_try
              r_minimal=r_try
            endif
        end do
      end do
    end do
  end if
  res=r_minimal
END FUNCTION vcut_minimal_image



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! tools from sax

function num_inverse(a) result(inv)
  use cutoff_ws_m, only : DP
  implicit none
  real(DP)              :: inv(0:2,0:2)
  real(DP), intent(in)  :: a(0:2,0:2)
  real(DP) :: tmp(0:2,0:2)
  real(DP) :: det
  real(DP),parameter :: eye3(3,3) = reshape((/ 1,0,0,0,1,0,0,0,1/),(/3,3/))
  real(DP), external :: num_determinant
  integer i,j
  do i=0,2
    do j=0,2
      tmp(i,j) = a(modulo(i+1,3),modulo(j+1,3)) * a(modulo(i+2,3),modulo(j+2,3)) &
  &            - a(modulo(i+1,3),modulo(j+2,3)) * a(modulo(i+2,3),modulo(j+1,3))
    end do
  end do
  det = num_determinant(a)
  inv = transpose(tmp) / det
  if(sum((matmul(inv,a))**2-eye3) > 1d-5) then
    !write(0,*) "AHIA",sum((matmul(inv,a)-eye3)**2)
    !write(0,*) "A",a
    !write(0,*) "inv",inv
    !write(0,*)">>", matmul(inv,a)
    stop
  end if
end function num_inverse

function num_determinant(a) result(det)
  use pars, only: DP
  implicit none
  real(DP), intent(in) :: a(3,3)
  real(DP)             :: det
  det = a(1,1)*a(2,2)*a(3,3) + a(1,2)*a(2,3)*a(3,1) + a(1,3)*a(2,1)*a(3,2) &
      - a(1,1)*a(2,3)*a(3,2) - a(1,2)*a(2,1)*a(3,3) - a(1,3)*a(2,2)*a(3,1)
end function num_determinant


!
! Copyright (C) 2002-2009 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!---------------------------------------------------------------------
function qe_erf (x)  
  !---------------------------------------------------------------------
  !
  !     Error function - computed from the rational approximations of
  !     W. J. Cody, Math. Comp. 22 (1969), pages 631-637.
  !
  !     for abs(x) le 0.47 erf is calculated directly
  !     for abs(x) gt 0.47 erf is calculated via erf(x)=1-erfc(x)
  !
  use pars,          ONLY:DP
  implicit none  
  real(DP), intent(in) :: x
  real(DP) :: x2, p1 (4), q1 (4)
  real(DP), external :: qe_erfc  
  real(DP) :: qe_erf
  data p1 / 2.426679552305318E2_DP, 2.197926161829415E1_DP, &
            6.996383488619136_DP,  -3.560984370181538E-2_DP /
  data q1 / 2.150588758698612E2_DP, 9.116490540451490E1_DP, &
            1.508279763040779E1_DP, 1.000000000000000_DP /
  !
  if (abs (x) > 6.0_DP) then  
     !
     !  erf(6)=1-10^(-17) cannot be distinguished from 1
     !
     qe_erf = sign (1.0_DP, x)  
  else  
     if (abs (x)  <= 0.47_DP) then  
        x2 = x**2  
        qe_erf=x *(p1 (1) + x2 * (p1 (2) + x2 * (p1 (3) + x2 * p1 (4) ) ) ) &
                / (q1 (1) + x2 * (q1 (2) + x2 * (q1 (3) + x2 * q1 (4) ) ) )
     else  
        qe_erf = 1.0_DP - qe_erfc (x)  
     endif
  endif
  !
  return  
end function qe_erf
!
!---------------------------------------------------------------------
function qe_erfc (x)  
  !---------------------------------------------------------------------
  !
  !     erfc(x) = 1-erf(x)  - See comments in erf
  !
  use pars,          ONLY:DP
  implicit none  
  real(DP),intent(in) :: x
  real(DP)            :: qe_erfc
  real(DP) :: ax, x2, xm2, p2 (8), q2 (8), p3 (5), q3 (5), pim1
  real(DP), external :: qe_erf  
  data p2 / 3.004592610201616E2_DP,  4.519189537118719E2_DP, &
            3.393208167343437E2_DP,  1.529892850469404E2_DP, &
            4.316222722205674E1_DP,  7.211758250883094_DP,   &
            5.641955174789740E-1_DP,-1.368648573827167E-7_DP /
  data q2 / 3.004592609569833E2_DP,  7.909509253278980E2_DP, &
            9.313540948506096E2_DP,  6.389802644656312E2_DP, &
            2.775854447439876E2_DP,  7.700015293522947E1_DP, &
            1.278272731962942E1_DP,  1.000000000000000_DP /
  data p3 /-2.996107077035422E-3_DP,-4.947309106232507E-2_DP, &
           -2.269565935396869E-1_DP,-2.786613086096478E-1_DP, &
           -2.231924597341847E-2_DP /
  data q3 / 1.062092305284679E-2_DP, 1.913089261078298E-1_DP, &
            1.051675107067932_DP,    1.987332018171353_DP,    &
            1.000000000000000_DP /

  data pim1 / 0.56418958354775629_DP /  
  !        ( pim1= sqrt(1/pi) )
  ax = abs (x)  
  if (ax > 26.0_DP) then  
     !
     !  erfc(26.0)=10^(-296); erfc( 9.0)=10^(-37);
     !
     qe_erfc = 0.0_DP  
  elseif (ax > 4.0_DP) then  
     x2 = x**2  
     xm2 = (1.0_DP / ax) **2  
     qe_erfc = (1.0_DP / ax) * exp ( - x2) * (pim1 + xm2 * (p3 (1) &
          + xm2 * (p3 (2) + xm2 * (p3 (3) + xm2 * (p3 (4) + xm2 * p3 (5) &
          ) ) ) ) / (q3 (1) + xm2 * (q3 (2) + xm2 * (q3 (3) + xm2 * &
          (q3 (4) + xm2 * q3 (5) ) ) ) ) )
  elseif (ax > 0.47_DP) then  
     x2 = x**2  
     qe_erfc = exp ( - x2) * (p2 (1) + ax * (p2 (2) + ax * (p2 (3) &
          + ax * (p2 (4) + ax * (p2 (5) + ax * (p2 (6) + ax * (p2 (7) &
          + ax * p2 (8) ) ) ) ) ) ) ) / (q2 (1) + ax * (q2 (2) + ax * &
          (q2 (3) + ax * (q2 (4) + ax * (q2 (5) + ax * (q2 (6) + ax * &
          (q2 (7) + ax * q2 (8) ) ) ) ) ) ) )
  else  
     qe_erfc = 1.0_DP - qe_erf (ax)  
  endif
  !
  ! erf(-x)=-erf(x)  =>  erfc(-x) = 2-erfc(x)
  !
  if (x < 0.0_DP) qe_erfc = 2.0_DP - qe_erfc  
  !
  return  
end function qe_erfc
!


