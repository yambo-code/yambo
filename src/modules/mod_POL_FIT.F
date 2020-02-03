!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
! POL_Gilbreth_fit based on:
!
! pfit.f90: Module for polynomial least-squares fitting
! http://infty.net/pfit/pfit.html
! v0.8.1
!
! Copyright (c) 2010-2013 Christopher N. Gilbreth
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.

module POL_FIT_m
 !
 use pars, ONLY:DP
 !
 contains
   !
   subroutine POL_LK_fit(N,X,Y,order,Tt,EPS,t_zero)
    !
    use pars,       ONLY:SP
    use functions,  ONLY:NAN
    !
    implicit none
    !
    integer, intent(in)    :: N
    integer, intent(inout) :: order
    real(SP),intent(in)    :: X(N),Y(N),t_zero
    real(SP),intent(out)   :: Tt(order+1),EPS
    ! 
    ! Work Space
    !
    integer  :: NDEG,IERR,I,I_l
    real(SP) :: W(N),R(N),wk(3*N+3*order+3)
    !
    W(:)=1._SP/sum(Y(:)**2)
    !
    if (N<=1) then
      EPS=-1._SP
      return
    endif
    !
    ! Simple linear fit on 2 points
    !
    if (N==2.and.order==1) then
      Tt(2) = ( Y(1)-Y(2) ) / ( X(1) - X(2) )
      Tt(1) = Y(1) - Tt(2)*X(1)
      return
    endif
    !
    ! General fit on N points
    !
    Tt = 0._SP
    EPS=-1._SP
    I_l= 0
#if defined _DOUBLE
    call DPOLFT (N,X,Y,W,order,NDEG,EPS,R,IERR,wk)
#else
    call POLFIT (N,X,Y,W,order,NDEG,EPS,R,IERR,wk)
#endif
    do I=1,3*N+3*order+3
      if (NAN(wk(I))) I_l=1
    enddo
    if (I_l==1) then
      order=max(order-1,0)
#if defined _DOUBLE
      call DPOLFT (N,X,Y,W,order,NDEG,EPS,R,IERR,wk)
#else
      call POLFIT (N,X,Y,W,order,NDEG,EPS,R,IERR,wk)
#endif
    endif
    order=NDEG 
    !
#if defined _DOUBLE
    call DPCOEF (order,t_zero,Tt(:order+1),wk)
#else
    call PCOEF (order,t_zero,Tt(:order+1),wk)
#endif
    !
   end subroutine POL_LK_fit
   !  
   subroutine POL_Gilbreth_fit(x,y,sig,a)
    !
    ! Fit data to a polynomial a_0 + a_1 x + ... + a_d x**d
    ! Inputs:
    !   x(1:npt)         - abscissas
    !   y(1:npt)         - data values
    !   sig(1:npt)       - data errors
    ! Outputs:
    !   a(1:d+1)         - max. likelihood parameters
    !   coeff(1:npt,1:d+1) - coefficients giving the max. likelihood parameters
    !                    in terms of the data:
    !                      a(i) = \Sum_{j} coeff(j,i) * y(j)
    !   cov(1:n,1:d+1)   - Covariance matrix, cov(i,j) = Cov(a(i),a(j))
    !                    The estimated error in a(i) is sqrt(Cov(a(i),a(i))).
    ! Notes:
    !   This routine uses a QR decomposition method, which should be more
    !   numerically stable than solving the normal equations.
#include<memory.h>
    real(DP), intent(in)  :: x(:), y(:), sig(:)
    real(DP), intent(out) :: a(:)
    real(DP), allocatable :: work(:), C(:,:), Q(:,:), R(:,:), b(:)
    integer :: ipiv(size(a)), lwork
    integer :: i,j,k,d,npt,ifail
    npt = size(x) ! Number of data points
    d = size(a)-1 ! Max degree of polynomial
    if (size(y) .ne. npt) stop "Error 1 in P_fit"
    if (size(sig) .ne. npt) stop "Error 2 in P_fit"
    if (d+1 .gt. npt) stop "Error 4 in P_fit"

    YAMBO_ALLOC(C,(npt,d+1))
    YAMBO_ALLOC(Q,(npt,d+1))
    YAMBO_ALLOC(R,(d+1,d+1))
    YAMBO_ALLOC(b,(d+1))

    ! Vandermonde matrix
    do j=1,d+1
       do i=1,npt
          C(i,j) = x(i)**(j-1)/sig(i)
       end do
    end do

    ! QR decomposition
    call DQRF(C,Q,R,work)

    ! Inversion of R factor
    R = dinverse(R)

    ! Compute max-likelihood parameters
    ! a = R^-1 Q^T y/σ
    b = 0.d0
    do j=1,d+1
       do k=1,npt
          b(j) = b(j) + Q(k,j) * y(k) / sig(k)
       end do
    end do

    a = 0.d0
    do i=1,d+1
       do j=1,d+1
          a(i) = a(i) + R(i,j) * b(j)
       end do
    end do
    !
    YAMBO_FREE(C)
    YAMBO_FREE(Q)
    YAMBO_FREE(R)
    YAMBO_FREE(b) 
    deallocate(work)
    !
  end subroutine POL_Gilbreth_fit
  !
  subroutine DQRF(A,Q,R,work)
    ! Compute the QR factorization of a general real matrix A:
    !   A = Q R
    ! where Q is unitary and R is upper triangular, using the LAPACK routine
    ! zgeqrf.
    ! Inputs:
    !   A:     Matrix to be factorized, m x n
    ! Ouputs:
    !   Q:     Unitary matrix, m x m
    !   R:     Upper triangular, n x n
    ! Input/output:
    !   work:  real(8) allocatable workspace array. If unallocated, this
    !          routine will allocate it to an appropriate size. If allocated,
    !          it is assumed to be the correct size for this problem.
    implicit none
    real(8), intent(in)  :: A(:,:)
    real(8), intent(out) :: Q(:,:), R(:,:)
    real(8), allocatable :: work(:)

    integer :: m, n, lwork, ierr, i, j
    real(8) :: tau(size(A,2)), qwork(1)
    real(8) :: A1(size(A,1),size(A,2))

    m = size(A,1)
    n = size(A,2)
    if (m .lt. n) stop "Error in DQRF: m < n"
    if (size(Q,1) .ne. m) stop "Error in DQRF (2)"
    if (size(Q,2) .ne. n) stop "Error in DQRF (3)"
    if (size(R,1) .ne. n) stop "Error in DQRF (4)"
    if (size(R,2) .ne. n) stop "Error in DQRF (5)"

    A1 = A
    if (.not. allocated(work)) then
       ! Compute size of workspace
       lwork = -1
       call DGEQRF(m, n, A1, m, TAU, qwork, LWORK, ierr)
       if (ierr .ne. 0) stop "Error calling DGEQRF (1)"
       lwork = qwork(1)
       allocate(work(lwork))
    end if

    lwork = size(work)
    call dgeqrf(m,n,A1,m,tau,work,lwork,ierr)
    if (ierr .ne. 0) stop "Error calling DGEQRF (2)"
    R = 0.d0
    do j=1,n
       do i=1,j
          R(i,j) = A1(i,j)
       end do
    end do
    Q(:,1:n) = A1
    call dorgqr(m,n,n,Q,m,tau,work,lwork,ierr)
    if (ierr .ne. 0) stop "Error calling DORGQR"
  end subroutine DQRF
  !
  function dinverse(A)
    ! Invert a square matrix
    implicit none
    real(8), intent(in)  :: A(:,:)
    real(8) :: dinverse(size(A,1),size(A,1))

    integer :: ipiv(size(A,1)), ierr, lwork
    real*8, allocatable :: work(:)
    real*8 :: work1(1)

    dinverse = A
    call dgetrf(size(A,1), size(A,1), dinverse, size(A,1), ipiv, ierr)
    if (ierr .ne. 0) stop "Error computing LU decomposition for matrix inverse."

    lwork = -1
    call dgetri(size(A,1), dinverse, size(A,1), ipiv, work1, lwork, ierr)
    if (ierr.ne.0) stop "Error allocating space for dgetri"
    lwork = int(work1(1),kind(lwork))
    allocate(work(max(1,lwork)))
    call dgetri(size(A,1), dinverse, size(A,1), ipiv, work, lwork, ierr)
    if (ierr .ne. 0) stop "Error calling zgetri."
  end function dinverse
  !
end module POL_FIT_m
