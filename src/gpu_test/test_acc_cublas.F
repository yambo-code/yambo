program test
  use devxlib_cublas
  implicit none

  integer, parameter :: N = 10
  integer :: i
  real*4 :: x_ref(N), y_ref(N), x(N), y(N), a

  a = 2.0

  do i = 1, N
     x(i) = 4.0 * i
     y(i) = 3.0
     x_ref(i) = x(i)
     y_ref(i) = y(i)
  end do

  call saxpy (N, a, x_ref, y_ref)

  !$acc data copyin (x) copy (y)
  !$acc host_data use_device (x, y)
  call cublassaxpy(N, a, x, 1, y, 1)
  !$acc end host_data
  !$acc end data

  do i = 1, N
     if (y(i) .ne. y_ref(i)) then 
       STOP "Test failed !"
     endif
  end do
  write(*,*) "Test successful !"
  !
end program test

subroutine saxpy (nn, aa, xx, yy)
  integer :: nn
  real*4 :: aa, xx(nn), yy(nn)
  integer i
  real*4 :: t
  !$acc routine

  do i = 1, nn
    yy(i) = yy(i) + aa * xx(i)
  end do
end subroutine saxpy
