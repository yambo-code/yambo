program rotation

implicit none

INTEGER :: i, j

!The polarisation versors: e_r = right circular, e_l = left circular, e_1 and e_2 = linear orthogonal versors
COMPLEX, ALLOCATABLE :: e_r(:), e_l(:), A(:,:,:)
REAL(8), ALLOCATABLE :: e_1(:), e_2(:), B(:,:,:)

!Polarisation type: 1 - circular, 2 - linear
INTEGER :: pol_type

!Photon momentum and rotation parameters 
REAL(8), DIMENSION(3) :: k!, k_new
REAL(8) :: cos_a, sin_a, cos_t, sin_t

k(1) = 0.0
k(2) = 0.0
k(3) = 1.0

pol_type = 1

if(pol_type /= 1 .and. pol_type /= 2) then
  
  print*, 'Polarisation type not existant. Must be either linear or circular polarisation.'
!  print*, 'ALL HAIL MEGATRON!!'

else
  if (k(1)==0.0 .and. k(2)==0.0 .and. k(3) == 0.0) then
    cos_a = 1.0
    sin_a = 0.0
    cos_t = 1.0
    sin_t = 0.0
  else if(k(1)==0.0 .and. k(2)==0.0) then
    cos_a = 1.0
    sin_a = 0.0
    cos_t = k(3)/sqrt(k(1)**2 + k(2)**2 + k(3)**2) 
    sin_t = sqrt(k(1)**2 + k(2)**2)/sqrt(k(1)**2 + k(2)**2 + k(3)**2)
  else
    cos_a = k(1)/sqrt(k(1)**2 + k(2)**2)
    sin_a = k(2)/sqrt(k(1)**2 + k(2)**2)
    cos_t = k(3)/sqrt(k(1)**2 + k(2)**2 + k(3)**2) 
    sin_t = sqrt(k(1)**2 + k(2)**2)/sqrt(k(1)**2 + k(2)**2 + k(3)**2)
  end if

!  k_new(1) = cos_t*cos_a*k(1) + cos_t*sin_a*k(2) - sin_t*k(3)
!  k_new(2) = -sin_a*k(1) + cos_a*k(2)
!  k_new(3) = sin_t*cos_a*k(1) + sin_t*sin_a*k(2) + cos_t*k(3)

  if(pol_type == 1) then
    ALLOCATE(e_r(3), e_l(3), A(2,3,3))
  
    e_r(1) = dcmplx(cos_a*cos_t,sin_a)
    e_r(2) = dcmplx(sin_a*cos_t,-cos_a)
    e_r(3) = dcmplx(-sin_t,0)

    e_l(1) = dcmplx(cos_a*cos_t,-sin_a)
    e_l(2) = dcmplx(sin_a*cos_t,cos_a)
    e_l(3) = dcmplx(-sin_t)

    e_r = e_r/sqrt(2.0)
    e_l = e_l/sqrt(2.0)
    
    do i = 1, 3
      do j = i, 3
        A(1,i,j) = CONJG(e_l(i))*e_l(j)
        A(1,j,i) = CONJG(A(1,i,j))
        A(2,i,j) = A(1,j,i)
        A(2,j,i) = A(1,i,j)
      end do
!      print*, A(1,i,:)
    end do
!    print*, 'hail megatron'
!    do i = 1, 3
!      print*, A(2,i,:)
!    end do
        
    DEALLOCATE(e_r, e_l, A)

  else if(pol_type == 2) then
    ALLOCATE(e_1(3), e_2(3), B(2,3,3))
  
    e_1(1) = cos_a*cos_t
    e_1(2) = sin_a*cos_t
    e_1(3) = -sin_t

    e_2(1) = -sin_a
    e_2(2) = cos_a
    e_2(3) = 0.0

!    print*, e_1
!    print*, e_2
!    print*, dot_product(e_1,e_2)

    do i = 1, 3
      do j = i, 3
        B(1,i,j) = e_1(i)*e_1(j)
        B(1,j,i) = B(1,i,j)
        B(2,i,j) = e_2(i)*e_2(j)
        B(2,j,i) = B(2,i,j)
      end do
!      print*, B(1,i,:)
    end do
!    print*, 'hail megatron'
!    do i = 1, 3
!      print*, B(2,i,:)
!    end do

    DEALLOCATE(e_1, e_2, B)

  end if
end if

end program rotation
