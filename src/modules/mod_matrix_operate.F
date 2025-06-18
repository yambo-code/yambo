!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
module matrix_operate
 !
 use pars, ONLY:SP,cI
 !
 implicit none
 !
 contains
   !
   integer function matrix_index(i1,i2,n)
     !
     ! Linear index of a matrix (nxn)
     !
     integer :: i1,i2,n
     matrix_index=(i1-1)*n+i2
     !
   end function
   !
   integer function UP_matrix_index(i1,i2)
     !
     ! Linear index of the upper triangular of a matrix
     !
     ! Note that:
     !
     !  UP_matrix_index(1,1)=2
     !  UP_matrix_index(2,2)=3
     !  UP_matrix_index(1,2)=4
     !  UP_matrix_index(3,3)=5
     !  UP_matrix_index(2,3)=6
     !  UP_matrix_index(1,3)=7
     !
     integer :: i1,i2
     UP_matrix_index=(i2**2+i2)/2-i1+2
     !
   end function
   !
   complex(SP) function trace(M)
     use pars, ONLY:cZERO
     implicit none
     complex(SP), intent(in) :: M(:,:)
     !
     ! Work Space
     !
     integer :: i1
     !
     trace=cZERO
     !
     do i1=1,size(M(1,:))
       trace=trace+M(i1,i1)
     enddo
     !
   end function trace   
   !
   function hermitian(M)
     implicit none
     complex(SP), intent(in) :: M(:,:)
     complex(SP) :: hermitian(size(M,2),size(M,1))
     !
     hermitian=conjg(transpose(M))
     !
   end function hermitian
   !
   subroutine mat_transpose(M) 
     complex(SP) :: M(:,:)
     integer     :: D,i1
     complex, allocatable :: V(:)
     D=size(M,1)
     !
     !$omp parallel default(shared), private(V,i1)
     allocate(V(D))
     !$omp do 
     do i1=1,D
       V(i1:D)=M(i1,i1:D)
       M(i1,i1:D)=M(i1:D,i1)
       M(i1:D,i1)=V(i1:D)
     enddo
     !$omp end do
     deallocate(V)
     !$omp end parallel
   end subroutine
   !
   subroutine mat_r2c_ordered(mr,mc,m_order) 
     !
     ! Copy mr in mc using m_order indexes for mc
     !
     real(SP)    :: mr(:,:,:)
     complex(SP) :: mc(:,:)
     integer     :: m_order(:)
     integer     :: i1,i2,mr_shp(3),mc_shp(2) ! Work Space
     !
     mr_shp=shape(mr)
     mc_shp=shape(mc)
     !
     do i1=1,min(mr_shp(1),mc_shp(1))
       do i2=1,min(mr_shp(2),mc_shp(2))
         mc(m_order(i1),m_order(i2))=mr(i1,i2,1)+cI*mr(i1,i2,2)
       enddo
     enddo
     !
   end subroutine
   !
   subroutine mat_r2c(mr,mc,shift)
     !
     ! Copy mr in mc 
     !
     real(SP)    :: mr(:,:,:)
     complex(SP) :: mc(:,:)
     integer, optional :: shift(2)
     integer     :: i1,i2,mr_shp(3),mc_shp(2) ! Work Space
     !
     mr_shp=shape(mr)
     mc_shp=shape(mc) 
     !
     do i1=1,min(mr_shp(1),mc_shp(1))
       do i2=1,min(mr_shp(2),mc_shp(2))
         if(.not.present(shift)) mc(i1,i2)=mr(i1,i2,1)+cI*mr(i1,i2,2)
         if(     present(shift)) mc(i1,i2)=mr(i1+shift(1),i2+shift(2),1)+cI*mr(i1+shift(1),i2+shift(2),2)
       enddo
     enddo 
     !
   end subroutine
   !
   subroutine mat_c2r(mc,mr)
     !
     ! Copy mc in mr
     !
     real(SP)    :: mr(:,:,:)
     complex(SP) :: mc(:,:)
     integer     :: i1,i2,mr_shp(3),mc_shp(2) ! Work Space
     mr_shp=shape(mr)
     mc_shp=shape(mc)
     do i1=1,min(mr_shp(1),mc_shp(1))
       do i2=1,min(mr_shp(2),mc_shp(2))
         mr(i1,i2,1)=real(mc(i1,i2),SP)
         mr(i1,i2,2)=aimag(mc(i1,i2))
       enddo
     enddo
     !
   end subroutine
   !
   subroutine m3inv(m,m_inv)
     !
     ! Analitic inverse of a real 3x3 matrix
     !
     real(SP)          :: m(3,3)
     real(SP),optional :: m_inv(3,3)
     real(SP)          :: det,inv(3,3) ! Work Space
     det=m3det(m)
     inv(1,1)=(-m(2,3)*m(3,2) + m(2,2)*m(3,3) )/det
     inv(1,2)=( m(1,3)*m(3,2) - m(1,2)*m(3,3) )/det
     inv(1,3)=(-m(1,3)*m(2,2) + m(1,2)*m(2,3) )/det
     inv(2,1)=( m(2,3)*m(3,1) - m(2,1)*m(3,3) )/det
     inv(2,2)=(-m(1,3)*m(3,1) + m(1,1)*m(3,3) )/det
     inv(2,3)=( m(1,3)*m(2,1) - m(1,1)*m(2,3) )/det
     inv(3,1)=(-m(2,2)*m(3,1) + m(2,1)*m(3,2) )/det
     inv(3,2)=( m(1,2)*m(3,1) - m(1,1)*m(3,2) )/det
     inv(3,3)=(-m(1,2)*m(2,1) + m(1,1)*m(2,2) )/det
     !
     if (abs(det)<=1.E-7) call error('Null determinant. Inversion failed.')
     if (present(m_inv))      m_inv=inv
     if (.not.present(m_inv)) m=inv
     !
   end subroutine
   !
   subroutine m3inv_c(m,m_inv)
     !
     ! Analitic inverse of a complex 3x3 matrix
     !
     complex(SP)          :: m(3,3)
     complex(SP),optional :: m_inv(3,3)
     complex(SP)          :: det,inv(3,3) ! Work Space
     det=m3det_c(m)
     inv(1,1)=(-m(2,3)*m(3,2) + m(2,2)*m(3,3) )/det
     inv(1,2)=( m(1,3)*m(3,2) - m(1,2)*m(3,3) )/det
     inv(1,3)=(-m(1,3)*m(2,2) + m(1,2)*m(2,3) )/det
     inv(2,1)=( m(2,3)*m(3,1) - m(2,1)*m(3,3) )/det
     inv(2,2)=(-m(1,3)*m(3,1) + m(1,1)*m(3,3) )/det
     inv(2,3)=( m(1,3)*m(2,1) - m(1,1)*m(2,3) )/det
     inv(3,1)=(-m(2,2)*m(3,1) + m(2,1)*m(3,2) )/det
     inv(3,2)=( m(1,2)*m(3,1) - m(1,1)*m(3,2) )/det
     inv(3,3)=(-m(1,2)*m(2,1) + m(1,1)*m(2,2) )/det
     !
     !if (abs(det)<=1.E-7) call error('Null determinant. Inversion failed.')
     !
     if (     present(m_inv)) m_inv=inv
     if (.not.present(m_inv)) m=inv
     !
   end subroutine
   !
   function m3det(m)
     real(SP) :: m(3,3),m3det
     m3det = m(1,1)*( m(2,2)*m(3,3) - m(2,3)*m(3,2) )   &
&           -m(1,2)*( m(2,1)*m(3,3) - m(2,3)*m(3,1) ) + &
&            m(1,3)*( m(2,1)*m(3,2) - m(2,2)*m(3,1) )
   end function
   !
   function m3det_c(m)
     complex(SP) :: m(3,3),m3det_c
     m3det_c = m(1,1)*( m(2,2)*m(3,3) - m(2,3)*m(3,2) )   &
&             -m(1,2)*( m(2,1)*m(3,3) - m(2,3)*m(3,1) ) + &
&              m(1,3)*( m(2,1)*m(3,2) - m(2,2)*m(3,1) )
   end function
   !
   function m3det_i(m)
     integer :: m(3,3),m3det_i
     m3det_i = m(1,1)*( m(2,2)*m(3,3) - m(2,3)*m(3,2) )   &
&             -m(1,2)*( m(2,1)*m(3,3) - m(2,3)*m(3,1) ) + &
&              m(1,3)*( m(2,1)*m(3,2) - m(2,2)*m(3,1) )
   end function
   !
end module
