# 3 "numrec_ddpoly.spp"
subroutine numrec_ddpoly_sgl(c,nc,x,pd,nd)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: nc
  real(sgl), intent(in)  :: c(nc)
  real(sgl), intent(in)  :: x
  integer,     intent(in)  :: nd
  real(sgl), intent(out) :: pd(nd)
  integer     :: i,j,nnd
  real(sgl) :: const
  pd(1)=c(nc)
  do j=2,nd
    pd(j)=0.0_sgl
  end do
  do i=nc-1,1,-1
    nnd=min(nd,nc+1-i)
    do j=nnd,2,-1
      pd(j)=pd(j)*x+pd(j-1)
    end do
    pd(1)=pd(1)*x+c(i)
  end do
  const=2.0_sgl
  do i=3,nd
    pd(i)=const*pd(i)
    const=const*i
  end do
end subroutine numrec_ddpoly_sgl
# 3 "numrec_ddpoly.spp"
subroutine numrec_ddpoly_dbl(c,nc,x,pd,nd)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: nc
  real(dbl), intent(in)  :: c(nc)
  real(dbl), intent(in)  :: x
  integer,     intent(in)  :: nd
  real(dbl), intent(out) :: pd(nd)
  integer     :: i,j,nnd
  real(dbl) :: const
  pd(1)=c(nc)
  do j=2,nd
    pd(j)=0.0_dbl
  end do
  do i=nc-1,1,-1
    nnd=min(nd,nc+1-i)
    do j=nnd,2,-1
      pd(j)=pd(j)*x+pd(j-1)
    end do
    pd(1)=pd(1)*x+c(i)
  end do
  const=2.0_dbl
  do i=3,nd
    pd(i)=const*pd(i)
    const=const*i
  end do
end subroutine numrec_ddpoly_dbl
