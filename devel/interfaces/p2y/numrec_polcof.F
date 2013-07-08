# 3 "numrec_polcof.spp"
subroutine numrec_polcof_sgl(xa,ya,n,cof)
  use numrec_module, only : numrec_polint
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(sgl), intent(in)  :: xa(n)
  real(sgl), intent(in)  :: ya(n)
  real(sgl), intent(out) :: cof(n)
  integer :: i,j,k
  real(sgl) :: dy,xmin,x(n),y(n)
  do j=1,n
    x(j)=xa(j)
    y(j)=ya(j)
  end do
  do j=1,n
    call numrec_polint(x,y,n+1-j,0.0_sgl,cof(j),dy)
    xmin=1.e38
    k=0
    do i=1,n+1-j
      if (abs(x(i))<xmin)then
        xmin=abs(x(i))
        k=i
      endif
      if(x(i)/=0.0_sgl) y(i)=(y(i)-cof(j))/x(i)
    end do
    do i=k+1,n+1-j
      y(i-1)=y(i)
      x(i-1)=x(i)
    end do
  end do
end subroutine numrec_polcof_sgl
# 3 "numrec_polcof.spp"
subroutine numrec_polcof_dbl(xa,ya,n,cof)
  use numrec_module, only : numrec_polint
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(dbl), intent(in)  :: xa(n)
  real(dbl), intent(in)  :: ya(n)
  real(dbl), intent(out) :: cof(n)
  integer :: i,j,k
  real(dbl) :: dy,xmin,x(n),y(n)
  do j=1,n
    x(j)=xa(j)
    y(j)=ya(j)
  end do
  do j=1,n
    call numrec_polint(x,y,n+1-j,0.0_dbl,cof(j),dy)
    xmin=1.e38
    k=0
    do i=1,n+1-j
      if (abs(x(i))<xmin)then
        xmin=abs(x(i))
        k=i
      endif
      if(x(i)/=0.0_dbl) y(i)=(y(i)-cof(j))/x(i)
    end do
    do i=k+1,n+1-j
      y(i-1)=y(i)
      x(i-1)=x(i)
    end do
  end do
end subroutine numrec_polcof_dbl
