# 3 "numrec_polint.spp"
subroutine numrec_polint_sgl(xa,ya,n,x,y,dy)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(sgl), intent(in)  :: xa(n)
  real(sgl), intent(in)  :: ya(n)
  real(sgl), intent(in)  :: x
  real(sgl), intent(out) :: y
  real(sgl), intent(out) :: dy
  integer     :: i,m,ns
  real(sgl) :: den,dif,dift,ho,hp,w,c(n),d(n)
  ns=1
  dif=abs(x-xa(1))
  do i=1,n
    dift=abs(x-xa(i))
    if (dift<dif) then
      ns=i
      dif=dift
    endif
    c(i)=ya(i)
    d(i)=ya(i)
  end do
  y=ya(ns)
  ns=ns-1
  do m=1,n-1
    do i=1,n-m
      ho=xa(i)-x
      hp=xa(i+m)-x
      w=c(i+1)-d(i)
      den=ho-hp
      if(den==0.0_sgl)stop 'failure in nr_polint'
      den=w/den
      d(i)=hp*den
      c(i)=ho*den
    end do
    if (2*ns<n-m)then
      dy=c(ns+1)
    else
      dy=d(ns)
      ns=ns-1
    endif
    y=y+dy
  end do
  return
end subroutine numrec_polint_sgl
# 3 "numrec_polint.spp"
subroutine numrec_polint_dbl(xa,ya,n,x,y,dy)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(dbl), intent(in)  :: xa(n)
  real(dbl), intent(in)  :: ya(n)
  real(dbl), intent(in)  :: x
  real(dbl), intent(out) :: y
  real(dbl), intent(out) :: dy
  integer     :: i,m,ns
  real(dbl) :: den,dif,dift,ho,hp,w,c(n),d(n)
  ns=1
  dif=abs(x-xa(1))
  do i=1,n
    dift=abs(x-xa(i))
    if (dift<dif) then
      ns=i
      dif=dift
    endif
    c(i)=ya(i)
    d(i)=ya(i)
  end do
  y=ya(ns)
  ns=ns-1
  do m=1,n-1
    do i=1,n-m
      ho=xa(i)-x
      hp=xa(i+m)-x
      w=c(i+1)-d(i)
      den=ho-hp
      if(den==0.0_dbl)stop 'failure in nr_polint'
      den=w/den
      d(i)=hp*den
      c(i)=ho*den
    end do
    if (2*ns<n-m)then
      dy=c(ns+1)
    else
      dy=d(ns)
      ns=ns-1
    endif
    y=y+dy
  end do
  return
end subroutine numrec_polint_dbl
