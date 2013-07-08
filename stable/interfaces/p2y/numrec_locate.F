# 3 "numrec_locate.spp"
subroutine numrec_locate_sgl(xx,n,x,j)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(sgl), intent(in)  :: xx(n)
  real(sgl), intent(in)  :: x
  integer,     intent(out) :: j
  integer :: jl,jm,ju
  jl=0
  ju=n+1
10 if(ju-jl>1)then
    jm=(ju+jl)/2
    if((xx(n)>xx(1)).eqv.(x>xx(jm)))then
      jl=jm
    else
      ju=jm
    endif
  goto 10
  endif
  j=jl
end subroutine numrec_locate_sgl
# 3 "numrec_locate.spp"
subroutine numrec_locate_dbl(xx,n,x,j)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(dbl), intent(in)  :: xx(n)
  real(dbl), intent(in)  :: x
  integer,     intent(out) :: j
  integer :: jl,jm,ju
  jl=0
  ju=n+1
10 if(ju-jl>1)then
    jm=(ju+jl)/2
    if((xx(n)>xx(1)).eqv.(x>xx(jm)))then
      jl=jm
    else
      ju=jm
    endif
  goto 10
  endif
  j=jl
end subroutine numrec_locate_dbl

