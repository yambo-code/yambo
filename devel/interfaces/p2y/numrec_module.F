# 2 "numrec_module.spp"

module numrec_module
implicit none
private
public :: numrec_polint
public :: numrec_ddpoly
public :: numrec_polcof
public :: numrec_polcoe
public :: numrec_locate

interface numrec_polint
# 17 "numrec_module.spp"
subroutine numrec_polint_sgl(xa,ya,n,x,y,dy)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(sgl), intent(in)  :: xa(n)
  real(sgl), intent(in)  :: ya(n)
  real(sgl), intent(in)  :: x
  real(sgl), intent(out) :: y
  real(sgl), intent(out) :: dy
end subroutine numrec_polint_sgl
# 17 "numrec_module.spp"
subroutine numrec_polint_dbl(xa,ya,n,x,y,dy)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(dbl), intent(in)  :: xa(n)
  real(dbl), intent(in)  :: ya(n)
  real(dbl), intent(in)  :: x
  real(dbl), intent(out) :: y
  real(dbl), intent(out) :: dy
end subroutine numrec_polint_dbl
# 28 "numrec_module.spp"
end interface

interface numrec_ddpoly
# 84 "numrec_module.spp"
subroutine numrec_ddpoly_sgl(c,nc,x,pd,nd)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: nc
  real(sgl), intent(in)  :: c(nc)
  real(sgl), intent(in)  :: x
  integer,     intent(in)  :: nd
  real(sgl), intent(out) :: pd(nd)
end subroutine numrec_ddpoly_sgl
# 84 "numrec_module.spp"
subroutine numrec_ddpoly_dbl(c,nc,x,pd,nd)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: nc
  real(dbl), intent(in)  :: c(nc)
  real(dbl), intent(in)  :: x
  integer,     intent(in)  :: nd
  real(dbl), intent(out) :: pd(nd)
end subroutine numrec_ddpoly_dbl
# 94 "numrec_module.spp"
end interface

interface numrec_polcof
# 98 "numrec_module.spp"
subroutine numrec_polcof_sgl(xa,ya,n,cof)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(sgl), intent(in)  :: xa(n)
  real(sgl), intent(in)  :: ya(n)
  real(sgl), intent(out) :: cof(n)
end subroutine numrec_polcof_sgl
# 98 "numrec_module.spp"
subroutine numrec_polcof_dbl(xa,ya,n,cof)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(dbl), intent(in)  :: xa(n)
  real(dbl), intent(in)  :: ya(n)
  real(dbl), intent(out) :: cof(n)
end subroutine numrec_polcof_dbl
# 107 "numrec_module.spp"
end interface

interface numrec_polcoe
# 112 "numrec_module.spp"
subroutine numrec_polcoe_REALsgl(x,y,n,cof)
  use numrec_kinds
  implicit none
  integer,      intent(in)  :: n
  REAL(sgl), intent(in)  :: x(n)
  REAL(sgl), intent(in)  :: y(n)
  REAL(sgl), intent(out) :: cof(n)
end subroutine numrec_polcoe_REALsgl
# 112 "numrec_module.spp"
subroutine numrec_polcoe_REALdbl(x,y,n,cof)
  use numrec_kinds
  implicit none
  integer,      intent(in)  :: n
  REAL(dbl), intent(in)  :: x(n)
  REAL(dbl), intent(in)  :: y(n)
  REAL(dbl), intent(out) :: cof(n)
end subroutine numrec_polcoe_REALdbl
# 112 "numrec_module.spp"
subroutine numrec_polcoe_COMPLEXsgl(x,y,n,cof)
  use numrec_kinds
  implicit none
  integer,      intent(in)  :: n
  COMPLEX(sgl), intent(in)  :: x(n)
  COMPLEX(sgl), intent(in)  :: y(n)
  COMPLEX(sgl), intent(out) :: cof(n)
end subroutine numrec_polcoe_COMPLEXsgl
# 112 "numrec_module.spp"
subroutine numrec_polcoe_COMPLEXdbl(x,y,n,cof)
  use numrec_kinds
  implicit none
  integer,      intent(in)  :: n
  COMPLEX(dbl), intent(in)  :: x(n)
  COMPLEX(dbl), intent(in)  :: y(n)
  COMPLEX(dbl), intent(out) :: cof(n)
end subroutine numrec_polcoe_COMPLEXdbl
# 122 "numrec_module.spp"
end interface

interface numrec_locate
# 60 "numrec_module.spp"
subroutine numrec_locate_sgl(xx,n,x,j)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(sgl), intent(in)  :: xx(n)
  real(sgl), intent(in)  :: x
  integer,     intent(out) :: j
end subroutine numrec_locate_sgl
# 60 "numrec_module.spp"
subroutine numrec_locate_dbl(xx,n,x,j)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(dbl), intent(in)  :: xx(n)
  real(dbl), intent(in)  :: x
  integer,     intent(out) :: j
end subroutine numrec_locate_dbl
# 69 "numrec_module.spp"
end interface



end module numrec_module
