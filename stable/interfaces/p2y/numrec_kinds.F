# 1 "numrec_kinds.spp"
module numrec_kinds
implicit none

private
public :: sgl,dbl

real :: sgl_ = 0.0
double precision :: dbl_ = 0.0d0

integer, parameter :: sgl = kind(sgl_)
integer, parameter :: dbl = kind(dbl_)

end module numrec_kinds
