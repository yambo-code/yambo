subroutine matrix_inv_cut(A,ndim,cutoff,pos_def)
use pars,           ONLY:SP,cONE,cZERO
use matrix_operate, ONLY:DIAGO,USE_LK,mat_dia_inv
use wrapper,        ONLY:Vstar_dot_V
implicit none
integer, intent(in)        :: ndim
complex(SP), intent(inout) :: A(ndim,ndim)
logical, intent(in)        :: pos_def
real(SP), intent(in)       :: cutoff
!
! Work space
!
complex(SP), allocatable   :: inv_A(:,:)
real(SP),    allocatable   :: eigenv(:)
real(SP) :: max_eigenv
integer  :: i1

allocate(inv_A(ndim,ndim),eigenv(ndim))
call mat_dia_inv(DIAGO,USE_LK,A,E_real=eigenv)

! Build the inverse cutting noisy directions

max_eigenv=maxval(abs(eigenv))

inv_A=cZERO

do i1=1,ndim
  if(abs(eigenv(i1)/max_eigenv)<cutoff) cycle
  if(pos_def.and.eigenv(i1)<0._SP) cycle
#if defined _DOUBLE
  call zgerc( ndim, ndim, cONE/cmplx(eigenv(i1),0._SP), A(:,i1), 1,  A(:,i1), 1, inv_A, ndim)
#else
  call cgerc( ndim, ndim, cONE/cmplx(eigenv(i1),0._SP), A(:,i1), 1,  A(:,i1), 1, inv_A, ndim)
#endif
enddo
!
A=inv_A
deallocate(inv_A,eigenv)
end subroutine matrix_inv_cut
