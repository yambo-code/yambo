!
! License-Identifier: GPL
!
! Copyright (C) 2019 The Yambo Team
!
! Authors (see AUTHORS file for details): AC MG 
!
subroutine EXP_op(U,H,dT)
  use pars,           ONLY:SP,cI,cZERO,cONE
  use nl_optics,      ONLY:NL_bands
  implicit none
  complex(SP), intent(out) :: U(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
  complex(SP), intent(in)  :: H(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
  real(SP),    intent(in)  :: dT
  !
  ! Work Space
  !
  complex(SP)  :: H_tmp(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
  integer      :: i1
  !
  ! Expand U=exp(-i*dt*H) up to the fourth order
  !
  U=cZERO
  do i1=NL_bands(1),NL_bands(2)
    U(i1,i1)=cONE
  enddo
  !
  H_tmp = matmul(H,H)
  U=U-cI*dT*H+(cI*dT)**2*H_tmp/2._SP-(cI*dT)**3/6._SP* matmul(H_tmp,H)
  U=U+(cI*dT)**4/24._SP*matmul(H_tmp,H_tmp)
    !
    ! H_tmp=matmul(H_tmp,H)
    ! !
    ! U=U-(cI*dT)**5/120._SP*H_tmp
    ! !
    ! H_tmp=matmul(H_tmp,H)
    ! !
    ! U=U+(cI*dT)**6/720._SP*H_tmp
    ! !
    ! H_tmp=matmul(H_tmp,H)
    ! !
    ! U=U-(cI*dT)**7/5040._SP*H_tmp
    ! !
    ! H_tmp=matmul(H_tmp,H)
    ! !
    ! U=U+(cI*dT)**8/40320._SP*H_tmp
    ! !
    ! U_out(ib,:,:)=U(:,:)
    !
end subroutine EXP_op
