!
! License-Identifier: GPL
!
! Copyright (C) 2019 The Yambo Team
!
! Authors (see AUTHORS file for details): AC MG 
!
subroutine EXP_step(K_out,V_in,H_in,I_in,nbf,dT)
  use pars,           ONLY:SP,cZERO
  use nl_optics,      ONLY:NL_bands
  implicit none
  integer,     intent(in)  :: nbf
  complex(SP), intent(in)  :: V_in(NL_bands(2),nbf)
  complex(SP), intent(out) :: K_out(NL_bands(2),nbf)
  complex(SP), intent(in)  :: H_in(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
  complex(SP), intent(in)  :: I_in(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),nbf)
  real(SP),    intent(in)  :: dT
  !
  ! Work Space
  !
  complex(SP)  :: U(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
  complex(SP)  :: H(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
  integer      :: ib
  !
  ! Expand U=exp(-i*dt*H) up to the fourth (eigth) order
  !
!$omp parallel do default(shared), private(ib,U,H)
  do ib=1,nbf
    !
    H=H_in+I_in(:,:,ib)
    !
    call EXP_op(U,H,dT)
    !
    K_out(NL_bands(1):,ib)=matmul(U,V_in(NL_bands(1):,ib))-V_in(NL_bands(1):,ib)
    !
  enddo
!$omp end parallel do
  !
  if(NL_bands(1)/=1) K_out(:NL_bands(1)-1,:)=cZERO
  !
end subroutine
