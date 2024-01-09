!
! License-Identifier: GPL
!
! Copyright (C) 2019 The Yambo Team
!
! Authors (see AUTHORS file for details): AC MG 
!
subroutine RK_basestep(K_out,V_in,H_in,I_in,nbf)
  use pars,           ONLY:SP,cI,cZERO
  use nl_optics,      ONLY:NL_bands
  implicit none
  integer,     intent(in)  :: nbf
  complex(SP), intent(in)  :: V_in(NL_bands(2),nbf)
  complex(SP), intent(out) :: K_out(NL_bands(2),nbf)
  complex(SP), intent(in)  :: H_in(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
  complex(SP), intent(in)  :: I_in(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),nbf)
  !
  ! Workspace
  !
  complex(SP)  :: H(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
  integer      :: ib
  !
  K_out=cZERO
  !
!$omp parallel do default(shared), private(ib,H)
  do ib=1,nbf
    H = H_in + I_in(:,:,ib)
    K_out(NL_bands(1):,ib)=-cI*matmul(H,V_in(NL_bands(1):,ib))
  enddo
!$omp end parallel do
  !
  if(NL_bands(1)/=1) K_out(1:NL_bands(1)-1,:)=cZERO
  !
end subroutine RK_basestep
