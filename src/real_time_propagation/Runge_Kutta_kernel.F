!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): CA DS AM
!
subroutine Runge_Kutta_kernel(ik,dG,K)
 !
 use pars,           ONLY:SP,cI
 use wrapper_omp,    ONLY:M_by_M_omp
 use real_time,      ONLY:Ho_plus_Sigma,RT_nbands,RT_bands,G_lesser_reference
 !
 implicit none
 !
 integer,     intent(in)  :: ik
 complex(SP), intent(in)  :: dG(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(out) :: K(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 ! Workspace
 !
 complex(SP)  :: I1(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP)  :: I2(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP)  :: G(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 ! I1 = Ho_plus_Sigma*G   
 ! I2 = G*Ho_plus_Sigma
 !
 G=dG+G_lesser_reference(:,:,ik)
 !
 call M_by_M_omp('n','n',RT_nbands,Ho_plus_Sigma(:,:,ik,1),G,I1)
 call M_by_M_omp('n','n',RT_nbands,G,Ho_plus_Sigma(:,:,ik,1),I2)
 !
 K=-cI*(I1-I2)
 !
end subroutine Runge_Kutta_kernel
