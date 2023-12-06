!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DS AM
!
subroutine RT_Dephasing_step(dG_out,dG_in,ik,deltaT)
 !
 use pars,           ONLY:SP
 use real_time,      ONLY:RT_is_statically_dephased,RT_Deph_Matrix,RT_deph_range,&
&                         RTibz,l_RT_EXP,RT_bands,NE_time
 !
 implicit none
 !
 complex(SP), intent(out) :: dG_out(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(in)  :: dG_in(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 integer,     intent(in)  :: ik
 real(SP),    intent(in)  :: deltaT
 !
 ! Note that dG_in is ONLY the coherent part of the equation. It must be added
 ! to dG_out if we are not using the EXP integrator. This is done here or in RT_apply_RWA
 !
 dG_out(:,:)=dG_out(:,:)+dG_in(:,:)
 !
 if (NE_time<RT_deph_range(1).and.RT_deph_range(1)>0._SP) return
 if (NE_time>RT_deph_range(2).and.RT_deph_range(2)>0._SP) return
 !
 if (.not.RT_is_statically_dephased) return
 !
 ! dG = dG * exp( -dT i*Sigma)
 !=============================
 if (     l_RT_EXP)  dG_out(:,:) = dG_out(:,:) *  exp(-RT_Deph_Matrix(:,:,RTibz%k_map(ik))*deltaT)
 !
 ! dG = dG * (1 - dT i*Sigma)
 !============================
 if (.not.l_RT_EXP)  dG_out(:,:) = dG_out(:,:) * (1._SP - deltaT*RT_Deph_Matrix(:,:,RTibz%k_map(ik)))
 !
end subroutine RT_Dephasing_step
