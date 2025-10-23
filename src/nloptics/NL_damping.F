!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA
!
!> @brief Dephasing for the real-time dynamics 
!!
!! @param[in]       dG_lesser                   variation of the G_lesser = G(t) - G^0
!! @param[in]       ik				k-point index
!! @param[in]       i_sp_pol			spin-polarization index
!! @param[in]       E 	                        energies and life-time array
!!
!! @param[out]      I_relax			relaxation matrix (distributed on k-points)
!
!
subroutine NL_damping(E,ik,i_sp_pol,dG_lesser,I_relax)
 !
 ! Output: I_relax
 !
 use pars,           ONLY:SP,cI
 use zeros,          ONLY:zero_dfl
 use parallel_m,     ONLY:PAR_Xk_nibz,PAR_Xk_ibz_index
 use electrons,      ONLY:levels,n_sp_pol
 use real_time,      ONLY:Phase_LifeTime,RAD_LifeTime
 use nl_optics,      ONLY:NL_bands
 use QP_m,           ONLY:QP_nk
 use real_time,      ONLY:RT_deph_deg_thresh
 !
 implicit none
 !                      
 type(levels), intent(in)  :: E
 integer,      intent(in)  :: ik,i_sp_pol
 complex(SP),  intent(in)  :: dG_lesser(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),QP_nk,n_sp_pol)
 complex(SP),  intent(out) :: I_relax(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),PAR_Xk_nibz,n_sp_pol)
 !
 ! Working Space
 !
 integer          :: i1,i2,ik_mem
 real(SP)         :: Gamma_ij
 !
 ! Damping
 !=========
 !
 ik_mem=PAR_Xk_ibz_index(ik)
 !
 ! Fictitious lifetime equivalent to the broadening
 ! in linear optics in term of density matrix
 !
 ! I = cI/Phase_LifeTime ( \rho_{ij}(t) -\rho^0_{ij} )
 !
 if(Phase_LifeTime<zero_dfl) return
 !
!$omp parallel do default(shared), private(i1,i2,Gamma_ij)
 do i1=NL_bands(1),NL_bands(2)
   do i2=i1+1,NL_bands(2)
     !
     ! In case of dengerate states I use the radiative life-time
     !
     if ( abs(E%E(i1,ik,1)-E%E(i2,ik,1))<RT_deph_deg_thresh ) cycle
     !
     Gamma_ij=0._SP
     !
     if(allocated(E%W))          Gamma_ij=Gamma_ij+abs(E%W(i1,ik,i_sp_pol))+abs(E%W(i2,ik,i_sp_pol))
     if(Phase_LifeTime>zero_dfl) Gamma_ij=Gamma_ij+1._SP/Phase_LifeTime 
     !
     I_relax(i1,i2,ik_mem,i_sp_pol) = -Gamma_ij*dG_lesser(i1,i2,ik,i_sp_pol)
     I_relax(i2,i1,ik_mem,i_sp_pol) = -conjg(I_relax(i1,i2,ik_mem,i_sp_pol))
     !
   enddo
 enddo
!$omp end parallel do
 !
 if(RAD_LifeTime>zero_dfl) then
   !      
!$omp parallel do default(shared), private(i1,i2)
   do i1=NL_bands(1),NL_bands(2)
     !
     I_relax(i1,i1,ik_mem,i_sp_pol)=I_relax(i1,i1,ik_mem,i_sp_pol)-1._SP/RAD_LifeTime*dG_lesser(i1,i1,ik,i_sp_pol)
     !
     ! I need this additional part for the degenerate states 
     !
     do i2=i1+1,NL_bands(2)
       if ( abs(E%E(i1,ik,1)-E%E(i2,ik,1))>=RT_deph_deg_thresh ) cycle
       I_relax(i1,i2,ik_mem,i_sp_pol) = I_relax(i1,i2,ik_mem,i_sp_pol)-1._SP/RAD_LifeTime*dG_lesser(i1,i2,ik,i_sp_pol)
       I_relax(i2,i1,ik_mem,i_sp_pol) = -conjg(I_relax(i1,i2,ik_mem,i_sp_pol))
     enddo
     !
   enddo
!$omp end parallel do
   !
 endif
 !
end subroutine NL_damping

