!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA
!
subroutine NL_damping(E,ik,i_sp_pol,V_bands)
 !
 ! Output: I_relax
 !
 use pars,           ONLY:SP,cI
 use zeros,          ONLY:zero_dfl
 use electrons,      ONLY:levels
 use real_time,      ONLY:Phase_LifeTime,RAD_LifeTime
 use nl_optics,      ONLY:I_relax,NL_bands
 use parallel_m,     ONLY:PAR_Xk_ibz_index
 use real_time,      ONLY:RT_deph_deg_thresh
 !
 implicit none
 !                      
 type(levels), intent(in) :: E
 integer,      intent(in) :: ik,i_sp_pol
 complex(SP),  intent(in) :: V_bands(NL_bands(2),E%nbf(i_sp_pol))
 !
 ! Working Space
 !
 integer          :: ib,i1,i2,ik_mem
 real(SP)         :: Gamma_ij
 !
 ! Damping
 !=========
 !
 ik_mem=PAR_Xk_ibz_index(ik)
 !
 ! Fictitious lifetime equivalent to the broadening
 ! in linear optics
 !
 ! In the 2013 version this term was:
 !
 ! I = 2*cI/Phase_LifeTime ( (|V_b(t=0)><V_b(t=0)| -|V_b(t)><V_b(t)| )
 !
 !  In the 2021 version diagonal and off-diagonal contribution are separated
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
     if(Phase_LifeTime>zero_dfl) Gamma_ij=Gamma_ij+2._SP/Phase_LifeTime 
     !
     do ib=1,E%nbf(i_sp_pol)
       I_relax(i1,i2,ib,ik_mem,i_sp_pol) = -cI*Gamma_ij*(conjg(V_bands(i2,ib))*V_bands(i1,ib))
       I_relax(i2,i1,ib,ik_mem,i_sp_pol) = -conjg(I_relax(i1,i2,ib,ik_mem,i_sp_pol))
     enddo
     !
   enddo
 enddo
!$omp end parallel do
 !
 if(RAD_LifeTime>zero_dfl) then
   !      
!$omp parallel do default(shared), private(i1,i2,ib)
   do i1=NL_bands(1),NL_bands(2)
     !
     do ib=1,E%nbf(i_sp_pol)
       I_relax(i1,i1,ib,ik_mem,i_sp_pol)=-cI*2._SP/RAD_LifeTime*(conjg(V_bands(i1,ib))*V_bands(i1,ib))
     enddo
     !
     ! I need this additional part for the degenerate states 
     !
     do i2=i1+1,NL_bands(2)
       if ( abs(E%E(i1,ik,1)-E%E(i2,ik,1))>=RT_deph_deg_thresh ) cycle
         do ib=1,E%nbf(i_sp_pol)
           I_relax(i1,i2,ib,ik_mem,i_sp_pol) = -cI*2._SP/RAD_LifeTime*(conjg(V_bands(i2,ib))*V_bands(i1,ib))
           I_relax(i2,i1,ib,ik_mem,i_sp_pol) = -conjg(I_relax(i1,i2,ib,ik_mem,i_sp_pol))
         enddo
     enddo
     !
   enddo
!$omp end parallel do
   !
   ! Remove the equilibrium contribution
   !
!$omp parallel do default(shared), private(ib)
   do ib=NL_bands(1),E%nbf(i_sp_pol)
     I_relax(ib,ib,ib,ik_mem,i_sp_pol)=I_relax(ib,ib,ib,ik_mem,i_sp_pol)+cI*2._SP/RAD_LifeTime
   enddo
!$omp end parallel do
   !
 endif
 !
end subroutine NL_damping

