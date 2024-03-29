!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): CA DS
!
subroutine RT_EULER_step(dG_in,dG_out,ik,nbf,deltaT)
 !
 use pars,           ONLY:SP,cZERO,cI
 use real_time,      ONLY:l_RT_PERT
 use wrapper_omp,    ONLY:M_by_M_omp
 use real_time,      ONLY:Ho_plus_Sigma,RT_bands,RT_nbands, &
&                         G_lesser_reference
 !
 implicit none
 !
 integer,     intent(in)  :: ik,nbf
 real(SP),    intent(in)  :: deltaT
 complex(SP), intent(in)  :: dG_in(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(out) :: dG_out(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 ! Workspace
 !
 complex(SP)  :: I1_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP)  :: I2_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP)  :: G_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 integer      :: ib1,ib2
 !
 ! I1 = Ho_plus_Sigma*G   
 ! I2 = G*Ho_plus_Sigma
 !
 if (.not.l_RT_PERT) G_k=dG_in+G_lesser_reference(:,:,ik)
 if (     l_RT_PERT) G_k=      G_lesser_reference(:,:,ik)
 !
 call M_by_M_omp('n','n',RT_nbands,Ho_plus_Sigma(:,:,ik,1),G_k,I1_k)
 call M_by_M_omp('n','n',RT_nbands,G_k,Ho_plus_Sigma(:,:,ik,1),I2_k)
 !
 ! dG_out = G(T0+dt) -G(T0)= -i dt * ( I1(T0)-I2(T0) )
 !
 dG_out=-cI*deltaT*(I1_k-I2_k)
 !
 if (.not.l_RT_PERT) return
 !
 I1_k=cZERO
 I2_k=cZERO
 !
 G_k=cZERO
 do ib1=RT_bands(1),nbf
   do ib2=nbf+1,RT_bands(2)
     G_k(ib1,ib2)=dG_in(ib1,ib2)
     G_k(ib2,ib1)=dG_in(ib2,ib1)
   enddo
 enddo
 !
 call M_by_M_omp('n','n',RT_nbands,Ho_plus_Sigma(:,:,ik,1),G_k,I1_k)
 call M_by_M_omp('n','n',RT_nbands,G_k,Ho_plus_Sigma(:,:,ik,1),I2_k)
 !
 do ib1=RT_bands(1),nbf
   do ib2=RT_bands(1),nbf
     dG_out(ib1,ib2)=-cI*deltaT*(I1_k(ib1,ib2)-I2_k(ib1,ib2))
   enddo
 enddo
 !
 do ib1=nbf+1,RT_bands(2)
   do ib2=nbf+1,RT_bands(2)
     dG_out(ib1,ib2)=-cI*deltaT*(I1_k(ib1,ib2)-I2_k(ib1,ib2))
   enddo
 enddo
 !
end subroutine RT_EULER_step
