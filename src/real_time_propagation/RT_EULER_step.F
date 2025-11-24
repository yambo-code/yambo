!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): CA DS
!
subroutine RT_EULER_step(dG_in,dG_out,ik,i_sp_pol,nbf,dT)
 !
 use pars,           ONLY:SP,cZERO,cI
 use real_time,      ONLY:l_RT_PERT,l_RT_FRES,l_RT_RWA,&
 &                        H_EQ,H_field,Ho_plus_Sigma,  &
 &                        G_lesser_reference,RT_bands
 !
 implicit none
 !
 integer,     intent(in)  :: ik,i_sp_pol,nbf
 real(SP),    intent(in)  :: dT
 complex(SP), intent(in)  :: dG_in(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(out) :: dG_out(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 ! Workspace
 !
 complex(SP)  :: G_tmp(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP)  :: G_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP)  :: H_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 integer      :: ib1,ib2
 !
 if (.not. l_RT_pert) then
   !
   G_k=dG_in+G_lesser_reference(:,:,ik,i_sp_pol)
   H_k=Ho_plus_Sigma(:,:,ik,i_sp_pol)
   call RT_EULER_step_apply(G_k,H_k,dG_out,nbf,dT,"def")
   !
   return
   !
 endif
 !
 ! Part of the hamiltonina multiplied by rho0+rho1
 !
 H_k=Ho_plus_Sigma(:,:,ik,i_sp_pol)
 if(.not.l_RT_RWA) H_k=H_k-H_EQ(:,:,ik,i_sp_pol)
 if(  l_RT_FRES  ) H_k=H_k-H_field(:,:,ik,i_sp_pol)
 !
 ! First the rho0 term. This gives rho1.
 !
 G_k=G_lesser_reference(:,:,ik,i_sp_pol)
 !
 call RT_EULER_step_apply(G_k,H_k,dG_out,nbf,dT,"def")
 !
 ! Then the rho1 term. This gives rho2.
 !
 ! The first order rho is zero in the vv' and cc' channel
 ! Thus I compute the second order rho, constructed using the 
 ! first order rho from the cv channel
 !
 ! N.B. In the case l_RT_FRES the above sentence is not true anymore
 !      However I still want only the cv channel to enter here
 !      to avoid energy shifts due to repulsion
 !
 ! Take in input only the cv channel
 !
 G_k=cZERO
 do ib1=RT_bands(1),nbf
   do ib2=nbf+1,RT_bands(2)
     G_k(ib1,ib2)=dG_in(ib1,ib2)
     G_k(ib2,ib1)=dG_in(ib2,ib1)
   enddo
 enddo
 !
 call RT_EULER_step_apply(G_k,H_k,dG_out,nbf,dT,"pop")
 !
 if( l_RT_RWA .and. (.not.l_RT_FRES) ) return
 !
 ! Part of the hamiltonian multiplied by the full rho
 !
 H_k=cZERO
 if(.not.l_RT_RWA) H_k=H_EQ(:,:,ik,i_sp_pol)
 if(  l_RT_FRES  ) H_k=H_k+H_field(:,:,ik,i_sp_pol)
 G_k=dG_in+G_lesser_reference(:,:,ik,i_sp_pol)
 !
 call RT_EULER_step_apply(G_k,H_k,dG_out,nbf,dT,"add")
 !
end subroutine RT_EULER_step
!
!
subroutine RT_EULER_step_apply(G_k,H_k,dG_out,nbf,deltaT,mode)
 !
 use pars,           ONLY:SP,cI
 use wrapper_omp,    ONLY:M_by_M_omp
 use real_time,      ONLY:RT_bands,RT_nbands
 !
 implicit none
 !
 character(*),intent(in)  :: mode
 integer,     intent(in)  :: nbf
 real(SP),    intent(in)  :: deltaT
 complex(SP), intent(in)  :: G_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(in)  :: H_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(out) :: dG_out(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 ! Workspace
 !
 complex(SP)  :: I1_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP)  :: I2_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 integer      :: ib1,ib2
 !
 ! I1 = Ho_plus_Sigma*G   
 ! I2 = G*Ho_plus_Sigma
 !
 call M_by_M_omp('n','n',RT_nbands,H_k,G_k,I1_k)
 call M_by_M_omp('n','n',RT_nbands,G_k,H_k,I2_k)
 !
 ! dG_out = G(T0+dt) -G(T0)= -i dt * ( I1(T0)-I2(T0) )
 !
 if(mode=="def") dG_out=-cI*deltaT*(I1_k-I2_k)
 !
 if(mode=="add") dG_out=dG_out-cI*deltaT*(I1_k-I2_k)
 !
 if(mode=="pop") then
   !
   ! Save in output only the vv' and cc' channels
   ! This will control the normalization of the residuals
   !
   do ib1=RT_bands(1),nbf
     do ib2=RT_bands(1),nbf
       dG_out(ib1,ib2)=dG_out(ib1,ib2)-cI*deltaT*(I1_k(ib1,ib2)-I2_k(ib1,ib2))
     enddo
   enddo
   !
   do ib1=nbf+1,RT_bands(2)
     do ib2=nbf+1,RT_bands(2)
       dG_out(ib1,ib2)=dG_out(ib1,ib2)-cI*deltaT*(I1_k(ib1,ib2)-I2_k(ib1,ib2))
     enddo
   enddo
   !
 endif
 !
end subroutine RT_EULER_step_apply
