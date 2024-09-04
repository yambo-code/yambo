!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA DS
!
subroutine RT_Glesser_evolve(En,kpt,dG_old,dG_in,dG_out,dT)
 !
 ! dG_old = dG(T0)     --> Time at first iteration in integrator with more than one step
 ! dG_in  = dG(T )     --> Effective time at the present step, i.e. for RK2 at step 2, T=T0+dt/2
 ! dG_out = dG(T0+dt)
 !
 !  T0+dt = T+dt'
 !
 use pars,           ONLY:SP,cZERO
 use electrons,      ONLY:levels,spin_occ
 use R_lattice,      ONLY:bz_samp
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PAR_G_k_range
 use real_time,      ONLY:G_lesser_reference,RT_bands,RT_nk,RTibz,l_slow_TE
 !
 implicit none
 !
 type(bz_samp),  intent(in) :: kpt
 type(levels),   intent(in) :: En
 !
 real(SP),       intent(in) :: dT
 complex(SP),    intent(in) :: dG_old(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2))
 complex(SP),    intent(in) :: dG_in(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2))
 complex(SP),   intent(out) :: dG_out(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2))
 !
 ! Work space
 !
 integer :: ik
 !
 dG_out=cZERO
 !
 do ik=1,RT_nk
   !
   if( .not.PAR_IND_Xk_ibz%element_1D(RTibz%k_map(ik)) ) cycle
   !
   if (     l_slow_TE) call RT_slow_dynamics_driver(dG_old(:,:,ik),dG_in(:,:,ik),dG_out(:,:,ik),ik,dT)
   if (.not.l_slow_TE) call RT_EULER_step(dG_in(:,:,ik),dG_out(:,:,ik),ik,En%nbf,dT)
   !
   ! Carriers relaxation step:
   !
   ! G(T0+dt) = G(T0) - i dt * Sigma_relax(T)
   !
   call RT_Scattering_step(dG_old(:,:,ik),dG_in(:,:,ik),dG_out(:,:,ik),ik,dT)
   !
   ! G(T+dt) = G(t)+dG(T)
   dG_out(:,:,ik)=dG_out(:,:,ik)+dG_in(:,:,ik)
   !
   call RT_G_symmetrization(dG_out(:,:,ik),ik)
   !
 enddo
 !
 contains
   !
   subroutine RT_G_symmetrization(G_inout,ik)
     !
     integer,     intent(in)    :: ik
     complex(SP), intent(inout) :: G_inout(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
     !
     integer                :: ib,ibp
     real(SP)               :: E_occ_tmp,H_occ_tmp
     complex(SP)            :: tmp
     !
     do ib=RT_bands(1),RT_bands(2)
       !
       G_inout(ib,ib)=cmplx(0._SP,aimag(G_inout(ib,ib)))
       !
       E_occ_tmp= aimag(G_inout(ib,ib))+            aimag(G_lesser_reference(ib,ib,ik))
       H_occ_tmp=-aimag(G_inout(ib,ib))+ ( spin_occ-aimag(G_lesser_reference(ib,ib,ik)) )
       !
       if ( E_occ_tmp<0._SP .or. H_occ_tmp>spin_occ ) G_inout(ib,ib)=                      G_lesser_reference(ib,ib,ik)
       if ( H_occ_tmp<0._SP .or. E_occ_tmp>spin_occ ) G_inout(ib,ib)=cmplx(0._SP,spin_occ)-G_lesser_reference(ib,ib,ik)
       !
       do ibp=ib+1,RT_bands(2)
         !
         tmp=( G_inout(ib,ibp)-conjg(G_inout(ibp,ib)) )/2._SP
         !
         G_inout(ib,ibp)=       tmp
         G_inout(ibp,ib)=-conjg(tmp)
         !
       enddo
       !
     enddo
     !
   end subroutine
   !
end subroutine RT_Glesser_evolve
