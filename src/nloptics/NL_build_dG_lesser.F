!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA
!
!> @brief Calculate the density matrix variation \Delta \rho_{ij}
!!
!! @param[in]       V_bands                     time-dependent valence bands
!! @param[in]       en                          energies array
!!
!! @param[out]      dG                          \Delta G-lesser diagonal part = -i\rho
!
subroutine NL_build_dG_lesser(en,V_bands,dG) 
 !
 ! Construct dG = G_lesser(t) - G_lesser(t=0) 
 ! from the time-dependent valence bands
 !
 ! \rho(i1,i2,ik) = \sum_{iv}^{N_v}  V*(i2,iv,ik) V(i1,iv,ik)
 !
 use pars,           ONLY:SP,cI,cZERO
 use electrons,      ONLY:levels,spin_occ,n_sp_pol
 use nl_optics,      ONLY:NL_bands
 use wrapper_omp,    ONLY:Vstar_dot_V_omp
 use QP_m,           ONLY:QP_nk
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX
 use parallel_int,   ONLY:PP_redux_wait
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 !
 type(levels), intent(in)  :: en
 complex(SP),  intent(in)  :: V_bands(NL_bands(2),maxval(en%nbf),QP_nk,n_sp_pol)
 complex(SP),  intent(out) :: dG(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),QP_nk,n_sp_pol)
 !
 ! Work Space
 !
 integer :: i1,i2,is
 integer :: ik
 !
#if defined _TIMING
 call timing('NL build_dG',OPR='start')
#endif
 !
!$OMP PARALLEL WORKSHARE  
 dG=cZERO
!$OMP END PARALLEL WORKSHARE  
 !
 do is=1,n_sp_pol
 do ik=1,QP_nk
   if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
   do i1=NL_bands(1),NL_bands(2)             
!$omp parallel do default(shared), private(i2)
     do i2=i1,NL_bands(2)
       dG(i1,i2,ik,is)=Vstar_dot_V_omp(en%nbf(1),V_bands(i2,:,ik,is),V_bands(i1,:,ik,is))
       dG(i2,i1,ik,is)=conjg(dG(i1,i2,ik,is))
     enddo
!$omp end parallel do
   enddo
 enddo
 enddo
 !
!$OMP PARALLEL WORKSHARE  
 dG=cI*spin_occ*dG
!$OMP END PARALLEL WORKSHARE  
 !
 call PP_redux_wait(dG,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 !
!$omp parallel do default(shared), private(i1)
 do i1=NL_bands(1),en%nbf(1)
   dG(i1,i1,:,:)=dG(i1,i1,:,:)-cI*spin_occ
 enddo
!$omp end parallel do
 !
#if defined _TIMING
 call timing('NL build_dG',OPR='stop')
#endif
 !
end subroutine NL_build_dG_lesser
