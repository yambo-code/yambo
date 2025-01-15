!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA
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
 complex(SP),  intent(out) :: dG(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),QP_nk)
 !
 ! Work Space
 !
 integer :: i1,i2
 integer :: ik
 !
 if (n_sp_pol==2) call error(" NL_build_dG not coded for n_sp_pol==2")
 !
#if defined _TIMING
 call timing('NL build_dG',OPR='start')
#endif
 !
 dG=cZERO
 !
 do ik=1,QP_nk
   if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
   do i1=NL_bands(1),NL_bands(2)             
!$omp parallel do default(shared), private(i2)
     do i2=i1,NL_bands(2)
       dG(i1,i2,ik)=Vstar_dot_V_omp(en%nbf(1),V_bands(i2,:,ik,1),V_bands(i1,:,ik,1))
       dG(i2,i1,ik)=conjg(dG(i1,i2,ik))
     enddo
!$omp end parallel do
   enddo
 enddo
 !
 dG=cI*spin_occ*dG
 !
 call PP_redux_wait(dG,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 !
!$omp parallel do default(shared), private(i1)
 do i1=NL_bands(1),en%nbf(1)
   dG(i1,i1,:)=dG(i1,i1,:)-cI*spin_occ
 enddo
!$omp end parallel do
 !
#if defined _TIMING
 call timing('NL build_dG',OPR='stop')
#endif
 !
end subroutine NL_build_dG_lesser
