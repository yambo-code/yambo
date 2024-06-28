!
! License-Identifier: GPL
!
! Copyright (C) 2014 The Yambo Team
!
! Authors (see AUTHORS file for details): AC
!
subroutine OSCLL_compose_nl(dG,H_nl_sc,k,q,E,i_k,i_sp_pol) 
 !
 ! This subroutine calculate the long-range part of the Screened exchange
 !
 use electrons,      ONLY:n_sp_pol,levels,spin_occ
 use pars,           ONLY:SP,cI,cZERO,cONE
 use FFT_m,          ONLY:fft_size
 use nl_optics,      ONLY:NL_bands,NL_nbands
 use collision_ext,  ONLY:ng_oscll,OSCLL,W
 use QP_m,           ONLY:QP_nk
 use R_lattice,      ONLY:bz_samp,qindx_S,nXkibz
 use wrapper_omp,    ONLY:V_dot_V
 use parallel_m,     ONLY:PAR_Xk_ibz_index
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 ! 
 implicit none
 !
 complex(SP), intent(inout) :: H_nl_sc(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
 integer,     intent(in)    :: i_k,i_sp_pol
 type(bz_samp), intent(in)  :: k,q
 type(levels),  intent(in)  :: E
 complex(SP),  intent(in)   :: dG(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),QP_nk)
 !
 ! Work Space
 !
 complex(SP) :: COLL(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),QP_nk)
#if defined _DOUBLE
 complex(SP), external :: ZDOTU
#else
 complex(SP), external :: CDOTU
#endif
 integer :: i1,i2,i_n,i_m
 !
#if defined _TIMING
 call timing('OSCLL compose NL',OPR='start')
#endif
 !
 do i_n=NL_bands(1),NL_bands(2)
   do i_m=i_n,NL_bands(2)
    !
    call OSCLL_compose_collision(k,q,i_n,i_m,i_k,COLL)
    !
#if defined _DOUBLE
        H_nl_sc(i_n,i_m)=H_nl_sc(i_n,i_m)-cI*ZDOTU(NL_nbands**2*QP_nk,COLL,1,dG,1)
#else
        H_nl_sc(i_n,i_m)=H_nl_sc(i_n,i_m)-cI*CDOTU(NL_nbands**2*QP_nk,COLL,1,dG,1)
#endif
    !
   enddo
 enddo
 !
 ! Symmetrization
 ! 
 do i_n=NL_bands(1),NL_bands(2)
   do i_m=i_n+1,NL_bands(2)
     H_nl_sc(i_m,i_n)=conjg(H_nl_sc(i_n,i_m))
   enddo
   H_nl_sc(i_n,i_n)=real(H_nl_sc(i_n,i_n))
 enddo
 !
#if defined _TIMING
 call timing('OSCLL compose NL',OPR='stop')
#endif
 !
end subroutine OSCLL_compose_nl
