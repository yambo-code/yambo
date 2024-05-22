!
!        Copyright (C) 2020-2023 the Yambo team
!
! Authors (see AUTHORS file for details): CA
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine OSCLL_compose_nl_fast(V_input,dG,H_nl_sc,k,q,E,i_k,i_sp_pol) 
 !
 ! This subroutine calculate the long-range part of the Screened exchange
 !
 use electrons,      ONLY:n_sp_pol,levels,spin_occ
 use pars,           ONLY:SP,cI,cZERO,cONE
 use FFT_m,          ONLY:fft_size
 use nl_optics,      ONLY:NL_bands,NL_nbands,Sigma_SEX_EQ
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
 complex(SP),  intent(in)  :: V_input(NL_bands(2),maxval(E%nbf),QP_nk,n_sp_pol)
 !
 ! Work Space
 !
 integer :: i1,i2,i_n,i_m,ib,ibp,i_kmq,ig1,ig2,ik_mem,iqbz,iv
 complex(SP) :: V_kmq(NL_bands(1):NL_bands(2),maxval(E%nbf),q%nbz)
 complex(SP) :: OSCLL_val(NL_bands(1):NL_bands(2),maxval(E%nbf),ng_oscll,q%nbz)
 complex(SP) :: val_sum(q%nbz,ng_oscll,ng_oscll)
#if defined _DOUBLE
 complex(SP), external :: ZDOTU
#else
 complex(SP), external :: CDOTU
#endif
 !
#if defined _TIMING
 call timing('OSCLL compose NL',OPR='start')
#endif
 !
 ik_mem=PAR_Xk_ibz_index(i_k)
 !
 ! Build Oscillators in term of time-dependent valence bands 
 !
!$omp parallel do default(shared), private(iqbz,i_kmq)
 do iqbz=1,q%nbz
   i_kmq           =  k%sstar(qindx_S(i_k,iqbz,1),1)
   V_kmq(:,:,iqbz) = V_input(NL_bands(1):,:,i_kmq,i_sp_pol)
 enddo
!$omp end parallel do
 !
!$omp parallel do default(shared), private(iv)
 do iv=1,maxval(E%nbf)
   forall(iqbz=1:q%nbz,i_n=NL_bands(1):NL_bands(2),ig1=1:ng_oscll)
     OSCLL_val(i_n,iv,ig1,iqbz)=sum(OSCLL(i_n,NL_bands(1):NL_bands(2),ik_mem,iqbz,ig1)*V_kmq(NL_bands(1):NL_bands(2),iv,iqbz))
   end forall
 enddo
!$omp end parallel do
 !
 ! Remove equilibrium Sigma-sex
 !
!$OMP WORKSHARE  
   H_nl_sc(:,:)=H_nl_sc(:,:)-Sigma_SEX_EQ(:,:,i_sp_pol,ik_mem)
!$OMP END WORKSHARE 
 !
 ! Long-range SEX
 !
 ! Evaluate Sigma_SEX(i_m,i_n,i_k)
 !
 do i_n=NL_bands(1),NL_bands(2)
   do i_m=i_n,NL_bands(2)
     !
!$omp parallel do default(shared), private(ig1,ig2,iqbz)
     do iqbz=1,q%nbz
       forall(ig1=1:ng_oscll,ig2=1:ng_oscll)
         val_sum(iqbz,ig1,ig2)=sum(OSCLL_val(i_n,:,ig1,iqbz)*conjg(OSCLL_val(i_m,:,ig2,iqbz))) ! sum of valence bands
       end forall
     enddo
!$omp end parallel do
     !
#if defined _DOUBLE
     H_nl_sc(i_n,i_m)=H_nl_sc(i_n,i_m)+spin_occ*ZDOTU(ng_oscll**2*q%nbz,W,1,val_sum,1)
#else
     H_nl_sc(i_n,i_m)=H_nl_sc(i_n,i_m)+spin_occ*CDOTU(ng_oscll**2*q%nbz,W,1,val_sum,1)
#endif
     !
   enddo
 enddo
 !
 ! Symmetrization
 ! 
!$omp parallel do default(shared), private(i_n,i_m)
 do i_n=NL_bands(1),NL_bands(2)
   do i_m=i_n+1,NL_bands(2)
     H_nl_sc(i_m,i_n)=conjg(H_nl_sc(i_n,i_m))
   enddo
   H_nl_sc(i_n,i_n)=real(H_nl_sc(i_n,i_n))
 enddo
!$omp end parallel do
 !
#if defined _TIMING
 call timing('OSCLL compose NL',OPR='stop')
#endif
 !
end subroutine OSCLL_compose_nl_fast
