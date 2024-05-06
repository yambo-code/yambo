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
! =============================================================== !
! !!!!  TEST SUBROUTINE !!!!!!
! The following subroutine is only for testing purpose
! It is not parallized and it's never used in the code 
! because the version OSCLL_compose_nl.F is much faster and does not 
! require the calculation of dG
!
! =============================================================== !
! 
subroutine OSCLL_old_compose_nl(V_bands,H_nl_sc,k,q,E,i_k,i_sp_pol) 
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
 use wrapper_omp,    ONLY:V_dot_V_c1_omp,Vstar_dot_V_omp
 use parallel_m,     ONLY:PAR_Xk_ibz_index
 ! 
 implicit none
 !
 complex(SP), intent(inout) :: H_nl_sc(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
 integer,     intent(in)    :: i_k,i_sp_pol
 type(bz_samp), intent(in)  :: k,q
 type(levels),  intent(in)  :: E
 complex(SP),  intent(in)   :: V_bands(NL_bands(2),E%nbf(1),QP_nk,n_sp_pol)
 !
 ! Work Space
 !
 complex(SP) :: dG(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),QP_nk)
 complex(SP) :: COLL(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),QP_nk)
 integer :: i1,i2,i_n,i_m
 integer :: ik,i_kmq
 !
 call NL_build_dG_lesser(E,V_bands,dG)
 !
 do i_n=NL_bands(1),NL_bands(2)
   do i_m=i_n,NL_bands(2)
     !
     call OSCLL_compose_collision(k,q,i_n,i_m,i_k,COLL)
     !
     do i_kmq=1,nXkibz
      !
      do i1=NL_bands(1),NL_bands(2)
        do i2=NL_bands(1),NL_bands(2)
          H_nl_sc(i_n,i_m)=H_nl_sc(i_n,i_m)-cI*COLL(i1,i2,i_kmq)*dG(i1,i2,i_kmq)
         enddo
       enddo
    enddo
    !
    H_nl_sc(i_m,i_n)=conjg(H_nl_sc(i_n,i_m))
    !
   enddo
   !
   H_nl_sc(i_n,i_n)=real(H_nl_sc(i_n,i_n))
   ! 
 enddo
 !
end subroutine OSCLL_old_compose_nl
