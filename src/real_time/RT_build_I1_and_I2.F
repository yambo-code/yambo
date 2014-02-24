!
! Copyright (C) 2000-2013 C. Attaccalite and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine RT_build_I1_and_I2(G_input_k,ik,I1_k,I2_k)
 !
 use pars,           ONLY:SP,cZERO
 use real_time,      ONLY:Ho_plus_Sigma,Sigma_relax
 use SC,             ONLY:SC_bands,SC_nbands
#if defined _OPENMP
 use wrapper,        ONLY:M_by_M_omp
#else
 use wrapper,        ONLY:M_by_M
#endif
 implicit none
 !
 integer, intent(in)      :: ik
 complex(SP), intent(in)  :: G_input_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP), intent(out) :: I1_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP), intent(out) :: I2_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 !
 I1_k=cZERO
 I2_k=cZERO
#if defined _OPENMP
 call M_by_M_omp(SC_nbands,Ho_plus_Sigma(:,:,ik,1),G_input_k,I1_k)
 call M_by_M_omp(SC_nbands,G_input_k,Ho_plus_Sigma(:,:,ik,1),I2_k)     
#else
 call M_by_M('n','n',SC_nbands,Ho_plus_Sigma(:,:,ik,1),G_input_k,I1_k)
 call M_by_M('n','n',SC_nbands,G_input_k,Ho_plus_Sigma(:,:,ik,1),I2_k)     
#endif
 !
 if (allocated(Sigma_relax))  I1_k = I1_k + Sigma_relax(:,:,ik)
 !
end subroutine RT_build_I1_and_I2
