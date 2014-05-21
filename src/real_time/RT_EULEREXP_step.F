!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
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
subroutine RT_EULEREXP_step(G_new_k,G_old_k,G_ref,U1_k,U2_k,I1_matrix)
 !
 use pars,           ONLY:SP,cZERO
#if defined _OPENMP
 use wrapper,        ONLY:M_by_M_omp
#else
 use wrapper,        ONLY:M_by_M
#endif
 use SC,             ONLY:SC_bands,SC_nbands
 !
 implicit none
 !
 complex(SP), intent(in)  :: G_old_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP), intent(in)  :: G_ref(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP), intent(in)  :: U1_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP), intent(in)  :: U2_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP), intent(in)  :: I1_matrix(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP), intent(out) :: G_new_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 !
 complex(SP) :: M1_tmp(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2)) 
 complex(SP) :: M2_tmp(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2)) 
 complex(SP) :: U1p1_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP) :: U2p1_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 !
 ! dG(T+dt) = U1(dt)*dG(T)*U2(dt) +               order= dG*1
 !            (U1(dt)-1)*G0 + G0*(U2(dt)-1) +     order= G0*dt
 !            (U1(dt)-1)*G0*(U2(dt)-1)            order= G0*dt**2
 !
 U1p1_k=U1_k+I1_matrix
 U2p1_k=U2_k+I1_matrix
 !
 G_new_k=cZERO
 M1_tmp=cZERO
#if defined _OPENMP
 call M_by_M_omp(SC_nbands,U1p1_k,G_old_k,M1_tmp)
 call M_by_M_omp(SC_nbands,M1_tmp,U2p1_k,G_new_k)
#else
 call M_by_M('n','n',SC_nbands,U1p1_k,G_old_k,M1_tmp)
 call M_by_M('n','n',SC_nbands,M1_tmp,U2p1_k,G_new_k)
#endif
 !
 M1_tmp=cZERO
 M2_tmp=cZERO
#if defined _OPENMP
 call M_by_M_omp(SC_nbands,U1_k,G_ref,M1_tmp)
 call M_by_M_omp(SC_nbands,G_ref,U2_k,M2_tmp)
#else
 call M_by_M('n','n',SC_nbands,U1_k,G_ref,M1_tmp)
 call M_by_M('n','n',SC_nbands,G_ref,U2_k,M2_tmp)
#endif
 G_new_k=G_new_k+M1_tmp+M2_tmp
 !
 M1_tmp=cZERO
 M2_tmp=cZERO
#if defined _OPENMP
 call M_by_M_omp(SC_nbands,U1_k,G_ref,M1_tmp)
 call M_by_M_omp(SC_nbands,M1_tmp,U2_k,M2_tmp)
#else
 call M_by_M('n','n',SC_nbands,U1_k,G_ref,M1_tmp)
 call M_by_M('n','n',SC_nbands,M1_tmp,U2_k,M2_tmp)
#endif
 G_new_k=G_new_k+M2_tmp
 !
end subroutine RT_EULEREXP_step
