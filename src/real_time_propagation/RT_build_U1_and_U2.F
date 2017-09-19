!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine RT_build_U1_and_U2(ik,U1_k,U2_k,dT)
  !
  use pars,           ONLY:SP,cI
  use real_time,      ONLY:Ho_plus_Sigma,RT_bands,RT_nbands
  use wrapper_omp,    ONLY:M_by_M_omp
  !
  implicit none
  !
  integer,     intent(in)  :: ik
  complex(SP), intent(out) :: U1_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
  complex(SP), intent(out) :: U2_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
  real(SP),    intent(in)  :: dT
  !
  complex(SP)  :: M1_tmp(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
  complex(SP)  :: M2_tmp(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
  complex(SP)  :: idT
  !
  ! Taylor expansion
  !
  ! U(t) = exp(H * t) = 1 + (Ht) + (Ht)^2/2! + (Ht)^3/3! + (Ht)^4/4! + O(Ht^5)
  !                     however the 1 is stored in another matrix
  !
  idT =-cI*dT
  U1_k=+idT*Ho_plus_Sigma(:,:,ik,1)
  U2_k=-idT*Ho_plus_Sigma(:,:,ik,1)
  !
  call M_by_M_omp('n','n',RT_nbands,Ho_plus_Sigma(:,:,ik,1),Ho_plus_Sigma(:,:,ik,1),M1_tmp)
  !
  U1_k=U1_k+idT**2*M1_tmp/2._SP
  U2_k=U2_k+idT**2*M1_tmp/2._SP
  !
  call M_by_M_omp('n','n',RT_nbands,Ho_plus_Sigma(:,:,ik,1),M1_tmp,M2_tmp)
  !
  U1_k=U1_k+idT**3*M2_tmp/6._SP
  U2_k=U2_k-idT**3*M2_tmp/6._SP
  !
  call M_by_M_omp('n','n',RT_nbands,Ho_plus_Sigma(:,:,ik,1),M2_tmp,M1_tmp)
  !
  U1_k=U1_k+idT**4*M1_tmp/24._SP
  U2_k=U2_k+idT**4*M1_tmp/24._SP
  !
  call M_by_M_omp('n','n',RT_nbands,Ho_plus_Sigma(:,:,ik,1),M1_tmp,M2_tmp)
  !
  U1_k=U1_k+idT**5*M2_tmp/120._SP
  U2_k=U2_k-idT**5*M2_tmp/120._SP
  !
end subroutine RT_build_U1_and_U2
