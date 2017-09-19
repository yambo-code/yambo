!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
!
! Copyright (C) 1992-2005, Lucia Reining, Valerio Olevano,
!   Francesco Sottile, Stefan Albrecht, Giovanni Onida,
!                    Fabien Bruneval
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
subroutine DIPOLE_p_matrix_elements(Xk,iv_wf,ic_wf,ik,wf_dim,rho)
 !
 use pars,          ONLY:SP,cZERO,cONE,pi
 use wrapper_omp,   ONLY:M_by_V_omp
 use D_lattice,     ONLY:alat
 use wave_func,     ONLY:WF,wf_ng
 use R_lattice,     ONLY:g_vec,bz_samp
 use electrons,     ONLY:n_spinor
 !
 implicit none
 !
 type(bz_samp), intent(in)  :: Xk
 integer,       intent(in)  :: iv_wf,ic_wf,ik,wf_dim
 complex(SP),   intent(out) :: rho(4)
 ! 
 ! Work Space
 !
#if defined _OPENMP
 integer      :: ig
#endif
 integer      :: i_spinor,i1
 complex(SP)  :: wf_xyz(wf_ng,wf_dim),coeff
 !
 rho=cZERO
 !
 do i_spinor=1,n_spinor
   !
   ! wf_xyz = -i grad fdo(iv,ik)
   !
#if defined _OPENMP
!$omp parallel default(shared), private(i1,ig)
   do i1=1,3
!$omp do
     do ig=1,wf_ng
       wf_xyz(ig,i1)=WF%c(ig,i_spinor,iv_wf)*(Xk%pt(ik,i1)+g_vec(ig,i1))*2.0_SP*pi/alat(i1)
     enddo
!$omp end do
   enddo
!$omp end parallel
#else
   do i1=1,3
     wf_xyz(:,i1)=WF%c(:,i_spinor,iv_wf)*(Xk%pt(ik,i1)+g_vec(:wf_ng,i1))*2.0_SP*pi/alat(i1)
   enddo
#endif
   ! 
   !
   ! WARNING: 
   ! ========
   ! The convention is to revert the order of the bands
   ! indexes in the definition of DIP_P and DIP_iR :
   ! DIP_P(iv,ic)  = P_cv ; DIP_iR(iv,ic) = (iR)_cv 
   !
   ! Note that P is Hermitian and iR anti-hermitian.
   !
   ! rho = <v|p|c> = P_vc =
   !     = wf_xyz^* fdo(ic,ik)= - i < fdo(iv,ik)| grad |  fdo(ic,ik) > =
   !       \sum_G -i u^*_{iv ik}(G) i ( k + G ) u_{ic ik}(G)
   !     = \sum_G u^*_{iv ik}(G) ( k + G ) u_{ic ik}(G)
   !
   ! rho(4) =  \sum_G u^*_{iv ik}(G) ( k + G )^2 u_{ic ik}(G)
   !
   coeff=cmplx(i_spinor-1,0,SP)
   call M_by_V_omp('C',wf_ng,wf_dim,cONE,wf_xyz,wf_ng,WF%c(:,i_spinor,ic_wf),1,coeff,rho(:wf_dim),1)
   !
 enddo
 !
end subroutine
