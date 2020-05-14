!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine DIPOLE_kb_sum(ic_wf,iv_wf,rho,kbv)
 !
 use pars,          ONLY:SP,cZERO
 use wrapper,       ONLY:V_dot_V
 use wave_func,     ONLY:WF,wf_ng
 use electrons,     ONLY:n_spinor
 use pseudo,        ONLY:pp_kbv_dim_yambo
 use cuda_m,        ONLY:have_cuda
 use deviceXlib_m,  ONLY:dev_memcpy
 !
#include<dev_defs.h>
 !
 implicit none
 !
 integer     :: iv_wf,ic_wf
 complex(SP) :: rho(3)
 complex(SP) DEV_ATTRIBUTE :: kbv(wf_ng,n_spinor,4,pp_kbv_dim_yambo)
 ! 
 ! Work Space
 !
 integer     :: i1,i2,i_spinor,ig
 complex(SP), pointer DEV_ATTRIBUTE :: WF_c(:,:,:)
 complex(SP) DEV_ATTRIBUTE :: XX(2,n_spinor,4,pp_kbv_dim_yambo)
 complex(SP) :: drho(n_spinor,3,pp_kbv_dim_yambo)
#ifdef _CUDA
 complex(SP) DEV_ATTRIBUTE :: DEV_VARNAME(drho)(n_spinor,3,pp_kbv_dim_yambo)
#endif
 !
 if (pp_kbv_dim_yambo==0) return
 !
 XX=cZERO
 DEV_VARNAME(drho)=cZERO
 !
 WF_c => DEV_VARNAME(WF%c)
 !
#if defined(_CUDA)
 !$cuf kernel do(3) <<<*,*>>>
#else
 !$omp parallel do default(shared), private(i1,i2,i_spinor), collapse(3)
#endif
 do i1=1,pp_kbv_dim_yambo
   do i2=1,4
     do i_spinor=1,n_spinor
       do ig=1,wf_ng
         XX(1,i_spinor,i2,i1)=XX(1,i_spinor,i2,i1)+WF_c(ig,i_spinor,iv_wf)*kbv(ig,i_spinor,i2,i1)
         XX(2,i_spinor,i2,i1)=XX(2,i_spinor,i2,i1)+WF_c(ig,i_spinor,ic_wf)*kbv(ig,i_spinor,i2,i1)
       enddo
     enddo
   enddo
 enddo
 !
 !
#if defined _CUDA
 !$cuf kernel do(3) <<<*,*>>>
#else
 !$omp parallel do default(shared), private(i1,i2,i_spinor), collapse(3)
#endif
 do i1=1,pp_kbv_dim_yambo
   do i2=1,3
     do i_spinor=1,n_spinor
       DEV_VARNAME(drho)(i_spinor,i2,i1)=conjg(XX(1,i_spinor,1,i1))*XX(2,i_spinor,i2+1,i1)+ &
&                                        conjg(XX(1,i_spinor,i2+1,i1))*XX(2,i_spinor,1,i1)
     enddo
   enddo
 enddo
 !
 if (have_cuda) call dev_memcpy(drho,DEV_VARNAME(drho))
 forall(i1=1:3) rho(i1)=rho(i1)+sum(drho(:,i1,:))
 !
end subroutine
