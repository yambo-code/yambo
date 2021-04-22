!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS, AF
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
 use pars,          ONLY:SP,cONE,cZERO
 use wrapper,       ONLY:M_by_M
 use wave_func,     ONLY:WF,wf_ng
 use electrons,     ONLY:n_spinor
 use pseudo,        ONLY:pp_kbv_dim_yambo
 use cuda_m,        ONLY:have_cuda
 use deviceXlib_m,  ONLY:dev_memcpy
 use iso_c_binding, ONLY:C_F_POINTER
#if defined _CUDA
 use cudafor,       ONLY:C_DEVLOC
#else
 use iso_c_binding, ONLY:C_LOC
#endif
!
#include<dev_defs.h>
 !
 implicit none
 !
 integer     :: iv_wf,ic_wf
 complex(SP) :: rho(3)
 complex(SP) DEV_ATTR, target :: kbv(wf_ng,n_spinor,4,pp_kbv_dim_yambo)
 ! 
 ! Work Space
 !
 integer     :: i1,i2,i3,ig,i_spinor,j_spinor,dim_flat
 !
 complex(SP) DEV_ATTR, target :: XXv(n_spinor,n_spinor,4,pp_kbv_dim_yambo),XXc(n_spinor,n_spinor,4,pp_kbv_dim_yambo)
 complex(SP) DEV_ATTR, pointer :: kbv_p(:,:),XXv_p(:,:),XXc_p(:,:)
 complex(SP)          :: drho(n_spinor,3,pp_kbv_dim_yambo)
 !complex(SP) DEV_ATTR, pointer :: WF_p(:,:,:)
#ifdef _CUDA
 complex(SP) DEV_ATTR :: WF_v(wf_ng,n_spinor),WF_c(wf_ng,n_spinor)
 complex(SP) DEV_ATTR :: DEV_VAR(drho)(n_spinor,3,pp_kbv_dim_yambo)
#endif
 !
 if (pp_kbv_dim_yambo==0) return
 !
 dim_flat=n_spinor*4*pp_kbv_dim_yambo
 !
 ! This works since WF has the target attribute in mod_wave_function 
 !
#if defined _CUDA
 call C_F_POINTER(C_DEVLOC(kbv), kbv_p, (/wf_ng,dim_flat/) )
 call C_F_POINTER(C_DEVLOC(XXv), XXv_p, (/n_spinor,dim_flat/) )
 call C_F_POINTER(C_DEVLOC(XXc), XXc_p, (/n_spinor,dim_flat/) )
 !
 !WF_p => WF%c
 call dev_memcpy(WF_v,WF%c(:,:,iv_wf))
 call dev_memcpy(WF_c,WF%c(:,:,ic_wf))
 !
 call M_by_M('T','N', n_spinor, dim_flat, wf_ng, cONE, WF_v, wf_ng, kbv_p, wf_ng, cZERO, XXv_p, n_spinor)
 call M_by_M('T','N', n_spinor, dim_flat, wf_ng, cONE, WF_c, wf_ng, kbv_p, wf_ng, cZERO, XXc_p, n_spinor)
#else
 call C_F_POINTER(C_LOC(kbv), kbv_p, (/wf_ng,dim_flat/) )
 call C_F_POINTER(C_LOC(XXv), XXv_p, (/n_spinor,dim_flat/) )
 call C_F_POINTER(C_LOC(XXc), XXc_p, (/n_spinor,dim_flat/) )
 !
 call M_by_M('T','N', n_spinor, dim_flat, wf_ng, cONE, WF%c(:,:,iv_wf), wf_ng, kbv_p, wf_ng, cZERO, XXv_p, n_spinor)
 call M_by_M('T','N', n_spinor, dim_flat, wf_ng, cONE, WF%c(:,:,ic_wf), wf_ng, kbv_p, wf_ng, cZERO, XXc_p, n_spinor)
#endif
 !
#if defined _CUDA
 !$cuf kernel do(3) <<<*,*>>>
#else
 !$omp parallel do default(shared), private(i1,i2,i_spinor), collapse(3)
#endif
 do i1=1,pp_kbv_dim_yambo
   do i2=1,3
     do i_spinor=1,n_spinor
       DEV_VAR(drho)(i_spinor,i2,i1)=conjg(XXv(i_spinor,i_spinor,1,i1))*XXc(i_spinor,i_spinor,i2+1,i1)+ &
&                                    conjg(XXv(i_spinor,i_spinor,i2+1,i1))*XXc(i_spinor,i_spinor,1,i1)
     enddo
   enddo
 enddo
 !
 if (have_cuda) call dev_memcpy(drho,DEV_VAR(drho))
 forall(i1=1:3) rho(i1)=rho(i1)+sum(drho(:,i1,:))
 !
end subroutine
