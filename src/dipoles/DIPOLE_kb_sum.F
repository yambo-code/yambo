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
#include<dev_defs.h>
!
subroutine DIPOLE_kb_sum(ic_wf,iv_wf,rho,kbv)
 !
 use iso_c_binding, ONLY:C_F_POINTER,C_LOC
 use pars,          ONLY:SP,cONE,cZERO
 use wrapper,       ONLY:M_by_M,DEV_SUB(M_by_M)
 use wave_func,     ONLY:WF,wf_ng
 use electrons,     ONLY:n_spinor
 use pseudo,        ONLY:pp_kbv_dim_yambo
 use cuda_m,        ONLY:have_cuda
 use deviceXlib_m,  ONLY:dev_memcpy_h2d,dev_memcpy_d2h,dev_allocated
 use DIPOLES,       ONLY:XXv=>DIP_work_XXv,XXc=>DIP_work_XXc,&
&                        WFv=>DIP_work_WFv,WFc=>DIP_work_WFc,&
&                        drho=>DIP_work_drho
#if defined _CUDAF
 use DIPOLES,       ONLY:DEV_VAR(drho)=>DEV_VAR(DIP_work_drho)
 use cudafor,       ONLY:C_DEVLOC
#endif
 !
 implicit none
 !
 integer     :: iv_wf,ic_wf
 complex(SP) :: rho(3)
 complex(SP) DEV_ATTR, target :: kbv(wf_ng,n_spinor,4,pp_kbv_dim_yambo)
 ! 
 ! Work Space
 !
 integer     :: i1,i2,i_spinor,dim_flat
 complex(SP) DEV_ATTR, pointer :: kbv_p(:,:),XXv_p(:,:),XXc_p(:,:)
 !
 if (pp_kbv_dim_yambo==0) return
 !
 dim_flat=n_spinor*4*pp_kbv_dim_yambo
 !
 ! This works since WF has the target attribute in mod_wave_function 
 !
#if defined _GPU && defined _CUDAF
 call C_F_POINTER(C_DEVLOC(kbv), kbv_p, (/wf_ng,dim_flat/) )
 call C_F_POINTER(C_DEVLOC(XXv), XXv_p, (/n_spinor,dim_flat/) )
 call C_F_POINTER(C_DEVLOC(XXc), XXc_p, (/n_spinor,dim_flat/) )
 !
 call dev_memcpy_h2d(WFv,WF%c(:,:,iv_wf))
 call dev_memcpy_h2d(WFc,WF%c(:,:,ic_wf))
 !
 call M_by_M_gpu('T','N', n_spinor, dim_flat, wf_ng, cONE, WFv, wf_ng, kbv_p, wf_ng, cZERO, XXv_p, n_spinor)
 call M_by_M_gpu('T','N', n_spinor, dim_flat, wf_ng, cONE, WFc, wf_ng, kbv_p, wf_ng, cZERO, XXc_p, n_spinor)
 !
#elif defined _GPU && defined _OPENACC
 !
 call C_F_POINTER(C_LOC(kbv), kbv_p, (/wf_ng,dim_flat/) )
 call C_F_POINTER(C_LOC(XXv), XXv_p, (/n_spinor,dim_flat/) )
 call C_F_POINTER(C_LOC(XXc), XXc_p, (/n_spinor,dim_flat/) )
 !
 !call dev_memcpy_h2d(WFv,WF%c(:,:,iv_wf))
 !call dev_memcpy_h2d(WFc,WF%c(:,:,ic_wf))
 !
 call M_by_M_gpu('T','N', n_spinor, dim_flat, wf_ng, cONE, WF%c(:,:,iv_wf), wf_ng, kbv_p, wf_ng, cZERO, XXv_p, n_spinor)
 call M_by_M_gpu('T','N', n_spinor, dim_flat, wf_ng, cONE, WF%c(:,:,ic_wf), wf_ng, kbv_p, wf_ng, cZERO, XXc_p, n_spinor)
 !
#else
 call C_F_POINTER(C_LOC(kbv), kbv_p, (/wf_ng,dim_flat/) )
 call C_F_POINTER(C_LOC(XXv), XXv_p, (/n_spinor,dim_flat/) )
 call C_F_POINTER(C_LOC(XXc), XXc_p, (/n_spinor,dim_flat/) )
 !
 call M_by_M('T','N', n_spinor, dim_flat, wf_ng, cONE, WF%c(:,:,iv_wf), wf_ng, kbv_p, wf_ng, cZERO, XXv_p, n_spinor)
 call M_by_M('T','N', n_spinor, dim_flat, wf_ng, cONE, WF%c(:,:,ic_wf), wf_ng, kbv_p, wf_ng, cZERO, XXc_p, n_spinor)
#endif
 !
 !DEV_ACC data present(drho,XXv,XXc)
 !DEV_ACC parallel loop collapse(3) async
 !DEV_CUF kernel do(3)
 !DEV_OMP parallel do default(shared), private(i1,i2,i_spinor), collapse(3)
 do i1=1,pp_kbv_dim_yambo
   do i2=1,3
     do i_spinor=1,n_spinor
       DEV_VAR(drho)(i_spinor,i2,i1)=conjg(XXv(i_spinor,i_spinor,1,i1))*XXc(i_spinor,i_spinor,i2+1,i1)+ &
&                                    conjg(XXv(i_spinor,i_spinor,i2+1,i1))*XXc(i_spinor,i_spinor,1,i1)
     enddo
   enddo
 enddo
 !DEV_ACC end data
 !
 if (have_cuda) call dev_memcpy_d2h(drho,DEV_VAR(drho))
 forall(i1=1:3) rho(i1)=rho(i1)+sum(drho(:,i1,:))
 !
end subroutine
