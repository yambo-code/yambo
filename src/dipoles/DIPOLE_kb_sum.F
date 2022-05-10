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
subroutine DIPOLE_kb_project(pp_range,npp,ncomp,WF_ib,kbv,XX)
 !
 use pars,          ONLY:SP,cONE,cZERO
 use wrapper,       ONLY:M_by_M
 use electrons,     ONLY:n_spinor
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
 integer,                      intent(in)    :: pp_range(2),npp,ncomp
 complex(SP) DEV_ATTR,         intent(in)    :: WF_ib(ncomp,n_spinor)
 complex(SP) DEV_ATTR, target, intent(in)    :: kbv(ncomp,n_spinor,4,npp)
 complex(SP) DEV_ATTR, target, intent(out)   :: XX(n_spinor,n_spinor,4,pp_range(1):pp_range(1))
 ! 
 ! Work Space
 !
 integer     :: dim_flat
 complex(SP) DEV_ATTR, pointer :: kbv_p(:,:),XX_p(:,:)
 !
 dim_flat=n_spinor*4*npp
 !
#if defined _CUDA
 call C_F_POINTER(C_DEVLOC(kbv), kbv_p, (/ncomp,dim_flat/) )
 call C_F_POINTER(C_DEVLOC(XX),  XX_p,  (/n_spinor,dim_flat/) )
#else
 call C_F_POINTER(C_LOC(kbv), kbv_p, (/ncomp,dim_flat/) )
 call C_F_POINTER(C_LOC(XX),  XX_p, (/n_spinor,dim_flat/) )
#endif
 !
 call M_by_M('T','N', n_spinor, dim_flat, ncomp, cONE, WF_ib, ncomp, kbv_p, ncomp, cZERO, XX_p, n_spinor)
 !
end subroutine DIPOLE_kb_project
!
!
subroutine DIPOLE_kb_sum(rho,XXv,XXc)
 !
 use pars,          ONLY:SP
 use electrons,     ONLY:n_spinor
 use pseudo,        ONLY:pp_kbv_dim_yambo
 use cuda_m,        ONLY:have_cuda
 use deviceXlib_m,  ONLY:dev_memcpy
 !
#include<dev_defs.h>
 !
 implicit none
 !
 complex(SP),          intent(inout) :: rho(3)
 complex(SP) DEV_ATTR, intent(in)    :: XXv(n_spinor,n_spinor,4,pp_kbv_dim_yambo)
 complex(SP) DEV_ATTR, intent(in)    :: XXc(n_spinor,n_spinor,4,pp_kbv_dim_yambo)
 ! 
 ! Work Space
 !
 integer     :: i1,i2,i_spinor
 !
 complex(SP)          :: drho(n_spinor,3,pp_kbv_dim_yambo)
#ifdef _CUDA
 complex(SP) DEV_ATTR :: DEV_VAR(drho)(n_spinor,3,pp_kbv_dim_yambo)
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
end subroutine DIPOLE_kb_sum
