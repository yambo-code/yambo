!
!        Copyright (C) 2000-2020 the YAMBO team
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
#if defined _CUDA
 use iso_c_binding, ONLY:C_DEVLOC,C_F_POINTER
#else
 use iso_c_binding, ONLY:C_LOC,C_F_POINTER
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
 integer     :: i1,i2,i_spinor,dim_flat
 complex(SP) DEV_ATTR, pointer :: WF_p(:,:,:),kbv_p(:,:),XX_p(:,:,:)
 complex(SP) DEV_ATTR, target :: XX(n_spinor,n_spinor,4,pp_kbv_dim_yambo,2)
 complex(SP)          :: drho(n_spinor,3,pp_kbv_dim_yambo)
#ifdef _CUDA
 complex(SP) DEV_ATTR :: DEV_VAR(drho)(n_spinor,3,pp_kbv_dim_yambo)
#endif
 !
 if (pp_kbv_dim_yambo==0) return
 !
 ! Note: this works since WF has the target attribute in mod_wave_function 
 WF_p => WF%c
 !
 ! This is the fortran expanded code for the M?by?M calls below
 ! note that an extra i_spinor index is added for the sake of using ZGEMM
 !
 ! XX=cZERO
 ! do i1=1,pp_kbv_dim_yambo
 !   do i2=1,4
 !     do i_spinor=1,n_spinor
 !       do ig=1,wf_ng
 !         XX(i_spinor,i2,i1,1)=XX(i_spinor,i2,i1,1)+WF_p(ig,i_spinor,iv_wf)*kbv(ig,i_spinor,i2,i1)
 !         XX(i_spinor,i2,i1,2)=XX(i_spinor,i2,i1,2)+WF_p(ig,i_spinor,ic_wf)*kbv(ig,i_spinor,i2,i1)
 !       enddo
 !     enddo
 !   enddo
 ! enddo
 !
 dim_flat = n_spinor*4*pp_kbv_dim_yambo
 !
 ! here I use the interface with all scalars instead of that for
 ! rank 2 objects because reshape seems not to work properly on
 ! "device" data (at least using PGI 19.10).
 ! A direct call to CGEMM/cublasCGEMM is also possible, but I think
 ! the wrapped M_by_M interface is cleaner
 !
#if defined _CUDA
 call C_F_POINTER(C_DEVLOC(kbv), kbv_p, (/wf_ng,dim_flat/) )
 call C_F_POINTER(C_DEVLOC(XX),  XX_p,  (/n_spinor,dim_flat,2/) )
#else
 call C_F_POINTER(C_LOC(kbv), kbv_p, (/wf_ng,dim_flat/) )
 call C_F_POINTER(C_LOC(XX),  XX_p,  (/n_spinor,dim_flat,2/) )
#endif
 call M_by_M('T','N', n_spinor, dim_flat, wf_ng, cONE, WF_p(:,:,iv_wf), wf_ng, kbv_p, wf_ng, cZERO, XX_p(:,:,1), n_spinor)
 call M_by_M('T','N', n_spinor, dim_flat, wf_ng, cONE, WF_p(:,:,ic_wf), wf_ng, kbv_p, wf_ng, cZERO, XX_p(:,:,2), n_spinor)
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
       DEV_VAR(drho)(i_spinor,i2,i1)=conjg(XX(i_spinor,i_spinor,1,i1,1))*XX(i_spinor,i_spinor,i2+1,i1,2)+ &
&                                    conjg(XX(i_spinor,i_spinor,i2+1,i1,1))*XX(i_spinor,i_spinor,1,i1,2)
     enddo
   enddo
 enddo
 !
 if (have_cuda) call dev_memcpy(drho,DEV_VAR(drho))
 forall(i1=1:3) rho(i1)=rho(i1)+sum(drho(:,i1,:))
 !
end subroutine
