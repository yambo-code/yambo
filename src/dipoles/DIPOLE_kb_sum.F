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
 integer     :: i1,i2,i_spinor
 complex(SP) :: XX(2,n_spinor,4,pp_kbv_dim_yambo),drho(n_spinor,3,pp_kbv_dim_yambo)
 !
 XX=cZERO
 drho=cZERO
 !
#if ! defined(_CUDA)
 !$omp parallel do default(shared), private(i1,i2,i_spinor), collapse(3)
#endif
 do i1=1,pp_kbv_dim_yambo
   do i2=1,4
     do i_spinor=1,n_spinor
       XX(1,i_spinor,i2,i1)=V_dot_V(wf_ng,DEV_VARNAME(WF%c)(:,i_spinor,iv_wf),kbv(:,i_spinor,i2,i1))
       XX(2,i_spinor,i2,i1)=V_dot_V(wf_ng,DEV_VARNAME(WF%c)(:,i_spinor,ic_wf),kbv(:,i_spinor,i2,i1))
     enddo
   enddo
 enddo
#if ! defined(_CUDA)
 !$omp end parallel do
#endif
 !
 !$omp parallel do default(shared), private(i1,i2,i_spinor), collapse(3)
 do i1=1,pp_kbv_dim_yambo
   do i2=1,3
     do i_spinor=1,n_spinor
       drho(i_spinor,i2,i1)=conjg(XX(1,i_spinor,1,i1))*XX(2,i_spinor,i2+1,i1)+conjg(XX(1,i_spinor,i2+1,i1))*XX(2,i_spinor,1,i1)
     enddo
   enddo
 enddo
 !$omp end parallel do
 !
 forall(i1=1:3) rho(i1)=rho(i1)+sum(drho(:,i1,:))
 !
end subroutine
