!
!        Copyright (C) 2000-2018 the YAMBO team
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
 use wrapper,       ONLY:Vstar_dot_V,V_dot_V,V_by_V_plus_V
 use wave_func,     ONLY:WF,wf_ng
 use electrons,     ONLY:n_spinor
 use pseudo,        ONLY:pp_kbv_dim
 !
 implicit none
 !
 integer    :: iv_wf,ic_wf
 complex(SP):: rho(3),kbv(wf_ng,pp_kbv_dim,4,n_spinor)
 ! 
 ! Work Space
 !
 integer     :: i1,i2,i_spinor,j_spinor
 complex(SP) :: Cv(n_spinor),Cc(n_spinor),Vv(3,n_spinor),Vc(3,n_spinor),drho(3,pp_kbv_dim,n_spinor),tmp(3)
 !
 drho=cZERO
 !
!$omp parallel do default(shared), private(i_spinor,i1,i2,Cv,Cc,Vv,Vc), collapse(2)
 do i_spinor=1,n_spinor
   !
   do i1=1,pp_kbv_dim
     !
     Cv(i_spinor)=V_dot_V(wf_ng,WF%c(:,i_spinor,iv_wf),kbv(:,i1,1,i_spinor))
     Cc(i_spinor)=V_dot_V(wf_ng,WF%c(:,i_spinor,ic_wf),kbv(:,i1,1,i_spinor))
     !
     if ( Cc(i_spinor)==cZERO .and. Cv(i_spinor)==cZERO ) cycle
     !
     do i2=1,3
       !
       Vv(i2,i_spinor)=V_dot_V(wf_ng,WF%c(:,i_spinor,iv_wf),kbv(:,i1,1+i2,i_spinor))
       Vc(i2,i_spinor)=V_dot_V(wf_ng,WF%c(:,i_spinor,ic_wf),kbv(:,i1,1+i2,i_spinor))
       !
       drho(i2,i1,i_spinor)=conjg(Cv(i_spinor))*Vc(i2,i_spinor)+conjg(Vv(i2,i_spinor))*Cc(i_spinor)
       !
     enddo
     !
     !write(100,*) i1,iv_wf,ic_wf,drho(:,i1,1) 
     !
   enddo
 enddo
!$omp end parallel do
 !
 !DIP_P(:,ic,iv,ik_mem,i_sp_pol)=rho
 !forall(i1=1:3) tmp(i1)=sum(drho(i1,:,:))
 !write(101,*) iv_wf,ic_wf,tmp 
 !
 forall(i1=1:3) rho(i1)=rho(i1)+sum(drho(i1,:,:))
 !
end subroutine
