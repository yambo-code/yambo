!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use timing_m
 !
 implicit none
 !
 integer    :: iv_wf,ic_wf
 complex(SP):: rho(3),kbv(wf_ng,pp_kbv_dim,4,n_spinor)
#ifdef _CUDA
 attributes(device) :: kbv
#endif
 ! 
 ! Work Space
 !
 integer     :: i1,i2,i3,i_spinor
#ifdef _CUDA
 complex(SP), pointer, device :: WF_c_d(:,:,:)
 complex(SP):: drho(3)
 complex(SP):: Cv_c,Cc_c,Vv_1c,Vv_2c,Vv_3c,Vc_1c,Vc_2c,Vc_3c
 real(SP)   :: Cv_r,Cc_r,Vv_1r,Vv_2r,Vv_3r,Vc_1r,Vc_2r,Vc_3r
 real(SP)   :: Cv_i,Cc_i,Vv_1i,Vv_2i,Vv_3i,Vc_1i,Vc_2i,Vc_3i
#else
 complex(SP) :: Cv(n_spinor),Cc(n_spinor),Vv(3,n_spinor),Vc(3,n_spinor),drho(3,pp_kbv_dim,n_spinor)
#endif
 !
 !call timing("DIPOLE_kb_sum","start")
 !
#ifdef _CUDA
 !
 WF_c_d => WF%c_d
 drho(:)=cZERO
 !
 ! All the complex algebra below has been worked out
 ! explicitly (at the level of the cuf kernel reduction)
 ! in order to workaround a bug of PGI compilers (17.x-18.x at least)
 !
 do i_spinor=1,n_spinor
   do i1=1,pp_kbv_dim
     !
     Cv_r=cZERO
     Cv_i=cZERO
     Cc_r=cZERO
     Cc_i=cZERO
     !
     Vv_1r=cZERO
     Vv_1i=cZERO
     Vv_2r=cZERO
     Vv_2i=cZERO
     Vv_3r=cZERO
     Vv_3i=cZERO
     !
     Vc_1r=cZERO
     Vc_1i=cZERO
     Vc_2r=cZERO
     Vc_2i=cZERO
     Vc_3r=cZERO
     Vc_3i=cZERO
     !
     !$cuf kernel do(1)
     do i3=1,wf_ng
       !
       Cv_c=WF_c_d(i3,i_spinor,iv_wf)*kbv(i3,i1,1,i_spinor)
       Cv_r=Cv_r +real(Cv_c)
       Cv_i=Cv_i +aimag(Cv_c)
       !
       Cc_c=WF_c_d(i3,i_spinor,ic_wf)*kbv(i3,i1,1,i_spinor)
       Cc_r=Cc_r +real(Cc_c)
       Cc_i=Cc_i +aimag(Cc_c)
       !
       Vv_1c=WF_c_d(i3,i_spinor,iv_wf)*kbv(i3,i1,2,i_spinor)
       Vv_1r=Vv_1r +real(Vv_1c)
       Vv_1i=Vv_1i +aimag(Vv_1c)
       !
       Vv_2c=WF_c_d(i3,i_spinor,iv_wf)*kbv(i3,i1,3,i_spinor)
       Vv_2r=Vv_2r +real(Vv_2c)
       Vv_2i=Vv_2i +aimag(Vv_2c)
       !
       Vv_3c=WF_c_d(i3,i_spinor,iv_wf)*kbv(i3,i1,4,i_spinor)
       Vv_3r=Vv_3r +real(Vv_3c)
       Vv_3i=Vv_3i +aimag(Vv_3c)
       !
       Vc_1c=WF_c_d(i3,i_spinor,ic_wf)*kbv(i3,i1,2,i_spinor)
       Vc_1r=Vc_1r +real(Vc_1c)
       Vc_1i=Vc_1i +aimag(Vc_1c)
       !
       Vc_2c=WF_c_d(i3,i_spinor,ic_wf)*kbv(i3,i1,3,i_spinor)
       Vc_2r=Vc_2r +real(Vc_2c)
       Vc_2i=Vc_2i +aimag(Vc_2c)
       !
       Vc_3c=WF_c_d(i3,i_spinor,ic_wf)*kbv(i3,i1,4,i_spinor)
       Vc_3r=Vc_3r +real(Vc_3c)
       Vc_3i=Vc_3i +aimag(Vc_3c)
       !
     enddo
     !
     Cv_c=cmplx(Cv_r,Cv_i,SP)
     Cc_c=cmplx(Cc_r,Cc_i,SP)
     !
     Vv_1c=cmplx(Vv_1r,Vv_1i,SP)
     Vv_2c=cmplx(Vv_2r,Vv_2i,SP)
     Vv_3c=cmplx(Vv_3r,Vv_3i,SP)
     !
     Vc_1c=cmplx(Vc_1r,Vc_1i,SP)
     Vc_2c=cmplx(Vc_2r,Vc_2i,SP)
     Vc_3c=cmplx(Vc_3r,Vc_3i,SP)
     !
     drho(1)=drho(1) +conjg(Cv_c)*Vc_1c+conjg(Vv_1c)*Cc_c
     drho(2)=drho(2) +conjg(Cv_c)*Vc_2c+conjg(Vv_2c)*Cc_c
     drho(3)=drho(3) +conjg(Cv_c)*Vc_3c+conjg(Vv_3c)*Cc_c
     !
   enddo
 enddo
 !
 rho(:)=rho(:)+drho(:)
 !
#else
 !
 drho=cZERO
 !
 !$omp parallel do default(shared), private(i_spinor,i1,i2,Cv,Cc,Vv,Vc), collapse(2)
 do i_spinor=1,n_spinor
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
   enddo
 enddo
 !$omp end parallel do
 !
 forall(i2=1:3) rho(i2)=rho(i2)+sum(drho(i2,:,:))
 !
#endif
 !
 !call timing("DIPOLE_kb_sum","stop")
 !
end subroutine
