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
subroutine DIPOLE_kb_sum(icfft,ivfft,rho,kbv)
 !
 use pars,          ONLY:SP,cZERO
 use wrapper,       ONLY:Vstar_dot_V,V_dot_V,V_by_V_plus_V
 use wave_func,     ONLY:WF,wf_ng
 use electrons,     ONLY:n_spinor
 use pseudo,        ONLY:pp_kbv_dim
#if defined _RT
 use drivers,       ONLY:l_real_time
#endif
 !
 implicit none
 !
 integer    :: ivfft(n_spinor),icfft(n_spinor)
 complex(SP):: rho(3,n_spinor,n_spinor),kbv(wf_ng,pp_kbv_dim,4,n_spinor)
 ! 
 ! Work Space
 !
 logical     :: eval_spinor_matrix
 integer     :: i1,i2,i_spinor,j_spinor
 complex(SP) :: Cv(n_spinor),Cc(n_spinor),Vv(3,n_spinor),Vc(3,n_spinor),drho(3,pp_kbv_dim,n_spinor,n_spinor)
 !
 drho=cZERO
 eval_spinor_matrix=.false.
 !
#if defined _RT
 eval_spinor_matrix=l_real_time.and.n_spinor==2
#endif
 !
!$omp parallel do default(shared), private(i_spinor,j_spinor,i1,i2,Cv,Cc,Vv,Vc), collapse(3)
 do i_spinor=1,n_spinor
   do j_spinor=1,n_spinor
   !
   if(.not.eval_spinor_matrix.and.i_spinor/=j_spinor) cycle
   !
   do i1=1,pp_kbv_dim
     !
     Cv(i_spinor)=V_dot_V(wf_ng,WF%c(:,ivfft(i_spinor)),kbv(:,i1,1,i_spinor))
     Cc(j_spinor)=V_dot_V(wf_ng,WF%c(:,icfft(j_spinor)),kbv(:,i1,1,j_spinor))
     !
     if ( Cc(j_spinor)==cZERO .and. Cv(i_spinor)==cZERO ) cycle
       !
       do i2=1,3
         !
         Vv(i2,i_spinor)=V_dot_V(wf_ng,WF%c(:,ivfft(i_spinor)),kbv(:,i1,1+i2,i_spinor))
         Vc(i2,j_spinor)=V_dot_V(wf_ng,WF%c(:,icfft(j_spinor)),kbv(:,i1,1+i2,j_spinor))
         !
         drho(i2,i1,i_spinor,j_spinor)=conjg(Cv(i_spinor))*Vc(i2,j_spinor)+conjg(Vv(i2,i_spinor))*Cc(j_spinor)
         !
       enddo
       !
     enddo
     !
   enddo
 enddo
!$omp end parallel do
 !
 forall(i1=1:3,i_spinor=1:n_spinor,j_spinor=1:n_spinor) &
&  rho(i1,i_spinor,j_spinor)=rho(i1,i_spinor,j_spinor)+sum(drho(i1,:,i_spinor,j_spinor))
 !
end subroutine

