!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF, PDA
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
subroutine environ_vcoul(q,v_environ,lcomp_asympt)
 !
 ! Here the effective Coulomb potential in the presence of 
 ! a polarizable continuum is initialized, according to:
 !
 ! v_env(q+G,q+G') = 4 pi * K^-1(q+G,q+G')
 ! K(q+G,q+G')     = (q+G).(q+G') * eps(G-G')
 !
 ! i.e.
 ! v_env^-1(q+G,q+G') = 1/4pi [ (q+G).(q+G') * eps(G-G') ]
 ! 
 ! A (parallel) matrix inversion is then performed
 ! 
 use pars,          ONLY:SP,DP,pi,cONE,cZERO
 use com,           ONLY:msg
 use vec_operate,   ONLY:iku_v_norm,c2a
 use matrix,        ONLY:PAR_matrix,MATRIX_reset
 use parallel_m,    ONLY:PP_redux_wait
 use linear_algebra,ONLY:INV
 use interfaces,    ONLY:LINEAR_ALGEBRA_driver,MATRIX_duplicate
 use environ_m,     ONLY:l_environ,eps_env_g, eps0, A0
 use R_lattice,     ONLY:b,G_m_G,g_vec,ng_vec,q_norm,minus_G
#include<memory.h>
 !
 real(SP),         intent(in)    :: q(3)
 type(PAR_matrix), intent(inout) :: v_environ
 logical,          intent(in)    :: lcomp_asympt
 !
 ! Work Space
 !
 integer     :: ig1,ig2,ng_loc,ig_m_g
 real(SP)    :: bare_qpg_v1(3),bare_qpg_v2(3),q_loc(3),q_vers(3),scal,scalp
 logical     :: l_this_is_q0
 complex(DP) :: dp_dummy
 real(SP), allocatable :: g_vec_loc(:,:)
 type(PAR_matrix) :: Kbar

 if (.not.l_environ) return
 if (trim(v_environ%kind)=="NAN".or..not.allocated(v_environ%blc)) &
&   call error(' [ENVIRON] v_environ not initialized')
 !
 if (v_environ%Nb/=1) call error(' [ENVIRON] Nb > 1 not implemented')
 !
 call section('=','building v_environ')
 !
 ! handle units
 ! AF: to be checked whether we really need this
 !
 ng_loc=ng_vec
 ng_loc=min(ng_loc,v_environ%cols(2))
 ng_loc=min(ng_loc,v_environ%rows(2))
 !
 YAMBO_ALLOC(g_vec_loc,(3,ng_loc))
 do ig1=1,ng_loc
   call c2a(b,g_vec(ig1,:),g_vec_loc(:,ig1),'ki2c')
 enddo
 !
 ! here we deal with the q-> term numerically
 call c2a(b,q,q_loc,'ki2c')
 !
 ! AF: probably not needed if q(3) passed in input is already
 ! taking care of properly defining q0.
 !
 if (iku_v_norm(q)<1.0d-6) then
   q_loc(:)=q_norm(1)*(/1.0_SP,0.0_SP,0.0_SP/)
 endif

 ! 
 ! main loop
 ! 
 !$omp parallel do default(shared), private(ig1,ig2,bare_qpg_v1,bare_qpg_v2,scal,ig_m_g)
 do ig2=v_environ%cols(1),v_environ%cols(2)
   !
   bare_qpg_v2(:)=q_loc(:)+g_vec_loc(:,ig2)
   !
   do ig1=v_environ%rows(1),v_environ%rows(2)
     !
     bare_qpg_v1(:)=q_loc(:)+g_vec_loc(:,ig1)
     scal=dot_product(bare_qpg_v1,bare_qpg_v2)
     !
     ig_m_g=G_m_G(ig1,ig2)
     v_environ%blc(ig1,ig2,1)=scal*eps_env_g(ig_m_g)/(4.0_SP*pi)
     !
   enddo
 enddo
 !$omp end parallel do
 !
 ! store the environ kernel in Kbar for later use if needed
 if (lcomp_asympt) call MATRIX_duplicate("+1",M_in=v_environ,M_out=Kbar)
 !
 ! compute v_environ
 !
 call LINEAR_ALGEBRA_driver(INV,M_slk=v_environ)

 !
 ! compute the Ao_q asymptotic (q->0 limit)
 ! v_environ ~ 4pi / [ q^2 * ( eps0+ A0 ) ]
 !
 ! A0 is calculated only for q-> 0
 !
 ! A0(q) = \sum_GG' vers(q).G eps(-G) Kbar^-1(qGG') G'.vers(q) eps(G')
 !
 ! Kbar is the body of K:   K v_environ = 4pi
 ! In order to handle it, the head is set to 1 and the wings to 0
 ! The factor 1/4pi needs to be removed
 !
 l_this_is_q0 = (iku_v_norm(q)<q_norm(1)*1.5_SP)
 !
 if (lcomp_asympt .and. l_this_is_q0) then
   !
   eps0=eps_env_g(1)
   Kbar%blc=Kbar%blc*4.0_SP*pi
   scal=dot_product(q_loc,q_loc)
   q_vers=q_loc/sqrt(scal)
   !
   if (Kbar%rows(1)==1                    ) Kbar%blc(1,:,1)=0.0
   if (                    Kbar%cols(1)==1) Kbar%blc(:,1,1)=0.0
   if (Kbar%rows(1)==1.and.Kbar%cols(1)==1) Kbar%blc(1,1,1)=1.0
   !
   call LINEAR_ALGEBRA_driver(INV,M_slk=Kbar)
   !
   dp_dummy=0.0_DP
   !
   !$omp parallel do default(shared), private(ig1,ig2,scal,scalp), &
   !$omp &           reduction(+:dp_dummy)
   do ig2=Kbar%cols(1),Kbar%cols(2)
     !
     scalp=dot_product(q_vers,g_vec_loc(:,ig2))
     !
     do ig1=Kbar%rows(1),Kbar%rows(2)
       !
       scal=dot_product(q_vers,g_vec_loc(:,ig2))
       !
       dp_dummy=dp_dummy + scal * eps_env_g(minus_G(ig1)) * Kbar%blc(ig1,ig2,1) * &
&                          scalp* eps_env_g(ig2)
     enddo
   enddo
   !
   A0=real(dp_dummy,SP)
   call PP_redux_wait(A0,COMM=Kbar%INTRA_comm%COMM)
   !
   call msg('r','[ENV] Asymptotic val: eps0, A0', &
&           (/real(eps0),aimag(eps0),real(A0),aimag(A0)/) )
   !
 endif

 !
 ! cleanup
 !
 YAMBO_FREE(g_vec_loc)
 call MATRIX_reset(Kbar)
 !
end subroutine environ_vcoul

