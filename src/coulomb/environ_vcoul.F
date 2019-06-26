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
subroutine environ_vcoul(q,v_environ)
 !
 ! Here the effective Coulomb potential in the presence of 
 ! a polarizable continuum is initialized, according to:
 !
 ! v_env^-1(q+G,q+G') = 1/4pi [ (q+G).(q+G') * eps^-1(G-G') ]
 ! 
 ! A (parallel) matrix inversion is then performed
 ! 
 use pars,          ONLY:SP,pi,cONE,cZERO
 use com,           ONLY:error
 use vec_operate,   ONLY:iku_v_norm,c2a
 use matrix,        ONLY:PAR_matrix
 use linear_algebra,ONLY:INV
 use interfaces,    ONLY:LINEAR_ALGEBRA_driver
 use environ_m,     ONLY:l_environ,eps_env_g
 use R_lattice,     ONLY:b,G_m_G,g_vec,ng_vec,q_norm
#include<memory.h>
 !
 real(SP),         intent(in)    :: q(3)
 type(PAR_matrix), intent(inout) :: v_environ
 !
 ! Work Space
 !
 integer :: ig1,ig2,ng_loc,ig_m_g
 real(SP):: bare_qpg_v1(3),bare_qpg_v2(3),q_loc(3),scal
 real(SP), allocatable :: g_vec_loc(:,:)

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
 YAMBO_ALLOC(g_vec_loc,(ng_loc,3))
 do ig1=1,ng_loc
   call c2a(b,g_vec(ig1,:),g_vec_loc(ig1,:),'ki2c')
 enddo
 !
 ! here we deal with the q-> term numerically
 call c2a(b,q,q_loc,'ki2c')
 !
 if (iku_v_norm(q)<1.0d-5) then
   q_loc(:)=q_norm(1)*(/1.0_SP,0.0_SP,0.0_SP/)   ! XXX to be updated
 endif

 ! 
 ! main loop
 ! 
 !$omp parallel do default(shared), private(ig1,ig2,bare_qpg_v1,bare_qpg_v2,scal,ig_m_g)
 do ig2=v_environ%cols(1),v_environ%cols(2)
   !
   bare_qpg_v2(:)=q_loc(:)+g_vec_loc(ig2,:)
   !
   do ig1=v_environ%rows(1),v_environ%rows(2)
     !
     bare_qpg_v1(:)=q_loc(:)+g_vec_loc(ig1,:)
     scal=dot_product(bare_qpg_v1,bare_qpg_v2)
     !
     ig_m_g=G_m_G(ig1,ig2)
     v_environ%blc(ig1,ig2,1)=scal*eps_env_g(ig_m_g)/(4.0_SP*pi)
     !
   enddo
   !
 enddo
 !$omp end parallel do
 !
 call LINEAR_ALGEBRA_driver(INV,M_slk=v_environ)
 !
 YAMBO_FREE(g_vec_loc)
 !
end subroutine environ_vcoul

