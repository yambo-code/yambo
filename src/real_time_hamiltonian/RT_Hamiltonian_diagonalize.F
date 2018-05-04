!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine RT_Hamiltonian_diagonalize(E,BGR)
 !
 ! This routine defines and diagonalize the Coherent part of the
 ! evolution operator.
 !
 ! This is by Ho_plus_Sigma which include a H/xc part plus RT_Vnl_xc
 !
 ! In this routine I do a double job. I diagonalize
 ! 
 !  1. Ho_plus_Sigma
 !  2. Ho_plus_Sigma-RT_Vnl_xc+Z*RT_Vnl_xc
 !
 ! The point 2 is done only of l_update_QP_Energies=.TRUE. and
 ! if Z_bgr/=1.
 !
 use pars,           ONLY:SP,rZERO
 use R_lattice,      ONLY:nkibz
 use timing_m,       ONLY:timing
 use electrons,      ONLY:levels
 use hamiltonian,    ONLY:H_rotation,E_reference
 use linear_algebra, ONLY:DIAGO,USE_LK
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_WF_k,HEAD_QP_cpu
 use interfaces,     ONLY:LINEAR_ALGEBRA_driver,OCCUPATIONS_Gaps
 use real_time,      ONLY:RT_E_and_k,Ho_plus_Sigma,RT_ibz_coo,RT_bands,&
&                         l_RT_uses_E_DbGd,l_update_QP_Energies,l_update_QP_WFs,&
&                         DbGd_ibz_coo,RT_TIME_status,NE_i_time
 use RT_control,     ONLY:RT_do_it
 !
 implicit none
 !
 type(levels)                 :: E
 real(SP),      intent(out)   :: BGR(2)
 !
 ! Work Space
 !
 complex(SP)  :: H(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 real(SP)     :: Ed(RT_bands(1):RT_bands(2)),dE
 integer      :: ib,ik,ik_RT,ik_DbGd
 logical      :: l_DO_E,l_DO_R
 !
 BGR=rZERO
 !
 l_do_E=l_update_QP_Energies.and.(RT_TIME_status(NE_i_time)>0.or.RT_do_it('OUT'))
 l_do_R=l_update_QP_WFs.and.RT_do_it('OUT')
 !
 if (.not.l_do_E .and. .not. l_do_R) return
 !
 call timing('RT H diago',OPR='start')
 !
 do ik=1,nkibz
   !
   !if (.not.PAR_IND_WF_k%element_1D(ik) ) cycle
   !if (.not.HEAD_QP_cpu) cycle
   !
   ik_RT=RT_ibz_coo(ik,1)
   !
   H(:,:)=Ho_plus_Sigma(:,:,ik_RT,1)
   !
   call LINEAR_ALGEBRA_driver(DIAGO,lib_in=USE_LK,M=H,E_real=Ed)
   !
   ! Rotation
   !
   if (l_do_R) H_rotation(:,:,ik,1)=H(:,:)
   !
   if (.not.l_do_E) cycle
   !
   do ib=RT_bands(1),RT_bands(2)
     !
     dE=Ed(ib)-E_reference%E(ib,ik,1)
     !
     do ik_RT=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       !
       ik_DbGd=ik_RT-RT_ibz_coo(ik,1)+DbGd_ibz_coo(ik,1)
       !
       if(l_RT_uses_E_DbGd) then
         E%E_DbGd(ib,ik_DbGd,1)   =E_reference%E_DbGd(ib,ik_DbGd,1)+dE
         RT_E_and_k%E(ib,ik_RT,1) =E%E_DbGd(ib,ik_DbGd,1)
       else
         E%E(ib,ik_DbGd,1)        =E_reference%E(ib,ik,1)+dE
         RT_E_and_k%E(ib,ik_RT,1) =E%E(ib,ik_DbGd,1)
       endif
       !
     enddo
   enddo
   !
 enddo
 !
 !if (l_RT_uses_E_DbGd) call PP_redux_wait(E%E_DbGd)
 !
 ! Evaluation of the contribution to the minimal gap
 !
 call OCCUPATIONS_Gaps(E)
 !
 BGR(1)=E%E_dir_gap(1)
 BGR(2)=E%E_ind_gap(1)
 !
 call timing('RT H diago',OPR='stop')
 !
end subroutine RT_Hamiltonian_diagonalize
