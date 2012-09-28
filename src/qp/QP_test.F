!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine QP_test(X,E,k,q,Xw)
 !
 use pars,          ONLY:SP
 use electrons,     ONLY:levels,spin
 use R_lattice,     ONLY:qindx_S,bz_samp
 use frequency,     ONLY:w_samp
 use X_m,           ONLY:X_t,X_mat
 use QP_m,          ONLY:QP_n_states,QP_table,QP_n_G_bands,QP_nb
 use collision,     ONLY:ggwinfo,collision_reset
 use wave_func,     ONLY:WF_load,WF_free
 use wrapper,       ONLY:M_by_V
 !
 implicit none
 type(levels) ::E
 type(bz_samp)::k,q
 type(X_t)    ::X
 type(w_samp) ::Xw
 !
 ! WorkSpace
 !
 integer          ::i_qp,iqbz,i_k,i_kmq,i_n,i_spin,i_kmq_s,i_np,i_q,i_qs,i_g1,i_g2
 complex(SP),allocatable      ::X_all_mats(:,:,:),A_mat(:,:)
 complex(SP)      ::local_rhotw(X%ng)
 type(ggwinfo)    ::isc
 !
 Xw%er=0.
 Xw%n =1
 !
 allocate(X_mat(X%ng,X%ng,1),X_all_mats(X%ng,X%ng,q%nibz))
 allocate(isc%gamp(X%ng,X%ng))
 !
 do i_q=1,q%nibz
   !
   call X_os(X_mat,i_q,(/1,1/),E,k,Xw,X)
   !
   call FREQUENCIES_reset(Xw)
   !
   isc%qs=(/1,i_q,1/)
   !
   call scatterGamp(isc,'c')
   !
   call X_delta_part(X,Xw,isc%gamp)
   !
   do i_g1=1,X%ng
     do i_g2=1,X%ng
       X_all_mats(i_g1,i_g2,i_q)=X_mat(i_g1,i_g2,1)*isc%gamp(i_g1,i_g2)
     enddo
   enddo
   !
 enddo
 !
 call WF_free( )
 !
 call WF_load(X%ng,maxval(qindx_S(:,:,2)),(/QP_n_G_bands(1),max(QP_n_G_bands(2),QP_nb)/),(/1,k%nibz/),title='-Sc')
 !
 call collision_reset(isc)
 !
 allocate(isc%rhotw(X%ng),A_mat(QP_n_states,QP_n_G_bands(2)))
 !
 A_mat=(0._SP,0._SP)
 !
 call QP_state_table_setup(E)
 !
 do iqbz=1,q%nbz
   !
   i_q =q%sstar(iqbz,1)
   i_qs=q%sstar(iqbz,2)
   isc%qs(2:)=(/i_q,i_qs/)
   !
   do i_qp=1,QP_n_states
     !
     i_n=QP_table(i_qp,1)
     i_k=QP_table(i_qp,3)
     i_spin=spin(QP_table(i_qp,:))
     !
     i_kmq  =k%sstar(qindx_S(i_k,iqbz,1),1)
     i_kmq_s=k%sstar(qindx_S(i_k,iqbz,1),2)
     !
     isc%is    =(/i_n,i_k,1,i_spin/)
     isc%os(2:)=(/    i_kmq,i_kmq_s,i_spin/)
     isc%qs(1 )=qindx_S(i_k,iqbz,2)
     !
     do i_np=QP_n_G_bands(1),QP_n_G_bands(2)
       !
       isc%os(1)=i_np
       call scatterBamp(isc)
       !
       call M_by_V('N', X%ng, X_all_mats(:,:,i_q), conjg(isc%rhotw), local_rhotw)
       !
       A_mat(i_qp,i_np)=A_mat(i_qp,i_np)+sum(isc%rhotw(1:X%ng)*local_rhotw(1:X%ng))
       !
     enddo
     !
   enddo
   !
 enddo
 !
 do i_qp=1,QP_n_states
   !
   i_n=QP_table(i_qp,1)
   i_k=QP_table(i_qp,3)
   !
   do i_np=QP_n_G_bands(1),QP_n_G_bands(2)
     !write (*,'(1x,3i3,2f20.10)') i_k,i_n,i_np,A_mat(i_qp,i_np)
     write (*,*) i_k,i_n,i_np,A_mat(i_qp,i_np)
   enddo
   !
 enddo
 !
end subroutine QP_test
