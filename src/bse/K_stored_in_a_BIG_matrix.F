!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM MG
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
subroutine K_stored_in_a_BIG_matrix(iq)
 !
 ! Here I fill the upper half of the kernel (coupling included):
 !
 !      | (K_r)     (K_c)    |  <- this part is done here
 !  K = |                    |
 !      | (-K_c^*)  (-K_r^*) |
 !
 ! 
 use pars,           ONLY:SP
 use memory_m,       ONLY:mem_est
 use parallel_m,     ONLY:PP_redux_wait,PP_indexes,PP_indexes_reset
 use interfaces,     ONLY:PARALLEL_index
 use R_lattice,      ONLY:nXkibz
 use BS,             ONLY:BS_mat,BS_K_dim,BS_nT_at_k,BS_k_and_row_restart,BS_eh_E,&
&                         BS_K_coupling,BS_blk,n_BS_blks,BS_T_grp,BSS_rhoq0
 use matrix_operate, ONLY:UP_matrix_index
 implicit none
 integer               ::iq
 !
 ! Work Space
 !
 integer         ::i_k,i_p,i1,i2,BS_H_dim,i_c,i_r,H_pos(2),i_Tk,i_Tp,i_B
 ! 
 ! I/O
 ! 
 integer           ::ioBS_err,ID,ACTION
 integer, external ::ioBS
 !
 if (allocated(BS_mat)) return
 !
 BS_H_dim=BS_K_dim
 if (BS_K_coupling) BS_H_dim=2*BS_K_dim
 !
 !call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=ID)
 !ioBS_err=ioBS(iq,X,ID)
 !
 allocate(BS_mat(BS_H_dim,BS_H_dim))
 call mem_est('BS_mat',(/size(BS_mat)/))
 allocate(BSS_rhoq0(BS_H_dim))
 call mem_est('BSS_rhoq0',(/size(BSS_rhoq0)/))
 !
 do i_B=1,n_BS_blks
   !
   i_k =BS_blk(i_B)%ik
   i_p =BS_blk(i_B)%ip
   i_Tk=BS_blk(i_B)%iT_k
   i_Tp=BS_blk(i_B)%iT_p
   !
   do i_r=1,BS_blk(i_B)%size(1)
     !
     H_pos(1)=sum(BS_T_grp(:I_Tk-1)%size)+i_r
     !
     BSS_rhoq0(H_pos(1))=BS_T_grp(i_Tk)%dipoles_R(i_r)
     !
     do i_c=1,BS_blk(i_B)%size(2)
       !
       H_pos(2)=sum(BS_T_grp(:I_Tp-1)%size)+i_c
       !
       if (i_k==i_p.and.i_r>i_c) cycle
       !
       write (*,*) H_pos,i_B,i_r,i_c,BS_blk(i_B)%mat(i_r,i_c)
       BS_mat(H_pos(1),H_pos(2))=BS_blk(i_B)%mat(i_r,i_c)
       BS_mat(H_pos(2),H_pos(1))=conjg(BS_blk(i_B)%mat(i_r,i_c))
       !
       if (i_k==i_p.and.i_r==i_c) then
         !
         BS_mat(H_pos(1),H_pos(1))=BS_mat(H_pos(1),H_pos(1))+BS_T_grp(i_Tk)%E(i_r)
         !
       endif
       !
     enddo
   enddo
   !
 enddo
 !
! do ik2=1,nXkibz
!   do ik1=ik2,1,-1
!     BS_k_and_row_restart(:2)=(/ sum(BS_nT_at_k(:ik1-1)),sum(BS_nT_at_k(:ik2-1)) /)
!     !
!     ACTION=RD
!     if (ik1==1.and.ik2==nXkibz) ACTION=RD_CL
!       !
!       ! BS I/O
!       ! 
!       call io_control(ACTION=ACTION,COM=NONE,SEC=(/UP_matrix_index(ik1,ik2)/),ID=ID)
!       ioBS_err=ioBS(iq,X,ID)
!       !
!       if (ik1==ik2) then
!         !
!         forall(i1=BS_k_and_row_restart(1)+1:BS_k_and_row_restart(1)+BS_nT_at_k(ik1)) &
!&              BS_mat(i1,i1)=BS_mat(i1,i1)+BS_eh_E(i1)
!         !
!         ! On diagonal block simmetrization
!         !
!         !  Resonant K is hermitian
!         !  Coupling K is simmetric
!         !
!         ! if i1=BS_blk_coord(1)+1,BS_blk_coord(1)+BS_blk_dim(ik1)
!         !
!         ! in resonant i2=1,i1-1
!         !
!         ! in coupling i2=BS_K_dim+1,BS_K_dim+i1-1
!         !
!         do i1=BS_k_and_row_restart(1)+1,BS_k_and_row_restart(1)+BS_nT_at_k(ik1)
!           do i2=1,i1-1
!             BS_mat(i1,i2)=conjg(BS_mat(i2,i1))
!             !
!             if (BS_K_coupling) BS_mat(i1,BS_K_dim+i2)=BS_mat(i2,BS_K_dim+i1)
!             !
!           enddo
!         enddo
!         !
!       else
!         !
!         ! Off diagonal block simmetrization
!         !
!         !  Resonant K is hermitian
!         !  Coupling K is simmetric
!         !
!         forall(i1=BS_k_and_row_restart(1)+1:BS_k_and_row_restart(1)+BS_nT_at_k(ik1),&
!&                i2=BS_k_and_row_restart(2)+1:BS_k_and_row_restart(2)+BS_nT_at_k(ik2)) &
!&                BS_mat(i2,i1)=conjg(BS_mat(i1,i2))
!         !
!         if (BS_K_coupling) then
!           forall(i1=BS_k_and_row_restart(1)+1:BS_k_and_row_restart(1)+BS_nT_at_k(ik1),&
!&                  i2=BS_k_and_row_restart(2)+1:BS_k_and_row_restart(2)+BS_nT_at_k(ik2)) &
!&                  BS_mat(i2,BS_K_dim+i1)=BS_mat(i1,BS_K_dim+i2)
!         endif
!           !
!       end if
!     end do
!   end do
!   !
!   ! Filling the anti-resonant and anti-coupling parts
!   !
!   if (BS_K_coupling) then
!     !
!     ! If Coupling the half lower part of K must be filled
!     !
!     ! Anti-resonant
!     !
!     forall(i1=BS_K_dim+1:BS_H_dim,i2=BS_K_dim+1:BS_H_dim) &
!&         BS_mat(i1,i2)=-conjg(BS_mat(i1-BS_K_dim,i2-BS_K_dim))
!     ! 
!     ! Anti-coupling
!     !
!     forall(i1=BS_K_dim+1:BS_H_dim,i2=1:BS_K_dim) &
!&         BS_mat(i1,i2)=-conjg(BS_mat(i1-BS_K_dim,i2+BS_K_dim))
!     !
!   endif
 !
end subroutine K_stored_in_a_BIG_matrix
