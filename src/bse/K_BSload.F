!
! Copyright (C) 2000-2009 A. Marini, M. Gruning and the YAMBO team 
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
subroutine K_BSload(iq)
 !
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
 use par_proc_m,     ONLY:pp_redux_wait,pp_indexes,pp_indexes_reset
 use par_indexes_m,  ONLY:par_indexes
 use R_lattice,      ONLY:nXkibz
 use BS,             ONLY:BS_mat,BS_K_dim,BS_blk_dim,BS_k_and_row_restart,BS_eh_E,&
&                         BS_DB_is_fragmented,BS_K_coupling,&
&                         BS_cpl_mat,BS_IO_index
 use X_m,            ONLY:X_t
 use IO_m,           ONLY:io_control,OP_RD,RD,NONE,RD_CL,OP_RD_CL
 implicit none
 integer               ::iq
 !
 ! Work Space
 !
 integer         ::ik1,ik2,i1,i2,BS_H_dim
 type(X_t)       ::X
 type(pp_indexes)::Kp
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
 call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=ID)
 ioBS_err=ioBS(iq,X,ID)
 !
 allocate(BS_mat(BS_H_dim,BS_H_dim))
 call mem_est('BS_mat',(/2*size(BS_mat)/))
 !
 do ik2=1,nXkibz
   do ik1=ik2,1,-1
     BS_k_and_row_restart(:2)=(/ sum(BS_blk_dim(:ik1-1)),sum(BS_blk_dim(:ik2-1)) /)
     !
     ACTION=RD
     if (ik1==1.and.ik2==nXkibz) ACTION=RD_CL
            !
       ! BS I/O
       ! 
       call io_control(ACTION=ACTION,COM=NONE,SEC=(/BS_IO_index(ik1,ik2)/),ID=ID)
       ioBS_err=ioBS(iq,X,ID)
       !
       if (ik1==ik2) then
         !
         forall(i1=BS_k_and_row_restart(1)+1:BS_k_and_row_restart(1)+BS_blk_dim(ik1)) &
&              BS_mat(i1,i1)=BS_mat(i1,i1)+BS_eh_E(i1)
         !
         ! On diagonal block simmetrization
         !
         !  Resonant K is hermitian
         !  Coupling K is simmetric
         !
         ! if i1=BS_blk_coord(1)+1,BS_blk_coord(1)+BS_blk_dim(ik1)
         !
         ! in resonant i2=1,i1-1
         !
         ! in coupling i2=BS_K_dim+1,BS_K_dim+i1-1
         !
         do i1=BS_k_and_row_restart(1)+1,BS_k_and_row_restart(1)+BS_blk_dim(ik1)
           do i2=1,i1-1
             BS_mat(i1,i2)=conjg(BS_mat(i2,i1))
             !
             if (BS_K_coupling) BS_mat(i1,BS_K_dim+i2)=BS_mat(i2,BS_K_dim+i1)
             !
           enddo
         enddo
         !
       else
         !
         ! Off diagonal block simmetrization
         !
         !  Resonant K is hermitian
         !  Coupling K is simmetric
         !
         forall(i1=BS_k_and_row_restart(1)+1:BS_k_and_row_restart(1)+BS_blk_dim(ik1),&
&                i2=BS_k_and_row_restart(2)+1:BS_k_and_row_restart(2)+BS_blk_dim(ik2)) &
&                BS_mat(i2,i1)=conjg(BS_mat(i1,i2))
         !
         ! GPL_EXCLUDE_START
         !
         if (BS_K_coupling) then
           forall(i1=BS_k_and_row_restart(1)+1:BS_k_and_row_restart(1)+BS_blk_dim(ik1),&
&                  i2=BS_k_and_row_restart(2)+1:BS_k_and_row_restart(2)+BS_blk_dim(ik2)) &
&                  BS_mat(i2,BS_K_dim+i1)=BS_mat(i1,BS_K_dim+i2)
         endif
           !
           ! GPL_EXCLUDE_END
           !
       end if
     end do
   end do
   !
   ! GPL_EXCLUDE_START
   !
   ! Filling the anti-resonant and anti-coupling parts
   !
   if (BS_K_coupling) then
     !
     ! If Coupling the half lower part of K must be filled
     !
     ! Anti-resonant
     !
     forall(i1=BS_K_dim+1:BS_H_dim,i2=BS_K_dim+1:BS_H_dim) &
&         BS_mat(i1,i2)=-conjg(BS_mat(i1-BS_K_dim,i2-BS_K_dim))
     ! 
     ! Anti-coupling
     !
     forall(i1=BS_K_dim+1:BS_H_dim,i2=1:BS_K_dim) &
&         BS_mat(i1,i2)=-conjg(BS_mat(i1-BS_K_dim,i2+BS_K_dim))
     !
   endif
   !
   ! GPL_EXCLUDE_END
   !
   !
 end subroutine K_BSload
 
