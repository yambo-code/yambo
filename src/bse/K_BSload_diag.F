!
! Copyright (C) 2000-2014 D.Sangalli, A. Marini, M. Gruning and the YAMBO team 
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
subroutine K_BSload_diag(iq)
 !
 !
 ! Here I fill the diagonal of K_r:
 !
 !      | (K_r)     (K_c)    |  
 !  K = |                    |
 !      | (-K_c^*)  (-K_r^*) |
 !
 ! 
 use pars,           ONLY:SP
 use memory_m,       ONLY:mem_est
 use R_lattice,      ONLY:nXkibz
 use matrix_operate, ONLY:matrix_linear_index
 use BS,             ONLY:BS_mat,BS_mat_diagonal,BS_K_dim,BS_blk_dim,BS_k_and_row_restart,&
&                         BS_DB_is_fragmented,BS_K_coupling
 use X_m,            ONLY:X_t
 use IO_m,           ONLY:io_control,OP_RD,RD,NONE,RD_CL,OP_RD_CL
 implicit none
 integer               ::iq
 !
 ! Work Space
 !
 integer         ::i1,ik,frag_pointer
 logical         ::BS_K_coupling_save
 type(X_t)       ::X
 ! 
 ! I/O
 ! 
 integer           ::ioBS_err,ID,ACTION
 integer, external ::ioBS
 !
 if (allocated(BS_mat_diagonal)) return
 !
 call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=ID)
 ioBS_err=ioBS(iq,X,ID)
 !
 allocate(BS_mat_diagonal(BS_K_dim))
 call mem_est('BS_mat_diagonal',(/2*BS_K_dim/))
 !
 BS_K_coupling_save=BS_K_coupling
 BS_K_coupling=.false.
 !
 if(.not.BS_DB_is_fragmented) then
   allocate(BS_mat(BS_K_dim,BS_K_dim))
   call mem_est('BS_mat',(/size(BS_mat)/))
 endif
 !
 do ik=1,nXkibz
   !
   BS_k_and_row_restart(:2)=(/ sum(BS_blk_dim(:ik-1)),sum(BS_blk_dim(:ik-1)) /)
   !
   ACTION=RD
   if (ik==nXkibz) ACTION=RD_CL
   !
   if (BS_DB_is_fragmented) then
     frag_pointer=BS_k_and_row_restart(1)
     BS_k_and_row_restart=0
     allocate(BS_mat(BS_blk_dim(ik),BS_blk_dim(ik)))
     ACTION=OP_RD_CL
   else
     frag_pointer=0
   endif
   !
   ! BS I/O
   ! 
   call io_control(ACTION=ACTION,COM=NONE,SEC=(/matrix_linear_index(ik,ik)/),ID=ID)
   ioBS_err=ioBS(iq,X,ID)
   !
   forall(i1=BS_k_and_row_restart(1)+1:BS_k_and_row_restart(1)+BS_blk_dim(ik)) &
&        BS_mat_diagonal(i1+frag_pointer)=BS_mat(i1,i1)
   !
   if (BS_DB_is_fragmented) deallocate(BS_mat)
   !       
 enddo
 !
 if(.not.BS_DB_is_fragmented) deallocate(BS_mat)
 !
 BS_K_coupling=BS_K_coupling_save
 !
end subroutine K_BSload_diag
 
