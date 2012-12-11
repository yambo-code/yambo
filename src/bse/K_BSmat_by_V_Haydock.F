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
subroutine K_BSmat_by_V_Haydock(iq,iter,Vi,Vo,iter_par)
 !
 !
 ! Here I fill the upper half of the kernel (coupling included):
 !
 !      | (K_r)     (K_c)    |  <- this part is done here
 !  K = |                    |
 !      | (-K_c^*)  (-K_r^*) |
 !
 ! and I also do
 !
 !  Vo = K x Vi
 !
 ! Parallel shared matrix multiplication implemented
 !
 ! Because of symmetries I never need the second half 
 ! (it can be reconstructed by the "iteration parity") 
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
 integer               ::iter
 complex(SP)           ::Vi(:),Vo(:)
 real(SP),optional     ::iter_par
 !
 ! Work Space
 !
 integer         ::ik1,ik2,i1,i2,frag_pointer(2)
 complex(SP)     ::alpha
 type(X_t)       ::X
 type(pp_indexes)::Kp
 ! 
 ! I/O
 ! 
 integer           ::ioBS_err,ID,ACTION
 integer, external ::ioBS
 logical           ::use_fragments,db_load
 !
 Vo=(0._SP,0._SP)
 alpha=(1._SP,0._SP) 
 if (present(iter_par)) alpha = iter_par*(1._SP,0._SP)
 !
 use_fragments=BS_DB_is_fragmented
 db_load= use_fragments.or..not.allocated(BS_mat)
 !
 if (db_load) then
   !
   if (.not.use_fragments) then
     call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=ID)
     ioBS_err=ioBS(iq,X,ID)
     !
     allocate(BS_mat(BS_K_dim,BS_K_dim))
     if (BS_K_coupling) allocate(BS_cpl_mat(BS_K_dim,BS_K_dim))
     call mem_est('BS_mat',(/2*size(BS_mat)/))
     !
   else
     call pp_indexes_reset(Kp)
     call par_indexes(Kp,(/BS_IO_index(1,nXkibz)-1/))
     if (iter==1) call mem_est('BS_mat',(/maxval(BS_blk_dim)**2/)) 
   endif
   !
   do ik2=1,nXkibz
     do ik1=ik2,1,-1
       BS_k_and_row_restart(:2)=(/ sum(BS_blk_dim(:ik1-1)),sum(BS_blk_dim(:ik2-1)) /)
       !
       ACTION=RD
       if (ik1==1.and.ik2==nXkibz) ACTION=RD_CL
       !
       if (use_fragments) then
         frag_pointer=BS_k_and_row_restart(:2)
         BS_k_and_row_restart=0
         if (.not.Kp%element_1D(BS_IO_index(ik1,ik2)-1)) cycle
         allocate(BS_mat(BS_blk_dim(ik1),BS_blk_dim(ik2)))
         if (BS_K_coupling) allocate(BS_cpl_mat(BS_blk_dim(ik1),BS_blk_dim(ik2)))
         ACTION=OP_RD_CL
       else
         frag_pointer=0
       endif
       !
       ! BS I/O
       ! 
       call io_control(ACTION=ACTION,COM=NONE,SEC=(/BS_IO_index(ik1,ik2)/),ID=ID)
       ioBS_err=ioBS(iq,X,ID)
       !
       if (ik1==ik2) then
         !
         forall(i1=BS_k_and_row_restart(1)+1:BS_k_and_row_restart(1)+BS_blk_dim(ik1)) &
&              BS_mat(i1,i1)=BS_mat(i1,i1)+BS_eh_E(frag_pointer(1)+i1)
         !
         ! On diagonal block simmetrization
         !
         !  Resonant K is hermitian
         !  Coupling K is simmetric
         !
         do i1=BS_k_and_row_restart(1)+1,BS_k_and_row_restart(1)+BS_blk_dim(ik1)
           do i2=1,i1-1
             BS_mat(i1,i2)=conjg(BS_mat(i2,i1))
             !
             if (BS_K_coupling) BS_cpl_mat(i1,i2)=BS_cpl_mat(i2,i1)
             !
           enddo
         enddo
         !
         if (use_fragments) then
#if defined _DOUBLE
           call zgemv('n',BS_blk_dim(ik1),BS_blk_dim(ik1),(1._SP,0._SP),&
&                     BS_mat,BS_blk_dim(ik1),Vi(frag_pointer(1)+1),1,(1._SP,0._SP),&
&                     Vo(frag_pointer(1)+1),1)
#else
           call cgemv('n',BS_blk_dim(ik1),BS_blk_dim(ik1),(1._SP,0._SP),&
&                     BS_mat,BS_blk_dim(ik1),Vi(frag_pointer(1)+1),1,(1._SP,0._SP),&
&                     Vo(frag_pointer(1)+1),1)
#endif
           !
           ! GPL_EXCLUDE_START
           !
           if (BS_K_coupling) then
             !
             ! Calculate the diagonal coupling part
             !
!             Vi=conjg(Vi)
#if defined _DOUBLE
             call zgemv('n',BS_blk_dim(ik1),BS_blk_dim(ik1),alpha,&
&                     BS_cpl_mat,BS_blk_dim(ik1),conjg(Vi(frag_pointer(1)+1:)),1,(1._SP,0._SP),&
&                     Vo(frag_pointer(1)+1),1)
#else
             call cgemv('n',BS_blk_dim(ik1),BS_blk_dim(ik1),alpha,&
&                     BS_cpl_mat,BS_blk_dim(ik1),conjg(Vi(frag_pointer(1)+1:)),1,(1._SP,0._SP),&
&                     Vo(frag_pointer(1)+1),1)
#endif       
!             Vi=conjg(Vi)
           end if
           !
           ! GPL_EXCLUDE_END
           !
         end if
         !
       else
         !
         ! Off diagonal block simmetrization
         !
         !  Resonant K is hermitian
         !  Coupling K is simmetric
         !
         if (.not.use_fragments) then
           forall(i1=BS_k_and_row_restart(1)+1:BS_k_and_row_restart(1)+BS_blk_dim(ik1),&
&                 i2=BS_k_and_row_restart(2)+1:BS_k_and_row_restart(2)+BS_blk_dim(ik2)) &
&                 BS_mat(i2,i1)=conjg(BS_mat(i1,i2))
           !
           ! GPL_EXCLUDE_START
           !
           if (BS_K_coupling) then
             forall(i1=BS_k_and_row_restart(1)+1:BS_k_and_row_restart(1)+BS_blk_dim(ik1),&
&                   i2=BS_k_and_row_restart(2)+1:BS_k_and_row_restart(2)+BS_blk_dim(ik2)) &
&                   BS_cpl_mat(i2,i1)=BS_cpl_mat(i1,i2)
           endif
           !
           ! GPL_EXCLUDE_END
           !
         else
           !
#if defined _DOUBLE
           call zgemv('n',BS_blk_dim(ik1),BS_blk_dim(ik2),(1._SP,0._SP),&
&                     BS_mat,BS_blk_dim(ik1),Vi(frag_pointer(2)+1),1,(1._SP,0._SP),&
&                     Vo(frag_pointer(1)+1),1)
           call zgemv('c',BS_blk_dim(ik1),BS_blk_dim(ik2),(1._SP,0._SP),&
&                     BS_mat,BS_blk_dim(ik1),Vi(frag_pointer(1)+1),1,(1._SP,0._SP),&
&                     Vo(frag_pointer(2)+1),1)
#else
           call cgemv('n',BS_blk_dim(ik1),BS_blk_dim(ik2),(1._SP,0._SP),&
&                     BS_mat,BS_blk_dim(ik1),Vi(frag_pointer(2)+1),1,(1._SP,0._SP),&
&                     Vo(frag_pointer(1)+1),1)
           call cgemv('c',BS_blk_dim(ik1),BS_blk_dim(ik2),(1._SP,0._SP),&
&                     BS_mat,BS_blk_dim(ik1),Vi(frag_pointer(1)+1),1,(1._SP,0._SP),&
&                     Vo(frag_pointer(2)+1),1)
#endif
           !
           ! GPL_EXCLUDE_START
           !
           if (BS_K_coupling) then
             !
             ! Calculate the off-diagonal coupling part
             !
             Vi=conjg(Vi)
#if defined _DOUBLE
             call zgemv('n',BS_blk_dim(ik1),BS_blk_dim(ik2),alpha,&
&                     BS_cpl_mat,BS_blk_dim(ik1),Vi(frag_pointer(2)+1),1,(1._SP,0._SP),&
&                     Vo(frag_pointer(1)+1),1)
             call zgemv('t',BS_blk_dim(ik1),BS_blk_dim(ik2),alpha,&
&                     BS_cpl_mat,BS_blk_dim(ik1),Vi(frag_pointer(1)+1),1,(1._SP,0._SP),&
&                     Vo(frag_pointer(2)+1),1)
#else
             call cgemv('n',BS_blk_dim(ik1),BS_blk_dim(ik2),alpha,&
&                     BS_cpl_mat,BS_blk_dim(ik1),Vi(frag_pointer(2)+1),1,(1._SP,0._SP),&
&                     Vo(frag_pointer(1)+1),1)
             call cgemv('t',BS_blk_dim(ik1),BS_blk_dim(ik2),alpha,&
&                     BS_cpl_mat,BS_blk_dim(ik1),Vi(frag_pointer(1)+1),1,(1._SP,0._SP),&
&                     Vo(frag_pointer(2)+1),1)
#endif       
             Vi=conjg(Vi)
           endif
           !
           ! GPL_EXCLUDE_END
           !
         endif
         !
       endif
       !
       if (use_fragments) deallocate(BS_mat)
       if (use_fragments.and.BS_K_coupling) deallocate(BS_cpl_mat)
       !
     enddo
   enddo
 endif
 !
 if (.not.use_fragments) then
   !
#if defined _DOUBLE
   call zgemv('n',BS_K_dim,BS_K_dim,(1._SP,0._SP),&
&             BS_mat,BS_K_dim,Vi,1,(0._SP,0._SP),Vo,1)
#else
   call cgemv('n',BS_K_dim,BS_K_dim,(1._SP,0._SP),&
&             BS_mat,BS_K_dim,Vi,1,(0._SP,0._SP),Vo,1)
#endif
   !
   ! GPL_EXCLUDE_START
   !
   if (BS_K_coupling) then
#if defined _DOUBLE
     call zgemv('n',BS_K_dim,BS_K_dim,alpha, BS_cpl_mat,&
&               BS_K_dim,conjg(Vi),1,(1._SP,0._SP),Vo,1)
#else
     call cgemv('n',BS_K_dim,BS_K_dim,alpha,BS_cpl_mat,&
&               BS_K_dim,conjg(Vi),1,(1._SP,0._SP),Vo,1)
#endif
   endif
   !
   ! GPL_EXCLUDE_END
   !
 else
   !
   call pp_redux_wait(Vo)
   call pp_indexes_reset(Kp)
   !
 endif
 !
end subroutine
