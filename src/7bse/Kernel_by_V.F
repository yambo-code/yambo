!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine Kernel_by_V(iq,iter,Vi,Vo)
 !
 !
 ! Here I fill the upper half of the kernel (coupling included):
 !
 !      | (K_r)     (K_c)    |  <- this part is done here
 !  K = |                    |
 !      | (-K_r^*)  (-K_c^*) |
 !
 ! if iter is present and K is resonant I also do
 !
 !  Vo = K_r x Vi
 !
 ! Parallel shared matrix multiplication implemented
 !
 use pars
 use memory_m,       ONLY:mem_est
 use par_proc_m,     ONLY:pp_redux_wait,pp_indexes,pp_indexes_reset 
 use par_indexes_m,  ONLY:par_indexes
 use R_lattice,      ONLY:nXkibz
 use BS,             ONLY:BS_mat,BS_K_dim,BS_blk_dim,BS_blk_coord,BS_eh_E,&
&                         cumulative_index,BS_DB_is_fragmented,BS_K_coupling
 use X_m,            ONLY:X_t
 use IO_m,           ONLY:io_control,OP_RD,RD,NONE,RD_CL,OP_RD_CL
 implicit none
 integer               ::iq
 integer,     optional ::iter
 complex(SP), optional ::Vi(:),Vo(:)
 !
 ! Work Space
 !
 integer         ::ik1,ik2,i1,i2,frag_pointer(2)
 type(X_t)       ::X
 type(pp_indexes)::Kp
 ! 
 ! I/O
 ! 
 integer           ::ioBS_err,ID,ACTION
 integer, external ::ioBS
 logical           ::use_fragments,db_load
 !
 if (present(Vo)) Vo=(0._SP,0._SP)
 !
 ! The I/O using the fragments can be done only during 
 ! an  Haydock cycle. Otherwise the fragments must  be
 ! used to fill BS_mat (like in diago/invert solvers)
 !
 use_fragments=.false.
 if (present(iter)) use_fragments=BS_DB_is_fragmented
 !
 db_load=.not.present(iter).or.use_fragments
 if (.not.db_load.and.present(iter)) db_load=iter==1
 !
 if (db_load) then
   !
   if (.not.use_fragments) then
     call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=ID)
     ioBS_err=ioBS(iq,X,ID)
     if (BS_K_coupling) then
       allocate(BS_mat(2*BS_K_dim,2*BS_K_dim))
       call mem_est('BS_mat',(/2*BS_K_dim**2/))
     else
       allocate(BS_mat(BS_K_dim,BS_K_dim))
       call mem_est('BS_mat',(/BS_K_dim**2/))
     endif
   else
     call pp_indexes_reset(Kp)
     call par_indexes(Kp,(/cumulative_index(1,nXkibz)-1/))
     if (present(iter)) then
       if (iter==1) call mem_est('BS_mat',(/maxval(BS_blk_dim)**2/))
     endif
   endif
   !
   do ik2=1,nXkibz
     do ik1=ik2,1,-1
       BS_blk_coord=(/ sum(BS_blk_dim(:ik1-1)),sum(BS_blk_dim(:ik2-1)) /)
       !
       ACTION=RD
       if (ik1==1.and.ik2==nXkibz) ACTION=RD_CL
       !
       if (use_fragments) then
         frag_pointer=BS_blk_coord
         BS_blk_coord=0
         if (.not.Kp%i1p(cumulative_index(ik1,ik2)-1)) cycle
         allocate(BS_mat(BS_blk_dim(ik1),BS_blk_dim(ik2)))
         ACTION=OP_RD_CL
       else
         frag_pointer=0
       endif
       !
       ! BS I/O
       ! 
       call io_control(ACTION=ACTION,COM=NONE,SEC=(/cumulative_index(ik1,ik2)/),ID=ID)
       ioBS_err=ioBS(iq,X,ID)
       !
       if (ik1==ik2) then
         !
         forall(i1=BS_blk_coord(1)+1:BS_blk_coord(1)+BS_blk_dim(ik1)) &
&              BS_mat(i1,i1)=BS_mat(i1,i1)+BS_eh_E(frag_pointer(1)+i1)
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
         do i1=BS_blk_coord(1)+1,BS_blk_coord(1)+BS_blk_dim(ik1)
           do i2=1,i1-1
             BS_mat(i1,i2)=conjg(BS_mat(i2,i1))
             if (BS_K_coupling) BS_mat(i1,BS_K_dim+i2)=BS_mat(i2,BS_K_dim+i1)
           enddo
         enddo
         !
         if (use_fragments) then
#if defined _DOUBLE
           call zgemv('n',BS_blk_dim(ik1),BS_blk_dim(ik1),(1.,0.),&
&                     BS_mat,BS_blk_dim(ik1),Vi(frag_pointer(1)+1),1,(1.,0.),&
&                     Vo(frag_pointer(1)+1),1)
#else
           call cgemv('n',BS_blk_dim(ik1),BS_blk_dim(ik1),(1.,0.),&
&                     BS_mat,BS_blk_dim(ik1),Vi(frag_pointer(1)+1),1,(1.,0.),&
&                     Vo(frag_pointer(1)+1),1)
#endif
         endif
       else
         !
         ! Off diagonal block simmetrization
         !
         !  Resonant K is hermitian
         !  Coupling K is simmetric
         !
         if (.not.use_fragments) then
           forall(i1=BS_blk_coord(1)+1:BS_blk_coord(1)+BS_blk_dim(ik1),&
&                 i2=BS_blk_coord(2)+1:BS_blk_coord(2)+BS_blk_dim(ik2)) &
&                 BS_mat(i2,i1)=conjg(BS_mat(i1,i2))
           if (BS_K_coupling) then
             forall(i1=BS_blk_coord(1)+1:BS_blk_coord(1)+BS_blk_dim(ik1),&
&                   i2=BS_blk_coord(2)+1:BS_blk_coord(2)+BS_blk_dim(ik2)) &
&                   BS_mat(i2,BS_K_dim+i1)=BS_mat(i1,BS_K_dim+i2)
           endif
         else
           !
#if defined _DOUBLE
           call zgemv('n',BS_blk_dim(ik1),BS_blk_dim(ik2),(1.,0.),&
&                     BS_mat,BS_blk_dim(ik1),Vi(frag_pointer(2)+1),1,(1.,0.),&
&                     Vo(frag_pointer(1)+1),1)
           call zgemv('c',BS_blk_dim(ik1),BS_blk_dim(ik2),(1.,0.),&
&                     BS_mat,BS_blk_dim(ik1),Vi(frag_pointer(1)+1),1,(1.,0.),&
&                     Vo(frag_pointer(2)+1),1)
#else
           call cgemv('n',BS_blk_dim(ik1),BS_blk_dim(ik2),(1.,0.),&
&                     BS_mat,BS_blk_dim(ik1),Vi(frag_pointer(2)+1),1,(1.,0.),&
&                     Vo(frag_pointer(1)+1),1)
           call cgemv('c',BS_blk_dim(ik1),BS_blk_dim(ik2),(1.,0.),&
&                     BS_mat,BS_blk_dim(ik1),Vi(frag_pointer(1)+1),1,(1.,0.),&
&                     Vo(frag_pointer(2)+1),1)
#endif
         endif
       endif
       !
       if (use_fragments) deallocate(BS_mat)
       !
     enddo
   enddo
 endif
 !
 if (.not.present(iter)) return
 !
 if (.not.use_fragments) then
#if defined _DOUBLE
   call zgemv('n',BS_K_dim,BS_K_dim,(1.,0.),BS_mat,BS_K_dim,Vi,1,(0.,0.),Vo,1)
#else
   call cgemv('n',BS_K_dim,BS_K_dim,(1.,0.),BS_mat,BS_K_dim,Vi,1,(0.,0.),Vo,1)
#endif
 else
   call pp_redux_wait(Vo)
   call pp_indexes_reset(Kp)
 endif
 !
 end subroutine
