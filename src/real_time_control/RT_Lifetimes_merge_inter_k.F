!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine RT_Lifetimes_merge_inter_k(LIFE)
 !
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,N_SND_RCV_operations,SND_RCV_op,&
&                         PAR_COM_Xk_ibz_INDEX
 use parallel_int,   ONLY:PP_wait,PP_redux_wait,PP_send_and_receive 
 use real_time,      ONLY:RTibz
 use R_lattice,      ONLY:nkibz
 use RT_lifetimes,   ONLY:RT_lifetime
 !
 implicit none
 !
 type(RT_lifetime), intent(inout) :: LIFE
 !
 ! Work Space
 !
 integer          :: ik,i_op,myid,i_el,i_cycle,COMM,ic_snd,ic_rcv,TAG,&
&                    NK_to_do(PAR_COM_Xk_ibz_INDEX%n_CPU),IK_to_do(PAR_COM_Xk_ibz_INDEX%n_CPU,nkibz)
 logical          :: do_I_rcv,do_I_snd
 !
 if (PAR_COM_Xk_ibz_INDEX%n_CPU==1) return
 if (.not.LIFE%active             ) return
 !
 myid=PAR_COM_Xk_ibz_INDEX%CPU_id+1
 COMM=PAR_COM_Xk_ibz_INDEX%COMM
 !
 NK_to_do=0
 IK_to_do=0
 do ik=1,nkibz
   if (PAR_IND_Xk_ibz%element_1D(ik)) then
     NK_to_do(myid)=NK_to_do(myid)+1
     IK_to_do(myid,NK_to_do(myid))=ik
   endif
 enddo
 !
 call PP_redux_wait(NK_to_do,COMM=COMM)
 call PP_redux_wait(IK_to_do,COMM=COMM)
 !
 do i_op=1,N_SND_RCV_operations
   !
   do i_cycle=1,2
     !
     if (i_cycle==1) then
       ic_snd=SND_RCV_op(i_op)%RCV_from_id(myid)
       ic_rcv=SND_RCV_op(i_op)%SND_to_id(myid)
     else
       ic_snd=SND_RCV_op(i_op)%SND_to_id(myid)
       ic_rcv=SND_RCV_op(i_op)%RCV_from_id(myid)
     endif
     !
     do_I_rcv=ic_snd>0
     do_I_snd=ic_rcv>0
     if (ic_snd>0) then
       ic_rcv=myid
     else if (ic_rcv>0) then
       ic_snd=myid
     endif
     !
     if (ic_snd==0.and.ic_rcv==0) cycle
     !
     do i_el=1,NK_to_do(ic_snd)
       ik=IK_to_do(ic_snd,i_el)
       if (do_I_snd) then
         call PP_send_and_receive("send",LIFE%emit(:,RTibz%k_range(ik,1):RTibz%k_range(ik,2)),ic_rcv-1,COMM=COMM,TAG=1)
         call PP_send_and_receive("send",LIFE%abs(:,RTibz%k_range(ik,1):RTibz%k_range(ik,2)),ic_rcv-1,COMM=COMM,TAG=2)
       endif
       if (do_I_rcv) then
         call PP_send_and_receive("receive",LIFE%emit(:,RTibz%k_range(ik,1):RTibz%k_range(ik,2)),ic_snd-1,COMM=COMM,TAG=1)
         call PP_send_and_receive("receive",LIFE%abs(:,RTibz%k_range(ik,1):RTibz%k_range(ik,2)),ic_snd-1,COMM=COMM,TAG=2)
       endif
     enddo
     !
   enddo
   call PP_wait(COMM=COMM)
 enddo
 !
end subroutine RT_Lifetimes_merge_inter_k
