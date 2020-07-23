!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine RT_Lifetimes_merge_inter_k(what,LIFE)
 !
 use pars,           ONLY:SP
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_Xk_ibz,N_SND_RCV_operations,SND_RCV_op,&
&                         PAR_COM_Xk_ibz_INDEX,PP_send_and_receive
 use real_time,      ONLY:RT_nk,RT_bands,RT_k_FG
 use R_lattice,      ONLY:nkibz
 !
 implicit none
 !
 character(*)            :: what
 real(SP), intent(inout) :: LIFE(RT_bands(1):RT_bands(2),RT_nk)
 !
 ! Work Space
 !
 integer          :: ik,i_op,myid,i_el,i_cycle,COMM
 integer          :: ic_snd,ic_rcv
 integer          :: NK_to_do(PAR_COM_Xk_ibz_INDEX%n_CPU),IK_to_do(PAR_COM_Xk_ibz_INDEX%n_CPU,nkibz)
 logical          :: do_I_rcv,do_I_snd
 !
 if (PAR_COM_Xk_ibz_INDEX%n_CPU==1) return
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
         call PP_send_and_receive("send",LIFE(:,RT_k_FG%k_range(ik,1):RT_k_FG%k_range(ik,2)),ic_rcv-1,COMM=COMM)
       endif
       if (do_I_rcv) then
         call PP_send_and_receive("receive",LIFE(:,RT_k_FG%k_range(ik,1):RT_k_FG%k_range(ik,2)),ic_snd-1,COMM=COMM)
       endif
     enddo
     !
   enddo
   call PP_redux_wait(COMM=COMM)
 enddo
 !
end subroutine RT_Lifetimes_merge_inter_k
