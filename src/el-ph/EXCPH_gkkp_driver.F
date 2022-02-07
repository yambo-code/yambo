!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM FP
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
! --- Theory ---
! (5) Figure out rotations/symmetry of exciton eigenvectors and EPC m.e.
! - FP
!
subroutine EXCPH_gkkp_driver(E,k,q,X)
 !
 use pars,           ONLY:SP,cZERO,rZERO,schlen
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp,qindx_B_load
 use X_m,            ONLY:X_t
 use BS_solvers,     ONLY:BS_mat,BSS_eh_table,BS_H_dim,BSS_n_eig
 use BS,             ONLY:BS_H_dim,BS_bands,L_kind,BS_res_ares_n_mat,BS_K_dim
 use ELPH,           ONLY:elph_branches
 use IO_m,           ONLY:NONE,OP_APP_CL,OP_RD_CL,DUMP,OP_RD,RD_CL_IF_END,OP_WR_CL,deliver_IO_error_message, &
&                         REP,VERIFY
 use IO_int,         ONLY:io_control
 use EXCPH,          ONLY:BS_mat_in,BSS_eh_table_in,EXCPH_gkkp,EXCPH_gkkp_sq,&
&                         EXCPH_states,EXCPH_sum,EXCPH_kind,BSS_eh_table_m1,k_plus_q_table,Xi
 use LIVE_t,         ONLY:live_timing
 use timing_m,       ONLY:timing
 use com,            ONLY:msg
 !
#include<memory.h>
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 type(X_t)     ::X
 !
 ! Work Space
 !
 integer, external :: io_ELPH,io_EXCPH_gkkp
 integer :: ID_gkkp,ID_exc_gkkp,io_err
 integer :: il,iq
 !
 call section('*','Excitonic gkkp')
 !======================================
 !
 ! Check if Lout and Lin are present
 ! and read their kind
 ! 
 call EXCPH_load_L(1,X,'check','Lout')
 call EXCPH_load_L(1,X,'check','Lin')
 !
 ! Check if exciton-phonon matrix elements have been already calculated
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID_exc_gkkp)
 io_err=io_EXCPH_gkkp(ID_exc_gkkp)
 !
 if(io_err==0) return
 if(io_err/=0) call warning("[EXCPH] Exc-ph matrix elements not correct or missing. To be computed")
 !
 call timing('EXCPH_gkkp',OPR='start')
 !
 call EXCPH_load_L(1,X,'check','Lin') ! At present the Lin can be only gamma iq=1
 !
 call k_build_up_BZ_tables(k)
 call k_build_up_BZ_tables(q)
 !
 call BSE_alloc()
 !
 call EXCPH_load_L(1,X,'load','Lin')
 !
 ! Save the Lin in BS_mat_in
 ! 
 BS_mat_in=BS_mat(:,EXCPH_states(1):EXCPH_states(2))
 BSS_eh_table_in=BSS_eh_table(EXCPH_states(1):EXCPH_states(2),:)
 !
 ! Alloage EXCPH_gkkp
 !
 call EXCPH_gkkp_alloc()
 !
 ! K plus q table
 !
 call build_k_plus_q_table()
 !
 ! BSS_eh_table_m1
 !
 call build_inverse_eh_table()
 !
 ! Check if gkkp_expanded is present
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),ID=ID_gkkp)
 io_err=io_ELPH(ID_gkkp,'gkkp_expanded') ! For now we have to work in the full BZ by turning all symmetries off.
 call deliver_IO_error_message(io_err,"GKKP")
 !
 if (io_err/=0) call error('El-Ph database not found')
 !
 ! Write headers
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1/),ID=ID_exc_gkkp)
 io_err=io_EXCPH_gkkp(ID_exc_gkkp)
 !
 !
 ! Main q-loop
 !
 call live_timing('Excitonic Gkkp',q%nibz*(elph_branches(2)-elph_branches(1)+1))
 !
 do iq=1,q%nibz
   ! 
   if(iq>1) call EXCPH_load_L(iq,X,'check','Lout')
   call EXCPH_load_L(iq,X,'load','Lout')
   !
   EXCPH_gkkp   =cZERO
   EXCPH_gkkp_sq=rZERO
   !
   do il=elph_branches(1),elph_branches(2)
     !
     call  EXCPH_gkkp_eval(iq,il,E,k,q,X,ID_gkkp)
     call live_timing(steps=1)
     !
   enddo
   !
   ! I/O
   !=====
   call io_control(ACTION=OP_APP_CL,SEC=(/iq+1/),ID=ID_exc_gkkp)
   io_err=io_EXCPH_gkkp(ID_exc_gkkp)
   !
 enddo
 !
 call live_timing( )
 ! 
 call timing('EXCPH_gkkp',OPR='stop')
 !
 return
 !
 contains
   !
   subroutine BSE_alloc()
     !      
     ! pre-allocs...
     !
     YAMBO_ALLOC(BS_mat,(BS_H_dim,BSS_n_eig))
     YAMBO_ALLOC(BSS_eh_table,(BS_H_dim,3+n_sp_pol-1))
     YAMBO_ALLOC(BSS_eh_table_m1,(k%nbz,BS_bands(1):BS_bands(2),BS_bands(1):BS_bands(2)))
     YAMBO_ALLOC(k_plus_q_table,(k%nbz,q%nbz))
     !
     BS_mat=cZERO
     k_plus_q_table  = 0
     BSS_eh_table    = 0
     BSS_eh_table_m1 = 0
     !
   end subroutine BSE_alloc
   !
   subroutine EXCPH_gkkp_alloc()
     YAMBO_ALLOC(EXCPH_Gkkp,(elph_branches(1):elph_branches(2),EXCPH_sum(1):EXCPH_sum(2),EXCPH_states(1):EXCPH_states(2)))
     YAMBO_ALLOC(EXCPH_Gkkp_sq,(elph_branches(1):elph_branches(2),EXCPH_sum(1):EXCPH_sum(2),EXCPH_states(1):EXCPH_states(2)))
     YAMBO_ALLOC(Xi,(BS_H_dim,EXCPH_sum(1):EXCPH_sum(2),2))    
   end subroutine EXCPH_gkkp_alloc
   !
   subroutine build_k_plus_q_table()
     !      
     integer, external ::qindx_B_init,qindx_B_close 
     integer :: qindx_ID,qindx_ID_frag,qindx_tmp(2)
     integer :: ik_bz,iq_bz,ip_bz
     !
     call live_timing('K plus q table',k%nbz)
     qindx_tmp=qindx_B_init(qindx_ID,qindx_ID_frag)
     do ik_bz=1,k%nbz
       call live_timing(steps=1)
       IQ_bz_loop: do iq_bz=1,q%nbz
         do ip_bz=1,k%nbz
           qindx_tmp=qindx_B_load(ip_bz,ik_bz,qindx_ID_frag)
           if (qindx_tmp(1)==iq_bz) then
             k_plus_q_table(ik_bz,iq_bz)=ip_bz
             cycle IQ_bz_loop
           endif
         enddo
       enddo IQ_bz_loop
     enddo
     qindx_tmp=qindx_B_close(qindx_ID,qindx_ID_frag)
     call live_timing( )
     !
   end subroutine build_k_plus_q_table
   !
   subroutine build_inverse_eh_table()
     !
     integer :: iv,ic,i_H,ik_bz
     !     
     call live_timing('BSE inverse eh-table',BS_H_dim)
     do i_H=1,BS_H_dim
       !
       ik_bz = BSS_eh_table(i_H,1)
       iv    = BSS_eh_table(i_H,2)
       ic    = BSS_eh_table(i_H,3)
       !
       BSS_eh_table_m1(ik_bz,iv,ic)=i_H
       call live_timing(steps=1)
       !
     enddo
     call live_timing( )
     !
   end subroutine build_inverse_eh_table
   !
end subroutine
