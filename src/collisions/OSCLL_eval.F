!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA
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
subroutine OSCLL_eval(q,k,en) 
 !
 ! Evaluate oscillators for the Local-SEX
 !
 use LIVE_t,              ONLY:live_timing
 use pars,                ONLY:SP,cZERO
 use R_lattice,           ONLY:bz_samp,qindx_S
 use electrons,           ONLY:levels,n_sp_pol
 use wave_func,           ONLY:WF,wf_ng
 use interfaces,          ONLY:WF_load,WF_free
 use QP_m,                ONLY:QP_nk
 use stderr,              ONLY:intc
 use collision_el,        ONLY:elemental_collision,elemental_collision_free, &
&                              elemental_collision_alloc         
 use collision_ext,       ONLY:COLL_bands,W,OSCLL_k,ng_oscll
 use parallel_m,          ONLY:PAR_IND_Xk_ibz,PAR_IND_Xk_ibz_ID
 use FFT_m,               ONLY:fft_size
 use IO_int,              ONLY:io_control,IO_and_Messaging_switch
 use IO_m,                ONLY:OP_RD_CL,VERIFY,REP,OP_WR_CL,OP_APP_CL,file_is_present,rm_file,DUMP, &
&                              OP_IF_START_APP_CL_IF_END,manage_action 
 use com,                 ONLY:msg
 use parallel_int,        ONLY:PP_redux_wait,PP_wait,PARALLEL_WF_distribute,PARALLEL_WF_index
 use parallel_m,          ONLY:PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX,master_cpu,myid,PAR_IND_G_b,master_cpu, &
&                              PAR_Xk_nibz,PAR_Xk_ibz_index,PAR_COM_Xk_ibz_A2A,PAR_COM_G_b_INDEX,myid,PP_indexes
 !
#include<dev_defs.h>
#include<memory.h>
 !  
 type(bz_samp), intent(in) :: q,k
 type(levels),  intent(in) :: en
 !
 ! Work Space
 !
 type(elemental_collision)            :: isc 
 integer :: i_k,i_m,i_n,i_q,iqbz,i_kmq_G0,iqs,i_kmq,i_kmq_s,i_k_s,ik_mem
 integer :: i_k_sp,i_kmq_sp,iqibz,ig,i_np,i_mp,io,ic,i_sp_pol,ib_idx
 integer :: ID,io_err,iGo_max,i_fragment,max_count,i_count,IO_ACT
 integer :: COLL_nbands,OSCLL_perc
 type(PP_indexes)  :: K_index
 logical, allocatable :: OSCLL_done(:)
 integer, external :: io_OSCLL
 logical :: IO_do_it
 !
 call PP_wait() 
 !
 i_sp_pol=1
 !
 call IO_and_Messaging_switch("SAVE")
 call IO_and_Messaging_switch("+io_in",CONDITION=.TRUE.)
 !   
 call msg('s','[OSC] Checking oscillators header ')
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
 io_err=io_OSCLL(q,ID)
 ! 
 YAMBO_ALLOC(OSCLL_done,(QP_nk))
 OSCLL_done=.FALSE.
 !
 YAMBO_ALLOC(K_index%element_1D,(QP_nk))
 !
 if(io_err==0)  then
   !
   ! Check if all oscillators has been evaluated
   !
   if(master_cpu) then
     !
     do i_k=1,QP_nk
       if(file_is_present('OSCLL_fragment_'//intc(i_k))) OSCLL_done(i_k)=.TRUE.
     enddo                                                                    
     !	    
   endif
   !
   call PP_redux_wait(OSCLL_done,imode=3)
   !    
   if(all(OSCLL_done.eqv..TRUE.)) then 
     return
   else
     OSCLL_perc=nint(count(OSCLL_done)/real(QP_nk)*100)
     call msg('s','Restarting Oscillators calculation : '//trim(intc(OSCLL_perc))//'% done')
   endif
   !
 else
    call msg('s','[OSC] Missing or broken oscillators ')
 endif
 !
 ! Oscillators not found, recalculated them
 !
 if(master_cpu) then
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
   io_err=io_OSCLL(q,ID)
 endif
 !
 max_count=120
 do while(io_err/=0)
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=DUMP,ID=ID)
   io_err=io_OSCLL(q,ID)
   if(io_err/=0) call sleep(1)
   i_count=i_count+1
   if((i_count==max_count).and.io_err/=0) &
&      call error(" [OSCLL] Header not found while writing dipoles ")
 enddo
 !
 call PP_wait()
 !
 iGo_max=maxval(qindx_S(:,:,2)) 
 !
 call elemental_collision_free(isc)
 !
 isc%iqref =0
 isc%ngrho =ng_oscll  ! only G=1 is required for local-SEX
 !
 call WF_load(WF,wf_ng,iGo_max,COLL_bands,(/1,1/),space='R',title='-OSC')  
 call elemental_collision_alloc(isc,NG=isc%ngrho,TITLE="OSCLL")
 call WF_free(WF,keep_fft=.TRUE.)
 !
 call live_timing('[OSC] Oscillators :',PAR_IND_Xk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1))
 !
 YAMBO_ALLOC(OSCLL_k,(COLL_bands(1):COLL_bands(2),COLL_bands(1):COLL_bands(2),q%nbz,ng_oscll))
 !
 COLL_nbands=COLL_bands(2)-COLL_bands(1)+1
 !
 do i_k=1,QP_nk
   !
   if (.not.PAR_IND_Xk_ibz%element_1D(i_k)) cycle
   !
   if(OSCLL_done(i_k)) then
     call live_timing(steps=1)
     cycle
   endif
   ! 
   ik_mem=PAR_Xk_ibz_index(i_k)
   !
   OSCLL_k=cZERO
   !
   do iqbz=1,q%nbz
     !
     i_kmq_G0  =qindx_S(i_k,iqbz,2)
     iqibz     =q%sstar(iqbz,1)
     iqs       =q%sstar(iqbz,2)
     isc%qs  =(/i_kmq_G0,iqibz,iqs/)
     !
     i_k_s=1
     i_kmq   = k%sstar(qindx_S(i_k,iqbz,1),1)
     i_kmq_s = k%sstar(qindx_S(i_k,iqbz,1),2)
     !
     !  Spin-Polarization not supported!!
     ! 
     i_k_sp  = 1
     i_kmq_sp= 1
     !
     ! Load only two k-points 
     !	   
     K_index%element_1D=.FALSE.
     K_index%element_1D(i_k) =.TRUE.
     K_index%element_1D(i_kmq)=.TRUE.
     !
     call IO_and_Messaging_switch("-report -log")
     call PARALLEL_WF_distribute(K_index=K_index,CLEAN_UP=.TRUE.)
     call WF_load(WF,wf_ng,iGo_max,COLL_bands,(/1,k%nibz/),space='R',title='-OSC')  
     call IO_and_Messaging_switch("+report +log")
     !
     do i_n=COLL_bands(1),COLL_bands(2)
       do i_m=COLL_bands(1),COLL_bands(2)
         !
         ib_idx=i_m-COLL_bands(1)+1+(i_n-COLL_bands(1))*COLL_nbands
         !
         if (.not.PAR_IND_G_b%element_1D(ib_idx)) cycle
         !
         isc%is  =(/i_n,i_k,i_k_s,i_k_sp/)
         isc%os  =(/i_m,i_kmq,i_kmq_s,i_kmq_sp/)
         call DEV_SUB(scatter_Bamp)(isc)            
         !
!$omp parallel do default(shared), private(ig)
         do ig=1,ng_oscll
           OSCLL_k(i_n,i_m,iqbz,ig)=isc%rhotw(ig)
         enddo
!$omp end parallel do
         !
       enddo
    enddo
    !
    call WF_free(WF,keep_fft=.TRUE.)
    !
   enddo
   !
   call PP_redux_wait(OSCLL_k(:,:,:,:),COMM=PAR_COM_G_b_INDEX%COMM)
   !
   i_fragment=i_k+(i_sp_pol-1)*k%nibz
   !
   IO_ACT=OP_APP_CL !manage_action(OP_IF_START_APP_CL_IF_END,i_fragment,1,k%nibz*n_sp_pol)
   call io_control(ACTION=IO_ACT,COM=REP,SEC=(/1+i_fragment/),MODE=VERIFY,ID=ID)
   io_err=io_OSCLL(q,ID)
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call IO_and_Messaging_switch("RESTORE")
 !
 call live_timing()
 !
 YAMBO_FREE(OSCLL_k)
 !
 call elemental_collision_free(isc)
 call WF_free(WF)  
 !
 call PP_wait() 
 !
end subroutine OSCLL_eval
