!
! Copyright (C) 2000-2009 C. Attaccalite and the YAMBO team
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
subroutine COLLISIONS_IO_compose(G_input)
 !
 ! HF(qp_state) = - (0.,1.) * \sum_{n,n',q} rho_rho_(n,n',q,qp_state) x G_lesser(n,n',k-q)
 ! 
 use drivers,        ONLY:l_sc_hf,l_sc_chosex
 use pars,           ONLY:SP
 use QP_m,           ONLY:QP_nk,QP_nb,QP_n_states,QP_Vnl_xc,QP_Sc
 use SC,             ONLY:SC_bands
 use global_XC,      ONLY:SC_HF,SC_CHOSEX
 use collisions_IO,  ONLY:COLLISIONS_v,n_collisions,COLLISIONS_table,io_COLLISIONS,V_kind 
 use par_proc_m,     ONLY:pp_indexes,pp_redux_wait,pp_indexes_reset
 use par_indexes_m,  ONLY:par_indexes
 use IO_m,           ONLY:io_control,OP_RD_CL
 use collisions_CACHE, ONLY:last_QP_mem,CACHE
 !
 implicit none
 !       
 complex(SP), intent(in)   :: G_input(SC_bands,SC_bands,QP_nk)
 !
 ! Work Space
 !
 integer           :: i1,ib,ibp,ik,ic
 integer           :: io_err,ID       
 type(pp_indexes)  :: px
 !
 if(.not.l_sc_hf.and..not.l_sc_chosex) return
 !
 call pp_indexes_reset(px)   
 !
 call par_indexes(px,(/QP_n_states/))
 !
 if(l_sc_hf.or.l_sc_chosex.or.last_QP_mem/=0) QP_Vnl_xc=(0._SP,0._SP)
 if(l_sc_chosex) QP_sc=(0._SP,0._SP)
 !
 ! First loop on oscillators in the Cache
 ! notice that in memory all oscillators are sum together
 ! so all the contribution to quasi-particle is stored in QP_Vnl_xc
 !
 do i1=1,last_QP_mem
   !
   if(.not.px%i1p(i1)) cycle  
   !
   ic=1
   !
   do ib=1,QP_nb
     do ibp=1,QP_nb
       do ik=1,QP_nk
         if(CACHE(i1)%COLLISIONS_table(ib,ibp,ik)/=0) then
           QP_Vnl_xc(i1)=QP_Vnl_xc(i1)-(0._SP,1._SP)*CACHE(i1)%COLLISIONS_v(ic)*G_input(ib,ibp,ik)
           ic=ic+1
         endif
       enddo
     enddo
   enddo
   !
 enddo
 !
 ! .. then read the remaning oscillators from disk
 ! On disk each contributions is stored in a different database
 !
 do i1=last_QP_mem+1,QP_n_states 
   !
   if(.not.px%i1p(i1)) cycle  
   !
   if(l_sc_hf.or.l_sc_chosex) then
     !
     V_kind=SC_HF
     !
     call io_control(ACTION=OP_RD_CL,SEC=(/i1+1/),ID=ID)
     io_err=io_COLLISIONS(i1,ID)
     !
     ic=1
     !
     do ib=1,QP_nb
       do ibp=1,QP_nb
         do ik=1,QP_nk
           if(COLLISIONS_table(ib,ibp,ik)/=0) then
             QP_Vnl_xc(i1)=QP_Vnl_xc(i1)-(0._SP,1._SP)*COLLISIONS_v(ic)*G_input(ib,ibp,ik)
             ic=ic+1
           endif
         enddo
       enddo
     enddo
     !
   endif
   !
   if(l_sc_chosex) then
     !
     V_kind=SC_CHOSEX
     !
     call io_control(ACTION=OP_RD_CL,SEC=(/i1+1/),ID=ID)
     io_err=io_COLLISIONS(i1,ID)
     !
     ic=1
     !
     do ib=1,QP_nb
       do ibp=1,QP_nb
         do ik=1,QP_nk
           if(COLLISIONS_table(ib,ibp,ik)/=0) then
             QP_Sc(i1,1)=QP_Sc(i1,1)-(0._SP,1._SP)*COLLISIONS_v(ic)*G_input(ib,ibp,ik)      
             ic=ic+1
           endif
          enddo
        enddo
     enddo
     !
   endif
   !
 enddo
 !
 ! When the Cache is used all the QP correction are put together
 ! in QP_Vnl_xc
 ! 
 if(l_sc_hf.or.l_sc_chosex.or.last_QP_mem/=0)   call pp_redux_wait(QP_Vnl_xc)
 if(l_sc_chosex)                                call pp_redux_wait(QP_Sc)
 !
 call pp_indexes_reset(px)
 !
end subroutine 
