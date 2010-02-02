!
! Copyright (C) 2000-2010 C. Attaccalite and the YAMBO team
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
 use drivers,          ONLY:l_sc_hf,l_sc_cohsex
 use pars,             ONLY:SP
 use QP_m,             ONLY:QP_nk,QP_nb,QP_n_states,QP_Vnl_xc,QP_Sc
 use SC,               ONLY:SC_bands
 use global_XC,        ONLY:SC_HF,SC_COHSEX
 use collisions_IO,    ONLY:COLLISIONS_v,COLLISIONS_table,V_kind 
 use par_proc_m,       ONLY:pp_indexes,pp_redux_wait,pp_indexes_reset,myid
 use par_indexes_m,    ONLY:par_indexes
 use IO_m,             ONLY:io_control,OP_RD_CL,NONE
 use collisions_CACHE, ONLY:last_QP_mem,CACHE
 !
 implicit none
 !       
 complex(SP), intent(in)   :: G_input(SC_bands,SC_bands,QP_nk)
 !
 ! Work Space
 !
 integer, external :: io_COLLISIONS
 integer           :: i_qp,ib,ibp,ik,ic
 integer           :: io_err,ID       
 type(pp_indexes)  :: px
 complex(SP)       :: CHO_factor
 !
 if(.not.l_sc_hf.and..not.l_sc_cohsex) return
 !
 call pp_indexes_reset(px)   
 !
 call par_indexes(px,(/QP_n_states/))
 !
 if(l_sc_hf.or.l_sc_cohsex.or.last_QP_mem/=0) QP_Vnl_xc=(0._SP,0._SP)
 if(l_sc_cohsex) QP_sc=(0._SP,0._SP)
 !
 ! First loop on collisions in the Cache
 ! notice that in memory all collisions are sum together
 ! so all the contribution to quasi-particle is stored in QP_Vnl_xc
 !
 do i_qp=1,last_QP_mem
   !
   if(.not.px%element_1D(i_qp)) cycle  
   !
   ic=1
   !
   do ib=1,QP_nb
     do ibp=1,QP_nb
       do ik=1,QP_nk
         if(CACHE(i_qp)%COLLISIONS_table(ib,ibp,ik)/=0) then
           QP_Vnl_xc(i_qp)=QP_Vnl_xc(i_qp)-(0._SP,1._SP)*CACHE(i_qp)%COLLISIONS_v(ic)*G_input(ib,ibp,ik)
           ic=ic+1
         endif
       enddo
     enddo
   enddo
   !
 enddo
 !
 ! .. then read the remaning collisions from disk
 ! On disk each contributions is stored in a different database
 !
 do i_qp=last_QP_mem+1,QP_n_states 
   !
   if(.not.px%element_1D(i_qp)) cycle  
   !
   if(l_sc_hf.or.l_sc_cohsex) then
     !
     V_kind=SC_HF
     !
     if (i_qp == px%first_of_1D(myid+1) ) then
       call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),ID=ID)
     else 
       call io_control(ACTION=OP_RD_CL,SEC=(/2/),ID=ID)
     endif
     io_err=io_COLLISIONS(i_qp,ID)
     !
     ic=1
     !
     do ib=1,QP_nb
       do ibp=1,QP_nb
         do ik=1,QP_nk
           if(COLLISIONS_table(ib,ibp,ik)/=0) then
             QP_Vnl_xc(i_qp)=QP_Vnl_xc(i_qp)-(0._SP,1._SP)*COLLISIONS_v(ic)*G_input(ib,ibp,ik)
             ic=ic+1
           endif
         enddo
       enddo
     enddo
     !
   endif
   !
   if(l_sc_cohsex) then
     !
     V_kind=SC_COHSEX
     !
     if (i_qp == px%first_of_1D(myid+1) ) then
       call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),ID=ID)
     else 
       call io_control(ACTION=OP_RD_CL,SEC=(/2/),ID=ID)
     endif
     io_err=io_COLLISIONS(i_qp,ID)
     !
     ic=1
     !
     do ib=1,QP_nb
       do ibp=1,QP_nb
         do ik=1,QP_nk
           if(COLLISIONS_table(ib,ibp,ik)/=0) then
             !
             CHO_factor=(0._SP,1._SP)*G_input(ib,ibp,ik)
             if (ib==ibp) CHO_factor=(0._SP,1._SP)*G_input(ib,ibp,ik) +1.
             !
             QP_Sc(i_qp,1)=QP_Sc(i_qp,1)-COLLISIONS_v(ic)*CHO_factor
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
 if(l_sc_hf.or.l_sc_cohsex.or.last_QP_mem/=0)   call pp_redux_wait(QP_Vnl_xc)
 if(l_sc_cohsex)                                call pp_redux_wait(QP_Sc)
 !
 call pp_indexes_reset(px)
 !
end subroutine 
