!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA
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
subroutine COLLISIONS_load(CHECK_EXISTENCE_ONLY)
 !
 ! Load all collisions contribution from any kind of self-energy 
 ! exchange, COHSEX etc.. 
 !
 use drivers,         ONLY:l_sc_hf,l_sc_cohsex,l_sc_exx,l_use_collisions,l_sc_srpa,&
&                          l_elel_scatt,l_elphoton_scatt,l_elphoton_corr
 use LIVE_t,          ONLY:live_timing
 use QP_m,            ONLY:QP_n_states,QP_QED_ng,QP_table
 use R_lattice,       ONLY:nqbz
 use plasma,          ONLY:N_plasma_poles_global
 use global_XC,       ONLY:SC_HF,SC_COHSEX,SE_GW_NEQ
#if defined _QED || defined _RT
 use global_XC,       ONLY:SE_GW_QED
 use collision,       ONLY:P_COLL_element,COLL_bands
#endif
 use collision,       ONLY:V_kind,GW_NEQ_COLL_element,SEX_COLL_element,FOCK_COLL_element
 use IO_m,            ONLY:OP_RD,RD_CL,OP_RD_CL,io_control,VERIFY,REP
 use parallel_m,      ONLY:PAR_nQP,PAR_QP_index,PAR_IND_QP,PAR_IND_Plasma,PAR_IND_Q,&
&                          PAR_nQ,PAR_Q_index,PAR_PLASMA_index
 use interfaces,      ONLY:io_COLLISIONS
 use electrons,       ONLY:spin
 use stderr,          ONLY:intc
 use com,             ONLY:warning,error
 !
 implicit none
 !
 logical            :: CHECK_EXISTENCE_ONLY
 !
 ! Parameters
 !
 integer, parameter :: max_V_kinds=10
 !
 ! Work Space
 !
 integer            :: iqbz,i_J,ic,i_qp,ID,io_err(max_V_kinds),V_kinds(max_V_kinds),n_V_kinds,&
&                      iv,i_qp_mem,i_q_mem,i_J_mem,i_b,i_g,state(4),i_db
 logical            :: condition
 integer            :: DB_table(QP_n_states) ! Table pointing to the COLLISION DB
 !
 n_V_kinds   =0
 io_err      =0
 V_kinds     =0
 !
 condition=l_sc_hf.or.l_sc_cohsex.or.l_sc_exx
 if (condition) then
   n_V_kinds          =n_V_kinds+1
   V_kinds(n_V_kinds) =SC_HF
 endif
 !
 condition=l_sc_cohsex.or.l_sc_srpa
 if (condition) then
   n_V_kinds          =n_V_kinds+1
   V_kinds(n_V_kinds) =SC_COHSEX
 endif
 !
#if defined _RT
 if(l_elel_scatt) then
   n_V_kinds          =n_V_kinds+1
   V_kinds(n_V_kinds) =SE_GW_NEQ
 endif
#endif
 !
#if defined _QED
 if(l_elphoton_scatt.or.l_elphoton_corr) then
   n_V_kinds          =n_V_kinds+1
   V_kinds(n_V_kinds) =SE_GW_QED
 endif
#endif
 !
 if (n_V_kinds==0) return
 !
 ! Check only if the DB exist and are readable
 !
 if (CHECK_EXISTENCE_ONLY) then
   !
   do iv=1,n_V_kinds
     !
     V_kind=V_kinds(iv)
     !
     call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
     io_err(iv)=io_COLLISIONS(0,ID)
     !
   enddo
   !
   l_use_collisions=all(io_err==0)
   !
   return
   !
 endif
 !
 if (.not.l_use_collisions) return
 !
! if (n_COLLISIONS/=QP_n_states) then
!   call warning('Found '//trim(intc(n_COLLISIONS))//' COLLISION components. Only '//trim(intc(QP_n_states))//' requested.')
!   if (n_COLLISIONS<QP_n_states) then
!     call error('Not enough COLLISION components')
!   endif
! endif
! !
! DB_table=0
! if(allocated(COLLISIONS_state)) then
!   do i_qp=1,QP_n_states
!     state(:3)=QP_table(i_qp,:3)
!     state( 4)=spin(QP_table(i_qp,:))
!     do i_db=1,n_COLLISIONS
!       if (all(state(:)==COLLISIONS_state(i_db,:))) DB_table(i_qp)=i_db
!     enddo
!     if (DB_table(i_qp)==0) call error('COLLISION components #'//trim(intc(i_qp))//' not found')
!   enddo
! else
!   do i_qp=1,QP_n_states
!     DB_table(i_qp)=i_qp
!   enddo
! endif
 !
 call live_timing('Collisions DB I/O:',PAR_nQP*n_V_kinds)
 !
 do iv=1,n_V_kinds
   !
   V_kind=V_kinds(iv)
   !
   if (V_kind==SC_HF)     call COLLISIONS_alloc_and_free("Fock","collision","alloc",0)
   if (V_kind==SC_COHSEX) call COLLISIONS_alloc_and_free("SEX","collision","alloc",0)
#if defined _QED
   if (V_kind==SE_GW_QED) call COLLISIONS_alloc_and_free("Momentum","collision","alloc",0)
   if (V_kind==SE_GW_QED) call COLLISIONS_alloc_and_free("Momentum","v3","alloc ",0)
#endif
   if (V_kind==SE_GW_NEQ) call COLLISIONS_alloc_and_free("GW_NEQ","collision","alloc",0)
   if (V_kind==SE_GW_NEQ) call COLLISIONS_alloc_and_free("GW_NEQ","v3","alloc distribute",0)
   !
   do i_qp=1,QP_n_states
     !
     if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
     !
     i_qp_mem=PAR_QP_index(i_qp)
     i_db    = i_qp
!DB_table(i_qp)
     !
     call io_control(ACTION=OP_RD,SEC=(/1,2/),ID=ID)
     !
     if (V_kind==SC_HF) then
       io_err(iv)=io_COLLISIONS(i_db,ID,FOCK_COLL_element(i_qp_mem))
       call COLLISIONS_alloc_and_free("Fock","v1 table","alloc",i_qp_mem)
     endif
     if (V_kind==SC_COHSEX) then
       io_err(iv)=io_COLLISIONS(i_db,ID,SEX_COLL_element(i_qp_mem))
       call COLLISIONS_alloc_and_free("SEX","v1 table","alloc",i_qp_mem)
     endif
     if (V_kind==SE_GW_NEQ) then
       if(.not.associated(GW_NEQ_COLL_element(1)%v_r)) then
         io_err(iv)=io_COLLISIONS(i_db,ID,GW_NEQ_COLL_element(1))
         call COLLISIONS_alloc_and_free("GW_NEQ","v1 table","alloc",1)
       endif
     endif
#if defined _QED
     if (V_kind==SE_GW_QED) then
       if(.not.associated(P_COLL_element(1)%v_r)) then
         io_err(iv)=io_COLLISIONS(i_db,ID,P_COLL_element(1))
         call COLLISIONS_alloc_and_free("Momentum","v1 table","alloc",1)
       endif
     endif
#endif
     !
     call io_control(ACTION=RD_CL,SEC=(/3/),ID=ID)
     !
     if (V_kind==SC_HF)     io_err(iv)=io_COLLISIONS(i_db,ID,FOCK_COLL_element(i_qp_mem))
     if (V_kind==SC_COHSEX) io_err(iv)=io_COLLISIONS(i_db,ID,SEX_COLL_element(i_qp_mem))
#if defined _RT
     if (V_kind==SE_GW_NEQ) io_err(iv)=io_COLLISIONS(i_db,ID,GW_NEQ_COLL_element(1))
#endif
#if defined _QED
     if (V_kind==SE_GW_QED) io_err(iv)=io_COLLISIONS(i_db,ID,P_COLL_element(1))
#endif
     !
     ! Now the transfer %v_r => %v3_r
     !
#if defined _RT || defined _QED
     if (V_kind==SE_GW_NEQ.or.V_kind==SE_GW_QED) then
       !
       ic=0
       !
       do iqbz=1,nqbz
         !
         if (V_kind==SE_GW_NEQ) then
           !
           i_q_mem=PAR_Q_index(iqbz)
           !
           if (.not.PAR_IND_Q%element_1D(iqbz)) then
             ic=ic+N_plasma_poles_global
             cycle
           endif
           do i_J=1,N_plasma_poles_global
             ic=ic+1
             if (.not.PAR_IND_Plasma%element_1D(i_J)) cycle
             i_J_mem  =PAR_PLASMA_index(i_J)
             GW_NEQ_COLL_element(i_qp_mem)%v3_r(i_J_mem,1,i_q_mem)=GW_NEQ_COLL_element(1)%v_r(ic)
           enddo
         else
           do i_b=COLL_bands(1),COLL_bands(2)
             do i_g=1,QP_QED_ng
               ic=ic+1
               P_COLL_element(i_qp_mem)%v3_r(i_g,i_b,iqbz)=P_COLL_element(1)%v_r(ic)
             enddo
           enddo
         endif
         !
       enddo
     endif
#endif
     !
     call live_timing(steps=1)
     !
   end do
   !
   ! CLEAN
   !
#if defined _RT
   if (V_kind==SE_GW_NEQ) call COLLISIONS_alloc_and_free("GW_NEQ","v1 table","clean",1)
#endif
#if defined _QED
   if (V_kind==SE_GW_QED) call COLLISIONS_alloc_and_free("Momentum","v1 table","clean",1)
#endif
   !
 enddo
 !
 call live_timing()
 !
end subroutine COLLISIONS_load
