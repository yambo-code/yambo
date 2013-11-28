!
! Copyright (C) 2000-2013 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine COLLISIONS_load(CHECK_EXISTENCE_ONLY)
 !
 ! Load all collisions contribution from any kind of self-energy 
 ! exchange, COHSEX etc.. 
 !
 use drivers,         ONLY:l_sc_hf,l_sc_cohsex,l_sc_exx,l_use_collisions,l_sc_srpa,l_elel_scatt
 use LIVE_t,          ONLY:live_timing
 use QP_m,            ONLY:QP_n_states
 use R_lattice,       ONLY:nqbz
 use plasma,          ONLY:max_N_plasma_poles
 use global_XC,       ONLY:SC_HF,SC_COHSEX,SE_GW_NEQ
 use collision,       ONLY:V_kind,GW_NEQ_collisions,COHSEX_collisions,HF_collisions
 use IO_m,            ONLY:OP_RD,RD_CL,OP_RD_CL,io_control,VERIFY,REP
 use parallel_m,      ONLY:PAR_nQP,PAR_QP_index,PAR_IND_QP,PAR_IND_Plasma,PAR_IND_Q,&
&                          PAR_nQ,PAR_Q_index,PAR_nPlasma,PAR_Plasma_index
 use interfaces,      ONLY:io_COLLISIONS
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
&                      iv,i_qp_mem,i_q_mem,i_J_mem
 logical            :: condition
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
 call live_timing('Collisions DB I/O:',PAR_nQP*n_V_kinds)
 !
 do iv=1,n_V_kinds
   !
   V_kind=V_kinds(iv)
   !
   if (V_kind==SC_HF)     call COLLISIONS_alloc_and_free("HF","collision","alloc",0)
   if (V_kind==SC_COHSEX) call COLLISIONS_alloc_and_free("COHSEX","collision","alloc",0)
   if (V_kind==SE_GW_NEQ) call COLLISIONS_alloc_and_free("GW_NEQ","collision","alloc",0)
   if (V_kind==SE_GW_NEQ) call COLLISIONS_alloc_and_free("GW_NEQ","v3","alloc distribute",0)
   !
   do i_qp=1,QP_n_states
     !
     if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
     !
     i_qp_mem=PAR_QP_index(i_qp)
     !
     call io_control(ACTION=OP_RD,SEC=(/1,2/),ID=ID)
     !
     ! To check for GW_NEQ_collisions allocation
     if (V_kind==SC_HF) then
        io_err(iv)=io_COLLISIONS(i_qp,ID,HF_collisions(i_qp_mem))
        call COLLISIONS_alloc_and_free("HF","v1 table","alloc",i_qp_mem)
     endif
     if (V_kind==SC_COHSEX) then
        io_err(iv)=io_COLLISIONS(i_qp,ID,COHSEX_collisions(i_qp_mem))
        call COLLISIONS_alloc_and_free("COHSEX","v1 table","alloc",i_qp_mem)
     endif
     if (V_kind==SE_GW_NEQ) then
        if(.not.associated(GW_NEQ_collisions(1)%v)) then
          io_err(iv)=io_COLLISIONS(i_qp,ID,GW_NEQ_collisions(1))
          call COLLISIONS_alloc_and_free("GW_NEQ","v1 table","alloc",1)
        endif
     endif
     !
     call io_control(ACTION=RD_CL,SEC=(/3/),ID=ID)
     !
     if (V_kind==SC_HF)     io_err(iv)=io_COLLISIONS(i_qp,ID,HF_collisions(i_qp_mem))
     if (V_kind==SC_COHSEX) io_err(iv)=io_COLLISIONS(i_qp,ID,COHSEX_collisions(i_qp_mem))
     if (V_kind==SE_GW_NEQ) io_err(iv)=io_COLLISIONS(i_qp,ID,GW_NEQ_collisions(1))
     !
     if (V_kind==SE_GW_NEQ) then
       ic=0
       do i_J=1,max_N_plasma_poles
         do iqbz=1,nqbz
           ic=ic+1
           if (.not.PAR_IND_Plasma%element_1D(i_J)) cycle
           if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
           i_J_mem  =PAR_Plasma_index(i_J)
           i_q_mem=PAR_Q_index(iqbz)
           GW_NEQ_collisions(i_qp_mem)%v3r(i_J_mem,1,i_q_mem)=real(GW_NEQ_collisions(1)%v(ic))
         enddo
       enddo
     endif
     !
     call live_timing(steps=1)
     !
   end do
   !
   ! CLEAN
   !
   if (V_kind==SE_GW_NEQ) then
     call COLLISIONS_alloc_and_free("GW_NEQ","v1 table","clean",1)
   endif
   !
 enddo
 !
 call live_timing()
 !
end subroutine COLLISIONS_load
