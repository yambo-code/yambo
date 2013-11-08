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
 ! exchange, COHSEX etc.. and store them together 
 !
 use pars,            ONLY:IP,SP,LP
 use drivers,         ONLY:l_sc_hf,l_sc_cohsex,l_sc_exx,l_eval_collisions,l_sc_srpa,l_elel_scatt,l_elel_corr
 use LIVE_t,          ONLY:live_timing
 use global_XC,       ONLY:SC_HF,SC_COHSEX,SE_GW_NEQ
 use QP_m,            ONLY:QP_n_states
 use D_lattice,       ONLY:nsym
 use memory_m,        ONLY:mem_est
 use collisions_IO,   ONLY:COLLISIONS_v,COLLISIONS_table,V_kind,collisions_IO_free,&
&                          io_COLLISIONS,COLLISIONS_range,collisions_RANGES_setup
 use IO_m,            ONLY:OP_RD_CL,io_control,VERIFY,REP
 use parallel_m,      ONLY:PAR_nQPp,PAR_nQP,PAR_QP_index,PAR_QPp_index,PAR_IND_QP,PAR_IND_QPp,&
&                          PAR_nQ,PAR_Q_index,PAR_IND_Q
 use collision,       ONLY:COLLISIONS
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
 integer            :: i1,i2,i3,ic,i_qp,ID,n_tot_rho_states,io_err(max_V_kinds),V_kinds(max_V_kinds),n_V_kinds,&
&                      iv,i_qp_mem,i_q_mem,i_qpp_mem
 complex(SP), allocatable :: COLLISIONS_disk(:,:,:)
 !
 n_V_kinds   =0
 io_err      =0
 !
 if((l_sc_hf.or.l_sc_cohsex.or.l_sc_exx).and.l_elel_corr) then
   n_V_kinds          =n_V_kinds+1
   V_kinds(n_V_kinds) =SC_HF
 endif
 !
 if((l_sc_cohsex.or.l_sc_srpa).and.l_elel_corr) then
   n_V_kinds          =n_V_kinds+1
   V_kinds(n_V_kinds) =SC_COHSEX
 endif
 !
 if(l_sc_cohsex.and.l_elel_scatt) then
   n_V_kinds          =n_V_kinds+1
   V_kinds(n_V_kinds) =SE_GW_NEQ
 endif
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
   l_eval_collisions=all(io_err==0)
   !
   return
   !
 endif
 !
 if (.not.l_eval_collisions) return
 !
 call live_timing('Collisions DB I/O:',PAR_nQP)
 !
 allocate(COLLISIONS(PAR_nQP))
 !
 do i_qp=1,QP_n_states
   !
   if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
   !
   i_qp_mem=PAR_QP_index(i_qp)
   !
   if (l_elel_corr) then
     !
     allocate(COLLISIONS(i_qp_mem)%table(COLLISIONS_range(1,1):COLLISIONS_range(1,2),&
&                                        COLLISIONS_range(2,1):COLLISIONS_range(2,2),&
&                                        COLLISIONS_range(3,1):COLLISIONS_range(3,2)))
     !
     COLLISIONS(i_qp_mem)%table(:,:,:)=0
     !
   endif
   !
   do iv=1,n_V_kinds
     !
     V_kind=V_kinds(iv)
     !
     if (V_kind==SC_HF.or.V_kind==SE_GW_NEQ) then
       !
       if (allocated(COLLISIONS_disk)) then
         deallocate(COLLISIONS_disk)
         call mem_est("COLLISIONS_disk")
       endif
       !
       ! Array ranges setup and allocation
       !
       if (V_kind==SC_HF)     call collisions_RANGES_setup("ee_corr")
       if (V_kind==SE_GW_NEQ) call collisions_RANGES_setup("ee_scatt")
       !
       allocate(COLLISIONS_disk(COLLISIONS_range(1,1):COLLISIONS_range(1,2),&
&                               COLLISIONS_range(2,1):COLLISIONS_range(2,2),&
&                               COLLISIONS_range(3,1):COLLISIONS_range(3,2)))
       call mem_est("COLLISIONS_disk",(/size(COLLISIONS_disk)/),(/2*SP/))
       !
       COLLISIONS_disk=(0.,0.)
       !
     endif
     !
     ! In the carrier dynamics only the HF part is not needed
     !
     call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),ID=ID)
     io_err(iv)=io_COLLISIONS(i_qp,ID)
     !
     ic=0
     !
     do i1=COLLISIONS_range(1,1),COLLISIONS_range(1,2)
       do i2=COLLISIONS_range(2,1),COLLISIONS_range(2,2)
         do i3=COLLISIONS_range(3,1),COLLISIONS_range(3,2)
           if (V_kinds(iv)==SE_GW_NEQ) then
             ic=ic+1
             COLLISIONS_disk(i1,i2,i3)=COLLISIONS_disk(i1,i2,i3)+COLLISIONS_v(ic)
           else
             if(COLLISIONS_table(i1,i2,i3)==1) then
               ic=ic+1
               COLLISIONS(i_qp_mem)%table(i1,i2,i3)=1
               COLLISIONS_disk(i1,i2,i3)=COLLISIONS_disk(i1,i2,i3)+COLLISIONS_v(ic)
             endif
           endif
         enddo
       enddo
     enddo
     !
   enddo
   !
   if (l_elel_scatt) allocate(COLLISIONS(i_qp_mem)%v3(PAR_nQPp,nsym,PAR_nQ))
   if (l_elel_corr) then
     n_tot_rho_states=sum(int(COLLISIONS(i_qp_mem)%table(:,:,:),IP))
     allocate(COLLISIONS(i_qp_mem)%v(n_tot_rho_states))
   endif
   !
   ic=0
   !
   do i1=COLLISIONS_range(1,1),COLLISIONS_range(1,2)
     do i2=COLLISIONS_range(2,1),COLLISIONS_range(2,2)
       do i3=COLLISIONS_range(3,1),COLLISIONS_range(3,2)
         if (l_elel_scatt) then
           if (.not.PAR_IND_QPp%element_1D(i1)) cycle
           if (.not.PAR_IND_Q%element_1D(i3)) cycle
           i_qpp_mem=PAR_QPp_index(i1)
           i_q_mem  =PAR_Q_index(i3)
           COLLISIONS(i_qp_mem)%v3(i_qpp_mem,i2,i_q_mem)=COLLISIONS_disk(i1,i2,i3)
         else
           if (COLLISIONS(i_qp_mem)%table(i1,i2,i3)==1) then
             ic=ic+1
             COLLISIONS(i_qp_mem)%v(ic)=COLLISIONS_disk(i1,i2,i3)
           endif
         endif
       enddo
     enddo
   enddo
   !
   call live_timing(steps=1)
   !
 end do
 !
 call live_timing()
 !
 if (l_elel_scatt) then
   call mem_est("QP_COLLISIONS",(/size(COLLISIONS(1)%v3)*PAR_nQP/),(/2*SP/))
 endif
 if (l_elel_corr) then
   call mem_est("QP_COLLISIONS_table",(/size(COLLISIONS(1)%table)*PAR_nQP/),(/LP/))
   call mem_est("QP_COLLISIONS",(/size(COLLISIONS(1)%v)*PAR_nQP/),(/2*SP/))
 endif
 !
 ! CLEAN
 !
 deallocate(COLLISIONS_disk)
 call mem_est("COLLISIONS_disk")
 !
 call collisions_IO_free()
 !
end subroutine COLLISIONS_load
