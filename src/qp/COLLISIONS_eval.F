!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine COLLISIONS_eval(E,X,k,xk,q,Xw)
 !
 use drivers,       ONLY:l_HF_and_locXC,l_cohsex,l_carrier_dynamics 
 use com,           ONLY:error
 use pars,          ONLY:schlen,SP,IP
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:i_time_rev,nsym
 use QP_m,          ONLY:QP_nb,QP_n_states,QP_nk,QP_n_G_bands,QP_state
 use wave_func,     ONLY:WF
 use global_XC,     ONLY:SC_HF,SC_COHSEX,SC_EXX,SC_EXXC,WF_kind,WF_xc_functional
 use SC,            ONLY:SC_bands
 use global_XC,     ONLY:SC_HF,SC_COHSEX
 use collisions_IO, ONLY:V_kind,collisions_IO_alloc,collisions_IO_free,TEMP_COLLISIONS_v,&
&                        io_COLLISIONS,collisions_RANGES_setup,COLLISIONS_range,l_STORE_COLLISIONS
 use collision,     ONLY:COLLISIONS
 use X_m,           ONLY:X_t
 use frequency,     ONLY:w_samp
 use memory_m,      ONLY:mem_est
 use IO_m,          ONLY:io_control,OP_RD_CL,VERIFY,REP
 use fragments,     ONLY:fragment_status
 use interfaces,    ONLY:PARALLEL_global_indexes
 use parallel_m,    ONLY:PAR_nQPp,PAR_nQ,PAR_nQP
 use parser_m,      ONLY:parser
 !
 implicit none
 type(levels)  :: E
 type(w_samp)  :: Xw(2) 
 type(bz_samp) :: k,Xk,q
 type(X_t)     :: X(4)
 !
 ! WorkSpace
 !
 integer           ::ID
 character(schlen) ::ch
 logical           ::l_COLLISIONS_on_the_fly
 !
 call section('*','Extended collisions')
 !=======================================
 !
 call parser('COLLISIONS_on_the_fly',l_COLLISIONS_on_the_fly)
 l_STORE_COLLISIONS    =.not.l_COLLISIONS_on_the_fly
 !
 ! QP states setup
 !
 ! When storing all collisions on file I need all SC bands
 !
 if(allocated(QP_state)) deallocate(QP_state)
 QP_nb=SC_bands(2)
 call QP_state_table_setup(E)
 !
 ! Check for the Time Reversal Symmetry
 !
 if (i_time_rev==1) call error(" COLLISIONS not compatible with Time Reversal Symmetry! ")
 !
 ! No DB => EVAL
 !===============
 !
 if (l_STORE_COLLISIONS) then
   allocate(fragment_status(QP_n_states))
   call mem_est("fragment_status",(/QP_n_states/),(/IP/))
 endif
 !
 if (l_HF_and_locXC.and..not.l_carrier_dynamics) then
   !
   call section('+','Hartree-Fock')
   !================================
   !
   ! PARALLEL Setup
   !================
   call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.)
   call PARALLEL_global_indexes(E,k,q,"HF")
   !
   V_kind=SC_HF
   fragment_status=-1
   if (l_STORE_COLLISIONS) call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
   !
   if (io_COLLISIONS(0,ID)/=0.or..not.l_STORE_COLLISIONS) then
     !
     if (l_STORE_COLLISIONS) then
       allocate(TEMP_COLLISIONS_v(QP_nb,QP_nb,QP_nk))
       call mem_est("TEMP_COLLISIONS_v",(/size(TEMP_COLLISIONS_v)/),(/2*SP/))
       call collisions_IO_alloc()
       call report_disk_usage('HF')
     endif
     !
     call XCo_Hartree_Fock(E,k,xk,q)
     !
   endif
   !
 endif
 !
 if (l_cohsex) then
   !
   if (l_HF_and_locXC)      call section('=','COHSEX')
   !                        ==========================
   if (.not.l_HF_and_locXC) call section('+','COHSEX')
   !
   ! PARALLEL Setup
   !================
   call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.)
   if (l_carrier_dynamics) then
     call PARALLEL_global_indexes(E,k,q,"Real_Time")
   else
     call PARALLEL_global_indexes(E,k,q,"GW")
   endif
   !
   if (l_STORE_COLLISIONS) then
     !
     V_kind=SC_COHSEX
     fragment_status=-1
     call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
     !
     if (io_COLLISIONS(0,ID)/=0) then
       !
       QP_n_G_bands(2)=SC_bands(2)
       !  
       ! RL vectors to dimension the COLLISIONS
       !  
       call collisions_RANGES_setup()
       !
       if (.not.allocated(TEMP_COLLISIONS_v)) then
         allocate(TEMP_COLLISIONS_v(COLLISIONS_range(1,1):COLLISIONS_range(1,2),&
&                                   COLLISIONS_range(2,1):COLLISIONS_range(2,2),&
&                                   COLLISIONS_range(3,1):COLLISIONS_range(3,2)))
         call mem_est("TEMP_COLLISIONS_v",(/size(TEMP_COLLISIONS_v)/),(/2*SP/))
         call collisions_IO_alloc()
       endif
       !
       call report_disk_usage('COHSEX')
       !
       call COLLISIONS_cohsex(X(2),Xk,E,k,q,Xw(2)) 
       !
     endif
     !
   else
     !
     allocate(COLLISIONS(PAR_nQP))
     !
     call COLLISIONS_cohsex(X(2),Xk,E,k,q,Xw(2)) 
     !
   endif
   !
 endif
 !
 ! CLEAN
 !=======
 !
 if (l_STORE_COLLISIONS.or.allocated(TEMP_COLLISIONS_v)) then
   deallocate(fragment_status)
   call mem_est("fragment_status")
   call WF_free()
   call collisions_IO_free()
   if (allocated(TEMP_COLLISIONS_v)) deallocate(TEMP_COLLISIONS_v)
   call mem_est("TEMP_COLLISIONS_v")
 endif
 !
 contains
   !
   subroutine report_disk_usage(what)
     !
     use com,   ONLY:msg
     use units, ONLY:Gigabyte
     !
     real(SP)     :: disk_space
     character(*) :: what
     !
     disk_space=real(QP_n_states*(QP_nb-SC_bands(1)+1)**2*QP_nk*SP)/Gigabyte
     if (disk_space > 0.05) call msg('s','[COLLISIONS I/O] '//what//' Disk space required [Gb]:',disk_space)  
     !
   end subroutine
   !
end subroutine
