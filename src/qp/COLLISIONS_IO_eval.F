!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
subroutine COLLISIONS_IO_eval(E,X,k,xk,q,Xw)
 !
 use drivers,       ONLY:l_HF_and_locXC,l_cohsex
 use com,           ONLY:error
 use pars,          ONLY:schlen,SP,IP
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:i_time_rev
 use QP_m,          ONLY:QP_nb,QP_n_states,QP_nk
 use wave_func,     ONLY:WF_free
 use global_XC,     ONLY:SC_HF,SC_COHSEX,SC_EXX,SC_EXXC,WF_kind,WF_xc_functional
 use SC,            ONLY:SC_bands
 use QP_m,          ONLY:QP_nb,COHSEX_use_empties,QP_n_G_bands,QP_t
 use SC,            ONLY:SC_bands
 use global_XC,     ONLY:SC_HF,SC_COHSEX
 use collisions_IO, ONLY:V_kind,collisions_IO_alloc,collisions_IO_free,TEMP_COLLISIONS_v
 use X_m,           ONLY:X_t
 use frequency,     ONLY:w_samp
 use memory_m,      ONLY:mem_est
 use IO_m,          ONLY:io_control,OP_RD_CL,VERIFY,REP
 use fragments,     ONLY:fragment_status
 !
 implicit none
 type(levels)  :: E
 type(w_samp)  :: Xw(2) 
 type(bz_samp) :: k,Xk,q
 type(X_t)     :: X(4)
 !
 ! WorkSpace
 !
 integer, external ::eval_G_minus_G
 integer, external ::io_COLLISIONS
 integer           ::ID
 type(QP_t)        ::qp_dummy
 character(schlen) ::ch
 !
 call section('*','Extended collisions')
 !=======================================
 !
 ! QP states setup
 !
 ! When storing all collisions on file I need all SC bands
 !
 QP_nb=SC_bands
 call QP_state_table_setup(E)
 !
 ! Check for the Time Reversal Symmetry
 !
 if(i_time_rev==1) &
&  call error(" COLLISIONS not compatible with Time Reversal Symmetry! ")
 !
 ! No DB => EVAL
 !===============
 !
 call IO_and_Messaging_switch("+all_write_fragments")
 !
 allocate(fragment_status(QP_n_states))
 call mem_est("fragment_status",(/QP_n_states/),(/IP/))
 !
 if (l_HF_and_locXC) then
   !
   call section('+','Hartree-Fock')
   !================================
   !
   V_kind=SC_HF
   fragment_status=-1
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
   !
   if (io_COLLISIONS(0,ID)/=0) then
     allocate(TEMP_COLLISIONS_v(QP_nb,QP_nb,QP_nk))
     call mem_est("TEMP_COLLISIONS_v",(/size(TEMP_COLLISIONS_v)/),(/2*SP/))
     call collisions_IO_alloc()
     !
     call report_disk_usage('HF')
     !
     call XCo_Hartree_Fock(E,k,xk,q)
   endif
   !
 endif
 !
 if (l_cohsex) then
   !
   if (l_HF_and_locXC)      call section('=','COHSEX')
   if (.not.l_HF_and_locXC) call section('+','COHSEX')
   !                        ==========================
   !
   V_kind=SC_COHSEX
   fragment_status=-1
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
   !
   if (io_COLLISIONS(0,ID)/=0) then
     !
     X(2)%ng=eval_G_minus_G(X(2)%ng,0)
     QP_n_G_bands(2)=SC_bands
     COHSEX_use_empties=.TRUE.
     !
     if (.not.allocated(TEMP_COLLISIONS_v)) then
       allocate(TEMP_COLLISIONS_v(QP_nb,QP_nb,QP_nk))
       call mem_est("TEMP_COLLISIONS_v",(/size(TEMP_COLLISIONS_v)/),(/2*SP/))
       call collisions_IO_alloc()
     endif
     !
     call report_disk_usage('COHSEX')
     !
     call QP_ppa_cohsex(X(2),Xk,E,k,q,qp_dummy,Xw(2),-1) 
     !
   endif
   !
 endif
 !
 call IO_and_Messaging_switch("-all_write_fragments")
 !
 ! CLEAN
 !=======
 !
 deallocate(fragment_status)
 call mem_est("fragment_status")
 if (.not.allocated(TEMP_COLLISIONS_v)) return
 !
 call WF_free()
 call collisions_IO_free()
 deallocate(TEMP_COLLISIONS_v)
 call mem_est("TEMP_COLLISIONS_v")
 !
 contains
   !
   subroutine report_disk_usage(what)
     !
     use com, ONLY:msg
     !
     real(SP)     :: disk_space
     character(*) :: what
     !
     disk_space=real(QP_n_states*QP_nb**2*QP_nk*SP)/1.E9
     if (disk_space > 0.05) call msg('s','[COLLISIONS I/O] '//what//' Disk space required [Gb]:',disk_space)  
     !
   end subroutine
   !
end subroutine
