!
! Copyright (C) 2000-2009 A. Marini and the YAMBO team 
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
 use drivers,       ONLY:l_HF_and_locXC,l_chosex
 use pars,          ONLY:schlen,SP,IP
 use electrons,     ONLY:levels,spin
 use R_lattice,     ONLY:bz_samp
 use QP_m,          ONLY:QP_nb,QP_n_states,QP_nk
 use wave_func,     ONLY:WF_free
 use global_XC,     ONLY:SC_HF,SC_CHOSEX,SC_EXX,SC_EXXC,WF_kind,WF_xc_functional
 use SC,            ONLY:SC_bands
 use QP_m,          ONLY:QP_nb,CHOSEX_use_empties,QP_n_G_bands,QP_t
 use SC,            ONLY:SC_bands
 use global_XC,     ONLY:SC_HF,SC_CHOSEX
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
 ! I/O check
 !===========
 !
 if (l_HF_and_locXC) V_kind=SC_HF
 if (l_chosex)       V_kind=SC_CHOSEX 
 !
 allocate(fragment_status(QP_n_states))
 call mem_est("fragment_status",(/QP_n_states/),(/IP/))
 fragment_status=-1
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
 if (io_COLLISIONS(0,ID)==0) then
   deallocate(fragment_status)
   return
 endif
 !
 ! Allocations
 !=============
 !
 call collisions_IO_alloc()
 !
 allocate(TEMP_COLLISIONS_v(QP_nb,QP_nb,QP_nk))
 call mem_est("TEMP_COLLISIONS_v",(/size(TEMP_COLLISIONS_v)/),(/2*SP/))
 !
 call IO_and_Messaging_switch("all_write_fragments")
 !
 ! No DB => EVAL
 !===============
 !
 call IO_and_Messaging_switch("all_write_fragments")
 !
 if (l_HF_and_locXC) then
   !
   call section('+','Hartree-Fock')
   !================================
   !
   call XCo_Hartree_Fock(E,k,xk,q)
   !
 else if (l_chosex) then
   !
   call section('+','CHOSEX')
   !==========================
   !
   X(2)%ng=eval_G_minus_G(X(2)%ng,0)
   QP_n_G_bands(2)=SC_bands
   CHOSEX_use_empties=.FALSE.
   !
   call QP_ppa_chosex(X(2),Xk,E,k,q,qp_dummy,Xw(2),-1) 
   !
 endif
 !
 call IO_and_Messaging_switch(" ")
 !
 ! CLEAN
 !=======
 !
 call WF_free()
 call collisions_IO_free()
 deallocate(TEMP_COLLISIONS_v,fragment_status)
 call mem_est("TEMP_COLLISIONS_v fragment_status")
 !
end subroutine
