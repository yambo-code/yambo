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
subroutine QP_eval_RHO_RHO(E,k,Xk,q)
 !
 use drivers,       ONLY:l_acfdt,l_rho_io,l_sc_run,l_real_time
 use pars,          ONLY:schlen
 use com,           ONLY:msg
 use units,         ONLY:HARTREE
 use electrons,     ONLY:levels,n_sp_pol,spin
 use stderr,        ONLY:intc,real2ch
 use R_lattice,     ONLY:bz_samp
 use par_proc_m,    ONLY:pp_redux_wait
 use IO_m,          ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP
 use QP_m,          ONLY:QP_table,QP_Vxc,QP_Vnl_xc,QP_n_states,Vxc_kind,Vnlxc_kind 
 use memory_m,      ONLY:mem_est
 use wave_func,     ONLY:WF_free
#if defined _SC
 use global_XC,     ONLY:SC_HF,SC_CHOSEX,SC_EXX,SC_EXXC,WF_kind,WF_xc_functional
#endif
#if defined _RT
 use SC,            ONLY:SC_bands
 use QP_m,          ONLY:QP_nb
#endif
 !
 implicit none
 type(levels) ::E       
 type(bz_samp)::k,Xk,q
 !
 ! WorkSpace
 !
 ! Reporting
 !
 integer           ::i_qp,ib,ik,ibp
 character(schlen) ::sec_mode_and_message(2),ch,ch_spin,nloc_fmt,loc_fmt
 !
 ! IO
 !
 integer           :: ID,io_err
 integer, external :: io_HF_and_locXC
 !
 ! Section
 !
 call section('*','Hartree-Fock extended oscillators')
 !
 ! QP states setup
 !
 ! When storing all oscillators on file I need all SC bands
 !
 QP_nb=SC_bands
 !
 call QP_state_table_setup(E)
 !
 ! HARTREE-FOCK
 !==============
 !
 call XCo_Hartree_Fock(E,k,xk,q)
 !
 call WF_free()
 !
end subroutine
