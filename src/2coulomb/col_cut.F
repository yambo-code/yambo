!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine col_cut()
!====================
!
 use pars,          ONLY:SP
 use memory_m,      ONLY:mem_est
 use X_m,           ONLY:q0_defmod
 use drivers,       ONLY:l_rim,l_col_cut
 use parser_m,      ONLY:parser
 use stderr,        ONLY:string_pack
 use com,           ONLY:msg,gen_fmt
 use vec_operate,   ONLY:iku_v_module,sort
 use D_lattice,     ONLY:DL_vol
 use R_lattice,     ONLY:d3q_factor,RIM_is_diagonal,RIM_qpg,bare_qpg,&
&                        nqibz,g_vec,qpt,nqbz,RIM_ng,RIM_n_rand_pts
 use wave_func,     ONLY:wf_ng
 use IO_m,          ONLY:io_control,OP_RD_CL,OP_WR_CL,REP,VERIFY
 implicit none

 call section('*','Coloumb potential CutOff')

 end subroutine
!
