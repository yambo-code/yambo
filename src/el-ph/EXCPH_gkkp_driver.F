!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM FP
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
! --- Theory ---
! (5) Figure out rotations/symmetry of exciton eigenvectors and EPC m.e.
! - FP
!
subroutine EXCPH_gkkp_driver(E,k,q,X)
 !
 use pars,           ONLY:SP,cZERO,rZERO
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp
 use BS_solvers,     ONLY:io_BSS_diago,BSS_n_eig,BS_mat,BSS_eh_table,BSS_free,BSS_write_eig_2_db
 use IO_m,           ONLY:NONE,OP_APP_CL,OP_RD_CL,DUMP,OP_RD,RD_CL_IF_END,OP_WR_CL,deliver_IO_error_message
 use IO_int,         ONLY:io_control
 use X_m,            ONLY:X_t
 use BS,             ONLY:BS_H_dim,BS_bands,L_kind,BS_res_ares_n_mat
 use ELPH,           ONLY:ph_freqs_sq,elph_branches
 use EXCPH,          ONLY:BS_mat_in,BSS_eh_table_in,EXCPH_Gkkp,EXCPH_Gkkp_sq,&
&                         EXCPH_states,EXCPH_sum,EXCPH_kind
 use LIVE_t,         ONLY:live_timing
 !
#include<memory.h>
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 type(X_t)     ::X
 !
 ! Work Space
 !
 call section('-','Excitonic Gkkp matrix elements')
 !======================================
 !
end subroutine
