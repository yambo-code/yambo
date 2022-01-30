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
 use pars,           ONLY:SP,cZERO,rZERO,schlen
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp
 use X_m,            ONLY:X_t
 use BS_solvers,     ONLY:BS_mat,BSS_eh_table,BS_H_dim,BSS_n_eig
 use BS,             ONLY:BS_H_dim,BS_bands,L_kind,BS_res_ares_n_mat,BS_K_dim
 use ELPH,           ONLY:ph_freqs_sq,elph_branches
 use EXCPH,          ONLY:BS_mat_in,BSS_eh_table_in,EXCPH_Gkkp,EXCPH_Gkkp_sq,&
&                         EXCPH_states,EXCPH_sum,EXCPH_kind
 use LIVE_t,         ONLY:live_timing
 use timing_m,       ONLY:timing
 use stderr,         ONLY:intc
 use com,            ONLY:msg
 !
#include<memory.h>
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 type(X_t)     ::X
 !
 ! Work Space
 !
 integer :: ID_BS,io_err
 complex(SP),allocatable :: BS_E(:),A_rot(:,:),Xi(:,:,:)
 integer,    allocatable :: BSS_eh_table_m1(:,:,:),k_plus_q_table(:,:)
 !
 call timing('EXCPH_gkkp',OPR='start')
 !
 call section('*','Excitonic gkkp')
 !======================================
 !
 call EXCPH_load_L(1,X,'check','Lin') ! At present the Lin can be only gamma iq=1
 !
 call k_build_up_BZ_tables(k)
 call k_build_up_BZ_tables(q)
 !
 call BSE_alloc()
 !
 call EXCPH_load_L(1,X,'load','Lin')
 !
 BS_mat_in=BS_mat(:,EXCPH_states(1):EXCPH_states(2))
 BSS_eh_table_in=BSS_eh_table(EXCPH_states(1):EXCPH_states(2),:)
 !
 call timing('EXCPH_gkkp',OPR='stop')
 !
 return
 !
 contains
   !
   subroutine BSE_alloc()
     !      
     ! pre-allocs...
     !
     YAMBO_ALLOC(BS_mat,(BS_H_dim,BSS_n_eig))
     YAMBO_ALLOC(BSS_eh_table,(BS_H_dim,3+n_sp_pol-1))
     YAMBO_ALLOC(BSS_eh_table_m1,(k%nbz,BS_bands(1):BS_bands(2),BS_bands(1):BS_bands(2)))
     YAMBO_ALLOC(k_plus_q_table,(k%nbz,q%nbz))
     !
     BS_mat=cZERO
     k_plus_q_table  = 0
     BSS_eh_table    = 0
     BSS_eh_table_m1 = 0
     !
   end subroutine BSE_alloc
   !
end subroutine
