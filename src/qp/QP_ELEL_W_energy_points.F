!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine QP_ELEL_W_energy_points(nW,W,E,k,q,qp)
 !
 use pars,          ONLY:rZERO
 use drivers,       ONLY:l_life
 use frequency,     ONLY:w_samp,coarse_grid_index,coarse_grid_N
 use QP_m,          ONLY:QP_n_W_freqs,QP_n_W_freqs_redux,QP_W_table,QP_dSc_steps,QP_t,&
&                        QP_G_damp,QP_dSc_delta,QP_Sc_steps,QP_solver
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels
 !
#include<memory.h>
 !
 integer      ::nW
 type(w_samp) ::W(nW)
 type(levels) ::E
 type(bz_samp)::k,q
 type(QP_t)   ::qp
 !
 ! Workspace
 integer             ::iq,i_trans,i_qp,iw
 integer, external   ::QP_life_transitions
 !
 if (l_life) then
   QP_n_W_freqs=QP_life_transitions(-1,E,k,q,W(1))
   YAMBO_ALLOC(QP_W_table,(q%nibz,QP_n_W_freqs))
   QP_W_table=0
   QP_n_W_freqs_redux=-1
   do iq=1,q%nibz
     i_trans=QP_life_transitions(iq,E,k,q,W(iq))
     QP_W_table(iq,:size(coarse_grid_index))=coarse_grid_index(:)
     QP_n_W_freqs_redux=max(QP_n_W_freqs_redux,coarse_grid_N)
     YAMBO_FREE(coarse_grid_index)
   enddo
 else
   ! Sc Energy points (1 type each QP state !)
   !-------------------------------------------
   if (trim(QP_solver)=='n') then
     do i_qp=1,qp%n_states
       W(i_qp)%n_freqs=QP_dSc_steps
       YAMBO_ALLOC(W(i_qp)%p,(W(i_qp)%n_freqs))
       do iw=1,QP_dSc_steps
         W(i_qp)%p(iw)=qp%E_bare(i_qp)+(iw-1)*QP_dSc_delta
       enddo
     enddo
   else if (trim(QP_solver)=='g') then
     !
     ! I need to put to 0. the G damping embodied in QP_W2Sc
     QP_G_damp=rZERO
     !
     do i_qp=1,qp%n_states
       W(i_qp)%n_freqs =QP_Sc_steps
       call FREQUENCIES_Green_Function(i_qp,W(i_qp),E%E,.FALSE.)
       QP_Sc_steps=W(i_qp)%n_freqs
     enddo
   endif
   !
 endif
 !
end subroutine
