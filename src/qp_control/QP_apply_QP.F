!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine QP_apply_QP(i_ACTION,bands,en,k,qp)
 !
 use pars,          ONLY:rZERO
 use QP_m,          ONLY:QP_t
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels,n_sp_pol
 use QP_CTL_m,      ONLY:QP_action
 !
#include<memory.h>
 !
 type(levels)    ::en
 type(QP_t)      ::qp
 type(bz_samp)   ::k
 integer         ::i_ACTION,bands(2)
 !
 ! Work Space
 integer :: i_spin
 !
 ! Allocations
 !
 if (QP_action(i_ACTION)%plan(2).and..not.allocated(en%W)) then
   YAMBO_ALLOC(en%W,(en%nb,en%nk,n_sp_pol))
   en%W=rZERO
 endif
 if (QP_action(i_ACTION)%plan(3).and..not.allocated(en%Z)) then
   YAMBO_ALLOC(en%Z,(en%nb,en%nk,n_sp_pol))
   en%Z=rZERO
 endif
 if (.not.allocated(en%Eo)) then
   YAMBO_ALLOC(en%Eo,(en%nb,en%nk,n_sp_pol))
   en%Eo=en%E
 endif
 YAMBO_ALLOC(QP_action(i_ACTION)%STATES_done,( bands(1):bands(2),en%nk,n_sp_pol ))
 QP_action(i_ACTION)%STATES_done=0
 !
 ! Interpolation
 !
 call QP_apply_interpolate_QP(i_ACTION,bands,en,k,qp)
 !
 ! FIT
 !
 if (QP_action(i_ACTION)%plan(1)) call QP_fit_energies(i_ACTION,bands,qp)
 if (QP_action(i_ACTION)%plan(2)) call QP_fit_widths(i_ACTION,bands,en,k,qp)
 if (QP_action(i_ACTION)%plan(3)) then
   do i_spin=1,n_sp_pol
     QP_action(i_ACTION)%CTL(i_spin)%Z=sum(qp%Z(:))/qp%n_states
   enddo
 endif
 !
 ! FIT report
 !
 call QP_fit_report(i_ACTION,bands,en,k,qp)
 !
end subroutine
