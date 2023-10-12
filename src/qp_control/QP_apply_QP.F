!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
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
 integer :: i_spin,ik,ib
 logical :: FIT_me
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
 if (.not.QP_action(i_ACTION)%plan(7)) call QP_apply_interpolate_QP(i_ACTION,bands,en,k,qp)
 !
 ! FIT?
 !
 FIT_me=.FALSE.
 !
 do ib=bands(1),bands(2)
   do ik=1,en%nk
     do i_spin=1,n_sp_pol
       if (allocated(QP_action(i_ACTION)%STATES_done)) then
         if (QP_action(i_ACTION)%STATES_done(ib,ik,i_spin)==1) cycle
       endif
       FIT_me=.TRUE.
     enddo
   enddo
 enddo
 if (.not.FIT_me) return
 !
 call section('p',"FIT on"//trim(QP_action(i_ACTION)%what) )
 !
 ! YES!
 !
 if (QP_action(i_ACTION)%plan(1)) call QP_fit_energies(i_ACTION,bands,en,qp)
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
