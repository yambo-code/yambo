!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine QP_load_DB(i_ACTION,bands,en,k)
 !
 use drivers,       ONLY:l_chi,l_bss
 use QP_m,          ONLY:QP_t,QP_reset
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels
 use QP_CTL_m,      ONLY:QP_action
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:OP_RD_CL,DUMP,REP
 !
#include<memory.h>
 !
 type(levels)    ::en
 type(bz_samp)   ::k
 integer         ::i_ACTION,bands(2)
 !
 ! Work Space
 !
 type(QP_t)        ::qp
 integer           ::io_err,ID
 integer, external ::io_QP_and_GF
 !
 call QP_reset(qp)
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2,3/),MODE=DUMP,ID=ID)
 io_err=io_QP_and_GF(trim(QP_action(i_ACTION)%CTL(1)%db),qp,ID)
 !
 if (io_err/=0) then
   call warning("I/O problems in reading QP database "//trim(QP_action(i_ACTION)%CTL(1)%db))
   QP_action(i_ACTION)%active=.FALSE.
   return
 endif
 !
#if defined _ELPH
 !
 ! The GF are implemented only in the linear response module (real and reciprocal space) and using 
 ! exactly the same number of bands/k-points
 !
 if (index(trim(QP_action(i_ACTION)%CTL(1)%db),'.G')/=0) then
   if (qp%nb/=bands(2).or.qp%nk/=k%nibz) then
     call warning("Wrong number of bands/k-points in "//trim(QP_action(i_ACTION)%CTL(1)%db))
     QP_action(i_ACTION)%active=.FALSE.
     return
   endif
   if (.not. ( l_chi.or.l_bss )) then 
     call warning("Green Functions not implemented for this runlevel. Skipping I/O of "//trim(QP_action(i_ACTION)%CTL(1)%db))
     QP_action(i_ACTION)%active=.FALSE.
     return
   endif
 endif
 !
 ! GF's cannot be interpolated
 !
 if (index(trim(QP_action(i_ACTION)%CTL(1)%db),'.G')/=0) then
   call QP_load_GFs(bands,qp,en)
   QP_action(i_ACTION)%active=.FALSE.
   return
 endif
 !
#endif
 !
 ! Extract a description
 !=======================
 call QP_DB_message( i_ACTION, qp ) 
 !
 ! QP application
 !================
 call QP_apply_QP(i_ACTION,bands,en,k,qp)
 !
end subroutine
