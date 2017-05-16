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
subroutine QP_load_DB(i_ACTION,band_range,en,k)
 !
 use drivers,       ONLY:l_chi,l_bss
 use QP_m,          ONLY:QP_t,QP_reset
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels
 use com,           ONLY:warning
 use QP_CTL_m,      ONLY:QP_action
 use IO_m,          ONLY:io_control,OP_RD_CL,DUMP,REP
 !
#include<memory.h>
 !
 type(levels)    ::en
 type(bz_samp)   ::k
 integer         ::i_ACTION,band_range(2)
 !
 ! Work Space
 !
 type(QP_t)        :: qp
 integer           ::io_err,ID
 integer, external ::io_QP_and_GF
 !
 call QP_reset(qp)
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2,3/),MODE=DUMP,ID=ID)
 io_err=io_QP_and_GF(trim(QP_action(i_ACTION)%CTL(1)%db),qp,ID)
 !
 if (io_err/=0) then
   call warning("I/O problems from QP database "//trim(QP_action(i_ACTION)%CTL(1)%db))
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
   if (qp%nb/=band_range(2).or.qp%nk/=k%nibz) then
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
   call QP_load_GFs(band_range,qp,en)
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
 call QP_apply_QP(i_ACTION,band_range,en,k,qp)
 !
end subroutine
