!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine QP_DB_expand( k )
 !
 use pars,          ONLY:schlen
 use R_lattice,     ONLY:bz_samp_reset,bz_samp
 use YPPm,          ONLY:QP_DB_to_expand
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:OP_WR_CL,DUMP,OP_RD_CL,LOG
 use QP_m,          ONLY:QP_t,QP_reset
 !
#include<memory.h>
 !
 type(bz_samp)           :: k
 !
 ! Work Space
 !
 integer                 :: i_err,ID,ik
 type(QP_t)              :: qp,qp_expanded
 type(bz_samp)           :: k_qp
 integer,    external    :: io_QP_and_GF
 character(schlen)       :: QP_expanded_name
 !
 call section("=",'QP database expansion in the full BZ')
 !=======================================================
 !
 ! Load
 !------
 call QP_reset(qp)
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2,3/),COM=LOG,MODE=DUMP,ID=ID)
 i_err=io_QP_and_GF(trim(QP_DB_to_expand),qp,ID)
 if (i_err/=0) then
   call warning("QP DB "//trim(QP_DB_to_expand)//" not found")
   return
 endif
 !
 ! Expand the QP k-points
 !-------------------------
 call bz_samp_reset(k_qp)
 k_qp%nibz=qp%nk
 YAMBO_ALLOC(k_qp%pt,(k_qp%nibz,3))
 do ik=1,k_qp%nibz
   k_qp%pt(ik,:)=qp%k(ik,:)
 enddo
 call k_expand(k_qp)
 !
 ! Alloc & Expand
 !----------------
 call QP_reset(qp_expanded)
 call QP_expand(k,qp,qp_expanded)
 !
 ! Output
 !--------
 QP_expanded_name='QP_expanded'
 call io_control(ACTION=OP_WR_CL,COM=LOG,SEC=(/1,2,3/),ID=ID)
 i_err=io_QP_and_GF(trim(QP_expanded_name),qp_expanded,ID)
 !
end subroutine
