!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine QP_DB_expand( k )
 !
 use pars,          ONLY:schlen
 use R_lattice,     ONLY:bz_samp_reset,bz_samp
 use YPPm,          ONLY:QP_DB_to_expand
 use IO_m,          ONLY:io_control,OP_WR_CL,DUMP,OP_RD_CL,LOG
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
