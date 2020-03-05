!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine fix_QP_DBs(k,k_save,kpoints_map,action_kind)
 !
 use pars,                ONLY:lchlen
 use QP_m,                ONLY:QP_t,QP_reset
 use stderr,              ONLY:intc
 use vec_operate,         ONLY:v_is_zero
 use com,                 ONLY:core_io_path
 use IO_m,                ONLY:io_control,OP_WR_CL,NONE,OP_RD_CL,DUMP
 use electrons,           ONLY:n_sp_pol
 use R_lattice,           ONLY:bz_samp
 !
#include<memory.h>
 !
 type(bz_samp),intent(in) :: k
 type(bz_samp),intent(in) :: k_save
 integer,intent(in)       :: kpoints_map(2,k%nibz)
 !
 integer,intent(in)       :: action_kind
 !
 ! Work space
 !
 type(QP_t) :: qp,qp_expanded
 !
 character(lchlen)     :: core_io_path_save
 integer               :: ID,ik,ik_old,ik_old_prev,ib,i1,i2,i_sp,i_err,io_QP_and_GF_err,i3(k_save%nibz)
 !
 ! External functions
 !
 integer, external :: io_QP_and_GF
 !
 !
 if(action_kind==2) return
 !
 core_io_path_save=core_io_path
 !
 ! Check presence of QP DBs
 !
 call QP_reset(qp)
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2,3/),COM=NONE,MODE=DUMP,ID=ID)
 io_QP_and_GF_err=io_QP_and_GF("QP",qp,ID)
 !  
 if(io_QP_and_GF_err==-1) return
 !
 if(qp%nk/=k_save%nibz) then
   call warning(" Found ndb.QP, but number of kpts does not match kpoints in the old IBZ")
   return
   i_err=0
   do ik=1,k_save%nibz
     if(v_is_zero(k_save%pt(ik,:)-qp%k(ik,:)) .and. &
&       abs(k_save%weights(ik)-qp%wk(ik))< 1.E-5 ) cycle
     call warning(" K point "//trim(intc(ik))//" did not match")
     return
   enddo
 endif 
 !
 call section('=',"QP energies")
 !===============================
 !
 call QP_reset(qp_expanded)
 !
 qp_expanded%description=qp%description
 qp_expanded%n_descs    =qp%n_descs
 !
 qp_expanded%nk      =k%nibz
 qp_expanded%nb      =qp%nb
 qp_expanded%n_states=(qp%n_states/qp%nk)*k%nibz
 !
 YAMBO_ALLOC(qp_expanded%k,(k%nibz,3))
 YAMBO_ALLOC(qp_expanded%wk,(k%nibz))
 YAMBO_ALLOC(qp_expanded%table,(qp_expanded%n_states,3+n_sp_pol-1))
 YAMBO_ALLOC(qp_expanded%E_bare,(qp_expanded%n_states))
 YAMBO_ALLOC(qp_expanded%E,(qp_expanded%n_states))
 YAMBO_ALLOC(qp_expanded%Z,(qp_expanded%n_states))
 !
 i1=0
 ik_old_prev=0
 i3(1)=0
 !
 do ik=1,qp_expanded%nk
   !
   i2=0
   ik_old=kpoints_map(1,ik)
   !
   do ib=1,qp_expanded%nb
     if(ib<qp%table(i3(ik_old)+1,1)) cycle
     do i_sp=1,n_sp_pol
       !
       i1=i1+1
       i2=i2+1
       !
       qp_expanded%table(i1,1)=qp%table(i2+i3(ik_old),1)
       qp_expanded%table(i1,2)=qp%table(i2+i3(ik_old),2)
       qp_expanded%table(i1,3)=ik
       if (n_sp_pol==2) qp_expanded%table(i1,4)=qp%table(i2+i3(ik_old),4)
       !
       qp_expanded%E_bare(i1)=qp%E_bare(i2+i3(ik_old))
       qp_expanded%E(i1)     =qp%E(i2+i3(ik_old))
       qp_expanded%Z(i1)     =qp%Z(i2+i3(ik_old))
       !
       !
     enddo
   enddo
   if(ik_old<qp%nk) i3(ik_old+1)=i2+i3(ik_old)
 enddo
 !
 qp_expanded%k  = k%pt
 qp_expanded%wk = k%weights
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1,2,3/),COM=NONE,MODE=DUMP,ID=ID)
 io_QP_and_GF_err=io_QP_and_GF("QP",qp_expanded,ID)
 !
 core_io_path=core_io_path_save
 !
end subroutine
