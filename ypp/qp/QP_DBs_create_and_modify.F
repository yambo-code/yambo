!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA AM
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
subroutine QP_DBs_create_and_modify(en,k)
 !
 use pars,          ONLY:SP
 use units,         ONLY:HA2EV
 use electrons,     ONLY:levels,n_sp_pol
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:OP_WR_CL,REP
 use QP_m,          ONLY:QP_t,QP_reset,QP_state,QP_nb,QP_nk,QP_n_states,QP_alloc
 use R_lattice,     ONLY:bz_samp
 use YPPm,          ONLY:QP_user_corrections
 !
#include<memory.h>
 !
 type(levels), intent(in) :: en
 type(bz_samp),intent(in) :: k
 !
 ! Work Space
 !
 integer  :: io_QP_and_GF_err,ID,i_u,ib,ik,ic,is,ik_qp,nk_lim(2),nb_lim(2)
 integer, external    :: io_QP_and_GF
 logical, allocatable :: QP_done(:,:,:,:)
 type(QP_t) :: qp
 !
 call section("=",'QP databases creation')
 !========================================
 !
 nk_lim=(/minval(QP_user_corrections(:,2)),maxval(QP_user_corrections(:,3))/)
 nb_lim=(/minval(QP_user_corrections(:,4)),maxval(QP_user_corrections(:,5))/)
 !
 ! ...resets
 !
 call QP_reset(qp)
 YAMBO_FREE(QP_state)
 YAMBO_ALLOC(QP_state,(en%nb,en%nk))
 QP_state=.FALSE.
 QP_nb=-1
 QP_nk=-1
 !
 ! ...mark the states to correct
 !
 do i_u=1,size(QP_user_corrections,1)
   do ik=int(QP_user_corrections(i_u,2)),int(QP_user_corrections(i_u,3))
     do ib=int(QP_user_corrections(i_u,4)),int(QP_user_corrections(i_u,5))
       QP_state(ib,ik)=.TRUE.
     enddo
   enddo
 enddo
 !
 QP_nk=nk_lim(2)
 QP_nb=nb_lim(2)
 !
 ! ... now turn QP_state into QP_table
 !
 call QP_state_table_setup(en)
 !
 QP_nk=nk_lim(2)-nk_lim(1)+1
 QP_nb=nb_lim(2)-nb_lim(1)+1
 !
 qp%n_states=QP_n_states
 qp%nb      =QP_nb
 qp%nk      =QP_nk
 ! 
 call QP_alloc(qp)
 !
 YAMBO_ALLOC(QP_done,(nb_lim(1):nb_lim(2),nb_lim(1):nb_lim(2),nk_lim(1):nk_lim(2),n_sp_pol))
 QP_done=.FALSE.
 !
 ! ...let's fill now
 !
 ic=1
 do i_u=1,size(QP_user_corrections,1)
   !
   if (QP_user_corrections(i_u,8)<epsilon(1._SP)) QP_user_corrections(i_u,8)=1._SP
   !
   do is=int(QP_user_corrections(i_u,1)),int(QP_user_corrections(i_u,1))
     do ik=int(QP_user_corrections(i_u,2)),int(QP_user_corrections(i_u,3))
       ik_qp=ik-QP_user_corrections(i_u,2)+1
       do ib=int(QP_user_corrections(i_u,4)),int(QP_user_corrections(i_u,5))
         !
         qp%E_bare(ic) =en%E(ib,ik,is)
         qp%E(ic)      =cmplx(QP_user_corrections(i_u,6)/HA2EV+en%E(ib,ik,is),QP_user_corrections(i_u,7)/HA2EV,kind=SP)
         qp%Z(ic)      =cmplx(QP_user_corrections(i_u,8),0._SP,kind=SP)
         qp%table(ic,1)=ib
         qp%table(ic,2)=ib
         qp%table(ic,3)=ik
         if(n_sp_pol==2) qp%table(ic,4)=is
         qp%k(ik_qp,:)  =k%pt(ik,:)
         !
         if(QP_done(ib,ib,ik,is)) call error("Duplicated quasi-particle index found")
         !
         QP_done(ib,ib,ik,is)=.TRUE.
         ic=ic+1   
         !
       enddo
     enddo
   enddo
 enddo
 !
 YAMBO_FREE(QP_done)
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1,2,3/),COM=REP,ID=ID)
 io_QP_and_GF_err=io_QP_and_GF("QP",qp,ID)
 !
 YAMBO_FREE(QP_user_corrections)
 YAMBO_FREE(qp%k)
 YAMBO_FREE(qp%table)
 YAMBO_FREE(qp%Z)
 YAMBO_FREE(qp%E_bare)
 YAMBO_FREE(qp%E)
 !
end subroutine
