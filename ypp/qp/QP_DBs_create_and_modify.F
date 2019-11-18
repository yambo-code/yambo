!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use IO_m,          ONLY:io_control,OP_WR_CL,REP
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
 integer  :: io_QP_and_GF_err,ID,i_u,ib,ik,ic,is
 integer, external    :: io_QP_and_GF
 logical, allocatable :: QP_done(:,:,:,:)
 type(QP_t) :: qp
 !
 call section("=",'QP databases creation')
 !========================================
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
   do is=1,n_sp_pol
     do ik=int(QP_user_corrections(i_u,1)),int(QP_user_corrections(i_u,2))
       do ib=int(QP_user_corrections(i_u,3)),int(QP_user_corrections(i_u,4))
         QP_state(ib,ik)=.TRUE.
         QP_nb=max(QP_nb,ib)
         QP_nk=max(QP_nk,ib)
       enddo
     enddo
   enddo
 enddo
 !
 ! ... not turn QP_state into QP_table
 !
 call QP_state_table_setup(en)
 !
 qp%n_states=QP_n_states
 qp%nb      =QP_nb
 qp%nk      =QP_nk
 ! 
 call QP_alloc(qp)
 !
 YAMBO_ALLOC(QP_done,(QP_nb,QP_nb,QP_nk,n_sp_pol))
 QP_done=.FALSE.
 !
 ! ...let's fill now
 !
 ic=1
 do i_u=1,size(QP_user_corrections,1)
   !
   if (QP_user_corrections(i_u,7)<epsilon(1._SP)) QP_user_corrections(i_u,7)=1.
   !
   do is=1,n_sp_pol
     do ik=int(QP_user_corrections(i_u,1)),int(QP_user_corrections(i_u,2))
       do ib=int(QP_user_corrections(i_u,3)),int(QP_user_corrections(i_u,4))
         !
         qp%E_bare(ic) =en%E(ib,ik,is)
         qp%E(ic)      =cmplx(QP_user_corrections(i_u,5)/HA2EV+en%E(ib,ik,is),QP_user_corrections(i_u,6)/HA2EV,kind=SP)
         qp%Z(ic)      =cmplx(QP_user_corrections(i_u,7),0.,kind=SP)
         qp%table(ic,1)=ib
         qp%table(ic,2)=ib
         qp%table(ic,3)=ik
         if(n_sp_pol==2) qp%table(ic,4)=is
         qp%k(qp%table(ic,3),:)  =k%pt(qp%table(ic,3),:)
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
