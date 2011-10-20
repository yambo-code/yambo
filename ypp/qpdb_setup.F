!
! Copyright (C) 2000-2011 C. Attaccalite and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine qpdb_setup(en,k)
 !
 use pars,          ONLY:SP,IP,DP
 use units,         ONLY:HA2EV
 use com,           ONLY:error,msg,warning
 use memory_m,      ONLY:mem_est
 use electrons,     ONLY:levels,n_sp_pol,spin_occ
 use parser_m,      ONLY:parser
 use IO_m,          ONLY:io_control,OP_WR_CL,REP
 use QP_m,          ONLY:QP_t,QP_reset,QP_state,QP_nb,QP_nk,QP_n_states
 use R_lattice,     ONLY:bz_samp
 implicit none
 type(levels), intent(in) :: en
 type(bz_samp),intent(in) :: k
 !
 ! Work Space
 !
 integer  :: io_QP,ID,icheck,i1,ib,ik,ic,is
 integer  :: k_rng_lines,ikrange(2),ibrange(2)
 integer, external :: ioQP
 real(SP), allocatable :: QP_r_corrections(:,:)
 logical,  allocatable :: QP_done(:,:,:,:)
 type(QP_t) :: qp
 !
 call QP_reset(qp)
 if(allocated(QP_state)) deallocate(QP_state)
 !
 call QP_state_table_setup(en)
 !
 qp%n_states=QP_n_states
 qp%nb      =QP_nb
 qp%nk      =QP_nk
 !
 allocate(qp%table(QP_n_states,3+n_sp_pol-1),qp%Z(QP_n_states),qp%E(QP_n_states),qp%E_bare(QP_n_states),qp%k(qp%nk,3))
 !
 k_rng_lines=1
 icheck=1
 do while(icheck/=0)
   if (allocated(QP_r_corrections)) deallocate(QP_r_corrections)
   allocate(QP_r_corrections(k_rng_lines,7))
   QP_r_corrections=0._SP
   call parser('QPkrange',QP_r_corrections)
   icheck=sum(QP_r_corrections(k_rng_lines,:))
   if (any(QP_r_corrections(k_rng_lines,1:4)<0)) icheck=0
   k_rng_lines=k_rng_lines+1
 enddo
 k_rng_lines=k_rng_lines-2
 !
 allocate(QP_done(QP_nb,QP_nb,QP_nk,n_sp_pol))
 QP_done=.FALSE.
 !
 ic=1
 is=1
 do i1=1,k_rng_lines
   ikrange(1:2)=int(QP_r_corrections(i1,1:2))
   ibrange(1:2)=int(QP_r_corrections(i1,3:4))
   !
   if(ikrange(2)>en%nk.or.ibrange(2)>en%nb) call error("Indices out of the range!")
   !
   do ik=ikrange(1),ikrange(2)
     do ib=ibrange(1),ibrange(2)
       qp%E(ic)      =cmplx(QP_r_corrections(i1,5)/HA2EV+en%E(ib,ik,1),QP_r_corrections(i1,6)/HA2EV)
       qp%E_bare(ic) =en%E(ib,ik,1)
       qp%Z(ic)      =QP_r_corrections(i1,7)
       qp%table(ic,1)=ib
       qp%table(ic,2)=ib
       qp%table(ic,3)=ik
       qp%table(ic,3+1:3+n_sp_pol-1)=is
       qp%k(qp%table(ic,3),:)       =k%pt(qp%table(ic,3),:)
       if(QP_done(ib,ib,ik,is)) call error("Duplicate quasi-particle indexes! ")
       QP_done(ib,ib,ik,is)=.TRUE.
       ic=ic+1   
     enddo
   enddo
 enddo
 !
 deallocate(QP_done)
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1,2,3/),COM=REP,ID=ID)
 io_QP=ioQP("QP",qp,ID)
 !
 deallocate(QP_r_corrections,qp%k,qp%table,qp%Z,qp%E_bare,qp%E)
 !
end subroutine
