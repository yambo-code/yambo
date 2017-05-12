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
module QP_CTL_m
 !
 use pars,  ONLY:SP,schlen,lchlen,n_QP_actions_MAX
 use units, ONLY:HA2EV
 use QP_m,  ONLY:QP_T
 !
 implicit none
 !
 ! QP Pars
 !
 !?fnQP_db= " E W Z < db-pp.qp "     # [?] QP database.
 !% ?fnQP_E
 !  0 |  0 | 0 |               # [?] QP parameters (E).
 !% 
 !% ?fnQP_W
 !  0 |  0 | 0 | 0 |           # [?] QP parameters (W).
 !%
 !?fbQP_Z= (  0.00000 ,  0.00000 )  # [?] QP parameters (Z).
 !  
 ! 1(X) 2(K) 3(G) 
 !
 type QP_ctl_t
   !
   ! Convention is first c then v
   !
   integer    :: interp_neigh
   real(SP)   :: db_scissor
   real(SP)   :: fit_scissor
   real(SP)   :: E(4)
   real(SP)   :: W(6)
   real(SP)   :: W_E_ref(2)
   real(SP)   :: E_err(2)
   real(SP)   :: W_err(6)
   complex(SP):: Z
   character(schlen):: db
   character(schlen):: short_descr
   !
 end type
 !
 type QP_apply_t
   character(lchlen)    :: name
   character(schlen)    :: what
   type(QP_ctl_t)       :: CTL
   integer, allocatable :: STATES_done(:,:,:,:)
 end type
 !
 ! QP DBs load and apply 
 !
 integer                :: n_QP_actions
 type(QP_apply_t)       :: QP_action(n_QP_actions_MAX)
 !
 interface
   !
   subroutine QP_apply(band_range,en,k,object,msg_fmt,qp_impose,QP_ctl_impose)
     !
     use electrons,     ONLY:levels,n_sp_pol
     use R_lattice,     ONLY:bz_samp
     use QP_m,          ONLY:QP_t
     !
     import
     !
     integer       ::band_range(2)
     type(levels)  ::en
     type(bz_samp) ::k
     character(*)  ::msg_fmt
     character(1)  ::object
     type(QP_t),       optional::qp_impose
     type(QP_ctl_t),   optional::QP_ctl_impose(n_sp_pol)
     !
   end subroutine QP_apply
   !
 end interface
 !
 contains
   !
   subroutine reset_QP_ctl(QP_ctl)
     type(QP_ctl_t)::QP_ctl
     QP_ctl%db="none"
     QP_ctl%E=(/0.,1.,0.,1./)
     QP_ctl%W=0.
     QP_ctl%Z=(1.,0.)
     QP_ctl%E_err=0.
     QP_ctl%W_err=0.
     QP_ctl%W_E_ref=0.
     QP_ctl%db_scissor=0.
     QP_ctl%fit_scissor=0.
   end subroutine
   !
end module
