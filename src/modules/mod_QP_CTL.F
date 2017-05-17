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
 use pars,      ONLY:SP,schlen,lchlen,n_QP_actions_MAX
 use units,     ONLY:HA2EV
 use QP_m,      ONLY:QP_t
 use electrons, ONLY:n_sp_pol
 use global_XC, ONLY:QP_SE_NONE
 !
 implicit none
 !
 ! INPUT file parameters
 !=======================
 !
 !XfnQPdb= "none"                # [EXTQP Xd] Database
 !XfnQP_N= 1                     # [EXTQP Xd] Interpolation neighbours
 !% XfnQP_E
 ! 0.000000 | 1.000000 | 1.000000 |        # [EXTQP Xd] E parameters  (c/v) eV|adim|adim
 !%
 !XfnQP_Z= ( 1.000000 , 0.000000 )       # [EXTQP Xd] Z factor  (c/v)
 !XfnQP_Wv_E= 0.000000   eV      # [EXTQP Xd] W Energy reference  (valence)
 !% XfnQP_Wv
 ! 0.00     | 0.00     | 0.00     |        # [EXTQP Xd] W parameters  (valence) eV| 1|eV^-1
 !%
 !XfnQP_Wv_dos= 0.000000 eV      # [EXTQP Xd] W dos pre-factor  (valence)
 !XfnQP_Wc_E= 0.000000   eV      # [EXTQP Xd] W Energy reference  (conduction)
 !% XfnQP_Wc
 ! 0.00     | 0.00     | 0.00     |        # [EXTQP Xd] W parameters  (conduction) eV| 1 |eV^-1
 !%
 !XfnQP_Wc_dos= 0.000000 eV      # [EXTQP Xd] W dos pre-factor  (conduction)
 !
 integer                 :: QP_ctl_ID   ! 1(X) 2(K) 3(G) 
 character(schlen)       :: QP_ctl_what ! X,K,G
 character(schlen)       :: QP_ctl_db(3)
 character(schlen)       :: QP_ctl_msg_fmt
 character(lchlen)       :: QP_ctl_xc_STRING
 integer                 :: QP_ctl_interp_neigh(3)
 !
 ! Note that QP_ctl_E/W/Z are dimensioned *,2)
 ! to describe spin polarized systems.
 ! The second element of the last argument 
 ! is not used in non-collinear or scalar cases.
 !
 real(SP)                :: QP_ctl_E(3,3,2)
 real(SP)                :: QP_ctl_Wc_E_ref(3,2)
 real(SP)                :: QP_ctl_Wv_E_ref(3,2)
 real(SP)                :: QP_ctl_Wc(3,3,2)
 real(SP)                :: QP_ctl_Wc_dos(3,2)
 real(SP)                :: QP_ctl_Wv(3,3,2)
 real(SP)                :: QP_ctl_Wv_dos(3,2)
 complex(SP)             :: QP_ctl_Z(3,2)
 !
 ! Internal QP_ctl's (derived from a QP type and/or dumped from the input)
 !===================
 type QP_ctl_t
   !
   ! Convention is first c then v
   !
   integer    :: interp_neigh
   real(SP)   :: db_scissor
   real(SP)   :: fit_scissor
   real(SP)   :: E(4)
   real(SP)   :: W(6)
   real(SP)   :: W_dos(2)
   real(SP)   :: W_E_ref(2)
   real(SP)   :: E_err(2)
   real(SP)   :: W_err(2)
   real(SP)   :: W_dos_err(2)
   complex(SP):: Z
   character(schlen):: db
 end type
 !
 ! "ACTION" type (can be a fit/interpolation/QP/...)
 !===============
 type QP_action_t
   integer              :: kind
   logical              :: plan(6) ! E W Z ukI ueI dos
   logical              :: active
   character(schlen)    :: what 
   character(lchlen)    :: desc 
   type(QP_ctl_t)       :: CTL(2) ! Spin polarizations
   integer, allocatable :: STATES_done(:,:,:)
 end type
 !
 ! QP DBs load and apply 
 !
 integer                :: n_QP_actions
 type(QP_action_t)      :: QP_action(n_QP_actions_MAX)
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
   subroutine QP_CTL_fill(iA,i_spin,ID,CBm,VBM)
     use pars, ONLY:rZERO
     integer  :: iA,i_spin,ID
     real(SP) :: CBm,VBM
     !
     QP_action(iA)%active=.TRUE.
     !
     QP_action(iA)%CTL(i_spin)%E=(/QP_ctl_E(ID,1,i_spin)/HA2EV,&
&                                  QP_ctl_E(ID,2,i_spin),rZERO,QP_ctl_E(ID,3,i_spin)/)
     QP_action(iA)%CTL(i_spin)%W(1:3)=(/QP_ctl_Wc(ID,1,i_spin)/HA2EV,&
&                                       QP_ctl_Wc(ID,2,i_spin)/HA2EV,QP_ctl_Wc(ID,3,i_spin)*HA2EV/)
     QP_action(iA)%CTL(i_spin)%W_dos(1)=QP_ctl_Wc_dos(ID,i_spin)/HA2EV
     QP_action(iA)%CTL(i_spin)%W_E_ref(:)=(/QP_ctl_Wc_E_ref(ID,i_spin)+CBm,&
&                                           QP_ctl_Wv_E_ref(ID,i_spin)+VBM/)
     QP_action(iA)%CTL(i_spin)%W(4:6)=(/QP_ctl_Wv(ID,1,i_spin)/HA2EV,&
&                                       QP_ctl_Wv(ID,2,i_spin)/HA2EV,QP_ctl_Wv(ID,3,i_spin)*HA2EV/)
     QP_action(iA)%CTL(i_spin)%W_dos(2)=QP_ctl_Wv_dos(ID,i_spin)/HA2EV
     QP_action(iA)%CTL(i_spin)%Z=QP_ctl_Z(ID,i_spin)
     !
   end subroutine
   !
   subroutine QP_actions_reset
#include<memory.h>
     integer :: i_a,i_s
     n_QP_actions=0
     do i_a=1,n_QP_actions_MAX
       QP_action(i_a)%plan=.FALSE.
       QP_action(i_a)%active=.FALSE.
       QP_action(i_a)%what=" "
       QP_action(i_a)%desc=" "
       QP_action(i_a)%kind=QP_SE_NONE
       call QP_ctl_reset(QP_action(i_a)%CTL(1))
       call QP_ctl_reset(QP_action(i_a)%CTL(2))
       YAMBO_FREE(QP_action(i_a)%STATES_done)
     enddo
   end subroutine
   !
   subroutine QP_ctl_copy(ctl1,ctl2)
     type(QP_ctl_t)::ctl1,ctl2
     ctl2%interp_neigh=ctl1%interp_neigh
     ctl2%db=ctl1%db  
     ctl2%E=ctl1%E  
     ctl2%W=ctl1%W  
     ctl2%W_dos=ctl1%W_dos
     ctl2%Z=ctl1%Z  
     ctl2%E_err=ctl1%E_err  
     ctl2%W_err=ctl1%W_err  
     ctl2%W_dos_err=ctl1%W_dos_err  
     ctl2%W_E_ref=ctl1%W_E_ref  
     ctl2%db_scissor=ctl1%db_scissor  
     ctl2%fit_scissor=ctl1%fit_scissor  
   end subroutine
   !
   subroutine QP_ctl_reset(QP_ctl)
     type(QP_ctl_t)::QP_ctl
     QP_ctl%interp_neigh=1
     QP_ctl%db="none"
     QP_ctl%E=(/0.,1.,0.,1./)
     QP_ctl%W=0.
     QP_ctl%W_dos=0.
     QP_ctl%Z=(1.,0.)
     QP_ctl%E_err=0.
     QP_ctl%W_err=0.
     QP_ctl%W_dos_err=0.
     QP_ctl%W_E_ref=0.
     QP_ctl%db_scissor=0.
     QP_ctl%fit_scissor=0.
   end subroutine
   !
end module
