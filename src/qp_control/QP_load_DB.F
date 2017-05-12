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
integer function QP_load_DB(band_range,en,k,QP_ctl_from_DB,msg_fmt,DB_corrected)
 !
 use drivers,       ONLY:l_chi,l_bss
 use pars,          ONLY:SP,schlen,lchlen,rZERO,rONE,n_QP_actions_MAX
 use stderr,        ONLY:string_split,string_pack
 use QP_m,          ONLY:QP_t,QP_reset
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels,n_sp_pol
 use com,           ONLY:msg,warning
 use QP_CTL_m,      ONLY:QP_ctl_t,n_QP_actions,QP_action,n_QP_actions
 use IO_m,          ONLY:io_control,OP_RD_CL,DUMP,REP,io_reset
 !
#include<memory.h>
 !
 type(levels)    ::en
 type(QP_ctl_t)  ::QP_ctl_from_DB(n_sp_pol)
 type(bz_samp)   ::k
 integer         ::band_range(2),DB_corrected(en%nb,en%nk,n_sp_pol)
 character(*)    ::msg_fmt
 !
 ! Work Space
 !
 integer          :: i1,i2,i_db,i_msg
 logical          :: GF_db,plan_EWZ_plus_Ik_IE(5) !  = ( E W Z INTERPOLATION/k INTERPOLATION/E )
 integer,parameter:: n_pieces=20
 character(lchlen):: QP_db_name
 character(schlen):: db_piece(n_pieces)
 type(QP_t)       :: qp
 !
 ! QP I/O
 !
 integer           ::io_err,ID
 integer, external ::io_QP_and_GF
 !
 call string_split(QP_ctl_from_DB(1)%db,db_piece)
 !
 ! Check the number of potential DB's and allocate
 !
 n_QP_actions=0
 !
 do i1=1,n_pieces
   !
   if (trim(db_piece(i1))=="<") then
     !
     plan_EWZ_plus_Ik_IE=.FALSE.
     !
     if (len_trim(db_piece(i1+1))==0) cycle
     !
     n_QP_actions=n_QP_actions+1
     !
     QP_db_name=db_piece(i1+1)
     !
     call QP_reset(qp)
     !
     do i2=min(i1-3,1),i1-1
       plan_EWZ_plus_Ik_IE(1)=any((/trim(db_piece(i2))=="E",trim(db_piece(i2))=="e"/))
       plan_EWZ_plus_Ik_IE(2)=any((/trim(db_piece(i2))=="W",trim(db_piece(i2))=="w"/))
       plan_EWZ_plus_Ik_IE(3)=any((/trim(db_piece(i2))=="Z",trim(db_piece(i2))=="z"/))
       GF_db=any((/trim(db_piece(i2))=="G",trim(db_piece(i2))=="g"/))
     enddo
     i2=i1+1
     plan_EWZ_plus_Ik_IE(4)=any((/trim(db_piece(i2))=="ui",trim(db_piece(i2))=="uI",&
&                                          trim(db_piece(i2))=="UI",trim(db_piece(i2))=="Ui",&
&                                          trim(db_piece(i2))=="uki",trim(db_piece(i2))=="ukI",&
&                                          trim(db_piece(i2))=="UkI",trim(db_piece(i2))=="Uki"/))
     plan_EWZ_plus_Ik_IE(5)=any((/trim(db_piece(i2))=="uei",trim(db_piece(i2))=="ueI",&
&                                          trim(db_piece(i2))=="UeI",trim(db_piece(i2))=="Uei"/))
     if (.not.plan_EWZ_plus_Ik_IE(4).and..not.plan_EWZ_plus_Ik_IE(5)) plan_EWZ_plus_Ik_IE(4)=.TRUE.
     !
     if ( CHECK_the_DB( ) < 0) then
       n_QP_actions=n_QP_actions-1
       cycle
     endif
     !
#if defined _ELPH
     !
     ! GF's cannot be interpolated
     !
     if (GF_db) then
       n_QP_actions=n_QP_actions-1
       call QP_load_GFs(band_range,qp,en,DB_corrected)
       cycle
     endif
     !
#endif
     !
     ! Allocations
     !
     call LOCAL_Alloc( ) 
     !
     ! Build Up a short message from the DB and append it to QP_ctl
     !
     call QP_DB_message( ) 
     !
     ! Interpolation & Transfer
     !
     if (QP_ctl_from_DB(1)%interp_neigh<=0) QP_ctl_from_DB(:)%interp_neigh=1
     !
!     call QP_apply_DB_interpolation(band_range,qp,en,k,plan_EWZ_plus_Ik_IE,&
!&                                   QP_db_STATES_done(n_QP_actions,:,:,:),QP_ctl_from_DB%interp_neigh)
     !
     ! FIT
     !
     !if (do_EWZ_using_Ik_IE(1)) call QP_fit_DB_energies(band_range,qp,QP_ctl_from_DB)
     !if (do_EWZ_using_Ik_IE(2)) call QP_fit_DB_widths(band_range,qp,en,k,QP_ctl_from_DB)
     !if (do_EWZ_using_Ik_IE(3)) QP_ctl_from_DB%Z=sum(qp%Z(:))/qp%n_states
     !
     !if (all(DB_corrected(band_range(1):band_range(2),:,:)==1)) do_EWZ_using_Ik_IE(:3)=.false.
     !
   endif
   !
 enddo
 !
 ! GPL_EXCLUDE_END
 stop
 !
 contains 
   !
   integer function CHECK_the_DB( )
     !
     CHECK_the_DB=0
     !
     call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2,3/),MODE=DUMP,ID=ID)
     io_err=io_QP_and_GF(trim(QP_db_name),qp,ID)
     !
     if (io_err/=0) then
       call warning("I/O problems from QP database "//trim(QP_db_name))
       CHECK_the_DB=-1
       return
     endif
     !
     ! GPL_EXCLUDE_START
     !
     ! The GF are implemented only in the linear response module (real and reciprocal space) and using 
     ! exactly the same number of bands/k-points
     !
     if (index(trim(QP_DB_name),'.G')/=0) then
       if (qp%nb/=band_range(2).or.qp%nk/=k%nibz) then
         call warning("Wrong number of bands/k-points in "//trim(QP_db_name))
         CHECK_the_DB=-1
         return
       endif
       if (.not. ( l_chi.or.l_bss )) then 
         call warning("Green Functions not implemented for the current runlevel. I/O switched off og "//trim(QP_db_name))
         CHECK_the_DB=-1
         return
       endif
     endif
     !
   end function 
   !
   subroutine LOCAL_alloc
     if (plan_EWZ_plus_Ik_IE(2).and..not.allocated(en%W)) then
       YAMBO_ALLOC(en%W,(en%nb,en%nk,n_sp_pol))
       en%W=rZERO
     endif
     if (plan_EWZ_plus_Ik_IE(3).and..not.allocated(en%Z)) then
       YAMBO_ALLOC(en%Z,(en%nb,en%nk,n_sp_pol))
       en%Z=rZERO
     endif
     if (.not.allocated(en%Eo)) then
       YAMBO_ALLOC(en%Eo,(en%nb,en%nk,n_sp_pol))
       en%Eo=en%E
     endif
!     if (.not.allocated(QP_action%STATES_done)) then
!       QP_action%STATES_done=0
!     endif
   end subroutine
   !
end function
