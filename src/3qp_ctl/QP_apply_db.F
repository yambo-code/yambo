!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine QP_apply_db(band_range,en,k,qp,qpfit,msg_fmt,DB_corrected)
 !
 use pars,          ONLY:SP,schlen,lchlen
 use stderr,        ONLY:string_split
 use QP_m,          ONLY:QP_t
 use memory_m,      ONLY:mem_est
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels,n_sp_pol
 use com,           ONLY:msg,warning
 use QP_CTL,        ONLY:qp_fit_ctl
 use IO_m,          ONLY:io_control,OP_RD_CL,DUMP,REP
 !
 implicit none
 type(levels)    ::en
 type(QP_t)      ::qp
 type(qp_fit_ctl)::qpfit
 type(bz_samp)   ::k
 integer         ::band_range(2),DB_corrected(en%nb,en%nk)
 character(*)    ::msg_fmt
 !
 ! Work Space
 !
 character(schlen)::ch,db_piece(10)
 character(lchlen)::qp_msg,qp_db_name
 logical    :: QP_apply_done
 integer    :: i1
 !
 logical :: l_qp(4) ! = ( E W Z INTERPOLATION )
 !
 ! QP I/O
 !
 integer           ::io_err,ID
 integer, external ::ioQP
 !
 ! Comments extracted from the QP descriptions
 !
 integer, parameter :: n_fields=4
 integer            :: if,c_pos
 character(lchlen)  :: field(n_fields,2)
 !
 field(1,1)="X G`s            [used]"
 field(1,2)="XG"
 field(2,1)="X bands"
 field(2,2)="Xb"
 field(3,1)="Sc/G bands"
 field(3,2)="Scb"
 field(4,1)="Sc. G bands"
 field(4,2)="Sphb"
 !
 ! Presets
 !
 l_qp=.false.
 qp_msg=' '
 !
 if (qp%n_states/=0) then
   !
   ! QP type is filled (defined internally and not read from the DB)
   !
   l_qp(1)=.true. ! Apply QP corrections to energies only
   qpfit%db="none"
   qpfit%short_descr="none"
   qpfit%interp_neigh=1
 else
   !
   ! DB.QP
   !
   call string_split(qpfit%db,db_piece)
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2,3/),MODE=DUMP,ID=ID)
   io_err=0
   qp_db_name=' '
   do i1=1,10
     if (trim(db_piece(i1))=="<") then
       qp_db_name=db_piece(i1+1)
       io_err=ioQP(trim( db_piece(i1+1) ),qp,ID)
     endif
     if (len_trim(qp_db_name)==0) then
       if (.not.l_qp(1)) l_qp(1)=any((/trim(db_piece(i1))=="E",trim(db_piece(i1))=="e"/))
       if (.not.l_qp(2)) l_qp(2)=any((/trim(db_piece(i1))=="W",trim(db_piece(i1))=="w"/))
       if (.not.l_qp(3)) l_qp(3)=any((/trim(db_piece(i1))=="Z",trim(db_piece(i1))=="z"/))
     endif
     if (.not.l_qp(4)) l_qp(4)=any((/trim(db_piece(i1))=="ui",trim(db_piece(i1))=="uI",&
&                                    trim(db_piece(i1))=="UI",trim(db_piece(i1))=="Ui"/))
   enddo
   !  
   ! No DB
   !
   if (io_err<0) then
     qpfit%db="none"
     qpfit%short_descr="none"
     call warning('DB '//trim(qp_db_name)//' not found')
     return
   endif
   !  
   ! Is there anything to do ?
   !
   if (l_qp(1).and.QP_apply_done(band_range,(/1,en%nk/),en,'E')) l_qp(1)=.false.
   if (l_qp(2).and.QP_apply_done(band_range,(/1,en%nk/),en,'W')) l_qp(2)=.false.
   if (l_qp(3).and.QP_apply_done(band_range,(/1,en%nk/),en,'Z')) l_qp(3)=.false.
   !
   !DEBUG >
   l_qp(3)=.false.
   !DEBUG <
   !
   if (.not.any(l_qp)) return
   !  
   ! Here I build the qp_msg char on the basis of the qp%description lines.
   ! This to give a compact description of the QP details.
   ! To be used in the DB headers.
   !  
   i1=2
   if (l_qp(1)) then
     qp_msg(i1:i1)="E"
     i1=i1+1
   endif
   if (l_qp(2)) then
     qp_msg(i1:i1)="W"
     i1=i1+1
     allocate(en%W(en%nb,en%nk,n_sp_pol))
     call mem_est("E-W",(/size(en%W)/),(/SP/))
   endif
   if (l_qp(3)) then
     qp_msg(i1:i1)="Z"
     i1=i1+1
     allocate(en%Z(en%nb,en%nk,n_sp_pol))
     call mem_est("E-Z",(/size(en%Z)/))
   endif
   !
   qp_msg=trim(qp_msg)//'<'//trim(qp_db_name)//'['
   !
   do i1=1,qp%n_descs
     do if=1,n_fields
       if (index(qp%description(i1),trim(field(if,1)))/=0) then
         c_pos=index(qp%description(i1),":")+1
         qp_msg=trim(qp_msg)//' '//trim(field(if,2))//':'
         call CHappend(qp%description(i1)(c_pos:))
       endif
     enddo
     if (index(qp%description(i1),"PPA")/=0) qp_msg=trim(qp_msg)//' PPA '
   enddo
   !
   qp_msg=trim(qp_msg)//']'
   !  
   qpfit%short_descr=trim(qpfit%short_descr)//trim(qp_msg)
   !
   call msg(msg_fmt,trim(qpfit%short_descr))
 endif
 !
 ! Backup bare energies (only once)
 !
 if (.not.associated(en%Eo)) then
   allocate(en%Eo(en%nb,en%nk,n_sp_pol))
   call mem_est("E-Eo",(/size(en%Eo)/),(/SP/))
   en%Eo=en%E
 endif
 !
 ! Interpolation & Transfer
 !
 if (qpfit%interp_neigh<=0) qpfit%interp_neigh=1
 call QP_apply_interpolate(band_range,qp,en,k,l_qp,DB_corrected,qpfit%interp_neigh)
 !
 ! FIT
 !
 if (l_qp(1)) call QP_apply_fit(band_range,qp,en,qpfit,'E')
 if (l_qp(2)) call QP_apply_fit(band_range,qp,en,qpfit,'W')
 if (l_qp(3)) qpfit%Z=sum(qp%Z(:))/qp%n_states
 !
 contains 
   !
   subroutine CHappend(c)
     character(*)::c
     integer     ::ic,ic_start,ic_end
     !
     ic_start=-1    
     ic_end=-1    
     do ic=1,len(c)
       if (c(ic:ic)/=' '.and.ic_start<0) ic_start=ic
       if (c(ic:ic)==' '.and.ic_start>0) ic_end=ic
     enddo
     !
     qp_msg=trim(qp_msg)//c(ic_start:ic_end)
     !
   end subroutine
   !
 end subroutine
