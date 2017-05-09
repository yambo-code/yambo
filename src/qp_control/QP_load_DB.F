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
integer function QP_load_DB(band_range,en,k,qp,QP_ctl_from_DB,msg_fmt,&
&                           DB_corrected,GFs_From_DB)
 !
 use drivers,       ONLY:l_chi,l_bss
 use pars,          ONLY:SP,schlen,lchlen,rZero
 use stderr,        ONLY:string_split,string_pack
 use QP_m,          ONLY:QP_t,QP_reset
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels,n_sp_pol
 use com,           ONLY:msg,warning
 use QP_CTL_m,      ONLY:QP_ctl_t
 use IO_m,          ONLY:io_control,OP_RD_CL,DUMP,REP,io_reset
 !
#include<memory.h>
 !
 type(levels)    ::en
 type(QP_t)      ::qp
 type(QP_ctl_t)  ::QP_ctl_from_DB(n_sp_pol)
 type(bz_samp)   ::k
 integer         ::band_range(2),DB_corrected(en%nb,en%nk,n_sp_pol)
 character(*)    ::msg_fmt
 logical         ::GFs_From_DB
 !
 ! Work Space
 !
 integer,parameter:: n_pieces=20,max_n_of_dbs=10
 character(schlen):: db_piece(n_pieces)
 type(QP_t)       :: qp_from_dbs(max_n_of_dbs)
 character(lchlen):: qp_msg,qp_db_name,dummy_db_name
 integer          :: i1,i_sp,n_qp_db,i2
 logical          :: do_EWZ_using_Ik_IE(5),& ! = ( E W Z INTERPOLATION/k INTERPOLATION/E )
&                    QPs_DB,GFs_DB,DB_plan_todo_EWZ(max_n_of_dbs,3)
 logical,external :: QP_check_if_corrected
 !
 ! QP I/O
 !
 integer           ::io_err,ID(max_n_of_dbs)
 integer, external ::io_QP_and_GF
 !
 ! Comments extracted from the QP descriptions
 !
 integer, parameter :: n_fields=4
 integer            :: if,c_pos
 character(lchlen)  :: field(n_fields,2)
 !
 ID(:)     =0
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
 do_EWZ_using_Ik_IE=.false.
 DB_plan_todo_EWZ=.false.
 GFs_From_DB=.false.
 qp_msg=' '
 QP_load_DB=1
 QPs_DB=.false.
 GFs_DB=.false.
 do i1=1,max_n_of_dbs
   call QP_reset(qp_from_dbs(i1))
 enddo
 !
 if (qp%n_states/=0) then
   !
   ! QP type is filled (defined internally and not read from the DB)
   !
   do_EWZ_using_Ik_IE(1)=.true. ! Apply QP corrections to energies only
   QP_ctl_from_DB(:)%db="none"
   QP_ctl_from_DB(:)%short_descr="none"
   QP_ctl_from_DB(:)%interp_neigh=1
   !
 else
   !
   ! DB.QP/DB.G
   !
   call string_split(QP_ctl_from_DB(1)%db,db_piece)
   !
   ! First check the DB file 
   !
   io_err=-1
   dummy_db_name=' '
   qp_db_name=' '
   n_qp_db=0
   do i1=1,n_pieces
     if (trim(db_piece(i1))=="<") then
       !
       dummy_db_name=db_piece(i1+1)
       !
       if (len_trim(dummy_db_name)==0) cycle
       !
       n_qp_db=n_qp_db+1
       !
       do i2=i1-3,i1-1
         if(i2<=0) cycle
         if (.not. DB_plan_todo_EWZ(n_qp_db,1)) &
&           DB_plan_todo_EWZ(n_qp_db,1)=any((/trim(db_piece(i2))=="E",trim(db_piece(i2))=="e"/))
         if (.not. DB_plan_todo_EWZ(n_qp_db,2)) &
&           DB_plan_todo_EWZ(n_qp_db,2)=any((/trim(db_piece(i2))=="W",trim(db_piece(i2))=="w"/))
         if (.not. DB_plan_todo_EWZ(n_qp_db,3)) &
&           DB_plan_todo_EWZ(n_qp_db,3)=any((/trim(db_piece(i2))=="Z",trim(db_piece(i2))=="z"/))
       enddo
       !
       if (n_qp_db==1) qp_db_name=dummy_db_name
       if (n_qp_db> 1) qp_db_name=string_pack(trim(qp_db_name),"+",trim(dummy_db_name))
       !
       if (n_qp_db>max_n_of_dbs) call warning('Too many QP databases provided.')
       !
       call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2,3/),MODE=DUMP,ID=ID(n_qp_db))
       !
#if defined _ELPH
       if (index(trim(dummy_db_name),'.QP')/=0.or.index(trim(dummy_db_name),'.G')/=0) then
         io_err=io_QP_and_GF(trim(dummy_db_name) ,qp_from_dbs(n_qp_db),ID(n_qp_db))
       endif
#else
       if (index(trim(dummy_db_name),'.QP')/=0) io_err=io_QP_and_GF(trim(dummy_db_name),qp_from_dbs(n_qp_db),ID(n_qp_db))
#endif
       !
       if (io_err/=0) then
         call warning("I/O problems from QP database "//trim(dummy_db_name))
         n_qp_db=n_qp_db-1
       endif
       !
     endif
   enddo
   !
   if (n_qp_db>0) io_err=0
   !
   ! If at least 1 DB is read io_err=0. In this case the database(s)
   ! are flagged to be QP-like (QPs_DB=.TRUE.) or GF-related (GFs_DB=.TRUE.)
   !
   ! If no DB are read both logicals are .FALSE. .
   !
   if (len_trim(qp_db_name)/=0.and.io_err==0) then
     !
     call QP_merge_DBs(n_qp_db,qp_from_dbs,qp,DB_plan_todo_EWZ(:n_qp_db,:))
     !
     QPs_DB=index(trim(qp_db_name),'.QP')/=0
     !
     if (QPs_DB) then
       do_EWZ_using_Ik_IE(1)=any( DB_plan_todo_EWZ(:n_qp_db,1) )
       do_EWZ_using_Ik_IE(2)=any( DB_plan_todo_EWZ(:n_qp_db,2) )
       do_EWZ_using_Ik_IE(3)=any( DB_plan_todo_EWZ(:n_qp_db,3) )
     endif
     !
     ! GPL_EXCLUDE_START
     !
     ! The GF are implemented only in the linear response module (real and reciprocal space)
     !
     GFs_DB=index(trim(qp_db_name),'.G')/=0.and. ( l_chi.or.l_bss )
     !
     ! GPL_EXCLUDE_END
     !
   endif
   !
   do i1=1,max_n_of_dbs
     if (ID(i1)/=0) call io_reset(ID=ID(i1))
   enddo
   !
   ! Then check what is asked to load and the Interpolation Scheme
   !
   do i1=1,n_pieces
     if (QPs_DB) then
       if (.not.do_EWZ_using_Ik_IE(4)) do_EWZ_using_Ik_IE(4)=any((/trim(db_piece(i1))=="ui",trim(db_piece(i1))=="uI",&
&                                                                  trim(db_piece(i1))=="UI",trim(db_piece(i1))=="Ui",&
&                                                                  trim(db_piece(i1))=="uki",trim(db_piece(i1))=="ukI",&
&                                                                  trim(db_piece(i1))=="UkI",trim(db_piece(i1))=="Uki"/))
       if (.not.do_EWZ_using_Ik_IE(5)) do_EWZ_using_Ik_IE(5)=any((/trim(db_piece(i1))=="uei",trim(db_piece(i1))=="ueI",&
&                                                                  trim(db_piece(i1))=="UeI",trim(db_piece(i1))=="Uei"/))
       !
     else if (GFs_DB) then
       if (.not.GFs_From_DB) GFs_From_DB=any((/trim(db_piece(i1))=="G",trim(db_piece(i1))=="g"/))
     endif
   enddo
   !
   ! Save fallback (BUG fix by AM, 5/1/2016)
   !
   if (.not.do_EWZ_using_Ik_IE(4).and..not.do_EWZ_using_Ik_IE(5)) do_EWZ_using_Ik_IE(4)=.TRUE.
   !
   ! GPL_EXCLUDE_START
   !
   ! note that the DB must contain the exact number of bands/k-pts
   !
   if (GFs_DB.and.io_err==0) GFs_From_DB=qp%nb==band_range(2).and.qp%nk==k%nibz
   !
   ! GPL_EXCLUDE_END
   !
   ! No compatible DB/actions 
   !
   if (.not. (QPs_DB.and.any(do_EWZ_using_Ik_IE)) .and..not. (GFs_DB.and.GFs_From_DB) ) then
     QP_ctl_from_DB(:)%db="none"
     QP_ctl_from_DB(:)%short_descr="none"
     if (.not.QPs_DB.and..not.GFs_DB) then
       if (len_trim(qp_db_name)>0) call warning('DB ('//trim(qp_db_name)//') not found')
     else
       call warning('Impossible to perform I/O. Incompatible actions ?')
     endif
     QP_load_DB=-1
     return
   endif
   !   
   ! Check if states are all corrected to see whether there is anything to do.
   !
   if (do_EWZ_using_Ik_IE(1).and.&
&      QP_check_if_corrected(band_range,(/1,en%nk/),(/1,n_sp_pol/),en,'E')) do_EWZ_using_Ik_IE(1)=.false.
   if (do_EWZ_using_Ik_IE(2).and.&
&      QP_check_if_corrected(band_range,(/1,en%nk/),(/1,n_sp_pol/),en,'W')) do_EWZ_using_Ik_IE(2)=.false.
   if (do_EWZ_using_Ik_IE(3).and.&
&      QP_check_if_corrected(band_range,(/1,en%nk/),(/1,n_sp_pol/),en,'Z')) do_EWZ_using_Ik_IE(3)=.false.
   if (GFs_From_DB.and.&
&      QP_check_if_corrected(band_range,(/1,en%nk/),(/1,n_sp_pol/),en,'G')) GFs_From_DB=.false.
   !
   if (.not.any(do_EWZ_using_Ik_IE).and..not.GFs_From_DB) then
     QP_load_DB=-1
     return
   endif
   !  
   ! Here I build the qp_msg char on the basis of the qp%description lines.
   ! This to give a compact description of the QP details.
   ! To be used in the DB headers.
   !  
   i1=2
   if (do_EWZ_using_Ik_IE(1)) then
     qp_msg(i1:i1)="E"
     i1=i1+1
   endif
   if (do_EWZ_using_Ik_IE(2)) then
     qp_msg(i1:i1)="W"
     i1=i1+1
     YAMBO_ALLOC(en%W,(en%nb,en%nk,n_sp_pol))
     en%W=rZero
   endif
   if (do_EWZ_using_Ik_IE(3)) then
     qp_msg(i1:i1)="Z"
     i1=i1+1
     YAMBO_ALLOC(en%Z,(en%nb,en%nk,n_sp_pol))
     en%Z=1._SP
   endif
   if (GFs_From_DB) then
     qp_msg(i1:i1)="G"
     i1=i1+1
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
   do i_sp=1,n_sp_pol
     QP_ctl_from_DB(i_sp)%short_descr=trim(QP_ctl_from_DB(i_sp)%short_descr)//trim(qp_msg)
     call msg(msg_fmt,trim(QP_ctl_from_DB(i_sp)%short_descr))
   enddo
   !
 endif
 !
 ! GPL_EXCLUDE_START
#if defined _ELPH
 !
 ! Green Functions
 !
 if (GFs_From_DB) then
   call QP_load_GFs(band_range,qp,en,DB_corrected)
   return
 endif
 !
#endif
 ! GPL_EXCLUDE_END
 !
 ! Backup bare energies (only once)
 !
 if (.not.allocated(en%Eo)) then
   YAMBO_ALLOC(en%Eo,(en%nb,en%nk,n_sp_pol))
   en%Eo=en%E
 endif
 !
 ! Interpolation & Transfer
 !
 if (QP_ctl_from_DB(1)%interp_neigh<=0) QP_ctl_from_DB(:)%interp_neigh=1
 !
 call QP_apply_DB_interpolation(band_range,qp,en,k,do_EWZ_using_Ik_IE,DB_corrected,QP_ctl_from_DB%interp_neigh)
 !
 ! FIT
 !
 if (do_EWZ_using_Ik_IE(1)) call QP_fit_DB_energies(band_range,qp,QP_ctl_from_DB)
 if (do_EWZ_using_Ik_IE(2)) call QP_fit_DB_widths(band_range,qp,en,k,QP_ctl_from_DB)
 if (do_EWZ_using_Ik_IE(3)) QP_ctl_from_DB%Z=sum(qp%Z(:))/qp%n_states
 !
 if (all(DB_corrected(band_range(1):band_range(2),:,:)==1)) do_EWZ_using_Ik_IE(:3)=.false.
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
end function
