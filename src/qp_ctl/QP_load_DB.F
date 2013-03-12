!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
integer function QP_load_DB(band_range,en,k,qp,QP_ctl_from_DB,msg_fmt,&
&                           DB_corrected,GFs_From_DB)
 !
 use drivers,       ONLY:l_chi,l_bss
 use pars,          ONLY:SP,schlen,lchlen
 use stderr,        ONLY:string_split,string_pack
 use QP_m,          ONLY:QP_t,QP_reset
 use memory_m,      ONLY:mem_est
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels,n_sp_pol
 use com,           ONLY:msg,warning,error
 use QP_CTL_m,      ONLY:QP_ctl_t
 use IO_m,          ONLY:io_control,OP_RD_CL,DUMP,REP,io_reset
 !
 implicit none
 !
 type(levels)    ::en
 type(QP_t)      ::qp
 type(QP_ctl_t)  ::QP_ctl_from_DB(n_sp_pol)
 type(bz_samp)   ::k
 integer         ::band_range(2),DB_corrected(en%nb,en%nk,n_sp_pol)
 character(*)    ::msg_fmt
 logical         ::GFs_from_DB
 !
 ! Work Space
 !
 integer,parameter::n_pieces=20,max_n_of_dbs=10
 character(schlen)::ch,db_piece(n_pieces)
 type(QP_t)       ::qp_from_dbs(max_n_of_dbs)
 character(lchlen)::qp_msg,qp_db_name,dummy_db_name
 integer          :: i1,i_sp,n_qp_db
 logical          :: l_plan_todo_EWZI(4),& ! = ( E W Z INTERPOLATION )
&                    QPs_DB,GFs_DB,DB_plan_todo_EWZ(max_n_of_dbs,3)
 logical,external :: QP_check_if_corrected
 !
 ! QP I/O
 !
 integer           ::io_err,ID(max_n_of_dbs)
 integer, external ::ioQP
 !
 ! Comments extracted from the QP descriptions
 !
 integer, parameter :: n_fields=4
 integer            :: if,c_pos
 character(lchlen)  :: field(n_fields,2)
 !
 ID(:) = 0
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
 l_plan_todo_EWZI=.false.
 DB_plan_todo_EWZ=.false.
 GFs_from_DB=.false.
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
   l_plan_todo_EWZI(1)=.true. ! Apply QP corrections to energies only
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
       DB_plan_todo_EWZ(n_qp_db,1)=any((/trim(db_piece(i1-1))=="E",trim(db_piece(i1))=="e"/))
       DB_plan_todo_EWZ(n_qp_db,2)=any((/trim(db_piece(i1-1))=="W",trim(db_piece(i1))=="w"/))
       DB_plan_todo_EWZ(n_qp_db,3)=any((/trim(db_piece(i1-1))=="Z",trim(db_piece(i1))=="z"/))
       !
       if (n_qp_db==1) qp_db_name=dummy_db_name
       if (n_qp_db> 1) qp_db_name=string_pack(trim(qp_db_name),"+",trim(dummy_db_name))
       !
       if (n_qp_db>max_n_of_dbs) call warning('Too many QP databases provided.')
       !
       call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2,3/),MODE=DUMP,ID=ID(n_qp_db))
       !
       ! GPL_EXCLUDE_START
#if defined _ELPH
       if (index(trim(dummy_db_name),'.QP')/=0.or.index(trim(dummy_db_name),'.G')/=0) then
         io_err=ioQP(trim(dummy_db_name) ,qp_from_dbs(n_qp_db),ID(n_qp_db))
       endif
#else
       ! GPL_EXCLUDE_END
       if (index(trim(dummy_db_name),'.QP')/=0) io_err=ioQP(trim(dummy_db_name),qp_from_dbs(n_qp_db),ID(n_qp_db))
       ! GPL_EXCLUDE_START
#endif
       ! GPL_EXCLUDE_END
       !
       if (io_err/=0) n_qp_db=n_qp_db-1
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
       l_plan_todo_EWZI(1)=any( DB_plan_todo_EWZ(:n_qp_db,1) )
       l_plan_todo_EWZI(2)=any( DB_plan_todo_EWZ(:n_qp_db,2) )
       l_plan_todo_EWZI(3)=any( DB_plan_todo_EWZ(:n_qp_db,3) )
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
   ! Then check what is asked to load
   !
   do i1=1,n_pieces
     if (QPs_DB) then
       if (.not.l_plan_todo_EWZI(4)) l_plan_todo_EWZI(4)=any((/trim(db_piece(i1))=="ui",trim(db_piece(i1))=="uI",&
&                                    trim(db_piece(i1))=="UI",trim(db_piece(i1))=="Ui"/))
     else if (GFs_DB) then
       if (.not.GFs_from_DB) GFs_from_DB=any((/trim(db_piece(i1))=="G",trim(db_piece(i1))=="g"/))
     endif
   enddo
   !
   ! GPL_EXCLUDE_START
   !
   ! note that the DB must contain the exact number of bands/k-pts
   !
   if (GFs_DB.and.io_err==0) GFs_from_DB=qp%nb==band_range(2).and.qp%nk==k%nibz
   !
   ! GPL_EXCLUDE_END
   !
   ! No compatible DB/actions 
   !
   if (.not. (QPs_DB.and.any(l_plan_todo_EWZI)) .and..not. (GFs_DB.and.GFs_from_DB) ) then
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
   ! Check if states are all corrected to see whether 
   ! there is anything to do.
   !
   if (l_plan_todo_EWZI(1).and.&
&      QP_check_if_corrected(band_range,(/1,en%nk/),(/1,n_sp_pol/),en,'E')) l_plan_todo_EWZI(1)=.false.
   if (l_plan_todo_EWZI(2).and.&
&      QP_check_if_corrected(band_range,(/1,en%nk/),(/1,n_sp_pol/),en,'W')) l_plan_todo_EWZI(2)=.false.
   if (l_plan_todo_EWZI(3).and.&
&      QP_check_if_corrected(band_range,(/1,en%nk/),(/1,n_sp_pol/),en,'Z')) l_plan_todo_EWZI(3)=.false.
   if (GFs_from_DB.and.&
&      QP_check_if_corrected(band_range,(/1,en%nk/),(/1,n_sp_pol/),en,'G')) GFs_from_DB=.false.
   !
   !DEBUG >
   l_plan_todo_EWZI(3)=.false.
   !DEBUG <
   !
   if (.not.any(l_plan_todo_EWZI).and..not.GFs_from_DB) then
     QP_load_DB=-1
     return
   endif
   !  
   ! Here I build the qp_msg char on the basis of the qp%description lines.
   ! This to give a compact description of the QP details.
   ! To be used in the DB headers.
   !  
   i1=2
   if (l_plan_todo_EWZI(1)) then
     qp_msg(i1:i1)="E"
     i1=i1+1
   endif
   if (l_plan_todo_EWZI(2)) then
     qp_msg(i1:i1)="W"
     i1=i1+1
     allocate(en%W(en%nb,en%nk,n_sp_pol))
     call mem_est("E-W",(/size(en%W)/),(/SP/))
   endif
   if (l_plan_todo_EWZI(3)) then
     qp_msg(i1:i1)="Z"
     i1=i1+1
     allocate(en%Z(en%nb,en%nk,n_sp_pol))
     call mem_est("E-Z",(/size(en%Z)/))
   endif
   if (GFs_from_DB) then
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
 if (GFs_from_DB) then
   call Load_Green_Functions(band_range,qp,en,DB_corrected)
   return
 endif
 !
#endif
 ! GPL_EXCLUDE_END
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
 if (QP_ctl_from_DB(1)%interp_neigh<=0) QP_ctl_from_DB(:)%interp_neigh=1
 call QP_apply_DB_interpolation(band_range,qp,en,k,l_plan_todo_EWZI,DB_corrected,QP_ctl_from_DB%interp_neigh)
 !
 ! FIT
 !
 if (l_plan_todo_EWZI(1)) call QP_fit_DB_values(band_range,qp,en,QP_ctl_from_DB,'E')
 if (l_plan_todo_EWZI(2)) call QP_fit_DB_values(band_range,qp,en,QP_ctl_from_DB,'W')
 if (l_plan_todo_EWZI(3)) QP_ctl_from_DB%Z=sum(qp%Z(:))/qp%n_states
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
