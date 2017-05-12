!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA
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
subroutine QP_DBs_initialize()
 !
 ! QPDB_states -> input file
 !
 use pars,      ONLY:schlen
 use QP_m,      ONLY:QP_t,QP_reset
 use drivers,   ONLY:infile_editing
 use it_m,      ONLY:nflines,flines,infile
 use com,       ONLY:warning
 use parser_m,  ONLY:parser       
 use IO_m,      ONLY:OP_RD_CL,DUMP,io_control,NONE
 use units,     ONLY:HA2EV
 use YPP,       ONLY:l_QP_DBs_create_and_modify,QP_db_operation,QP_db_prefactor,n_QP_dbs,QP_db,QP_db_what,QP_db_name
 !
 implicit none
 ! 
 ! Work Space
 !
 type(QP_t) :: qp
 integer :: i_qp,i_f,i_c,nflines_save,io_QP_and_GF_err,ID
 character(schlen) :: DB_file_name(40,4)
 integer, external :: io_QP_and_GF
 !
 ! If there are already the DB lines or there's not the input file todo return
 !
 if (.not.infile_editing.and.l_QP_DBs_create_and_modify) return 
 !
 nflines_save=nflines
 !
 if (l_QP_DBs_create_and_modify) then
   !
   call QP_reset(qp)
   call io_control(ACTION=OP_RD_CL,SEC=(/1,2,3/),COM=NONE,MODE=DUMP,ID=ID)
   io_QP_and_GF_err=io_QP_and_GF("QP",qp,ID)
   !
   ! QP corrections
   ! 
   write (flines(nflines+1),'(a,t22,a)') '%QPkrange',&
&                           '# [QPDB] Correction( kp1| kp2| bnd1| bnd2| E-Eo[eV]| Img(E)[eV]| Re[Z] |)'
   if(io_QP_and_GF_err==0) then
     do i_qp=1,qp%n_states
       nflines=nflines+1
       write (flines(nflines+1),'(4(i3,a),3(f12.6,a))') qp%table(i_qp,3),'|',qp%table(i_qp,3),'|',&
&                                                       qp%table(i_qp,1),'|',qp%table(i_qp,1),'|', &
&                               real(qp%E(i_qp)-qp%E_bare(i_qp))*HA2EV,'|',aimag(qp%E(i_qp))*HA2EV,'|',real(qp%Z(i_qp)),'|'
     enddo
   else
     nflines=nflines+1
     write (flines(nflines+1),'(4(i3,a),3(f4.1,a))') -1,'|',-1,'|',-1,'|',-1,'|',0.0,'|',0.0,'|',0.0,'|'
   endif
   nflines=nflines+1
   flines(nflines+1)='%'
   nflines=nflines+1
   !
 else
   !
   n_QP_dbs=0
   DB_file_name=" "
   call parser("Actions_and_names",DB_file_name)
   !
   do i_c=1,2
     n_QP_dbs=0
     do i_qp=1,size(DB_file_name,1)
       if (len_trim(DB_file_name(i_qp,1))==0) exit
       n_QP_dbs=n_QP_dbs+1
       if (i_c==2) then
         call QP_reset(QP_db(n_QP_dbs))
         QP_db_what(n_QP_dbs)=DB_file_name(i_qp,1)
         QP_db_operation(n_QP_dbs)=DB_file_name(i_qp,2)
         read (DB_file_name(i_qp,3),*) QP_db_prefactor(n_QP_dbs)
         QP_db_name(n_QP_dbs)=DB_file_name(i_qp,4)
         call io_control(ACTION=OP_RD_CL,SEC=(/1,2,3/),COM=NONE,MODE=DUMP,ID=ID)
         io_QP_and_GF_err=io_QP_and_GF(trim(DB_file_name(i_qp,4)),QP_db(n_QP_dbs),ID)
         if (io_QP_and_GF_err/=0) then
           call warning("QP DB "//trim(DB_file_name(i_qp,4))//" not found")
           call QP_reset(QP_db(n_QP_dbs))
           n_QP_dbs=n_QP_dbs-1
         endif
       endif
     enddo
     if (i_c==1) then
       allocate(QP_db_what(n_QP_dbs))
       allocate(QP_db(n_QP_dbs))
       allocate(QP_db_name(n_QP_dbs))
       allocate(QP_db_operation(n_QP_dbs))
       allocate(QP_db_prefactor(n_QP_dbs))
       QP_db_operation="+"
       QP_db_prefactor=1.
     endif
   enddo
   !
   if  ( infile_editing ) then
     !
     nflines=nflines+1
     write (flines(nflines),'(a,t30,a)') '%Actions_and_names',&
&                           '# [QPDB] QP databases and Actions (format is "what"|"OP"|"prefactor"|"DB"|). OP can be +/-. '
     !
     if (n_QP_dbs==0) then
       nflines=nflines+1
       flines(nflines)=' "E" | "+" | "1" | "none" |'
     else
       do i_qp=1,n_QP_dbs
         nflines=nflines+1
         flines(nflines)=' "'//trim(DB_file_name(i_qp,1))//'" | "'//trim(DB_file_name(i_qp,2))//'" |'//&
&                        ' "'//trim(DB_file_name(i_qp,3))//'" | "'//trim(DB_file_name(i_qp,4))//'" |'
       enddo
     endif
     !
     nflines=nflines+1
     flines(nflines)='%'
     !
   endif
   !
 endif
 !
 if  ( .not.infile_editing ) return
 !
 ! Write to input file
 !
 open(unit=12,file=trim(infile),position='append')
 do i_f=nflines_save+1,nflines
   write (12,'(a)') trim(flines(i_f))
 enddo 
 close(12)
 return
 !
end subroutine
