!
!        Copyright (C) 2000-2018 the YAMBO team
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
 use QP_m,      ONLY:QP_t,QP_reset
 use parser_m,  ONLY:PARSER_array
 use IO_m,      ONLY:OP_RD_CL,DUMP,io_control,NONE
 use units,     ONLY:HA2EV
 use YPP,       ONLY:l_QP_DBs_create_and_modify,n_QP_dbs,&
&                    QP_user_corrections,QP_db_input_control
 !
#include<memory.h>
 ! 
 ! Work Space
 !
 type(QP_t) :: qp
 integer :: i_qp,io_QP_and_GF_err,ID,n_user_qp_corrections
 integer, external :: io_QP_and_GF
 !
 ! If there are already the DB lines or there's not the input file todo return
 !
 if (l_QP_DBs_create_and_modify) then
   !
   call QP_reset(qp)
   call io_control(ACTION=OP_RD_CL,SEC=(/1,2,3/),COM=NONE,MODE=DUMP,ID=ID)
   io_QP_and_GF_err=io_QP_and_GF("QP",qp,ID)
   !
   ! QP corrections
   ! 
   call PARSER_array(mode="REAL dimensions",VAR="QP_user_corrections",N=n_user_qp_corrections,M=7)
   if (n_user_qp_corrections>0) then
     YAMBO_ALLOC(QP_user_corrections,(n_user_qp_corrections,7))
     call PARSER_array(mode="read",VAR="QP_user_corrections",N=n_user_qp_corrections,M=7,R_v=QP_user_corrections)
   else if (io_QP_and_GF_err==0) then
     n_user_qp_corrections=qp%n_states
     YAMBO_ALLOC(QP_user_corrections,(n_user_qp_corrections,7))
     do i_qp=1,qp%n_states
       QP_user_corrections(i_qp,1:2)=qp%table(i_qp,3)  
       QP_user_corrections(i_qp,3:4)=qp%table(i_qp,1)  
       QP_user_corrections(i_qp,5)  =real(qp%E(i_qp)-qp%E_bare(i_qp))*HA2EV
       QP_user_corrections(i_qp,6)  =aimag(qp%E(i_qp))*HA2EV
       QP_user_corrections(i_qp,7)  =real(qp%Z(i_qp))
     enddo
   else
     n_user_qp_corrections=1
     YAMBO_ALLOC(QP_user_corrections,(n_user_qp_corrections,7))
     QP_user_corrections(1,:)=(/-1,-1,-1,-1,0,0,0/)
   endif
   !
   call PARSER_array(mode="write",VAR="QP_user_corrections",&
&                    DESC="[QPDB] Correction( kp1| kp2| bnd1| bnd2| E-Eo[eV]| Img(E)[eV]| Re[Z] |)",&
&                    N=n_user_qp_corrections,M=7,R_v=QP_user_corrections,Kn=(/"i","i","i","i","r","r","r"/))
   !
 else
   !
   call PARSER_array(mode="CH dimensions",VAR="Actions_and_names",N=n_QP_dbs,M=4)
   if (n_QP_dbs>0) then
     allocate(QP_db_input_control(n_QP_dbs,4))
     call PARSER_array(mode="read",VAR="Actions_and_names",N=n_QP_dbs,M=4,CH_v=QP_db_input_control)
   else
     n_QP_dbs=1
     allocate(QP_db_input_control(n_QP_dbs,4))
     QP_db_input_control(1,1:3)=(/"E","+","1"/)
     QP_db_input_control(1,4:4)=(/"none"/)
   endif
   call PARSER_array(mode="write",VAR="Actions_and_names",&
&                    DESC='[QPDB] Format is "what"|"OP"|"prefactor"|"DB"|. OP can be +/-/x(only for Z)',&
&                    N=n_QP_dbs,M=4,CH_v=QP_db_input_control)
   !
 endif
 !
end subroutine
