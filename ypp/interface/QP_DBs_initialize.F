!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA
!
subroutine QP_DBs_initialize()
 !
 ! QPDB_states -> input file
 !
 use QP_m,      ONLY:QP_t,QP_reset
 use electrons, ONLY:spin
 use parser_m,  ONLY:PARSER_array
 use IO_int,    ONLY:io_control
 use IO_m,      ONLY:OP_RD_CL,DUMP,NONE
 use units,     ONLY:HA2EV
 use YPPm,      ONLY:l_QP_DBs_create_and_modify,n_QP_dbs,&
&                    QP_user_corrections,QP_db_input_control,l_QP_DBs_manipulate
 !
#include<memory.h>
 ! 
 ! Work Space
 !
 type(QP_t) :: qp
 integer :: i_qp,io_QP_and_GF_err,ID,n_user_qp_corrections,n_lines
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
   n_lines=8
   call PARSER_array(mode="REAL dimensions",VAR="QP_user_corrections",N=n_user_qp_corrections,M=n_lines)
   if (n_user_qp_corrections>0) then
     YAMBO_ALLOC(QP_user_corrections,(n_user_qp_corrections,n_lines))
     call PARSER_array(mode="read",VAR="QP_user_corrections",N=n_user_qp_corrections,M=n_lines,R_v=QP_user_corrections)
   else if (io_QP_and_GF_err==0) then
     n_user_qp_corrections=qp%n_states
     YAMBO_ALLOC(QP_user_corrections,(n_user_qp_corrections,n_lines))
     do i_qp=1,qp%n_states
       QP_user_corrections(i_qp,1)  =spin(qp%table(i_qp,:))
       QP_user_corrections(i_qp,2:3)=qp%table(i_qp,3)  
       QP_user_corrections(i_qp,4:5)=qp%table(i_qp,1)  
       QP_user_corrections(i_qp,6)  =real(qp%E(i_qp)-qp%E_bare(i_qp))*HA2EV
       QP_user_corrections(i_qp,7)  =aimag(qp%E(i_qp))*HA2EV
       QP_user_corrections(i_qp,8)  =real(qp%Z(i_qp))
     enddo
   else
     n_user_qp_corrections=1
     YAMBO_ALLOC(QP_user_corrections,(n_user_qp_corrections,n_lines))
     QP_user_corrections(1,:)=(/-1,-1,-1,-1,-1,0,0,0/)
   endif
   !
   call PARSER_array(mode="write",VAR="QP_user_corrections",&
&                    DESC="[QPDB] Correction( spin | kp1| kp2| bnd1| bnd2| E-Eo[eV]| Img(E)[eV]| Re[Z] |)",&
&                    N=n_user_qp_corrections,M=n_lines,R_v=QP_user_corrections,Kn=(/"i","i","i","i","i","r","r","r"/))
   !
 else if (l_QP_DBs_manipulate) then
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
