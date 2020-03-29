!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DALV
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
integer function io_MPA(X,Xw,ID)
 !
 use pars,          ONLY:SP,schlen
 use com,           ONLY:error
 use units,         ONLY:HA2EV
 use R_lattice,     ONLY:nqibz,g_vec
 use X_m,           ONLY:X_t,X_mat,X_RLcomp_ig,MPA_description,X_alloc
 use frequency,     ONLY:w_samp
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,io_com,&
&                        io_elemental,io_status,io_bulk,&
&                        read_is_on,write_is_on,io_header,&
&                        manage_OP_IF_START_and_CL_IF_END,&
&                        ver_is_gt_or_eq,io_serial_number,serial_number,&
&                        NONE,IO_INCOMPATIBLE_VAR,io_MULTIPOLE,IO_NOT_ALLOWED,&
&                        io_fragment,IO_OUTDATED_DB,frag_MULTIPOLE,&
&                        IO_NO_DATABASE,io_mode,DUMP,&
&                        def_variable_bulk,io_variable_bulk,def_variable_elemental,io_variable_elemental
 use stderr,        ONLY:intc
 use timing_m,      ONLY:timing
 use mpa_m,         ONLY:MPA_E_mat,MPA_R_mat,MPA_freqs,MPA_alloc
#if defined _PAR_IO
 use X_m,           ONLY:X_par
 use IO_m,          ONLY:io_unit,netcdf_call,io_file,close_is_on,io_resetable,io_MULTIPOLE,&
&                        io_netcdf_var,io_action,OP_WR_CL
 use parallel_m,    ONLY:PAR_COM_X_WORLD,PAR_Q_index
#endif
 !
#include<memory.h>
 type(X_t)   :: X
 type(w_samp):: Xw
 integer     :: ID
 !
 !Work Space
 !
 integer             ::sec_size,i1,iq,i_err,io_com_save,io_err
 logical             ::different_db_RL_order,perform_IO
 character (schlen)  ::ch,db_desc
 character (2)       ::local_XC_KIND
 real(SP),   allocatable :: RL_vecs_disk(:,:)
 complex(SP),allocatable :: X_on_disk(:,:)
   !
 integer,    external    :: variables_MPA
 !
 if (io_MULTIPOLE) call timing('io_MPA',OPR='start')
 !
 if (.not.io_MULTIPOLE) then
   io_MPA=IO_NOT_ALLOWED
   call io_disconnect(ID,FORCE=.TRUE.)
   return
 endif
 !
 different_db_RL_order=.false.
 !
 db_desc='ER'
 !
#if defined _PAR_IO
 perform_IO=PAR_COM_X_WORLD%CPU_id==0.or.read_is_on(ID)
#else
 perform_IO=.true.
#endif
 !
 if(perform_IO) then
   io_MPA=io_connect(desc=trim(db_desc),type=2,ID=ID) 
   if (io_MPA/=0) goto 1
 endif
 !
 if (any((/io_sec(ID,:)==1/)).and.perform_IO) then
   !
   ! Header
   !
   local_XC_KIND="rE-iE-rR-iR"
   io_MPA=io_header(ID,QPTS=.true.,R_LATT=.false.,WF=.false.,T_EL=.false.,XC_KIND=local_XC_KIND,&
&                    CUTOFF=.false.,GAUGE=.false.,FRAG=frag_MULTIPOLE)
   if (io_MPA/=0) goto 1
   !
   if (.not.ver_is_gt_or_eq(ID,revision=308)) then
     io_MPA=IO_OUTDATED_DB
     goto 1
   endif
   !
   ! Global Descriptions
   !=====================
   !
   io_MPA=variables_MPA(ID,MPA_description,X)
   if (io_MPA/=0) goto 1
   !
   ! Close & Menu lower line
   !
   if (.not.any((/io_sec(ID,:)==2/))) then
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
     io_MPA=io_status(ID)
     if (io_MPA/=0) goto 1
   else
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
 endif
 !
 do i1=1,size(io_sec(ID,:))
   if (io_sec(ID,i1)==0) cycle
   iq=io_sec(ID,i1)/2
 enddo
 !
 if (iq==0) goto 1
 !
 ! Manage OP_IF_START_and_RD_CL_IF_END
 !
 call manage_OP_IF_START_and_CL_IF_END(ID,iq,nqibz)
#if defined _PAR_IO
 call io_fragment(ID,i_fragment=iq,ierr=io_err,COMM=PAR_COM_X_WORLD%COMM,cpu_id=PAR_COM_X_WORLD%CPU_id)
#else
 call io_fragment(ID,i_fragment=iq,ierr=io_err)
#endif
 !
 !  type(X_t)    ::X(5)  x s d p m
 !
 if (any((/io_sec(ID,:)==2*iq/)) ) then
   !
   sec_size=6
   !
   ch="FREQ_PARS_sec_iq"//trim(intc(iq))
   !
   ! Important: once the file has been opend in PAR mode all the CPUs belonging to the COMM
   !            must go through the definitions of the variables although only one is writing
   !
   io_com_save=io_com(ID)
#if defined _PAR_IO
   call def_variable_elemental(ID,trim(ch),sec_size,SP,1,par_io_kind='independent')
#else
   call def_variable_elemental(ID,trim(ch),sec_size,SP,1)
#endif
   !
   if (perform_IO) then
     call io_variable_elemental(ID,           VAR=" :: Current Q-pt index     :",I0=iq)
     call io_variable_elemental(ID,           VAR=" :: Number of poles  :",I0=Xw%n_freqs/2,CHECK=.true.,OP=(/"=="/))
     !
   endif
   !
   call def_variable_elemental(ID,"",0,0,1)
   io_MPA=io_status(ID)
   !
   io_com(ID)=io_com_save
   !
   if (io_MPA/=0) goto 1
   !
 endif
 !
 if (any((/io_sec(ID,:)==2*iq+1/))) then
   !
   ! Xw%p
   !
   if (read_is_on(ID)) then
     YAMBO_FREE(Xw%p)
     YAMBO_ALLOC(Xw%p,(Xw%n_freqs))
   endif
   !
   ch="FREQ_sec_iq"//trim(intc(iq))
#if defined _PAR_IO
   call def_variable_bulk(ID,trim(ch),1,(/4,Xw%n_freqs/2/),SP,par_io_kind='independent')
#else
   call def_variable_bulk(ID,trim(ch),1,(/4,Xw%n_freqs/2/),SP)
#endif
   if(perform_IO) call io_variable_bulk(ID,1,C1=Xw%p)
   !
   ! allocations
   !
   if (read_is_on(ID)) call MPA_alloc((/X%ng,X%ng,Xw%n_freqs/2,Xw%n_freqs/))
   !
   ! poles @iq
   !
   ch="reE-imE_Q_"//trim(intc(iq))
   !
#if defined _PAR_IO
   if(write_is_on(ID)) then
     !
     call def_variable_bulk(ID,trim(ch),1,shape(MPA_E_par(1)%blc(:,:,:)),SP,par_io_kind='collective')
     call  io_variable_bulk(ID,1,C3=MPA_E_par(1)%blc(:,:,:),IPOS=(/1,MPA_E_par(1)%rows(1),MPA_E_par(1)%cols(1),1/))
     !
   else if(read_is_on(ID)) then
#endif
     !
     call def_variable_bulk(ID,trim(ch),1,(/2,shape(MPA_E_mat)/),SP)
     call  io_variable_bulk(ID,1,C3=MPA_E_mat,IPOS=(/1,1,1,1/))
     !
#if defined _PAR_IO
   endif
#endif
   !
   ! residues @iq
   !
   ch="reR-imR_Q_"//trim(intc(iq))
   !
#if defined _PAR_IO
   if(write_is_on(ID)) then
     !
     call def_variable_bulk(ID,trim(ch),1,shape(MPA_R_par(1)%blc(:,:,:)),SP,par_io_kind='collective')
     call  io_variable_bulk(ID,1,C3=MPA_R_par(1)%blc(:,:,:),IPOS=(/1,MPA_R_par(1)%rows(1),MPA_R_par(1)%cols(1),1/))
     !
   else if(read_is_on(ID)) then
#endif
     !
     call def_variable_bulk(ID,trim(ch),1,(/2,shape(MPA_R_mat)/),SP)
     call  io_variable_bulk(ID,1,C3=MPA_R_mat,IPOS=(/1,1,1,1/))
     !
#if defined _PAR_IO
   endif
#endif
   !
   if (read_is_on(ID) .and. different_db_RL_order) then 
     call error('[io_MPA] different_db_RL_order    not implemented')
     !
     YAMBO_ALLOC(X_on_disk,(X%ng,X%ng))
     do i1=1,Xw%n_freqs
       X_on_disk=X_mat(:,:,i1)
       X_mat(X_RLcomp_ig(:),X_RLcomp_ig(:),i1)=X_on_disk
     enddo
     YAMBO_FREE(X_on_disk)
   endif
   !
 endif
 !
 ! Sections 1 & 2 are used in VERIFY mode. 
 ! Now that the menu is closed I can return with io_MPA/=0 if there is a restart point
 !
 if (io_mode(ID)/=DUMP) then
   io_MPA=IO_NO_DATABASE
   if (io_err==0) io_MPA=iq
 endif
 !
1 call io_disconnect(ID)
 !
 if (io_MULTIPOLE) call timing('io_MPA',OPR='stop')
 !
end function
!
!
integer function variables_MPA(ID,local_description,X)
 !
 use pars,          ONLY:schlen,lchlen
 use units,         ONLY:HA2EV
 use X_m,           ONLY:X_t,n_MPA_descs
 use IO_m,          ONLY:io_elemental,io_status,ver_is_gt_or_eq,read_is_on
 implicit none
 !
 integer            ::ID
 character(lchlen)  ::local_description(100)
 type(X_t)          ::X
 !
 ! Work Space
 !
 integer             ::i_local_desc,sec_size
 logical             ::local_long_gauge
 character(schlen)   ::X_label,MPA_label
 !
 i_local_desc=0
 local_description=' '
 MPA_label=' '
 if (ver_is_gt_or_eq(ID,revision=955)) MPA_label="MPA_"
 !
 call io_elemental(ID,DB_I0=X%ng_db,&
&     VAR=" Matrix size          :",I0=X%ng,CHECK=.true.,OP=(/"<="/),&
&     DESCRIPTION=local_description(i_local_desc))
 i_local_desc=i_local_desc+1
 call io_elemental(ID,VAR=trim(MPA_label)//"npoles",I0=X%mpa_npoles,MENU=0)
 i_local_desc=i_local_desc+1
 call io_elemental(ID,VAR=trim(MPA_label)//"reimER",I0=4)
 i_local_desc=i_local_desc+1
 !
 n_MPA_descs=i_local_desc
 !
 variables_MPA=io_status(ID)
 !
end function
