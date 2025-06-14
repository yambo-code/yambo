!
! License-Identifier: GPL
!
! Copyright (C) 2014 The Yambo Team
!
! Authors (see AUTHORS file for details): DALV AF
!
integer function io_MPA(X,Xw,ID)
 !
 use pars,          ONLY:SP,schlen
 use X_m,           ONLY:X_t,X_mat,X_RLcomp_ig,MPA_desc
 use frequency,     ONLY:w_samp
 use IO_int,        ONLY:io_connect,io_disconnect,io_elemental,io_header,io_fragment,&
&                        def_variable_bulk,io_variable_bulk,def_variable_elemental,io_variable_elemental
 use IO_m,          ONLY:io_sec,io_com,io_status,read_is_on,write_is_on,&
&                        io_MULTIPOLE,IO_NOT_ALLOWED,io_extension,NONE,&
&                        frag_MULTIPOLE,IO_NO_DATABASE,io_mode,DUMP,frag_RESPONSE
 use stderr,        ONLY:intc
 use timing_m,      ONLY:timing
 use mpa_m,         ONLY:MPA_freqs,MPA_alloc,MPA_E_par,MPA_R_par
 !
#include<y_memory.h>
 !
 type(X_t)   :: X
 type(w_samp):: Xw
 integer     :: ID
 !
 !Work Space
 !
 integer           :: sec_size,i1,iq,io_com_save,io_err,ID_frag
 logical           :: different_db_RL_order
 character(schlen) :: ch
 complex(SP),allocatable :: X_on_disk(:,:)
   !
 integer, external :: variables_MPA
 !
 ! checks & return
 !
 if (.not.io_MULTIPOLE) then
   io_MPA=IO_NOT_ALLOWED
   call io_disconnect(ID,FORCE=.TRUE.)
   return
 endif
 !
 call timing('io_MPA',OPR='start')
 !
 different_db_RL_order=.false.
 !
 ID_frag=0
 !
 io_extension(ID)='mpa_ER'
 !
 iq=0
 do i1=1,size(io_sec(ID,:))
   if (io_sec(ID,i1)==0) cycle
   iq=io_sec(ID,i1)/2
 enddo
 !
 io_MPA=io_connect(desc=trim(io_extension(ID)),type=2,ID=ID,FORCE_READ_MODE=iq>0.and.frag_RESPONSE)
 if (io_MPA/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   ! Header
   !
   io_MPA=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.false.,FRAG=frag_MULTIPOLE)
   if (io_MPA/=0) goto 1
   !
   ! Global Descriptions
   !=====================
   !
   io_MPA=variables_MPA(ID,MPA_desc,X)
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
 if (iq==0) goto 1
 !
 call io_fragment(ID,ID_frag,i_fragment=iq,ierr=io_err)
 io_MPA=io_err
 if (io_err/=0) goto 1
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
   io_com_save=io_com(ID_frag)
   if (iq>2) io_com(ID_frag)=NONE
   call def_variable_elemental(ID_frag,trim(ch),sec_size,SP,1,par_io_kind='independent')
   !
   call io_variable_elemental(ID_frag,  VAR=" :: Current Q-pt index     :",I0=iq)
   call io_variable_elemental(ID_frag,  VAR=" :: Number of poles  :",I0=Xw%n_freqs/2,CHECK=.true.,OP=(/"=="/))
   !
   call def_variable_elemental(ID_frag,"",0,0,1)
   io_MPA=io_status(ID_frag)
   !
   io_com(ID_frag)=io_com_save
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
   ! poles @iq
   !
   ch="MPA_E_Q_"//trim(intc(iq))
   !
   if(write_is_on(ID)) then
#if defined _PAR_IO
     call def_variable_bulk(ID_frag,trim(ch),1,(/2,X%ng_db,X%ng_db,Xw%n_freqs/),SP,par_io_kind='collective')
#else
     call def_variable_bulk(ID_frag,trim(ch),1,(/2,X%ng_db,X%ng_db,Xw%n_freqs/),SP,par_io_kind='independent')
#endif
     call  io_variable_bulk(ID_frag,1,C3=MPA_E_par(1)%blc(:,:,:Xw%n_freqs),IPOS=(/1,MPA_E_par(1)%rows(1),MPA_E_par(1)%cols(1),1/))
     !
   else if(read_is_on(ID)) then
     call def_variable_bulk(ID_frag,trim(ch),1,(/2,X%ng_db,X%ng_db,Xw%n_freqs/),SP)
     call  io_variable_bulk(ID_frag,1,C3=MPA_E_par(1)%blc(:,:,:Xw%n_freqs),IPOS=(/1,MPA_E_par(1)%rows(1),MPA_E_par(1)%cols(1),1/))
   endif
   !
   ! residues @iq
   !
   ch="MPA_R_Q_"//trim(intc(iq))
   !
   if(write_is_on(ID)) then
#if defined _PAR_IO
     call def_variable_bulk(ID_frag,trim(ch),1,(/2,X%ng_db,X%ng_db,Xw%n_freqs/),SP,par_io_kind='collective')
#else
     call def_variable_bulk(ID_frag,trim(ch),1,(/2,X%ng_db,X%ng_db,Xw%n_freqs/),SP,par_io_kind='independent')
#endif
     call  io_variable_bulk(ID_frag,1,C3=MPA_R_par(1)%blc(:,:,:Xw%n_freqs),IPOS=(/1,MPA_R_par(1)%rows(1),MPA_R_par(1)%cols(1),1/))
     !
   else if(read_is_on(ID)) then
     call def_variable_bulk(ID_frag,trim(ch),1,(/2,X%ng_db,X%ng_db,Xw%n_freqs/),SP)
     call  io_variable_bulk(ID_frag,1,C3=MPA_R_par(1)%blc(:,:,:Xw%n_freqs),IPOS=(/1,MPA_R_par(1)%rows(1),MPA_R_par(1)%cols(1),1/))
   endif
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
1 call io_fragment_disconnect(ID,ID_frag)
 call io_disconnect(ID)
 !
 call timing('io_MPA',OPR='stop')
 !
end function
!
integer function variables_MPA(ID,local_desc,X)
 !
 use pars,          ONLY:schlen
 use descriptors,   ONLY:IO_desc,IO_desc_reset
 use X_m,           ONLY:X_t
 use IO_int,        ONLY:io_elemental
 use IO_m,          ONLY:io_status
 implicit none
 !
 integer            ::ID
 type(IO_desc)      ::local_desc
 type(X_t)          ::X
 !
 ! Work Space
 !
 character(schlen)   ::MPA_label
 !
 call IO_desc_reset(local_desc)
 MPA_label='MPA_'
 !
 call io_elemental(ID,VAR=trim(MPA_label)//"PARS_1",VAR_SZ=2,MENU=0)
 call io_elemental(ID,DB_I0=X%ng_db,&
&     VAR="X matrix size         :",I0=X%ng,CHECK=.true.,OP=(/"<="/),DESCRIPTOR=local_desc)
 call io_elemental(ID,&
&     VAR="MPA npoles            :",I0=X%mpa_npoles,MENU=0,DESCRIPTOR=local_desc)
 !
 variables_MPA=io_status(ID)
 !
end function
