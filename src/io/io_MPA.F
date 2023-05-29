!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DALV, AF
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
 use units,         ONLY:HA2EV
 use R_lattice,     ONLY:nqibz,g_vec
 use X_m,           ONLY:X_t,X_mat,X_RLcomp_ig,MPA_desc
 use frequency,     ONLY:w_samp
 use IO_int,        ONLY:io_connect,io_disconnect,io_elemental,io_bulk,io_header,&
&                        def_variable_bulk,io_variable_bulk,def_variable_elemental,io_variable_elemental,&
&                        io_fragment,ver_is_gt_or_eq
 use IO_m,          ONLY:io_sec,io_com,io_status,read_is_on,write_is_on,io_serial_number,serial_number,&
&                        NONE,IO_INCOMPATIBLE_VAR,io_MULTIPOLE,IO_NOT_ALLOWED,io_extension,&
&                        IO_OUTDATED_DB,frag_MULTIPOLE,IO_NO_DATABASE,io_mode,DUMP,frag_RESPONSE
 use stderr,        ONLY:intc
 use timing_m,      ONLY:timing
 use mpa_m,         ONLY:MPA_E_mat,MPA_R_mat,MPA_freqs,MPA_alloc
 !
#include<memory.h>
 !
 type(X_t)   :: X
 type(w_samp):: Xw
 integer     :: ID
 !
 !Work Space
 !
 integer           :: sec_size,i1,iq,i_err,io_com_save,io_err,ID_frag
 logical           :: different_db_RL_order
 character(schlen) :: ch,db_desc
 character(2)      :: local_XC_KIND
 real(SP),   allocatable :: RL_vecs_disk(:,:)
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
   call def_variable_elemental(ID_frag,trim(ch),sec_size,SP,1)
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
   !
   call def_variable_bulk(ID_frag,trim(ch),1,(/2,shape(MPA_E_mat)/),SP)
   call  io_variable_bulk(ID_frag,1,C3=MPA_E_mat,IPOS=(/1,1,1,1/))
   !
   ! residues @iq
   !
   ch="MPA_R_Q_"//trim(intc(iq))
   !
   call def_variable_bulk(ID_frag,trim(ch),1,(/2,shape(MPA_R_mat)/),SP)
   call  io_variable_bulk(ID_frag,1,C3=MPA_R_mat,IPOS=(/1,1,1,1/))
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
!
integer function variables_MPA(ID,local_desc,X)
 !
 use pars,          ONLY:schlen,lchlen
 use units,         ONLY:HA2EV
 use descriptors,   ONLY:IO_desc,IO_desc_reset
 use X_m,           ONLY:X_t
 use IO_int,        ONLY:io_elemental,ver_is_gt_or_eq
 use IO_m,          ONLY:io_status,read_is_on
 implicit none
 !
 integer            ::ID
 type(IO_desc)      ::local_desc
 type(X_t)          ::X
 !
 ! Work Space
 !
 integer             ::i_local_desc,sec_size
 character(schlen)   ::X_label,MPA_label
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