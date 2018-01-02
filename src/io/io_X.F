!
!        Copyright (C) 2000-2018 the YAMBO team
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
integer function io_X(X,Xw,ID)
 !
 use pars,          ONLY:SP,schlen
 use units,         ONLY:HA2EV
 use R_lattice,     ONLY:nqibz,g_vec
 use X_m,           ONLY:X_t,X_mat,X_RLcomp_ig,X_description,X_alloc
 use frequency,     ONLY:w_samp
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,io_com,&
&                        io_elemental,io_status,io_bulk,&
&                        read_is_on,write_is_on,io_header,&
&                        manage_OP_IF_START_and_CL_IF_END,&
&                        ver_is_gt_or_eq,io_serial_number,serial_number,&
&                        NONE,IO_INCOMPATIBLE_VAR,io_RESPONSE,IO_NOT_ALLOWED,&
&                        io_fragment,IO_OUTDATED_DB,frag_RESPONSE,&
&                        IO_NO_DATABASE,io_mode,DUMP
 use stderr,        ONLY:intc
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
#include<memory.h>
 type(X_t)   :: X
 type(w_samp):: Xw
 integer     :: ID
 !
 !Work Space
 !
 integer             ::sec_size,i1,iq,i_err,io_com_save,io_err
 logical             ::different_db_RL_order
 character (schlen)  ::ch,db_desc
 character (2)       ::local_XC_KIND
 real(SP),   allocatable :: RL_vecs_disk(:,:)
 complex(SP),allocatable :: X_on_disk(:,:)
 !
#if defined _TIMING
 if (io_RESPONSE) call timing('io_X',OPR='start')
#endif
 !
 if (.not.io_RESPONSE) then
   io_X=IO_NOT_ALLOWED
   call io_disconnect(ID,FORCE=.TRUE.)
   return
 endif
 !
 ! I assume that he RL ordering used to build X is not different
 ! from the one in DB1
 !
 different_db_RL_order=.false.
 !
 if (X%whoami==1) db_desc='Xx'
 if (X%whoami==2) db_desc='em1s'
 if (X%whoami==3) db_desc='em1d'
 if (X%whoami==4) db_desc='pp'
 !
 io_X=io_connect(desc=trim(db_desc),type=2,ID=ID) 
 if (io_X/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   ! Header
   !
   if (X%whoami==1) local_XC_KIND="Xx"
   if (X%whoami==2) local_XC_KIND="Xs"
   if (X%whoami==3) local_XC_KIND="Xd"
   if (X%whoami==4) local_XC_KIND="Xp"
   io_X=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,T_EL=.true.,XC_KIND=local_XC_KIND,&
&                    CUTOFF=.true.,GAUGE=.TRUE.,FRAG=frag_RESPONSE)
   if (io_X/=0) goto 1
   !
   if (.not.ver_is_gt_or_eq(ID,revision=308)) then
     io_X=IO_OUTDATED_DB
     goto 1
   endif
   !
   ! Global Descriptions
   !=====================
   !
   call variables_X(ID,X_description,X)
   !
   ! When the SN is different and I am importing this DB 
   ! from a different calculation I check the RL vectors
   !
   YAMBO_ALLOC(RL_vecs_disk,(X%ng_db,3))
   !
   if (write_is_on(ID)) RL_vecs_disk(:,:)=g_vec(:X%ng_db,:)
   !
   call io_bulk(ID,VAR='X_RL_vecs',VAR_SZ=shape(RL_vecs_disk))
   call io_bulk(ID,R2=RL_vecs_disk)
   !
   if (io_serial_number(ID)/=serial_number.and.read_is_on(ID).and.allocated(g_vec)) then
     YAMBO_ALLOC(X_RLcomp_ig,(X%ng))
     i_err = X_RLcomp_table() 
     if ( i_err < 0 ) io_status(ID) = IO_INCOMPATIBLE_VAR
     if ( i_err > 0 ) different_db_RL_order=.true.
   endif
   YAMBO_FREE(RL_vecs_disk)
   !
   ! Close & Menu lower line
   !
   if (.not.any((/io_sec(ID,:)==2/))) then
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
     io_X=io_status(ID)
     if (io_X/=0) goto 1
   else
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
 endif
 !
 do i1=1,size(io_sec(ID,:))
   if (io_sec(ID,i1)==0) cycle
   iq=io_sec(ID,i1)/2
 enddo
 if (iq==0) goto 1
 !
 ! Manage OP_IF_START_and_RD_CL_IF_END
 !
 call manage_OP_IF_START_and_CL_IF_END(ID,iq,nqibz)
 !
 ! Fragmentation
 !
 call io_fragment(ID,i_fragment=iq,ierr=io_err)
 !
 !  type(X_t)    ::X(4)  x s d p
 !
 if (any((/io_sec(ID,:)==2*iq/))) then
   !
   sec_size=6
   !
   if (X%whoami==4) sec_size=1
   ch="FREQ_PARS_sec_iq"//trim(intc(iq))
   !
   io_com_save=io_com(ID)
   if (iq>2 .and. trim(db_desc)/="em1d") io_com(ID)=NONE
   call io_elemental(ID,VAR=trim(ch),VAR_SZ=sec_size,MENU=1)
   !
   if (X%whoami==4) then
     call io_elemental(ID,UNIT=HA2EV,&
&         VAR=" PPA Im  energy     [ev]:",R0=X%ppaE,CHECK=.true.,OP=(/"=="/))
   endif
   if (X%whoami/=4) then
     call io_elemental(ID,&
&         VAR=" :: Current Q-pt index     :",I0=iq)
     call io_elemental(ID,UNIT=HA2EV,&
&         VAR=" :: X energy range     [ev]:",R1=Xw%er,CHECK=.true.,OP=(/"==","=="/))
     call io_elemental(ID,UNIT=HA2EV,&
&         VAR=" :: X damping range    [ev]:",R1=Xw%dr,CHECK=.true.,OP=(/"==","=="/))
     call io_elemental(ID,&
&         VAR=" :: Number of frequencies  :",I0=Xw%n_freqs,CHECK=.true.,OP=(/"=="/))
   endif
   !
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   io_X=io_status(ID)
   !
   io_com(ID)=io_com_save
   !
   if (io_X/=0) goto 1
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
   call io_bulk(ID,VAR=trim(ch),VAR_SZ=(/2,Xw%n_freqs/))
   call io_bulk(ID,C1=Xw%p)
   !
   ! X_mat @iq
   !
   ch="X_Q_"//trim(intc(iq))
   !
   if (.not.allocated(X_mat).and.read_is_on(ID)) call X_alloc('X',(/X%ng,X%ng,Xw%n_freqs/))
   !
   call io_bulk(ID,VAR=trim(ch),VAR_SZ=(/2,X%ng_db,X%ng_db,Xw%n_freqs/))
   call io_bulk(ID,C3=X_mat(:,:,:Xw%n_freqs),IPOS=(/1,1,1,1/))
   !
   if (read_is_on(ID) .and. different_db_RL_order) then 
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
 ! Now that the menu is closed I can return with io_X/=0 if there is a restart point
 !
 if (io_mode(ID)/=DUMP) then
   io_X=IO_NO_DATABASE
   if (io_err==0) io_X=iq
 endif
 !
1 call io_disconnect(ID)
 !
#if defined _TIMING
 if (io_RESPONSE) call timing('io_X',OPR='stop')
#endif
 !
 contains
   !
   integer function X_RLcomp_table()
   !--------------------------------
   !
   ! X has been written using a different RL vectors ordering.
   ! X_RLcomp_ig is defined in such a way that
   !
   ! X_( X_RLcomp_ig(i), X_RLcomp_ig(j) ) = X_on_disk_(i,j)
   !
   use vec_operate,   ONLY:v_is_zero
   use R_lattice,     ONLY:ng_in_shell,n_g_shells
   use zeros,         ONLY:define_zeros
   !
   integer :: is,ig1,ig2,g_limits(2)
   real(SP):: v(3),local_zero(3)
   !
   X_RLcomp_table=0
   X_RLcomp_ig=0
   !
   do is=1,n_g_shells
    if (ng_in_shell(is) > X%ng) then
      if (any(X_RLcomp_ig==0)) X_RLcomp_table=-1
      return
    endif
    !
    g_limits=1
    if (is > 1 ) g_limits = (/ ng_in_shell(is-1), ng_in_shell(is) /)
    !
    call define_zeros(vector_=g_vec,zero_=local_zero)
    !
    g_loop: do ig1=g_limits(1),g_limits(2)
      do ig2=g_limits(1),g_limits(2)
        !
        v=RL_vecs_disk(ig1,:)-g_vec(ig2,:)
        !
        if (v_is_zero(v,zero_=local_zero)) then
          X_RLcomp_ig(ig1)=ig2
          cycle g_loop
        endif
        !
      enddo
    enddo g_loop
   enddo
   !
   end function
   !
end function
