!
!        Copyright (C) 2000-2020 the YAMBO team
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
integer function io_BS_Fxc(iq,FXC_W,ID,X)
 !
 use pars,           ONLY:schlen,lchlen,IP,SP
 use units,          ONLY:HA2EV
 use stderr,         ONLY:intc
 use BS,             ONLY:BS_K_dim
 use BS_solvers,     ONLY:BSS_q0,variables_BS
 use TDDFT,          ONLY:FXC_n_g_corr,FXC_desc,&
&                         FXC_K_diagonal,FXC_n_mem_freqs,FXC,FXC_is_retarded
 use frequency,      ONLY:w_samp
 use X_m,            ONLY:X_t
 use IO_m,           ONLY:io_connect,io_disconnect,io_sec,&
&                         io_elemental,def_variable_elemental,io_variable_elemental,&
&                         io_status,io_bulk,io_mode,&
&                         read_is_on,io_header,DUMP,&
&                         close_is_on,RD_CL,io_action,&
&                         IO_desc,IO_desc_duplicate,IO_desc_reset
#include<memory.h>
 type(w_samp)        ::FXC_W
 integer             ::ID,iq
 type(X_t), optional ::X 
 ! 
 ! Work Space
 !
 integer             ::i1,FXC_n_g_db,w_group
 character (schlen)  ::db_name,ch
 type(IO_desc)       ::local_desc
 type(X_t)           ::X_dummy
 !
 w_group=1
 if (maxval(io_sec(ID,:))>2.and.FXC_W%n_freqs>1) then
   w_group=maxval(io_sec(ID,:)-2)/FXC_n_mem_freqs+1
 endif
 !
 db_name='BS_FXC_Q'//trim(intc(iq))//'_W_group_'//trim(intc(w_group))
 !
 io_BS_Fxc=io_connect(desc=trim(db_name),type=2,ID=ID)
 !
 if (io_BS_Fxc/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   ! As this DB must respect the BS identifier I only need to
   ! check the K QP corrections
   !
   io_BS_Fxc=io_header(ID,XC_KIND="K_E force",CUTOFF=.true.)
   !
   ! Exit if header is wrong
   !
   if (io_BS_Fxc/=0) goto 1
   !
   call def_variable_elemental(ID,"FXC_parameters",11,IP+100,0)
   !
   call io_variable_elemental(ID,DB_I0=FXC_n_g_db,&
&       VAR=" FXC|Matrix size              ",I0=FXC_n_g_corr,CHECK=.true.,OP=(/"<="/),DESCRIPTOR=FXC_desc)
   call io_variable_elemental(ID,&
&       VAR="    |Kernel is causal         ",L0=FXC_is_retarded,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=FXC_desc)
   call io_variable_elemental(ID,UNIT=HA2EV,&
&       VAR="    |Energy range        [ev] ",R1=FXC_W%er,CHECK=.true.,OP=(/"==","=="/),DESCRIPTOR=FXC_desc)
   call io_variable_elemental(ID,UNIT=HA2EV,&
&       VAR="    |Damping range       [ev] ",R1=FXC_W%dr,CHECK=.true.,OP=(/"==","=="/),DESCRIPTOR=FXC_desc)
   call io_variable_elemental(ID,&
&       VAR="    |Total Energy steps       ",I0=FXC_W%n_freqs,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=FXC_desc)
   call io_variable_elemental(ID,&
&       VAR="    |Memory Energy steps      ",I0=FXC_n_mem_freqs,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=FXC_desc)
   call io_variable_elemental(ID,&
&       VAR="    |Field direction          ",R1=BSS_q0,CHECK=.true.,OP=(/"==","==","=="/),DESCRIPTOR=FXC_desc)
   !
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   io_BS_Fxc=io_status(ID)
   !
   ! Add BS variables
   !
   if (io_BS_Fxc==0) then
     if (present(X)) then
       io_BS_Fxc=variables_BS(ID,iq,local_desc,.TRUE.,X=X) 
     else
       i1=io_mode(ID)
       io_mode(ID)=DUMP
       io_BS_Fxc=variables_BS(ID,iq,local_desc,.TRUE.,X=X_dummy) 
       io_mode(ID)=i1
     endif
   endif
   !
   ! Create FXC description fields
   !
   call IO_desc_duplicate(local_desc,FXC_desc)
   !
   ! Parameters check failed in section 1. In case both secs 1 and 2
   ! were planned to be read, we need to switch the action to RD_CL
   ! to close the unit in io_disconnect
   !
   if (io_BS_Fxc/=0) then
      call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
      call IO_desc_reset(FXC_desc)
      if (.not.close_is_on(ID)) io_action(ID)=RD_CL
      goto 1
   endif
   !
   if (maxval(io_sec(ID,:))==1) goto 1
   !
 endif
 !
 if (any((/io_sec(ID,:)==2/))) then
   !
   if (read_is_on(ID)) then
     YAMBO_FREE(FXC_W%p)
     YAMBO_ALLOC(FXC_W%p,(FXC_W%n_freqs))
   endif
   !
   call io_bulk(ID,VAR='FREQUENCIES',VAR_SZ=(/2,FXC_W%n_freqs/))
   call io_bulk(ID,C1=FXC_W%p)
   !
   if (.not.allocated(FXC_K_diagonal)) then
     YAMBO_ALLOC(FXC_K_diagonal,(BS_K_dim))
   endif
   !
   call io_bulk(ID,VAR='FXC_K_diagonal',VAR_SZ=(/BS_K_dim/))
   call io_bulk(ID,R1=FXC_K_diagonal)
   !
 endif
 !
 if (maxval(io_sec(ID,:))==2) goto 1
 !
 write (ch,'(a,i3.3)') "FXC_W_GROUP_",w_group
 !
 call io_bulk(ID,VAR=trim(ch),VAR_SZ=(/2,FXC_n_g_corr,FXC_n_g_corr,FXC_n_mem_freqs/))
 call io_bulk(ID,C3=FXC)
 !
1 call io_disconnect(ID)
 !
end function
