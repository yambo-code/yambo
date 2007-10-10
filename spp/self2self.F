!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
integer function self2self(instr,en,k,q)
 !
 ! s/ns.db1 -> SAVE
 ! s/ns.db2 -> SAVE
 !
 use pars,             ONLY:SP
 use IO_m,             ONLY:io_control,OP_RD_CL,DUMP,OP_WR_CL,&
&                           OP_RD,RD_CL,OP_APP_WR_CL,RD,dbs_fragment,write_to_dbs
 use electrons,        ONLY:levels,n_bands,E_reset,n_spin
 use D_lattice,        ONLY:dl_sop
 use R_lattice,        ONLY:g_vec,bz_samp,nkibz,nqibz,nkbz,nXkbz,q_pt,&
&                           qindx_free,bz_samp_reset
 use wave_func,        ONLY:wf_nc_k,wf_igk,wf_ncx,wf_ng,ioWF
 use com,              ONLY:code_version,msg,error
 use X_m,              ONLY:X_t,X_reset,X_mat
 use frequency,        ONLY:w_samp,W_reset
 use par_proc_m,       ONLY:pp_redux_wait,pp_indexes,ncpu,pp_indexes_reset,master_node
 use par_indexes_m,    ONLY:par_indexes
 implicit none
 type(levels)  :: en
 type(bz_samp) :: k,q
 character(*)  :: instr
 ! 
 ! Work Space
 !
 integer          ::ik,iq,i_type
 type(X_t)        ::X
 type(w_samp)     ::Xw
 type(pp_indexes) ::px
 real(SP), allocatable :: wf_disk(:,:,:,:)
 !
 !I/O
 !
 integer           :: ID,ID_WF,ID_KB_PP,ID_X,io_err(4)
 integer, external :: ioDB1,ioDB1_ver_2,ioWF_ver_2,&
&                     ioX_ver_2,ioQINDX,ioX,ioKB_PP,ioKB_PP_ver_2
 !
 include 'version.inc'
 self2self=0
 call pp_indexes_reset(px)
 !
 if (index(instr,'s2s')==0) return
 !
 call section('*','Version 2 databases trasformation ')
 !
 ! DB1/WF/KB_PP
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),MODE=DUMP,ID=ID)
 io_err(1)=ioDB1(en,k,ID)
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),MODE=DUMP,ID=ID)
 io_err(2)=ioWF(ID)
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),MODE=DUMP,ID=ID)
 io_err(3)=ioKB_PP(ID)
 !
 ! DB1 
 !
 if (io_err(1)==0) goto 2
 call pp_redux_wait()
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),MODE=DUMP,ID=ID)
 io_err(1)=ioDB1_ver_2(en,k,ID)
 if (io_err(1)==0) then
   call io_control(ACTION=OP_WR_CL,SEC=(/1,2/),ID=ID)
   io_err(1)=ioDB1(en,k,ID)
   call msg('l',' [DB1] ')
 endif
 deallocate(en%E,g_vec,k%pt,dl_sop,wf_igk)
 !
 ! With NCPU > 1 I must return and restart to reload the DB1
 !
 if (ncpu>1) then
   self2self=-1
   return
 endif
 !
 ! WF
 !
2 call pp_redux_wait()
 if (io_err(2)==0) goto 3 
 allocate(wf_disk(2,n_bands,wf_ncx,n_spin))
 !
 write_to_dbs=dbs_fragment.or.master_node
 !
 call par_indexes(px,(/nkibz/))
 do ik=1,nkibz
   !
   if (ik==1    ) call io_control(ACTION=OP_RD,MODE=DUMP,SEC=(/ik/),ID=ID)
   if (ik> 1    ) call io_control(ACTION=RD,MODE=DUMP,SEC=(/ik/),ID=ID)
   if (ik==nkibz) call io_control(ACTION=RD_CL,MODE=DUMP,SEC=(/ik/),ID=ID)
   io_err(1)=ioWF_ver_2(ID,wf_disk)
   if (io_err(1)/=0) cycle
   !
   if (dbs_fragment.and..not.px%i1p(ik)) cycle
   !
   if (ik==1) call io_control(ACTION=OP_WR_CL,SEC=(/1,2/),ID=ID_WF)
   if (ik> 1) call io_control(ACTION=OP_APP_WR_CL,SEC=(/ik+1/),ID=ID_WF)
   io_err(1)=ioWF(ID_WF,wf_disk)
   !
 enddo
 call msg('l',' [DB2] ')
 call pp_indexes_reset(px)
 !
 write_to_dbs=master_node
 !
 deallocate(wf_disk,wf_nc_k)
 !
 ! KB_PP
 !
3 call pp_redux_wait()
 if (io_err(3)==0) goto 4 
 wf_ng=wf_ncx
 do ik=1,nkibz
   !
   if (ik==1    ) call io_control(ACTION=OP_RD,MODE=DUMP,SEC=(/1,2/),ID=ID)
   if (ik> 1    ) call io_control(ACTION=RD,MODE=DUMP,SEC=(/ik+1/),ID=ID)
   if (ik==nkibz) call io_control(ACTION=RD_CL,MODE=DUMP,SEC=(/ik+1/),ID=ID)
   io_err(1)=ioKB_PP_ver_2(ID)
   if (io_err(1)/=0) goto 4
   !
   if (ik==1) call io_control(ACTION=OP_WR_CL,SEC=(/1,2/),ID=ID_KB_PP)
   if (ik> 1) call io_control(ACTION=OP_APP_WR_CL,SEC=(/ik+1/),ID=ID_KB_PP)
   io_err(1)=ioKB_PP(ID_KB_PP)
   !
 enddo
 call msg('l',' [KB_PP] ')
 !
 ! KINDX
 !
4 self2self=-1
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2,3/),MODE=DUMP,ID=ID)
 io_err(1)=ioQINDX(k,q,ID)
 if (io_err(1)/=0) return
 !
 ! EPSM1 
 !
 io_err=-1
 do i_type=1,4
   if (i_type==1.or.i_type==3) cycle
   call X_reset(X,i_type)
   call W_reset(Xw)
   call io_control(ACTION=OP_RD_CL,MODE=DUMP,SEC=(/1/),ID=ID)
   io_err(i_type)=ioX_ver_2(X,Xw,ID)
 enddo
 if (all((/io_err(:4)<0/))) goto 5
 !
 allocate(q_pt(q%nibz,3))
 q_pt=q%pt
 nkbz=nXkbz
 do i_type=1,4
   if (i_type==1.or.i_type==3) cycle
   call X_reset(X,i_type)
   call W_reset(Xw)
   call io_control(ACTION=OP_RD,MODE=DUMP,SEC=(/1/),ID=ID)
   io_err(1)=ioX_ver_2(X,Xw,ID)
   if (io_err(1)<0) cycle
   !
   call par_indexes(px,(/nqibz/))
   write_to_dbs=dbs_fragment.or.master_node
   !
   do iq=1,nqibz
     call io_control(ACTION=RD,MODE=DUMP,SEC=(/iq+1/),ID=ID)
     if (iq==nqibz) call io_control(ACTION=RD_CL,MODE=DUMP,SEC=(/iq+1/),ID=ID)
     io_err(1)=ioX_ver_2(X,Xw,ID)
     if (dbs_fragment.and..not.px%i1p(iq)) cycle
     if (iq==1) then
       call io_control(ACTION=OP_WR_CL,SEC=(/1,2,3/),ID=ID_X)
     else
       call io_control(ACTION=OP_APP_WR_CL,SEC=(/2*iq,2*iq+1/),ID=ID_X)
     endif
     io_err=ioX(X,Xw,ID_X)
     if (iq==nqibz) deallocate(X_mat,Xw%p)
   enddo
   if (i_type==2) call msg('l',' [EM1S] ')
   if (i_type==4) call msg('l',' [PP] ')
   call par_indexes(px,(/nqibz/))
   write_to_dbs=master_node
 enddo
 !
 ! CLEAN
 !
5 self2self=-1
 call qindx_free()
 call bz_samp_reset(k)
 call bz_samp_reset(q)
 call E_reset(en)
 !
end function
