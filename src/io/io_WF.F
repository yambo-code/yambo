!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
integer function io_WF(ID,wf)
 !
 use pars,             ONLY:SP,schlen
 use electrons,        ONLY:n_sp_pol,n_spinor
 use stderr,           ONLY:intc
 use R_lattice,        ONLY:nkibz
 use wave_func,        ONLY:wf_ncx,wf_nb_io,wf_nb_to_load,wf_b_indx,wf_nb_io_groups
 use IO_m,             ONLY:io_connect, io_disconnect, io_sec, &
&                           io_bulk,io_header,ver_is_gt_or_eq,&
&                           io_action,io_fragment,io_elemental,frag_WF,&
&                           manage_OP_IF_START_and_CL_IF_END
#if defined _TIMING
 use timing_m,         ONLY:timing
#endif
 implicit none
 integer,       intent(in) :: ID
 complex(SP),   optional   :: wf(:,:,:)
 !
 integer                   :: nb_io,i_sp_pol,ik,ifrag,ib_grp,ib,ib_io,head_io
 character(schlen)         :: VAR_NAME
 !
#if defined _TIMING
 call timing('io_WF',OPR='start')
#endif
 !
 head_io=0
 select case ( count((/io_sec(ID,:)/=0/)) )
   case(1)
     ifrag=0
     ib_grp=0
     head_io=1
   case(2)
     ifrag=io_sec(ID,1)-1
     ib_grp=io_sec(ID,2)
   case(3)
     ifrag=io_sec(ID,2)-1
     ib_grp=io_sec(ID,3)
     head_io=1
 end select
 !
 if(ifrag<=nkibz) then
   ik=ifrag
   i_sp_pol=1
 else
   ik=ifrag-nkibz
   i_sp_pol=2
 endif
 !
 io_WF=io_connect(desc="wf",type=0,ID=ID)
 if (io_WF/=0) goto 1
 !
 ! S/N
 !
 if (head_io==1) then
   !
   io_WF=io_header(ID,IMPOSE_SN=.true.,FRAG=frag_WF)
   !
   call io_elemental(ID, VAR="BAND_GROUPS",VAR_SZ=2,MENU=0)
   call io_elemental(ID, VAR=" Bands in each block             :",I0=wf_nb_io)
   call io_elemental(ID, VAR=" Blocks                          :",I0=wf_nb_io_groups)
   call io_elemental(ID, VAR="",VAR_SZ=0)
   !
   if (io_WF/=0) goto 1
   !
 endif
 !
 if (ifrag==0) goto 1
 !
 ! Manage RD_CL_IF_END
 !
 call manage_OP_IF_START_and_CL_IF_END(ID,ifrag,nkibz*n_sp_pol)
 !
 ! Fragmentation
 !
 call io_fragment(ID,i_fragment=ifrag,j_fragment=ib_grp)
 !
 if(.not.allocated(wf_b_indx)) nb_io=wf_nb_io
 if(     allocated(wf_b_indx)) nb_io=wf_nb_to_load
 !
 VAR_NAME="WF_COMPONENTS_@_SP_POL"//trim(intc(i_sp_pol))//"_K"//trim(intc(ik))//"_BAND_GRP_"//trim(intc(ib_grp))
 call io_bulk(ID,VAR=trim(VAR_NAME),VAR_SZ=(/2,wf_ncx,n_spinor,wf_nb_io/))
 !
 if(nb_io==wf_nb_io) then
   !
   call io_bulk(ID,C3=wf)
   !
 else
   !
   do ib_io=1,nb_io
     if(.not.allocated(wf_b_indx)) ib=ib_io
     if(     allocated(wf_b_indx)) ib=wf_b_indx(ib_io)
     call io_bulk(ID,C3=wf(:,:,ib_io:ib_io),IPOS=(/1,1,1,ib/))
   enddo
   !
 endif
 !
1 call io_disconnect(ID=ID)
 !
#if defined _TIMING
 call timing('io_WF',OPR='stop')
#endif
 !
end function io_WF
