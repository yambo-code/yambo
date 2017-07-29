!
!        Copyright (C) 2000-2017 the YAMBO team
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
integer function io_WF(ID,wf)
 !
 use pars,             ONLY:SP,schlen
 use electrons,        ONLY:n_bands,n_spin
 use stderr,           ONLY:intc
 use wave_func,        ONLY:wf_ncx,wf_nb_io,wf_nb_io_groups
 use IO_m,             ONLY:io_connect, io_disconnect, io_sec,io_bulk,io_header,ver_is_gt_or_eq,&
&                           io_fragment,io_elemental,io_resetable,frag_WF
#if defined _TIMING
 use timing_m,         ONLY:timing
#endif
 implicit none
 integer,       intent(in) :: ID
 real(SP),      optional   :: wf(:,:,:,:)
 !
 integer                   :: ik,ib_grp,head_io
 character(schlen)         :: VAR_NAME
 !
#if defined _TIMING
 call timing('io_WF',OPR='start')
#endif
 !
 head_io=0
 select case ( count((/io_sec(ID,:)/=0/)) )
   case(0)
     io_resetable(ID)=.true.
     goto 1
   case(1)
     ik=0
     ib_grp=0
     head_io=1
   case(2)
     ik=io_sec(ID,1)-1
     ib_grp=io_sec(ID,2)
   case(3)
     ik=io_sec(ID,2)-1
     ib_grp=io_sec(ID,3)
     head_io=1
 end select
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
   if (ver_is_gt_or_eq(ID,(/3,1,1/))) then
     call io_elemental(ID, VAR="BAND_GROUPS",VAR_SZ=2,MENU=0)
     call io_elemental(ID,VAR=&
&         ' Bands in each block             :',I0=wf_nb_io)
     call io_elemental(ID,VAR=&
&         ' Blocks                          :',I0=wf_nb_io_groups)
     call io_elemental(ID,VAR="",VAR_SZ=0)
   endif
   !
   if (io_WF/=0) goto 1
   !
 endif
 !
 if (ik==0) goto 1
 !
 if (ver_is_gt_or_eq(ID,(/3,1,1/))) then
   !
   !##################################################################
   ! BANDS BLOCKS (andrea 21/4/2008) VERSION >= 3.1.1
   !##################################################################
   !
   ! Fragmentation
   !
   call io_fragment(ID,i_fragment=ik,j_fragment=ib_grp)
   !
   VAR_NAME="WF_REAL_COMPONENTS_@_K"//trim(intc(ik))//"_BAND_GRP_"//trim(intc(ib_grp))
   call io_bulk(ID,VAR=trim(VAR_NAME),VAR_SZ=(/wf_nb_io,wf_ncx,n_spin/))
   call io_bulk(ID,R3=wf(1,:,:,:))
   !
   VAR_NAME="WF_IM_COMPONENTS_@_K"//trim(intc(ik))//"_BAND_GRP_"//trim(intc(ib_grp))
   call io_bulk(ID,VAR=trim(VAR_NAME),VAR_SZ=(/wf_nb_io,wf_ncx,n_spin/))
   call io_bulk(ID,R3=wf(2,:,:,:))
   !
 else
   !
   ! Fragmentation
   !
   call io_fragment(ID,i_fragment=ik)
   !
   write (VAR_NAME,'(a,i6.6)') 'WF_IK_RE_',ik
   call io_bulk(ID,VAR=trim(VAR_NAME),VAR_SZ=(/n_bands,wf_ncx,n_spin/))
   call io_bulk(ID,R3=wf(1,:,:,:))
   !
   write (VAR_NAME,'(a,i6.6)') 'WF_IK_IM_',ik
   call io_bulk(ID,VAR=trim(VAR_NAME),VAR_SZ=(/n_bands,wf_ncx,n_spin/))
   call io_bulk(ID,R3=wf(2,:,:,:))
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
