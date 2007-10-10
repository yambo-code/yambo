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
 integer function ioWF(ID,wf)
 !
 use pars,             ONLY:SP,schlen
 use electrons,        ONLY:n_bands,n_spin
 use R_lattice,        ONLY:nkibz
 use wave_func,        ONLY:wf_ncx
 use IO_m,             ONLY:io_connect, io_disconnect, io_sec, &
&                           io_bulk,io_header,&
&                           io_fragment,RD_CL_IF_END,RD_CL,io_action
 implicit none
 integer,       intent(in) :: ID
 real(SP),optional         :: wf(:,:,:,:)
 !
 integer :: ik
 character(schlen)         :: VAR_NAME
 !
 ik=maxval(io_sec(ID,:))-1
 !
 ioWF=io_connect(desc="wf",type=0,ID=ID)
 if (ioWF/=0) goto 1
 !
 ! S/N
 !
 if (any((/io_sec(ID,:)==1/))) then
   ioWF=io_header(ID,IMPOSE_SN=.true.)
   if (ioWF/=0) goto 1
 endif
 !
 if (ik==0) goto 1
 !
 ! Manage RD_CL_IF_END
 !
 if (io_action(ID)==RD_CL_IF_END.and.ik==nkibz) io_action(ID)=RD_CL
 !
 ! Fragmentation
 !
 call io_fragment(ID,i_pointer=ik)
 !
 write (VAR_NAME,'(a,i6.6)') 'WF_IK_RE_',ik
 call io_bulk(ID,VAR=trim(VAR_NAME),VAR_SZ=(/n_bands,wf_ncx,n_spin/))
 call io_bulk(ID,R3=wf(1,:,:,:))
 !
 write (VAR_NAME,'(a,i6.6)') 'WF_IK_IM_',ik
 call io_bulk(ID,VAR=trim(VAR_NAME),VAR_SZ=(/n_bands,wf_ncx,n_spin/))
 call io_bulk(ID,R3=wf(2,:,:,:))
 !
1 call io_disconnect(ID=ID)
 !
 end function ioWF
