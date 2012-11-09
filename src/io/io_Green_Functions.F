!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
integer function io_Green_Functions(ID)
 !
 use pars,           ONLY:SP
 use units,          ONLY:FS2AUT
 use IO_m,           ONLY:io_connect,io_disconnect,io_header,io_sec,io_elemental,&
&                         io_action,RD_CL_IF_END,RD_CL,WR_CL,WR_CL_IF_END,&
&                         write_is_on,io_bulk
 use real_time,      ONLY:Gr,Gr_kind,Gr_bands,Gr_T_steps,Gr_T_window
 use R_lattice,      ONLY:nkibz
 use stderr,         ONLY:intc
 !
 implicit none
 integer     ::ID
 !
 ! Work Space
 !
 integer              :: i_k
 real(SP),allocatable :: Gr_disk(:,:,:)
 !
 io_Green_Functions=io_connect(desc="Retarded_Green_Function",type=2,ID=ID)
 if (io_Green_Functions/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_Green_Functions=io_header(ID,XC_KIND="G")
   !
   !use real_time,      ONLY:Gr,Gr_kind,Gr_bands,Gr_T_steps,Gr_T_window
   !
   call io_elemental(ID,VAR="PARAMETERS",VAR_SZ=4,MENU=0)
   !
   call io_elemental(ID,I1=Gr_bands,&
&       VAR=' Bands                           :',CHECK=.true.,OP=(/"==","=="/))
   call io_elemental(ID,R0=Gr_T_window,&
&       VAR=' Time range                  [fs]:',UNIT=1./FS2AUT,CHECK=.true.,OP=(/"==","=="/))
   call io_elemental(ID,I0=Gr_T_steps,&
&       VAR=' Time steps                      :',UNIT=FS2AUT,CHECK=.true.,OP=(/"==","=="/))
   !
   call io_elemental(ID,VAR=" ",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR='G_retarded_KIND',CH0="",VAR_SZ=1,MENU=0)
   call io_elemental(ID,CH0=Gr_kind,&
&       VAR=' Retarded Green`s function       :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0)
   !
 endif
 !
 i_k=maxval(io_sec(ID,:))-1
 !
 if (i_k==0) goto 1
 !
 allocate(Gr_disk(Gr_bands(2),Gr_T_steps,2))
 !
 call io_bulk(ID,VAR="G_retarded_k"//trim(intc(i_k)),VAR_SZ=(/Gr_bands(2),Gr_T_steps,2/))
 if(write_is_on(ID)) then
   Gr_disk(:,:,1) =real(Gr(i_k,:,:))
   Gr_disk(:,:,2)=aimag(Gr(i_k,:,:))
   call io_bulk(ID,R3=Gr_disk)
 else
   call io_bulk(ID,R3=Gr_disk)
   Gr(i_k,:,:)=CMPLX(Gr_disk(:,:,1),Gr_disk(:,:,2))
 endif
 !
 deallocate(Gr_disk)
 !
 if (io_action(ID)==RD_CL_IF_END.and.i_k==nkibz) io_action(ID)=RD_CL
 !
1 call io_disconnect(ID=ID)
 !
end function
