!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA
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
integer function io_OSCLL(q,ID)
 !
 use pars,          ONLY:SP,schlen,IP_YIO
 use electrons,     ONLY:levels
 use matrix_operate,ONLY:mat_c2r,mat_r2c
 use R_lattice,     ONLY:nXkibz,bz_samp
 use QP_m,          ONLY:QP_nk
 use IO_int,        ONLY:io_connect,io_disconnect,io_header,io_fragment, &
&                        def_variable_bulk,io_variable_bulk,io_variable_elemental,def_variable_elemental    
 use IO_m,          ONLY:io_sec,frag_DIP,read_is_on,write_is_on,io_extension,&
&                        RD_CL_IF_END,RD_CL,io_action,IO_NOT_ALLOWED,io_DIP, &
&                        io_status,io_PAR_cpu
 use collision_ext, ONLY:COLL_bands,OSCLL,OSCLL_k,ng_oscll
 use parallel_m,    ONLY:PAR_Xk_ibz_index,PAR_Xk_nibz
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 !
#include<y_memory.h>
 !
 integer     ::ID
 type(bz_samp), intent(in) :: q
 !
 ! Work Space
 !
 integer :: ik,ik_mem,i_fragment,COLL_nbands,i_sp_pol,iqbz,ig,ib,ID_frag
 character(schlen)    :: VAR_name
 complex(SP),allocatable :: disk_data(:,:)
 !
#if defined _TIMING
 call timing('io_OSCLL',OPR='start')
#endif
 !
 ID_frag=0
 io_OSCLL=0
 !
 io_extension(ID)='OSCLL'
 io_OSCLL=io_connect(desc=trim(io_extension(ID)),type=2,ID=ID,FORCE_READ_MODE=(.not.any((/io_sec(ID,:)==1/))))
 !
 if (io_OSCLL/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_OSCLL=io_header(ID,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.)
   if (io_OSCLL/=0) goto 1
   !
   ! Variable for the Oscillators
   !
   call def_variable_elemental(ID,"ng_oscll",1,IP_YIO,0)
   call io_variable_elemental(ID,VAR="RL vectors in oscillators",I0=ng_oscll,CHECK=.true.,OP=(/"=="/))
   !
   call def_variable_elemental(ID,"COLL_bands",2,IP_YIO,0)
   call io_variable_elemental(ID,VAR="Oscillators bands range",I1=COLL_bands,CHECK=.true.,OP=(/"==","=="/))
   !
   io_OSCLL=io_status(ID)
   if (io_OSCLL/=0) goto 1
   !
 endif
 !
 i_fragment=maxval(io_sec(ID,:))-1
 !
 if(i_fragment<=nXkibz) then
   i_sp_pol=1
   ik=i_fragment
 else
   i_sp_pol=2
   ik=i_fragment-nXkibz
 endif
 !
 if (ik==0) goto 1
 !
 ik_mem=PAR_Xk_ibz_index(ik)
 !
 COLL_nbands=COLL_bands(2)-COLL_bands(1)+1
 YAMBO_ALLOC(disk_data,(COLL_nbands,q%nbz))
 !
 if (read_is_on(ID)) then
   if(.not.allocated(OSCLL)) then
     YAMBO_ALLOC(OSCLL,(COLL_bands(1):COLL_bands(2),COLL_bands(1):COLL_bands(2),PAR_Xk_nibz,q%nbz,ng_oscll))
   endif
 endif
 !
 ! Fragmentation
 !
 call io_fragment(ID,ID_frag,i_fragment=i_fragment)
 !
 ! Manage RD_CL_IF_END
 !
 if (io_action(ID)==RD_CL_IF_END.and.ik==nXkibz) io_action(ID)=RD_CL
 !
 do ib=COLL_bands(1),COLL_bands(2)
   do ig=1,ng_oscll
     !
     if (write_is_on(ID)) disk_data=OSCLL_k(ib,:,:,ig)
     !
     write (VAR_name,'(3(a,i4.4))') 'OSCLL_',ik,'_ib_',ib,'_ig_',ig
     call def_variable_bulk(ID_frag,trim(VAR_name),1,(/2,COLL_nbands,q%nbz/),SP)
     call io_variable_bulk(ID_frag,1,C2=disk_data)
     !
     io_OSCLL=io_status(ID)
     if (io_OSCLL/=0) goto 1
     !
     if (read_is_on(ID)) OSCLL(ib,:,ik_mem,:,ig)=disk_data
     !
   enddo
 enddo
 !
 YAMBO_FREE(disk_data)
 !
1 call io_fragment_disconnect(ID,ID_frag)
 !
 call io_disconnect(ID=ID)
 !
#if defined _TIMING
 call timing('io_OSCLL',OPR='stop')
#endif
 !
end function
