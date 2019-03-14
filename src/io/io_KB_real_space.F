!
!        Copyright (C) 2000-2019 the YAMBO team
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
integer function io_KB_real_space(ID,kbv_real_space,kbv_real_space_table)
 !
 use pars,             ONLY:SP
 use stderr,           ONLY:intc
 use R_lattice,        ONLY:nkibz
 use electrons,        ONLY:n_spinor
 use pseudo,           ONLY:pp_kbv_dim
 use FFT_m,            ONLY:fft_size
 use IO_m,             ONLY:io_connect,io_disconnect,io_sec,io_fragment, &
&                           io_elemental,io_bulk,io_header,frag_WF
 !
#include<memory.h>
 !
 integer,       intent(in)    :: ID
 integer,       intent(inout) :: kbv_real_space_table(pp_kbv_dim)
 complex(SP),   intent(inout) :: kbv_real_space(fft_size,n_spinor,pp_kbv_dim)
 !
 ! Workspace
 !
 integer :: ik,ifrag,i_sp_pol
 !
 ifrag=maxval(io_sec(ID,:))-1
 if(ifrag<=nkibz) then
   ik=ifrag
   i_sp_pol=1
 else
   ik=ifrag-nkibz
   i_sp_pol=2
 endif
 !
 io_KB_real_space=io_connect(desc="kb_pp_real_space",type=2,ID=ID)
 if (io_KB_real_space/=0) goto 1
 !
 ! Dimensions
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_KB_real_space=io_header(ID,IMPOSE_SN=.true.,FRAG=frag_WF)
   if (io_KB_real_space/=0) goto 1
   !
   call io_elemental(ID,VAR="PP_KBV_DIM",VAR_SZ=1,MENU=0)
   call io_elemental(ID,I0=pp_kbv_dim)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   !
   call io_bulk(ID,VAR="PP_KBV_TABLE",VAR_SZ=(/pp_kbv_dim/))
   call io_bulk(ID,I1=kbv_real_space_table)

   !
   if (io_KB_real_space/=0) goto 1
   !
 endif
 !
 if (ifrag==0) goto 1
 !
 call io_fragment(ID,i_fragment=ifrag)
 !
 call io_bulk(ID,VAR='PP_KB_RSPACE_IK'//trim(intc(ik))//'_SP_POL'//trim(intc(i_sp_pol)),VAR_SZ=(/2,fft_size,n_spinor,pp_kbv_dim/))
 call io_bulk(ID,C3=kbv_real_space(:,:,:))
 ! 
1 call io_disconnect(ID=ID)
 !
end function io_KB_real_space
