!
!        Copyright (C) 2000-2014 the YAMBO team
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
integer function ioRIM(ID)
 !
 use pars,           ONLY:SP,schlen
 use memory_m,       ONLY:mem_est
 use R_lattice,      ONLY:RIM_ng,RIM_n_rand_pts,RIM_is_diagonal,RL_vol,RIM_RL_vol,&
&                         RIM_id_epsm1_reference,RIM_epsm1,RIM_anisotropy,RIM_qpg,&
&                         nqibz
 use IO_m,           ONLY:io_connect,io_disconnect,io_sec,io_header,&
&                         io_elemental,io_status,io_bulk,read_is_on,write_is_on
 implicit none
 integer :: ID
 ! 
 ! Work Space
 !
 real(SP)            ::RL_vol_disk
 real(SP),allocatable::RIM_qpg_disk(:,:)
 integer             ::RIM_ng_disk,i1
 character(schlen)   ::VAR_name
 !
 ioRIM=io_connect(desc="RIM",type=2,ID=ID)
 !
 if (ioRIM/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   ioRIM=io_header(ID,QPTS=.true.,R_LATT=.true.,IMPOSE_SN=.true.)
   if (ioRIM/=0) goto 1   
   !
   RL_vol_disk=RL_vol
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=10,MENU=0)
   !
   call io_elemental(ID,I0=RIM_ng,DB_I0=RIM_ng_disk,&
&       VAR=' Coulombian RL components        :',CHECK=.true.,OP=(/"<="/))
   call io_elemental(ID,L0=RIM_is_diagonal,&
&       VAR=' Coulombian diagonal components  :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,I0=RIM_n_rand_pts,&
&       VAR=' RIM random points               :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,R0=RIM_RL_vol,&
&       VAR=' RIM  RL volume             [a.u.]:')
   call io_elemental(ID,R0=RL_vol_disk,&
&       VAR=' Real RL volume             [a.u.]:')
   call io_elemental(ID,I0=RIM_id_epsm1_reference,&
&       VAR=' Eps^-1 reference component       :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,R1=RIM_epsm1,&
&       VAR=' Eps^-1 components                :',CHECK=.true.,OP=(/"==","==","=="/))
   call io_elemental(ID,R0=RIM_anisotropy,&
&       VAR=' RIM anysotropy factor            :')
   !
   call io_elemental(ID,VAR="",VAR_SZ=0)
   ioRIM=io_status(ID)
   if (ioRIM/=0) goto 1
 endif
 !
 if (any((/io_sec(ID,:)==2/))) then
   !
   allocate(RIM_qpg_disk(RIM_ng_disk,RIM_ng_disk))
   !
   if (read_is_on(ID)) then
     allocate(RIM_qpg(nqibz,RIM_ng,RIM_ng))
     call mem_est("RIM_qpg",(/size(RIM_qpg)/),(/SP/))
   endif
   !
   do i1=1,nqibz 
     !
     if (write_is_on(ID)) RIM_qpg_disk=RIM_qpg(i1,:,:)
     !
     write (VAR_name,'(a,i4.4)') 'RIM_q_',i1
     call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=shape(RIM_qpg_disk))
     call io_bulk(ID,R2=RIM_qpg_disk)
     !
     if (read_is_on(ID)) RIM_qpg(i1,:,:)=RIM_qpg_disk(:RIM_ng,:RIM_ng)
     !
   enddo
   !
   deallocate(RIM_qpg_disk)
   !
 endif
 !
1 call io_disconnect(ID=ID)
 !
end function
