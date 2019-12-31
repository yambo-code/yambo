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
integer function io_QINDX(Xk,q,io_db)
 !
 use pars,           ONLY:SP,IP
 use R_lattice,      ONLY:nqibz,nqbz,qindx_X,qindx_B,qindx_S,qindx_C,&
&                         bse_scattering,QP_states_k,nXkibz,qindx_alloc,&
&                         Xk_grid_is_uniform,bz_samp,nXkbz,coll_scattering,q_pt
 use IO_m,           ONLY:io_connect,io_disconnect,io_sec,frag_QINDX,         &
&                         io_elemental,io_status,read_is_on,io_header,&
&                         ver_is_gt_or_eq,io_extension,io_fragment,IO_OUTDATED_DB,        &
&                         def_variable_bulk,io_variable_bulk
 use timing_m,       ONLY:timing
 !
#include<memory.h>
 type(bz_samp)::q,Xk
 integer      ::io_db
 !
 ! Work Space
 !
 integer      :: ioScatt
 !
 call timing('io_QINDX',OPR='start')
 !
 io_QINDX=io_connect(desc='kindx',type=1,ID=io_db)
 if (io_QINDX/=0) goto 1
 !
 if (any((/io_sec(io_db,:)==1/))) then
   !
   io_QINDX=io_header(io_db,IMPOSE_SN=.true.,FRAG=frag_QINDX)
   !
   ! In V. 3.0.7 a real parameter (RL_v_comp_norm) has been removed
   ! In revision 3085 the coll indx has been ceated with two new logicals
   ! in the header
   !
   if (.not.ver_is_gt_or_eq(io_db,revision=5545)) io_QINDX=IO_OUTDATED_DB
   if (io_QINDX/=0) goto 1
   !
   call io_elemental(io_db,VAR="PARS",VAR_SZ=9,MENU=0)
   !
   call io_elemental(io_db,I0=nXkbz)
   call io_elemental(io_db,VAR=" Polarization last K   :",I0=nXkibz)
   call io_elemental(io_db,VAR=" QP states             :",I1=QP_states_k,CHECK=.true.,OP=(/">=","<="/))
   call io_elemental(io_db,I0=q%nibz)
   call io_elemental(io_db,I0=q%nbz)
   call io_elemental(io_db,VAR=" X grid is uniform     :",L0=Xk_grid_is_uniform)
   io_QINDX=io_status(io_db)
   !
   call io_elemental(io_db,VAR=" BS scattering         :",L0=bse_scattering,CHECK=.true.,OP=(/"=="/))
   call io_elemental(io_db,VAR=" COLL scattering       :",L0=coll_scattering,CHECK=.true.,OP=(/"=="/))
   !
   call io_elemental(io_db,VAR="",VAR_SZ=0)
   ioScatt=io_status(io_db)
   nqbz=q%nbz
   nqibz=q%nibz
   !
   if (io_QINDX/=0) goto 1
   if (.not.any((/io_sec(io_db,:)>1/)) ) then
     if (ioScatt/=0) io_QINDX=ioScatt
     goto 1
   endif
   !
 endif
 !
 if (any((/io_sec(io_db,:)==2/))) then
   !
   if(ioScatt==0) then
     if (read_is_on(io_db)) then
       YAMBO_ALLOC(q%pt,(q%nibz,3))
     endif
     call def_variable_bulk(io_db,"Qpts",10,shape(q%pt),SP)
     call  io_variable_bulk(io_db,10,R2=q%pt)
   else
     if(.not.allocated(q_pt)) then
       YAMBO_ALLOC(q_pt,(nqibz,3))
       call def_variable_bulk(io_db,"Qpts",10,shape(q_pt),SP)
       call  io_variable_bulk(io_db,10,R2=q_pt)
     endif
     io_QINDX=ioScatt
     goto 1
   endif
 endif
 !
 ! qindx_X(nqibz,nXkbz,2)
 ! qindx_S(qp_states_k(2),nqbz,2)
 ! (bse_scattering)  -> qindx_B(nXkbz,nXkbz,2)
 ! (coll_scattering) -> qindx_C(nXkbz,nXkbz,2)
 !
 if (any((/io_sec(io_db,:)==3/))) then
   if (read_is_on(io_db)) call qindx_alloc('ALL')
   !
   ! Fragmentation (1): eXchange indx
   !
   io_extension(io_db)='kindx'
   call io_fragment(io_db,i_fragment=1)
   !
   call def_variable_bulk(io_db,"Qindx",1,shape(qindx_X),IP+100)
   call  io_variable_bulk(io_db,1,I3=qindx_X)
   !
   if (Xk_grid_is_uniform) then
     !
     ! Fragmentation (2): Self-Energy indx
     !
     io_extension(io_db)='kindx'
     call io_fragment(io_db,i_fragment=2)
     call def_variable_bulk(io_db,"Sindx",2,shape(qindx_S),IP+100)
     call  io_variable_bulk(io_db,2,I3=qindx_S)
   endif
   if (bse_scattering) then
     !
     ! Fragmentation (3): BSE indx
     !
     io_extension(io_db)='kindx'
     call io_fragment(io_db,i_fragment=3)
     call def_variable_bulk(io_db,"Bindx",3,shape(qindx_B),IP+100)
     call  io_variable_bulk(io_db,3,I3=qindx_B)
   endif
   if (coll_scattering.and.ver_is_gt_or_eq(io_db,revision=3085)) then
     !
     ! Fragmentation (4): COLL indx
     !
     io_extension(io_db)='kindx'
     call io_fragment(io_db,i_fragment=4)
     call def_variable_bulk(io_db,"Cindx",4,shape(qindx_C),IP+100)
     call  io_variable_bulk(io_db,4,I3=qindx_C)
   endif
 endif
 !
1 call io_disconnect(ID=io_db)
 !
 call timing('io_QINDX',OPR='stop')
 !
end function
