!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
integer function ioQINDX(Xk,q,io_db)
 !
 use R_lattice,      ONLY:nqibz,nqbz,qindx_X,qindx_B,qindx_S,qindx_C,&
&                         bse_scattering,qp_states_k,nXkibz,qindx_alloc,&
&                         Xk_grid_is_uniform,bz_samp,nXkbz,coll_scattering,q_pt,&
&                         Xk_impose_minus_q
 use IO_m,           ONLY:io_connect,io_disconnect,io_sec,&
&                         io_elemental,io_status,io_bulk,read_is_on,io_header,&
&                         ver_is_gt_or_eq,io_fragmented,io_extension
 use fragments,      ONLY:io_fragment
 implicit none
 type(bz_samp)::q,Xk
 integer      ::io_db
 !
 ! Work Space
 !
 integer      :: ioScatt
 !
 ioQINDX=io_connect(desc='kindx',type=1,ID=io_db)
 if (ioQINDX/=0) goto 1
 !
 if (any((/io_sec(io_db,:)==1/))) then
   !
   ioQINDX=io_header(io_db,IMPOSE_SN=.true.)
   !
   ! In V. 3.0.7 a real parameter (RL_v_comp_norm) has been removed
   ! In revision 3085 the coll indx has been ceated with two new logicals
   ! in the header
   !
   if (.not.ver_is_gt_or_eq(io_db,(/3,0,8/)))     ioQINDX=-1
   if (.not.ver_is_gt_or_eq(io_db,revision=3085)) ioQINDX=-1
   if (ioQINDX/=0) goto 1
   !
   call io_elemental(io_db,VAR="PARS",VAR_SZ=10)
   !
   call io_elemental(io_db,I0=nXkbz)
   call io_elemental(io_db,&
&       VAR=" Polarization last K   :",I0=nXkibz)
   call io_elemental(io_db,&
&       VAR=" QP states             :",I1=qp_states_k,CHECK=.true.,OP=(/">=","<="/))
   call io_elemental(io_db,I0=q%nibz)
   call io_elemental(io_db,I0=q%nbz)
   call io_elemental(io_db,&
&       VAR=" X grid is uniform     :",L0=Xk_grid_is_uniform)
   call io_elemental(io_db,&
&       VAR=" X grid impose -q      :",L0=Xk_impose_minus_q,CHECK=.true.,OP=(/"=="/))
   ioQINDX=io_status(io_db)
   !
   call io_elemental(io_db,&
&       VAR=" BS scattering         :",L0=bse_scattering,CHECK=.true.,OP=(/"=="/))
   call io_elemental(io_db,&
&       VAR=" COLL scattering       :",L0=coll_scattering,CHECK=.true.,OP=(/"=="/))
   !
   call io_elemental(io_db,VAR="",VAR_SZ=0)
   ioScatt=io_status(io_db)
   nqbz=q%nbz
   nqibz=q%nibz
   !
   if (ioQINDX/=0) goto 1
   if (.not.any((/io_sec(io_db,:)>1/)) ) then
     if (ioScatt/=0) ioQINDX=ioScatt
     goto 1
   endif
   !
 endif
 !
 if (any((/io_sec(io_db,:)==2/))) then
   if(ioScatt==0) then
     if (read_is_on(io_db)) allocate(q%pt(q%nibz,3))
     call io_bulk(io_db,VAR="Qpts",VAR_SZ=shape(q%pt))
     call io_bulk(io_db,R2=q%pt)
   else
     if(.not.allocated(q_pt)) then
       allocate(q_pt(nqibz,3))
       call io_bulk(io_db,VAR="Qpts",VAR_SZ=shape(q_pt))
       call io_bulk(io_db,R2=q_pt)
     endif
     ioQINDX=ioScatt
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
   if (read_is_on(io_db)) call qindx_alloc()
   !
   ! Fragmentation (1): eXchange indx
   !
   io_extension(io_db)='kindx'
   if (io_fragmented(io_db)) call io_fragment(io_db,i_fragment=1)
   !
   call io_bulk(io_db,VAR="Qindx",VAR_SZ=shape(qindx_X))
   call io_bulk(io_db,I3=qindx_X)
   !
   if (Xk_grid_is_uniform) then
     !
     ! Fragmentation (2): Self-Energy indx
     !
     io_extension(io_db)='kindx'
     if (io_fragmented(io_db)) call io_fragment(io_db,i_fragment=2)
     call io_bulk(io_db,VAR="Sindx",VAR_SZ=shape(qindx_S))
     call io_bulk(io_db,I3=qindx_S)
   endif
   if (bse_scattering) then
     !
     ! Fragmentation (3): BSE indx
     !
     io_extension(io_db)='kindx'
     if (io_fragmented(io_db)) call io_fragment(io_db,i_fragment=3)
     call io_bulk(io_db,VAR="Bindx",VAR_SZ=shape(qindx_B))
     call io_bulk(io_db,I3=qindx_B)
   endif
   if (coll_scattering.and.ver_is_gt_or_eq(io_db,revision=3085)) then
     !
     ! Fragmentation (4): COLL indx
     !
     io_extension(io_db)='kindx'
     if (io_fragmented(io_db)) call io_fragment(io_db,i_fragment=4)
     call io_bulk(io_db,VAR="Cindx",VAR_SZ=shape(qindx_C))
     call io_bulk(io_db,I3=qindx_C)
   endif
 endif
 !
1 call io_disconnect(ID=io_db)
 !
end function
