!
!        Copyright (C) 2000-2022 the YAMBO team
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
!> @brief Read and write scattering indexes
!
! io_sec=1   general parameters   
! io_sec=2   q-points   
! io_sec=3   qindx_X   
! io_sec=4   qindx_S   
! io_sec=5   qindx_B          
! io_Sec=6   qindx_C
!
integer function io_QINDX(Xk,q,io_db)
 !
 use pars,           ONLY:SP,IP,LP,IP_YIO,LP_YIO,CR_YIO
 use R_lattice,      ONLY:nqibz,nqbz,qindx_X,qindx_B,qindx_S,qindx_C,&
&                         bse_scattering,QP_states_k,nXkibz,qindx_alloc,&
&                         Xk_grid_is_uniform,bz_samp,nXkbz,coll_scattering,q_pt,&
&                         sigma_scattering,X_scattering,k_GRIDS_string,k_GRIDS_vector, &
&                         qindx_S_max_Go
 use IO_m,           ONLY:io_sec,frag_QINDX,io_status,read_is_on,IO_OUTDATED_DB, &
&                         write_is_on,io_extension
 use IO_int,         ONLY:io_connect,io_disconnect,&
&                         io_variable_elemental,def_variable_elemental,io_header,&
&                         ver_is_gt_or_eq,io_fragment,def_variable_bulk,io_variable_bulk
 use timing_m,       ONLY:timing
 use parallel_m,     ONLY:PAR_K_scheme
 !
#include<memory.h>
 !
 type(bz_samp)::q,Xk
 integer      ::io_db,i3,i2,i2_dn,i2_up,i2_mem,i2_step,IO_size
 !
 ! Work Space
 !
 integer      :: ioScatt,id_frag,n_pars
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
   if (.not.ver_is_gt_or_eq(io_db,revision=22000)) io_QINDX=IO_OUTDATED_DB
   if (io_QINDX/=0) goto 1
   !
   call def_variable_elemental(io_db,"nXkbz",1,IP_YIO,0)
   call io_variable_elemental(io_db,VAR="Polarization last K ( bz)",I0=nXkbz)
   !
   call def_variable_elemental(io_db,"nXkibz",1,IP_YIO,0)
   call io_variable_elemental(io_db,VAR="Polarization last K (ibz)",I0=nXkibz)
   !
   call def_variable_elemental(io_db,"QP_states_k",2,IP_YIO,0)
   call io_variable_elemental(io_db,VAR="QP states",I1=QP_states_k,CHECK=.true.,OP=(/">=","<="/))
   !
   call def_variable_elemental(io_db,"q_nibz",1,IP_YIO,0)
   call io_variable_elemental(io_db,I0=q%nibz)
   !
   call def_variable_elemental(io_db,"q_nbz",1,IP_YIO,0)
   call io_variable_elemental(io_db,I0=q%nbz)
   !
   call def_variable_elemental(io_db,"qindx_S_max_Go",1,IP_YIO,0)
   call io_variable_elemental(io_db,I0=qindx_S_max_Go)
   !
   call def_variable_elemental(io_db,"Xk_grid_is_uniform",1,LP_YIO,0)
   call io_variable_elemental(io_db,VAR="X grid is uniform",L0=Xk_grid_is_uniform)
   !
   io_QINDX=io_status(io_db)
   !
   call def_variable_elemental(io_db,"GRIDS_CH",1,CR_YIO,0)
   call io_variable_elemental(io_db,VAR="Grids (string)",CH0=k_GRIDS_string,CHECK=.FALSE.)
   call def_variable_elemental(io_db,"GRIDS_IV",4,IP_YIO,0)
   call io_variable_elemental(io_db,VAR="Grids (int-vector)",I1=k_GRIDS_vector,CHECK=.TRUE.,OP=(/"<=","<=","<=","<="/))
   !
   call def_variable_elemental(io_db,"",0,0,1)
   !
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
   if (X_scattering) then
     if (read_is_on(io_db)) call qindx_alloc('X')
     !
     ! Fragmentation (1): eXchange indx
     !
     io_extension(io_db)='kindx'
     call io_fragment(io_db,id_frag,i_fragment=1)
     call def_variable_bulk(id_frag,"Qindx",1,shape(qindx_X),IP_YIO)
     call  io_variable_bulk(id_frag,1,I3=qindx_X)
     call io_fragment_disconnect(io_db,id_frag)
     !
   endif
 endif
 !
 if (any((/io_sec(io_db,:)==4/))) then 
   if (sigma_scattering) then
     if (read_is_on(io_db)) call qindx_alloc('S')
     !
     ! Fragmentation (2): Self-Energy indx
     !
     io_extension(io_db)='kindx'
     call io_fragment(io_db,id_frag,i_fragment=2)
     call def_variable_bulk(id_frag,"Sindx",2,shape(qindx_S),IP_YIO)
     call  io_variable_bulk(id_frag,2,I3=qindx_S)
     call io_fragment_disconnect(io_db,id_frag)
     !
   endif
 endif
 !
 if (any((/io_sec(io_db,:)==5/))) then 
   if (bse_scattering) then
     if (read_is_on(io_db)) call qindx_alloc('B')
     !
     ! Fragmentation (3): BSE indx
     !
     io_extension(io_db)='kindx'
     call io_fragment(io_db,id_frag,i_fragment=3)
     call def_variable_bulk(id_frag,"Bindx",3,(/nXkbz,nXkbz,2/),IP_YIO,par_io_kind='independent')
     ! 46341 is the sqare root of the maximum integer 2147483647
     ! qindx_B is size nXkbz*nXkbz
     ! NETCDF has problems in reading when this number overcomes 2147483647
     IO_size=nXkbz !size(qindx_B(1,1,:))
     i2_step=IO_size/(IO_size/46341+1)
     do i3=1,2
       do i2_dn=1,IO_size,i2_step
         i2_up=min(i2_dn-1+i2_step,IO_size)
#if !defined _PAR_IO
         if (read_is_on(id_frag)) then
#endif
           ! due to the following loop, the previous loop is not needed
           ! however I keep also the previous one in case, for future changes,
           ! the next loop is removed
           do i2=i2_dn,i2_up
             if (.not.PAR_K_scheme%IND_bz%element_1D(i2)) cycle
             if (write_is_on(id_frag).and..not.PAR_K_scheme%COM_ibz_A2A%CPU_id==0) cycle
             i2_mem=PAR_K_scheme%bz_index(i2)
             call io_variable_bulk(id_frag,3,I3=qindx_B(:,i2_mem:i2_mem,i3:i3),IPOS=(/1,i2,i3/))
           enddo
#if !defined _PAR_IO
         else
           call  io_variable_bulk(id_frag,3,I3=qindx_B(:,i2_dn:i2_up,i3:i3),IPOS=(/1,i2_dn,i3/))
         endif
#endif
       enddo
     enddo
     call io_fragment_disconnect(io_db,id_frag)
   endif
 endif
 !
 if (any((/io_sec(io_db,:)==6/))) then 
   if (coll_scattering) then
     if (read_is_on(io_db)) call qindx_alloc('C')
     !
     ! Fragmentation (4): COLL indx
     !
     io_extension(io_db)='kindx'
     call io_fragment(io_db,id_frag,i_fragment=4)
     call def_variable_bulk(id_frag,"Cindx",4,shape(qindx_C),IP_YIO)
     call  io_variable_bulk(id_frag,4,I3=qindx_C)
     call io_fragment_disconnect(io_db,id_frag)
   endif
 endif
 !
1 call io_disconnect(ID=io_db)
 !
 call timing('io_QINDX',OPR='stop')
 !
end function io_QINDX
!
integer function qindx_B_init(ID,ID_frag)
 !
 use pars,       ONLY:IP,IP_YIO
 use R_lattice,  ONLY:nXkbz
 use IO_m,       ONLY:OP_RD,RD,io_status
 use IO_int,     ONLY:io_control,io_connect,def_variable_bulk,io_fragment
 !
 implicit none
 !
 integer  :: ID,ID_frag
 !
 call io_control(ACTION=OP_RD,ID=ID)
 qindx_B_init=io_connect(desc='kindx',type=1,ID=ID)
 !
 call io_fragment(ID,ID_frag,i_fragment=3)
 call def_variable_bulk(ID_frag,"Bindx",3,(/nXkbz,nXkbz,2/),IP_YIO,par_io_kind='independent')
 !
 call io_control(ACTION=RD,ID=ID_frag)
 !
 qindx_B_init=io_status(ID_frag)
 !
end function qindx_B_init
!
integer function qindx_B_close(ID,ID_frag)
 !
 use IO_m,    ONLY:RD_CL
 use IO_int,  ONLY:io_control,io_disconnect
 !
 implicit none
 !
 integer :: ID,ID_frag
 !
 call io_control(ACTION=RD_CL,ID=ID_frag)
 call io_disconnect(ID=ID_frag)
 call io_control(ACTION=RD_CL,ID=ID)
 call io_disconnect(ID=ID)
 qindx_B_close=0
 !
end function qindx_B_close
!
!
function qindx_B_load(ikbz,ikp_bz,ID_frag)
 !
 use IO_int,  ONLY:io_variable_bulk
 !
 implicit none
 !
 integer, dimension(2) :: qindx_B_load
 integer, intent(in) :: ID_frag,ikbz,ikp_bz
 integer :: IO_var(1,1,2)
 !
 call io_variable_bulk(ID_frag,3,I3=IO_var,IPOS=(/ikbz,ikp_bz,1/))
 qindx_B_load=IO_var(1,1,:)
 !
end function qindx_B_load
