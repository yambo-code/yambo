!
!        Copyright (C) 2000-2023 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA DS
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
integer function io_COLLISIONS(ID,COLL,COLL_element,i_coll_)
 !
 use pars,          ONLY:schlen,IP,SP,IP_YIO,CR_YIO
 use parallel_m,    ONLY:master_cpu
 use iso_c_binding, ONLY:C_LOC,C_F_POINTER
 use collision_ext, ONLY:COLLISIONS_element,COLLISIONS_group,LAST_COLL_sync,PAR_COLL_min
 use IO_m,          ONLY:io_sec,io_status,IO_OUTDATED_DB,read_is_on,IO_mode,io_unit,&
&                        VERIFY,io_extension,write_is_on,nf90_sync,netcdf_call,YAMBO_DEF_CHUNK
 use IO_int,        ONLY:io_connect,io_header,io_elemental,ver_is_gt_or_eq,io_fragment, &
&                        io_variable_bulk,def_variable_bulk,io_disconnect
 use global_XC,     ONLY:QP_SE_GW_NEQ,QP_SE_GW_QED
 use plasma,        ONLY:N_plasma_poles_global,PLASMA_redux_percent
 use QP_m,          ONLY:QP_ng_Sx,QP_ng_SH,SC_band_mixing,QP_QED_ng
 use X_m,           ONLY:X_desc
 use drivers,       ONLY:l_elel_scatt,l_elphoton_scatt
 use timing_m,      ONLY:timing
 !
#include<memory.h>
 !
 integer, intent(inout)            ::ID
 type(COLLISIONS_group)            ::COLL
 type(COLLISIONS_element),optional, target :: COLL_element
 integer, intent(in)     ,optional         :: i_coll_
 !
 real(SP),   pointer :: tmp_link_real(:,:)
 complex(SP),pointer :: tmp_link_complex(:,:)
 !
 character(1)      :: TMP_VAR(1)
 character(schlen) :: db_name
 integer           :: i_coll,stat,COLL_chunksize(3)
 logical           :: def1,def2,io2,io3,io4,file_sync
 !
 call timing('io_COLLISIONS',OPR='start')
 !
 ! Collision element...
 !
 i_coll=0
 if (present(COLL_element)) i_coll=COLL_element%I
 if (present(i_coll_))      i_coll=i_coll_
 !
 call COLLISIONS_dbname(COLL%kind,db_name)
 !
 io_COLLISIONS=io_connect(desc=trim(db_name),type=2,ID=ID)
 !
 if(io_COLLISIONS/=0) goto 1
 !
 def1=any(io_sec(ID,:)==1)
 def2=any(io_sec(ID,:)==2)
 !
 io2=any(io_sec(ID,:)==2)
 io3=any(io_sec(ID,:)==3)
 io4=any(io_sec(ID,:)==4)
 !
 if (def1) then
   call def_variable_bulk(ID,'N_COLLISIONS_STATES',1,(/1/),IP_YIO,par_io_kind='independent')
   if (write_is_on(ID)) COLL_element%N=(COLL%D(3,2)-COLL%D(3,1)+1)*(COLL%D(2,2)-COLL%D(2,1)+1)*(COLL%D(1,2)-COLL%D(1,1)+1)
 endif
 !
 ! Despite all processors write to file (and also define the variable), only the master fills it
 !
 if (io2 .and. ((write_is_on(ID).and.master_cpu).or.read_is_on(ID))) then
   call io_variable_bulk(ID,1,I0=COLL_element%N)
 endif
 !
 if (io3.or.def2) then
   !
   if(COLL%kind==QP_SE_GW_NEQ.or.COLL%kind==QP_SE_GW_QED) then
     ! Real collisions
     if(def2) then
       COLL_chunksize(1:2)=YAMBO_DEF_CHUNK
       if (COLL_element%N<YAMBO_DEF_CHUNK) COLL_chunksize(1)=COLL_element%N
       if (COLL%N<YAMBO_DEF_CHUNK)         COLL_chunksize(2)=0
       call def_variable_bulk(ID,'COLLISIONS_v',2,(/COLL_element%N,COLL%N/),SP,&
       &                      par_io_kind='independent',chunksize=COLL_chunksize(1:2))
     endif
     if(io3 ) then
       call C_F_POINTER(C_LOC(COLL_element%v_r),tmp_link_real,(/COLL_element%N,1/))
       call  io_variable_bulk(ID,2,R2=tmp_link_real,IPOS=(/1,i_coll/))
       nullify(tmp_link_real)
     endif
   else
     ! Complex collisions
     if(def2) then
       COLL_chunksize(1)=2
       COLL_chunksize(2:3)=YAMBO_DEF_CHUNK
       if (COLL_element%N<YAMBO_DEF_CHUNK) COLL_chunksize(2)=COLL_element%N
       if (COLL%N<YAMBO_DEF_CHUNK)         COLL_chunksize(3)=0
       call def_variable_bulk(ID,'COLLISIONS_v',2,(/2,COLL_element%N,COLL%N/),SP,&
       &                      par_io_kind='independent',chunksize=COLL_chunksize(1:2))
     endif
     if(io3 ) then
       call C_F_POINTER(C_LOC(COLL_element%v_c),tmp_link_complex,(/COLL_element%N,1/))
       call  io_variable_bulk(ID,2,C2=tmp_link_complex,IPOS=(/1,1,i_coll/))
       nullify(tmp_link_complex)
     endif
   endif
   !
   ! This should be used for restart like in BSE ...
   ! if(BS_blk(i_block)%mat(i1,i2)==YAMBO_CMPLX_NO_FILL) cycle
   ! BS_blk(i_block)%done(i1,i2)='t'
   if(io3.and.read_is_on(ID)) COLL_element%table='t'
   !
 endif
 !
 if (def1) call def_variable_bulk(ID,'COLLISIONS_DONE',3,(/COLL%N/),CR_YIO,par_io_kind='independent')
 !
 if (io4) then
   if (write_is_on(ID)) then
     COLL%IO_status(i_coll)=1
     TMP_VAR="t"
   endif
   call io_variable_bulk(ID,3,CH1=TMP_VAR(1:1),IPOS=[i_coll])
   if (read_is_on(ID)) then
     COLL%IO_status(i_coll)=-1
     if(TMP_VAR(1)=="t") COLL%IO_status(i_coll)=1
   endif
 endif
 !
#if defined _RT
 ! nf90_sync is performed up to LAST_COLL_sync and at steps of 5% of bse kernel
 file_sync= i_coll<=LAST_COLL_sync .and. mod(i_coll,max(1,PAR_COLL_min/20))==0 .and. index(trim(db_name),"HXC")/=0
 !
 if(write_is_on(ID).and.file_sync.and.io3) then
   stat = nf90_sync(io_unit(ID))
   call netcdf_call(stat,ID)
 endif
#endif
 !
1 call io_disconnect(ID=ID)
 !
 call timing('io_COLLISIONS',OPR='stop')
 !
end function io_COLLISIONS
