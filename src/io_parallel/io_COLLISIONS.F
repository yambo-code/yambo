!
! License-Identifier: GPL
!
! Copyright (C) 2022 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA DS
!
integer function io_COLLISIONS(ID,COLL_grp,COLL_element,i_coll_)
 !
 use pars,          ONLY:schlen,IP,SP,DP,IP_YIO,CR_YIO
 use parallel_m,    ONLY:master_cpu
 use iso_c_binding, ONLY:C_LOC,C_F_POINTER
 use collision_ext, ONLY:COLLISIONS_element,COLLISIONS_group,PAR_COLL_min,&
&                        COLLISIONS_CV_only,COLLISIONS_FEW_only,COLLISIONS_load_SP
 use IO_m,          ONLY:io_sec,io_status,IO_OUTDATED_DB,read_is_on,IO_mode,io_unit,&
&                        VERIFY,io_extension,write_is_on,nf90_sync,netcdf_call,YAMBO_DEF_CHUNK
 use IO_int,        ONLY:io_connect,io_header,io_elemental,ver_is_gt_or_eq,io_fragment, &
&                        io_variable_bulk,def_variable_bulk,io_disconnect
 use global_XC,     ONLY:QP_SE_GW_NEQ,QP_SE_GW_QED
 use plasma,        ONLY:N_plasma_poles_global,PLASMA_redux_percent
 use QP_m,          ONLY:QP_ng_Sx,QP_ng_SH,SC_band_mixing,QP_QED_ng
 use X_m,           ONLY:X_desc
 use electrons,     ONLY:n_sp_pol
 use drivers,       ONLY:l_elel_scatt,l_elphoton_scatt
 use timing_m,      ONLY:timing
 !
#include<y_memory.h>
 !
 integer, intent(inout)            ::ID
 type(COLLISIONS_group)            ::COLL_grp
 type(COLLISIONS_element),optional, target :: COLL_element
 integer, intent(in)     ,optional         :: i_coll_
 !
 real(SP),   pointer :: tmp_link_real(:,:)
 complex(SP),pointer :: tmp_link_complex(:,:)
 !
 character(1)      :: TMP_VAR(1)
 character(schlen) :: db_name
 integer           :: i_coll,i_coll_mem,sync_step,stat,COLL_chunksize(3),N(3)
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
 call COLLISIONS_dbname(COLL_grp%kind,db_name)
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
   if (write_is_on(ID)) then
     if (COLLISIONS_CV_only.or.COLLISIONS_FEW_only) then
       N(3)=(COLL_grp%D(3,2)-COLL_grp%D(3,1)+1)*2  ! 2*Nk
       N(2)=COLL_grp%D(2,2)-COLL_grp%D(2,3)        ! Nc
       N(1)=COLL_grp%D(1,4)-COLL_grp%D(1,1)+1      ! Nv
     else
       N(3)=COLL_grp%D(3,2)-COLL_grp%D(3,1)+1
       N(2)=COLL_grp%D(2,2)-COLL_grp%D(2,1)+1
       N(1)=COLL_grp%D(1,2)-COLL_grp%D(1,1)+1
     endif
     COLL_element%N=N(1)*N(2)*N(3)*n_sp_pol
   endif
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
   if(COLL_grp%kind==QP_SE_GW_NEQ.or.COLL_grp%kind==QP_SE_GW_QED) then
     ! Real collisions
     if(def2) then
       COLL_chunksize(1:2)=YAMBO_DEF_CHUNK
       if (COLL_element%N<YAMBO_DEF_CHUNK) COLL_chunksize(1)=COLL_element%N
       if (COLL_grp%N<YAMBO_DEF_CHUNK)     COLL_chunksize(2)=0
       call def_variable_bulk(ID,'COLLISIONS_v',2,(/COLL_element%N,COLL_grp%N/),SP,&
       &                      par_io_kind='independent',chunksize=COLL_chunksize(1:2))
     endif
     if(io3 ) then
       call C_F_POINTER(C_LOC(COLL_element%v_r(:COLL_element%N)),tmp_link_real,(/COLL_element%N,1/))
       call  io_variable_bulk(ID,2,R2=tmp_link_real,IPOS=(/1,i_coll/))
       nullify(tmp_link_real)
     endif
   else
     ! Complex collisions
     if(def2) then
       COLL_chunksize(1)=2
       !COLL_chunksize(2:3)=YAMBO_DEF_CHUNK
       !if (COLL_element%N<YAMBO_DEF_CHUNK) COLL_chunksize(2)=COLL_element%N
       !if (COLL_grp%N<YAMBO_DEF_CHUNK)     COLL_chunksize(3)=COLL_grp%N
       COLL_chunksize(2)=COLL_element%N
       COLL_chunksize(3)=1 !COLL_grp%N
       call def_variable_bulk(ID,'COLLISIONS_v',2,(/2,COLL_element%N,COLL_grp%N/),SP,&
       &                      par_io_kind='independent',chunksize=COLL_chunksize)
     endif
     if(io3 ) then
       call C_F_POINTER(C_LOC(COLL_element%v_c(:COLL_element%N)),tmp_link_complex,(/COLL_element%N,1/))
       call  io_variable_bulk(ID,2,C2=tmp_link_complex,IPOS=(/1,1,i_coll/))
       if(SP==DP.and.COLLISIONS_load_SP) then
         COLL_element%v_c_SP=cmplx(COLL_element%v_c)
         YAMBO_FREE(COLL_element%v_c)
       endif
       nullify(tmp_link_complex)
     endif
   endif
   !
   ! Here I could set it to "t" only when COLL_element%v_c/=0 or COLL_element%v_r/=0 ?
   if(io3.and.read_is_on(ID)) COLL_element%table='t'
   !
 endif
 !
 if (def1) call def_variable_bulk(ID,'COLLISIONS_DONE',3,(/COLL_grp%N/),CR_YIO,par_io_kind='independent')
 !
 if (io4) then
   if (write_is_on(ID)) then
     COLL_grp%IO_status(i_coll)=1
     TMP_VAR="t"
   endif
   call io_variable_bulk(ID,3,CH1=TMP_VAR(1:1),IPOS=[i_coll])
   if (read_is_on(ID)) then
     COLL_grp%IO_status(i_coll)=-1
     if(TMP_VAR(1)=="t") COLL_grp%IO_status(i_coll)=1
   endif
 endif
 !
#if defined _RT
 if (write_is_on(ID).and.io3 .and. index(trim(db_name),"HXC")/=0) then
   !
   ! nf90_sync is performed up to PAR_COLL_min and at steps of 5% of bse kernel
   i_coll_mem = COLL_grp%PAR_map(i_coll)
   sync_step  = max(1,PAR_COLL_min/20)
   file_sync= (i_coll_mem<PAR_COLL_min .and. mod(i_coll_mem,sync_step)==0) .or. i_coll_mem==PAR_COLL_min
   !
   if(file_sync) then
     stat = nf90_sync(io_unit(ID))
     call netcdf_call(stat,ID)
   endif
   !
 endif
#endif
 !
1 call io_disconnect(ID=ID)
 !
 call timing('io_COLLISIONS',OPR='stop')
 !
end function io_COLLISIONS
