!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine def_variable_elemental(ID,VAR,VAR_SZ,VAR_KIND,MENU,dim_names,par_io_kind)
 !
 use netcdf
 use pars,       ONLY:SP,DP,IP,LP,schlen,lchlen,nf90_SP,DB_menu_length
 use pars,       ONLY:IP_YIO,LP_YIO,SP_YIO,DP_YIO,CR_YIO
#if !defined _io_lib
 use drivers,    ONLY:list_dbs
#endif
 use com,        ONLY:msg
 use parallel_m, ONLY:ncpu
 use stderr,     ONLY:STRING_pack
 use IO_m,       ONLY:write_is_on,io_unit,netcdf_call,io_netcdf_var,&
&                     netcdf_dim,io_rec_pos,read_is_on,io_mode,VERIFY,&
&                     io_status,DUMP,REP,LOG,io_com,io_file,&
&                     io_code_version,io_serial_number,io_code_revision,IO_INCOMPATIBLE_VAR
 !
 implicit none
 !
 integer     , intent(in) :: ID
 character(*), intent(in) :: VAR
 integer     , intent(in) :: VAR_SZ
 integer     , intent(in) :: VAR_KIND
 integer     , intent(in) :: MENU          ! MENU = 1 (on) 0 (off)
 character(schlen), optional, intent(in) :: dim_names(:)
 character(*),      optional, intent(in) :: par_io_kind
 !
 ! Work Space
 !
 character(lchlen) :: ch
 character(1)      :: msg_where
 character(3)      :: MODE
 integer           :: nf_error,netcdf_par_io_kind,netcdf_var_kind
 !
 integer, allocatable :: netcdf_var_dim(:)
 !
 if (VAR_SZ>0) then
   !
   io_rec_pos(ID)=1
   !
   if(VAR_KIND==SP_YIO)     netcdf_var_kind=nf90_SP
   if(VAR_KIND==DP_YIO)     netcdf_var_kind=nf90_double
   if(VAR_KIND==IP_YIO)     netcdf_var_kind=nf90_int
   if(VAR_KIND==LP_YIO)     netcdf_var_kind=nf90_byte
   if(VAR_KIND==CR_YIO)     netcdf_var_kind=nf90_char
   !
#if defined _PAR_IO
   !
   ! Define par IO kind
   !
   if(present(par_io_kind).and.ncpu>1) then
     if(trim(par_io_kind)=="independent") netcdf_par_io_kind=nf90_independent
     if(trim(par_io_kind)=="collective")  netcdf_par_io_kind=nf90_collective
   endif
#endif
   !
   if (write_is_on(ID)) then
     call netcdf_call(nf90_redef(io_unit(ID)),ID,VAR=VAR)
     nf_error=nf90_inq_varid(io_unit(ID),VAR,io_netcdf_var(ID,1))
     if (nf_error/=NF90_NOERR) then
       if(     VAR_KIND==0) then
         allocate(netcdf_var_dim(2))
         netcdf_var_dim=(/netcdf_dim(ID,schlen),netcdf_dim(ID,VAR_SZ)/)
       endif
       if(.not.VAR_KIND==0) then
         allocate(netcdf_var_dim(1))
         netcdf_var_dim=(/                      netcdf_dim(ID,VAR_SZ)/)
       endif
       nf_error=nf90_def_var(io_unit(ID),VAR,netcdf_var_kind,netcdf_var_dim,io_netcdf_var(ID,1))
       call netcdf_call(nf_error,ID,VAR=VAR)
       deallocate(netcdf_var_dim)
#if defined _PAR_IO
       if(present(par_io_kind).and.ncpu>1) then
         nf_error=nf90_var_par_access(io_unit(ID), io_netcdf_var(ID,1), netcdf_par_io_kind)
         call netcdf_call(nf_error,ID,VAR=VAR)
       endif
#endif
     endif
     call netcdf_call(nf90_enddef(io_unit(ID)),ID,VAR=VAR) 
   else if (read_is_on(ID)) then
     call netcdf_call(nf90_inq_varid(io_unit(ID),VAR,io_netcdf_var(ID,1)),ID,VAR=VAR)
   endif
   !
 endif
 !
 if (MENU==1) then
   !
   msg_where=""
   if (io_com(ID)==REP            ) msg_where="r"
#if !defined _io_lib
   if (io_com(ID)==LOG.or.list_dbs) msg_where="s"
#endif
   !
   if (VAR_SZ>0) then
     if (write_is_on(ID)) MODE="[WR"
     if ( read_is_on(ID)) MODE="[RD"
     ch=STRING_pack(MODE,trim(io_file(ID)),"]",repeat('-',max( DB_menu_length-len_trim(io_file(ID))-4,1) ))
   else
     !
     write (ch,'(a,i6.6,2a,3(a,i2.2),a,i5.5,a)')  '- S/N ',io_serial_number(ID),' ',&
&            repeat('-',DB_menu_length-35),'- v.',io_code_version(ID,1),'.',io_code_version(ID,2),&
&            '.',io_code_version(ID,3),' r.',io_code_revision(ID,1),' -'
     !
   endif
   !
   call msg(msg_where,trim(ch))
   !
 endif
 !
 return
 !
end subroutine def_variable_elemental
