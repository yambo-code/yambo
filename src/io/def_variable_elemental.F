!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine def_variable_elemental(ID,VAR,VAR_SZ,VAR_KIND,MENU,par_io_kind)
 !
 use netcdf
 use pars,       ONLY:SP,DP,IP,LP,schlen,lchlen
 use drivers,    ONLY:list_dbs
 use com,        ONLY:msg
 use parallel_m, ONLY:ncpu
 use stderr,     ONLY:string_pack
 use IO_m,       ONLY:write_is_on,io_unit,netcdf_call,io_netcdf_var,ver_is_gt_or_eq,&
&                     netcdf_dim,io_rec_pos,read_is_on,io_mode,VERIFY,&
&                     io_status,DUMP,REP,LOG,io_com,io_file,&
&                     io_code_version,io_serial_number,io_code_revision,IO_INCOMPATIBLE_VAR
 !
 implicit none
 !
 integer               :: ID
 character(*)          :: VAR
 integer               :: VAR_SZ
 integer               :: VAR_KIND
 integer               :: MENU          ! MENU = 1 (on) 0 (off)
 character(*),optional :: par_io_kind
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
   ! Define var kind. Notice that SP=IP, this is why I need to shift by 100
   !
   if(VAR_KIND==SP)     netcdf_var_kind=nf90_real
   if(VAR_KIND==DP)     netcdf_var_kind=nf90_double
   if(VAR_KIND==IP+100) netcdf_var_kind=nf90_int
   if(VAR_KIND==LP+100) netcdf_var_kind=nf90_byte
   if(VAR_KIND==0 )     netcdf_var_kind=nf90_char
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
     nf_error=nf90_inq_varid(io_unit(ID),VAR,io_netcdf_var(ID))
     if (nf_error/=NF90_NOERR) then
       if(     VAR_KIND==0) then
         allocate(netcdf_var_dim(2))
         netcdf_var_dim=(/netcdf_dim(ID,schlen),netcdf_dim(ID,VAR_SZ)/)
       endif
       if(.not.VAR_KIND==0) then
         allocate(netcdf_var_dim(1))
         netcdf_var_dim=(/                      netcdf_dim(ID,VAR_SZ)/)
       endif
       nf_error=nf90_def_var(io_unit(ID),VAR,netcdf_var_kind,netcdf_var_dim,io_netcdf_var(ID))
       call netcdf_call(nf_error,ID,VAR=VAR)
       deallocate(netcdf_var_dim)
#if defined _PAR_IO
       if(present(par_io_kind).and.ncpu>1) then
         nf_error=nf90_var_par_access(io_unit(ID), io_netcdf_var(ID), netcdf_par_io_kind)
         call netcdf_call(nf_error,ID,VAR=VAR)
       endif
#endif
     endif
     call netcdf_call(nf90_enddef(io_unit(ID)),ID,VAR=VAR) 
   else if (read_is_on(ID)) then
     call netcdf_call(nf90_inq_varid(io_unit(ID),VAR,io_netcdf_var(ID)),ID,VAR=VAR)
   endif
   !
 endif
 !
 !
 if (MENU==1) then
   !
   msg_where=""
   if (io_com(ID)==REP            ) msg_where="r"
   if (io_com(ID)==LOG.or.list_dbs) msg_where="s"
   !
   if (VAR_SZ>0) then
     if (write_is_on(ID)) MODE="[WR"
     if ( read_is_on(ID)) MODE="[RD"
     ch=string_pack(MODE,trim(io_file(ID)),"]",repeat('-',max( 60-len_trim(io_file(ID))-4,1) ))
   else
     !
     ! GPL_EXCLUDE_START
     !
     write (ch,'(a,i6.6,2a,3(a,i2.2),a,i5.5,a)')  '- S/N ',io_serial_number(ID),' ',&
&            repeat('-',25),'- v.',io_code_version(ID,1),'.',io_code_version(ID,2),&
&            '.',io_code_version(ID,3),' r.',io_code_revision(ID,1),' -'
     !
     ! GPL_EXCLUDE_END
     !
     ! GPL_INCLUDE_START
!     write (ch,'(a,i6.6,2a,3(a,i2.2),a,i5.5,a)')  '- S/N ',io_serial_number(ID),' ',&
!&          repeat('-',25),'- v.',io_code_version(ID,1),'.',io_code_version(ID,2),&
!&          '.',io_code_version(ID,3),' r.',io_code_revision(ID,2),' -'
     ! GPL_INCLUDE_END
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
