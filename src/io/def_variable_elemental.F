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
subroutine def_variable_elemental(ID,VAR,VAR_SZ,MENU,CH0,CH1,par_io_kind)
 !
 use pars,    ONLY:SP,DP,schlen,lchlen,nf90_SP
 use pointers,ONLY:linkC2R
 use netcdf
 use drivers, ONLY:list_dbs
 use com,     ONLY:msg
 use stderr,  ONLY:string_pack,intc,real2ch,complex2ch
 use IO_m,    ONLY:write_is_on,io_unit,netcdf_call,io_netcdf_var,ver_is_gt_or_eq,&
&                  netcdf_dim,io_rec_pos,read_is_on,io_mode,VERIFY,&
&                  io_status,DUMP,REP,LOG,io_com,io_file,&
&                  io_code_version,io_serial_number,io_code_revision,IO_INCOMPATIBLE_VAR
 implicit none
 !
 integer               :: ID
 character(*)          :: VAR
 integer               :: VAR_SZ
 integer               :: MENU          ! MENU = 1 (on) 0 (off)
 character(*),optional :: CH0,CH1(:)
 integer,     optional :: par_io_kind
 !
 ! Work Space
 !
 character(lchlen) :: ch
 character(1)      :: msg_where
 character(3)      :: MODE
 integer           :: nf_error
 !
 io_rec_pos(ID)=1
 !
 if (VAR_SZ>0) then
   !
   if (write_is_on(ID)) MODE="[WR"
   if ( read_is_on(ID)) MODE="[RD"
   ch=string_pack(MODE,trim(io_file(ID)),"]",repeat('-',max( 60-len_trim(io_file(ID))-4,1) ))
   !
   if (write_is_on(ID)) then
     call netcdf_call(nf90_redef(io_unit(ID)),ID,VAR=VAR)
     nf_error=nf90_inq_varid(io_unit(ID),VAR,io_netcdf_var(ID))
     if (nf_error/=NF90_NOERR) then
       if (present(CH0).or.present(CH1) ) then
         nf_error=nf90_def_var(io_unit(ID),VAR,nf90_char,(/netcdf_dim(ID,schlen),netcdf_dim(ID,VAR_SZ)/),io_netcdf_var(ID))
       else
         nf_error=nf90_def_var(io_unit(ID),VAR,nf90_SP,netcdf_dim(ID,VAR_SZ),io_netcdf_var(ID))
       endif
       call netcdf_call(nf_error,ID,VAR=VAR)
#if defined _PAR_IO
       if(present(par_io_kind)) then
         nf_error=nf90_var_par_access(io_unit(ID), io_netcdf_var(ID), par_io_kind)
         call netcdf_call(nf_error,ID,VAR=VAR)
       endif
#endif
     endif
     call netcdf_call(nf90_enddef(io_unit(ID)),ID,VAR=VAR) 
   else if (read_is_on(ID)) then
     call netcdf_call(nf90_inq_varid(io_unit(ID),VAR,io_netcdf_var(ID)),ID,VAR=VAR)
   endif
   !
 else
   !
   ! GPL_EXCLUDE_START
   !
   write (ch,'(a,i6.6,2a,3(a,i2.2),a,i5.5,a)')  '- S/N ',io_serial_number(ID),' ',&
&          repeat('-',25),'- v.',io_code_version(ID,1),'.',io_code_version(ID,2),&
&          '.',io_code_version(ID,3),' r.',io_code_revision(ID,1),' -'
   !
   ! GPL_EXCLUDE_END
   !
   ! GPL_INCLUDE_START
!   write (ch,'(a,i6.6,2a,3(a,i2.2),a,i5.5,a)')  '- S/N ',io_serial_number(ID),' ',&
!&        repeat('-',25),'- v.',io_code_version(ID,1),'.',io_code_version(ID,2),&
!&        '.',io_code_version(ID,3),' r.',io_code_revision(ID,2),' -'
   ! GPL_INCLUDE_END
   !
 endif
 !
 !
 if (MENU==1) then
   msg_where=""
   if (io_com(ID)==REP            ) msg_where="r"
   if (io_com(ID)==LOG.or.list_dbs) msg_where="s"
   call msg(msg_where,trim(ch))
 endif
 !
 return
 !
end subroutine def_variable_elemental
