!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, HM, DS
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
subroutine def_variable_bulk(ID,VAR,VAR_SZ,par_io_kind)
 !
 use pars,    ONLY:nf90_SP
 use com,     ONLY:warning,error
 use netcdf
 use IO_m,    ONLY:write_is_on,read_is_on,io_unit,netcdf_call,io_netcdf_var,&
&                  netcdf_dim,io_file
 !
 implicit none
 !
 integer      :: ID
 character(*) :: VAR
 integer      :: VAR_SZ(:)
 integer, optional  :: par_io_kind
 !
 ! Work Space
 !
 integer, allocatable :: var_dim(:),var_dim_db(:)
 integer              :: j1,dims_db,nf_error
#if defined _HDF5_COMPRESSION
 integer              :: shuffle,deflate,deflate_level
#endif
 !
 ! Variable definition and ID
 !
 if (write_is_on(ID)) then
   !
   call netcdf_call(nf90_redef(io_unit(ID)),ID,VAR=VAR)
   allocate(var_dim(size(VAR_SZ)))
   do j1=1,size(VAR_SZ)
     var_dim(j1)=netcdf_dim(ID,VAR_SZ(j1))
   enddo
   !
   nf_error=nf90_inq_varid(io_unit(ID),VAR,io_netcdf_var(ID))
   !
   if (nf_error==NF90_NOERR) then
     allocate(var_dim_db(size(VAR_SZ)))
     nf_error=nf90_inquire_variable(io_unit(ID),io_netcdf_var(ID),ndims=dims_db,dimids=var_dim_db)
     call netcdf_call(nf_error,ID,VAR=VAR)
     if(any((/var_dim(:)/=var_dim_db(:)/))) &
&        call error(' Trying to owrite variable '//trim(VAR)//' in '//trim(io_file(ID))//' with wrong dimensions ')
     call warning(' Overwriting variable '//trim(VAR)//' in file '//trim(io_file(ID)))
     deallocate(var_dim_db)
   endif
   !
   if (nf_error/=NF90_NOERR) then
     nf_error=nf90_def_var(io_unit(ID),VAR,nf90_SP,var_dim,io_netcdf_var(ID)) 
     call netcdf_call(nf_error,ID,VAR=VAR)
#if defined _PAR_IO
     if(present(par_io_kind)) then
       nf_error=nf90_var_par_access(io_unit(ID), io_netcdf_var(ID), par_io_kind)
       call netcdf_call(nf_error,ID,VAR=VAR)
     endif
#endif
   endif
   !
#if defined _HDF5_COMPRESSION
   shuffle=0
   deflate=1
   deflate_level=2
   call netcdf_call(nf90_def_var_deflate(io_unit(ID),io_netcdf_var(ID),shuffle,deflate,deflate_level),ID,VAR=VAR)
#endif
   !
   call netcdf_call(nf90_enddef(io_unit(ID)),ID,VAR=VAR) 
   deallocate(var_dim)
   !
 else if (read_is_on(ID)) then
   !
   call netcdf_call(nf90_inq_varid(io_unit(ID),VAR,io_netcdf_var(ID)),ID,VAR=VAR)
#if defined _HDF5_COMPRESSION
   call netcdf_call(nf90_inq_var_deflate(io_unit(ID),io_netcdf_var(ID),shuffle,deflate,deflate_level),ID,VAR=VAR)
#endif
   !
 endif
 !
 return
 !
end subroutine def_variable_bulk
