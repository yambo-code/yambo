!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CH
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
module etsf_data
  !
  ! ETSF kind can be KSS or WFK at the moment
  !
  character(3) :: ETSF_kind
  !
  contains

  function netcdf_inquire_varid(ncid, var_name) result(varid)
    use netcdf
    implicit none
    character (len = *), intent(in)  :: var_name
    integer,             intent(in)  :: ncid
    integer                          :: varid
    integer                          :: netcdf_error

    netcdf_error = nf90_inq_varid(ncid, var_name, varid)
    call netcdf_check(ncid,netcdf_error, varid, var_name)
  end function

  function netcdf_get_dimension(ncid,dim_name) result(dim_value)
    use netcdf
    implicit none
    character (len = *), intent(in)  :: dim_name
    integer,             intent(in)  :: ncid
    integer                          :: dim_value
    integer                          :: netcdf_error, varid, dimid
    
    netcdf_error = nf90_inq_dimid(ncid,dim_name,dimid)
    call netcdf_check(ncid,netcdf_error, dimid, dim_name)
    netcdf_error = nf90_inquire_dimension(ncid, dimid, len = dim_value)
    call netcdf_check(ncid,netcdf_error, dimid)
  end function

  subroutine netcdf_check(ncid,netcdf_error,varid,var_name)
    use netcdf
    use pars, only:lchlen
    use com,  only:error
    implicit none
    integer,     intent (in) :: ncid,varid,netcdf_error
    character (len = *), optional, intent(in) :: var_name
    character (lchlen)       :: msg

    if(netcdf_error /= nf90_noerr) then
        if (present(var_name)) then
            write(msg,*) 'variable: ', var_name, ' ', trim(nf90_strerror(netcdf_error))
        else
            write(msg,*) nf90_strerror(netcdf_error)
        end if
        call error(msg)
    end if
  end subroutine netcdf_check
end module etsf_data
