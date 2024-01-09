!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): CH HM
!
module netcdf_data
  !
  ! ETSF kind can be KSS or WFK at the moment
  !
  character(3) :: NETCDF_kind
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
    if ( netcdf_error/=0) then
      call warning(" Dimension "//trim(dim_name//" not found"))
      dim_value=-1
      return
    endif
    call netcdf_check(ncid,netcdf_error, dimid, dim_name)
    netcdf_error = nf90_inquire_dimension(ncid, dimid, len = dim_value)
    call netcdf_check(ncid,netcdf_error, dimid)
  end function

  subroutine netcdf_check(ncid,netcdf_error,varid,var_name)
    use netcdf
    use pars, only:lchlen
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
end module netcdf_data
