!
!        Copyright (C) 2000-2017 the YAMBO team
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
integer function io_connect(desc,subfolder,type,no_full_RESET,ID)
 !
 use pars,          ONLY:schlen,lchlen
 use stderr,        ONLY:string_pack
 use IO_m,          ONLY:read_is_on,write_is_on,open_is_on,io_reset,io_file,io_unit,&
&                        io_raw_extension,mk_dir,io_extension,io_folder,io_type,netcdf_call,&
&                        IO_read,IO_write,IO_NO_DATABASE,IO_CREATE_MODE,IO_READ_MODE,IO_WRITE_MODE,&
&                        IO_NOT_ALLOWED,OP_APP,OP_APP_WR_CL,io_action,OP_WR,OP_WR_CL
 use netcdf,        ONLY:nf90_open,nf90_create,nf90_enddef
 use com,           ONLY:get_name,jobstr,file_exists,more_io_path,all_locations,&
&                        num_of_alternative_locations,n_alt_jobstr
 !
 implicit none
 !
 character(*)           :: desc
 integer                :: type,ID
 character(*), optional :: subfolder
 logical,      optional :: no_full_RESET
 !
 ! Work Space
 !
 integer          ::CREATE_MODE,i_jobstr
 character(lchlen)::alternative_file(num_of_alternative_locations),folder
 character(schlen)::subfolder_
 logical          ::file_found,full_RESET
 !
 full_RESET=.true.
 if (present(no_full_RESET)) full_RESET=.not.no_full_RESET
 !
 io_connect=0
 if ( ( read_is_on(ID).and..not.IO_read ).or.&
&     (write_is_on(ID).and..not.IO_write)      ) then
   io_connect=IO_NOT_ALLOWED
   call io_reset(ID)
   return
 endif
 !
 ! Build the file name
 !
 subfolder_=' '
 if (present(subfolder)) subfolder_=subfolder
 !
 alternative_file=" "
 !
 if (len_trim(io_file(ID))==0) then
   !
   if (write_is_on(ID)) then
     !
     ! NEW databases are always written respecting the "type" (see mod_com.F)
     !
     io_file(ID)=get_name(desc,subfolder_,type,CORE_IO=.FALSE.,MORE_IO=.TRUE.,COM_IO=.FALSE.)
     !
   else if (read_is_on(ID)) then
     !
     io_file(ID)=get_name(desc,subfolder_,type,CORE_IO=.FALSE.,MORE_IO=.TRUE.,COM_IO=.FALSE.)
     !
     if (type==1.or.type==2) then
       !
       alternative_file=all_locations(desc,subfolder_)
       !
     endif
     !
   endif
   !
 endif
 !
 ! Keep the extension for the fragment-related procedures
 !
 if (len_trim(io_raw_extension(ID))==0) io_raw_extension(ID)=desc
 io_extension(ID) =desc
 io_folder(ID)    =trim(subfolder_)
 io_type(ID)      =type
 !
 ! Create Directories 
 !
 write (folder,'(2a)') trim(more_io_path),'/SAVE'
 if (len_trim(jobstr)>0) write (folder,'(3a)') trim(more_io_path),'/',trim(jobstr)
 if (write_is_on(ID).and.(type==2.or.len_trim(jobstr)==0)) then
   call mk_dir(more_io_path)
   call mk_dir(folder)
   if (present(subfolder)) call mk_dir(string_pack(folder,"/",subfolder))
 endif
 !
 ! Open if to open and to write
 !
 if (open_is_on(ID).and.write_is_on(ID)) then
   !
   if ( (io_action(ID)==OP_APP_WR_CL.or.io_action(ID)==OP_APP) .and. file_exists(trim(io_file(ID))) ) then
     !
     call netcdf_call(nf90_open(trim(io_file(ID)),IO_WRITE_MODE,io_unit(ID)),ID)
     !
   else
     !
     call netcdf_call(nf90_create(trim(io_file(ID)),IO_CREATE_MODE,io_unit(ID)),ID)
     call netcdf_call(nf90_enddef(io_unit(ID)),ID)
     if (io_action(ID)==OP_APP_WR_CL) io_action(ID)=OP_WR_CL
     if (io_action(ID)==OP_APP) io_action(ID)=OP_WR
     !
   endif
   !
 endif
 !
 ! Open if to open and to read
 !
 if (open_is_on(ID).and.read_is_on(ID)) then
   !
   if (.not.file_exists(trim(io_file(ID)))) then
     !
     file_found=.FALSE.
     !
     do i_jobstr=1,4+2*n_alt_jobstr
       file_found=file_exists(trim(alternative_file(i_jobstr)))
       if(file_found) exit
     enddo
     !
     if (.not.file_found) then
       io_connect=IO_NO_DATABASE
       if (.not.full_RESET) io_file(ID)=''
       if (     full_RESET) call io_reset(ID)
       return
     endif
     !
     io_file(ID)=alternative_file(i_jobstr)
     !
   endif
   !
   !
   if (type==-2) open(unit=io_unit(ID),file=trim(io_file(ID)),form='unformatted')
   if (type/=-2) call netcdf_call(nf90_open(trim(io_file(ID)),IO_READ_MODE,io_unit(ID)),ID)
   !
 endif
 !
end function io_connect
