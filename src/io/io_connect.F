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
   integer function io_connect(desc,subfolder,type,no_full_RESET,OWRITE,ID)
     !
     use stderr,        ONLY:string_pack
     use com,           ONLY:get_name,jobstr,file_exists,more_io_path,all_locations,&
&                            num_of_alternative_locations,n_alt_jobstr
     character(*)           :: desc
     integer                :: type,ID
     character(*), optional :: subfolder
     logical,      optional :: no_full_RESET,OWRITE
     !
     ! Work Space
     !
     integer          ::CREATE_MODE,i_jobstr
     character(lchlen)::alternative_file(num_of_alternative_locations),folder
     character(schlen)::subfolder_
     logical          ::file_found,no_full_RESET_,OWRITE_
     !
     no_full_RESET_=.false.
     if (present(no_full_RESET)) no_full_RESET_=no_full_RESET
     !
     OWRITE_=.false.
     if (present(OWRITE)) OWRITE_=OWRITE
     !
     io_connect=0
     if ( (read_is_on(ID).and..not.IO_read).or.&
&         (write_is_on(ID).and..not.IO_write) ) then
       io_connect=IO_NO_DATABASE
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
       CREATE_MODE=ior(nf90_share,nf90_64bit_offset)
       !
#if defined _NC_CLASSIC
       CREATE_MODE=ior(nf90_share,nf90_classic)
#elif defined _HDF5_IO
       CREATE_MODE=ior(nf90_share,nf90_hdf5)
#endif
       !
       if( file_exists(trim(io_file(ID))) .and. (.not.OWRITE_) ) then
         call netcdf_call(nf90_open(trim(io_file(ID)),ior(nf90_write,nf90_share),io_unit(ID)),ID)
       else
         call netcdf_call(nf90_create(trim(io_file(ID)),CREATE_MODE,io_unit(ID)),ID)
         call netcdf_call(nf90_enddef(io_unit(ID)),ID)
       endif
       !
       io_type(ID)=type
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
         if (file_exists(trim(alternative_file(1)))) then
           io_file(ID)=alternative_file(1)
           file_found=.TRUE.
         else if (file_exists(trim(alternative_file(2)))) then
           io_file(ID)=alternative_file(2)
           file_found=.TRUE.
         else if (file_exists(trim(alternative_file(3)))) then
           io_file(ID)=alternative_file(3)
           file_found=.TRUE.
         else if (file_exists(trim(alternative_file(4)))) then
           io_file(ID)=alternative_file(4)
           file_found=.TRUE.
         endif
         !
         if (.not.file_found) then
           do i_jobstr=1,n_alt_jobstr
             if (file_exists(trim(alternative_file(3+2*i_jobstr)))) then
               io_file(ID)=alternative_file(3+2*i_jobstr)
               file_found=.TRUE.
               exit
             else if (file_exists(trim(alternative_file(4+2*i_jobstr)))) then
               io_file(ID)=alternative_file(4+2*i_jobstr)
               file_found=.TRUE.
               exit
             endif
           enddo
         endif
         !
         if (.not.file_found) then
           io_connect=IO_NO_DATABASE
           if (no_full_RESET_) then
             io_file(ID)=''
           else
             call io_reset(ID)
             io_type(ID)=0
           endif
           return
         endif
       endif
       !
       if (type==-2) then
         open(unit=io_unit(ID),file=trim(io_file(ID)),form='unformatted')
       else
         call netcdf_call(nf90_open(trim(io_file(ID)),nf90_nowrite,io_unit(ID)),ID)
       endif
       !
       io_type(ID)=type
       !
     endif
     !
   end function io_connect
