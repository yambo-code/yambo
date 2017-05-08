!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
module IO_m
 !
 use pars,       ONLY:SP,lchlen,schlen,max_io_units,max_active_sections
 use LOGO,       ONLY:code_version,code_revision,code_GPL_revision
 use parallel_m, ONLY:myid
 use com,        ONLY:error
 use netcdf
 implicit none
 !
 ! World known variables
 !
 real(SP):: db_alat(3)
 !
 ! The serial number is assigned by the YAMBO interfaces.
 ! It defines uniquely the db1/wf/kb_pp databases.
 !
 integer :: serial_number
 !
 logical :: IO_write
 logical :: IO_read
 !
 ! Global DB I/O control string
 !
 character(lchlen) :: DBs_IO_control_string = "none"
 logical           :: io_DIP
 logical           :: io_RESPONSE
 logical           :: io_HF
 logical           :: io_COLLs
 logical           :: io_J
 logical           :: io_CARRIERs
 logical           :: io_GF
 logical           :: io_SC
 logical           :: io_BS_K
 logical           :: io_SCREEN
 !
 character(lchlen) :: DBs_FRAG_control_string = "none"
 logical           :: frag_DIP
 logical           :: frag_RESPONSE
 logical           :: frag_WF
 logical           :: frag_QINDX
 logical           :: frag_HF
 logical           :: frag_SC
 logical           :: frag_RT
 logical           :: frag_ELPH
 logical           :: frag_BS_K
 logical           :: frag_SCREEN
 !
 ! Actions
 !
 integer, parameter:: RD_CL=1,OP_RD_CL=2,OP_WR_CL=3,OP_APP_WR_CL=4,OP_RD=5,OP_APP=6,&
&                     OP_WR=7,RD=8,WR=9,RD_CL_IF_END=10,WR_CL=11,WR_CL_IF_END=12,   &
&                     OP_IF_START_RD_CL_IF_END=13,OP_IF_START_WR_CL_IF_END=14
 !
 ! Modes
 !
 integer, parameter:: DUMP=31,VERIFY=32
 !
 ! COM
 !
 integer, parameter:: REP=41,LOG=42,NONE=43
 !
 ! I/O
 !
 integer, parameter:: MASTER=51,ALL=52
 !
 ! I/O errors
 !
 integer, parameter:: IO_NO_DATABASE=-1,IO_INCOMPATIBLE_VAR=-2,IO_GENERIC_ERROR=-3,&
&                     IO_NO_BINDING_ERROR=-4,IO_NO_ERROR=0,IO_OUTDATED_DB=-5,IO_NOT_ALLOWED=-6
 !
 ! I/O create / read / write modes
 !
 integer, parameter:: IO_WRITE_MODE =ior(nf90_share,nf90_write)
 integer, parameter:: IO_READ_MODE  =               nf90_nowrite
 !
#if defined _NC_CLASSIC
 integer, parameter:: IO_CREATE_MODE=    nf90_share
#elif defined _HDF5_IO
 integer, parameter:: IO_CREATE_MODE=ior(nf90_share,nf90_hdf5)
#else
 integer, parameter:: IO_CREATE_MODE=ior(nf90_share,nf90_64bit_offset)
#endif
 !
 ! Units
 !
 integer :: io_action(max_io_units)
 integer :: io_com(max_io_units)
 integer :: io_mode(max_io_units)
 integer :: io_status(max_io_units)
 integer :: io_sec(max_io_units,max_active_sections)
 integer :: io_rec_pos(max_io_units)
 integer :: io_unit(max_io_units)
 integer :: io_restart_point(max_io_units)
 integer :: io_code_version(max_io_units,3)
 integer :: io_code_revision(max_io_units,2) ! devel,GPL
 integer :: io_serial_number(max_io_units)
 integer :: io_netcdf_var(max_io_units)
 logical :: io_resetable(max_io_units)
 character(lchlen)          :: io_file(max_io_units)
 character(lchlen)          :: io_folder(max_io_units)
 !
 ! Type & extension
 !
 character(schlen)          :: io_raw_extension(max_io_units) ! Used to define the restart files.
                                                              ! It differs from io_extension as it does not contain any
                                                              ! i_fragment/j_fragment.
 character(schlen)          :: io_extension(max_io_units)
 integer                    :: io_type(max_io_units)
 !
 ! Save variables (to try different DB locations at the same time)
 !
 integer, private :: io_unit_save
 integer, private :: io_action_save
 integer, private :: io_mode_save
 integer, private :: io_sec_save(max_io_units)
 integer, private :: io_com_save
 !
 ! Interface
 !
 interface 
   !
   subroutine io_elemental(ID,VAR,VAR_SZ,CHECK,WARN,OP,I0,DB_I0,I1,DB_I1,&
&                          DB_R0,R0,R1,DB_R1,C0,DB_C0,CH0,DB_CH0,CH1,L0,DB_L0,UNIT,MENU,&
&                          DESCRIPTION,DO_NOT_DUMP)
     !
     use pars
     integer      :: ID
     character(*),optional :: VAR,OP(:)
     integer,     optional :: VAR_SZ,MENU
     integer,     optional :: I0,DB_I0,I1(:),DB_I1(:)
     real(SP),    optional :: R0,DB_R0,R1(:),DB_R1(:),UNIT
     complex(SP), optional :: C0,DB_C0
     logical     ,optional :: L0,DB_L0,CHECK,WARN,DO_NOT_DUMP
     character(*),optional :: CH0,DB_CH0,CH1(:)
     character(lchlen),optional :: DESCRIPTION
     !
   end subroutine
   !
   subroutine io_bulk(ID,VAR,VAR_SZ,I0,I1,I2,I3,I4,I5,&
                      &             R0,R1,R2,R3,R4,R5,&
                      &             C0,C1,C2,C3,C4,C5,&
                      &             L0,L1,L2,L3,L4,   &
                      &                            IPOS)
     !
     use pars
     integer      :: ID, error
     character(*),optional :: VAR
     integer,     optional :: VAR_SZ(:),IPOS(:)
     integer,     optional :: I0,I1(:),I2(:,:),I3(:,:,:),I4(:,:,:,:),I5(:,:,:,:,:)
     real(SP),    optional :: R0,R1(:),R2(:,:),R3(:,:,:),R4(:,:,:,:),R5(:,:,:,:,:)
     complex(SP), optional :: C0,C1(:),C2(:,:),C3(:,:,:),C4(:,:,:,:),C5(:,:,:,:,:)
     integer(LP), optional :: L0,L1(:),L2(:,:),L3(:,:,:),L4(:,:,:,:)
     !
   end subroutine
   !
   integer function io_header(ID,QPTS,R_LATT,WF,IMPOSE_SN,T_EL,KPTS,D_LATT,XC_KIND,CUTOFF,GAUGE,&
&                             IMPOSE_GAUGE,PARALLEL_STRUCTURE,FRAG)
     integer               :: ID
     logical,optional      :: QPTS,R_LATT,WF,IMPOSE_SN,T_EL,KPTS,D_LATT,CUTOFF,GAUGE,IMPOSE_GAUGE,FRAG
     character(*),optional :: XC_KIND,PARALLEL_STRUCTURE
   end function
   !
   logical function ver_is_gt_or_eq(ID,version,revision)
     integer          :: ID
     integer,optional :: version(3),revision
   end function
   !
   subroutine IO_and_Messaging_switch(what,CONDITION)
     character(*)      :: what
     logical, optional :: CONDITION
   end subroutine
   !
   subroutine io_fragment(ID,i_fragment,j_fragment,ierr)
     integer           :: ID
     integer, optional :: i_fragment,j_fragment,ierr
   end subroutine
   !
 end interface
 !
 contains
   ! 
   logical function open_is_on(ID)
     !
     ! Note that an identical IF statement must be defined in io_control.
     !
     integer :: ID
     open_is_on=any((/io_action(ID)==OP_RD_CL,io_action(ID)==OP_WR_CL,&
&                     io_action(ID)==OP_APP_WR_CL,io_action(ID)==OP_RD,&
&                     io_action(ID)==OP_APP,io_action(ID)==OP_WR/))
   end function
   !
   logical function close_is_on(ID)
     !
     integer :: ID
     close_is_on=any((/io_action(ID)==RD_CL,io_action(ID)==OP_RD_CL,&
&                      io_action(ID)==OP_WR_CL,io_action(ID)==OP_APP_WR_CL,&
&                      io_action(ID)==WR_CL/))
   end function
   !
   logical function read_is_on(ID)
     !
     integer :: ID
     read_is_on=any((/io_action(ID)==OP_RD_CL,io_action(ID)==OP_RD,&
&                     io_action(ID)==RD_CL,   io_action(ID)==RD,&
&                     io_action(ID)==RD_CL_IF_END,io_action(ID)==OP_IF_START_RD_CL_IF_END/))
   end function
   !
   logical function write_is_on(ID)
     !
     integer :: ID
     write_is_on=any((/io_action(ID)==OP_WR_CL,io_action(ID)==OP_APP_WR_CL,&
&                      io_action(ID)==WR      ,io_action(ID)==OP_APP,&
&                      io_action(ID)==OP_WR   ,io_action(ID)==WR_CL,&
&                      io_action(ID)==WR_CL_IF_END,io_action(ID)==OP_IF_START_WR_CL_IF_END/))
   end function
   ! 
   logical function DB_is_OK(ID)
     !
     integer :: ID
     DB_is_OK=any((/io_status(ID)==IO_NO_ERROR,io_status(ID)==IO_NO_BINDING_ERROR/))
     !
   end function
   !
   integer function io_connect(desc,subfolder,type,no_full_RESET,ID)
     !
     use stderr,        ONLY:string_pack
     use com,           ONLY:get_name,jobstr,file_exists,more_io_path,all_locations,&
&                            num_of_alternative_locations,n_alt_jobstr
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
     logical          ::file_found
     !
     io_connect=0
     !
     if ( (read_is_on(ID).and..not.IO_read).or.&
&         (write_is_on(ID).and..not.IO_write) ) then
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
       if ( (io_action(ID)==OP_APP_WR_CL.or.io_action(ID)==OP_APP) ) then
         !
         if( file_exists(trim(io_file(ID))) ) then
           call netcdf_call(nf90_open(trim(io_file(ID)),IO_WRITE_MODE,io_unit(ID)),ID)
         else
           call netcdf_call(nf90_create(trim(io_file(ID)),IO_CREATE_MODE,io_unit(ID)),ID)
           call netcdf_call(nf90_enddef(io_unit(ID)),ID)
           if (io_action(ID)==OP_APP_WR_CL) io_action(ID)=OP_WR_CL
           if (io_action(ID)==OP_APP) io_action(ID)=OP_WR
         endif
         !
       else
         !
         call netcdf_call(nf90_create(trim(io_file(ID)),IO_CREATE_MODE,io_unit(ID)),ID)
         call netcdf_call(nf90_enddef(io_unit(ID)),ID)
         !
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
           if (present(no_full_RESET)) then
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
         call netcdf_call(nf90_open(trim(io_file(ID)),IO_READ_MODE,io_unit(ID)),ID)
       endif
       !
       io_type(ID)=type
       !
     endif
     !
   end function
   !
   subroutine io_disconnect(ID,FORCE)
     !
     integer           :: ID
     logical, optional :: FORCE
     !
     if (present(FORCE)) then
       !
       if (.not.FORCE.and..not.close_is_on(ID).and.io_status(ID)>=0) return
       !
     else
       !
       if (.not.close_is_on(ID).and.io_status(ID)>=0) return
       !
     endif
     !
     !
     if ( (io_type(ID)/=-2.and.file_is_open(io_file(ID),ID)) .or.&
&         (io_type(ID)==-2.and.file_is_open(io_file(ID)))          ) then
       !
       if (io_type(ID)==-2) then
         close(unit=io_unit(ID))
       else
         call netcdf_call(nf90_close(io_unit(ID)),ID)
       endif
       !
       io_unit(ID)=0
       !
     endif
     !
     if (io_resetable(ID)) call io_reset(ID)
     !
   end subroutine
   !
   subroutine io_control(ACTION,MODE,COM,SEC,ID)
     !
     integer,intent(in)    :: ACTION
     integer,intent(inout) :: ID
     integer,optional,intent(in) :: MODE,COM,SEC(:)
     !
     ! Work Space
     !
     integer :: i1
     !
     ! Assign a new unit if the unit is not already open
     !
     if ( any((/ACTION==OP_RD_CL,ACTION==OP_WR_CL,&
&               ACTION==OP_APP_WR_CL,ACTION==OP_RD,&
&               ACTION==OP_APP,ACTION==OP_WR/)) ) then
       ID=-1
       do i1=1,max_io_units
         if (io_unit(i1)==0) then
           ID=i1
           call io_reset(ID)
           io_unit(ID)=40+i1
           exit
         endif
       enddo
       if(ID==-1) call error(" Recompile Yambo with a larger: max_io_units")
     endif
     !
     io_action(ID)=ACTION
     if (present(MODE)) io_mode(ID)=MODE
     if (present(COM )) io_com(ID)=COM
     if (present(SEC)) then
       io_sec(ID,:)=0
       io_sec(ID,:size(SEC))=SEC
     endif
     !
   end subroutine
   !
   subroutine manage_OP_IF_START_and_CL_IF_END(ID,start_,end_)
     !
     integer :: ID
     integer :: start_,end_
     !
     if (read_is_on(ID)) then
       if (io_action(ID)==RD_CL_IF_END.and.start_==end_) io_action(ID)=RD_CL
     else  if (write_is_on(ID)) then
       if (io_action(ID)==WR_CL_IF_END.and.start_==end_) io_action(ID)=WR_CL
     endif
     !
     if (read_is_on(ID)) then
       if (io_action(ID)==OP_IF_START_RD_CL_IF_END.and.start_==1                 ) io_action(ID)=OP_RD
       if (io_action(ID)==OP_IF_START_RD_CL_IF_END.and.              start_==end_) io_action(ID)=   RD_CL
       if (io_action(ID)==OP_IF_START_RD_CL_IF_END.and.start_==1.and.start_==end_) io_action(ID)=OP_RD_CL
     else  if (write_is_on(ID)) then
       if (io_action(ID)==OP_IF_START_WR_CL_IF_END.and.start_==1                 ) io_action(ID)=OP_WR
       if (io_action(ID)==OP_IF_START_WR_CL_IF_END.and.              start_==end_) io_action(ID)=   WR_CL
       if (io_action(ID)==OP_IF_START_WR_CL_IF_END.and.start_==1.and.start_==end_) io_action(ID)=OP_WR_CL
     endif
     !
   end subroutine
   !
   subroutine io_reset(ID)
     integer :: ID
     io_status(ID)=0
     io_unit(ID)=0
     io_mode(ID)=0
     io_sec(ID,:)=0
     io_com(ID)=NONE
     io_file(ID)=' ' 
     io_folder(ID)=' ' 
     io_raw_extension(ID)=' ' 
     io_extension(ID)=' ' 
     io_rec_pos(ID)=1
     io_restart_point(ID)=1
     io_code_version(ID,:)=code_version
     io_code_revision(ID,:)=(/code_revision,code_GPL_revision/)
     io_serial_number(ID)=serial_number
     io_resetable(ID)=.true.
   end subroutine
   !
   subroutine mk_dir(dirname)
     use stderr,         ONLY:cstr
     implicit none
     character(*)      :: dirname
     if (len_trim(dirname)==0) return
     call imkdir( cstr(trim(dirname)) )
   end subroutine
   !
   subroutine cp_file(file_,dest_,ierr_)
     use stderr,         ONLY:cstr
     implicit none
     character(*)      :: file_,dest_
     integer           :: ierr_
     call isystem( cstr("bash -c 'cp "//file_//" "//dest_//" >& /dev/null' " ), ierr_ )
   end subroutine
   !
   subroutine cp_directory(dir_,dest_,ierr_)
     use stderr,         ONLY:cstr
     implicit none
     character(*)      :: dir_,dest_
     integer           :: ierr_
     call isystem( cstr("bash -c 'cp -R "//dir_//" "//dest_//" >& /dev/null' " ), ierr_ )
   end subroutine
   !
   subroutine rm_file(filename)
     use stderr,         ONLY:cstr
     implicit none
     character(*)      :: filename
     if (len_trim(filename)==0) return
     call iremove( cstr(trim(filename)) )
   end subroutine
   !
   subroutine rename_file(filename_old,filename_new)
     use stderr,    ONLY:cstr
     implicit none
     character(*)      :: filename_old,filename_new
     if (len_trim(filename_old)==0) return
     call irename( cstr(trim(filename_old)), cstr(trim(filename_new)) )
   end subroutine
   !
   logical function file_is_present(desc,subfolder)
     !
     use com,           ONLY:all_locations,file_exists,num_of_alternative_locations
     !
     character(*)           :: desc
     character(*), optional :: subfolder
     !
     ! Work Space
     !
     integer            :: i_f
     character(lchlen)  :: possible_locations(num_of_alternative_locations)
     character(schlen)  :: subfolder_
     !
     subfolder_=' '
     if (present(subfolder)) subfolder_=subfolder
     !
     possible_locations=all_locations(desc,subfolder_)
     !
     file_is_present=.true.
     do i_f=1,num_of_alternative_locations
       if( file_exists(trim( possible_locations(i_f) )  ) ) return
     enddo
     file_is_present=.false.
     !
   end function file_is_present
   !
   logical function file_is_open(filename,ID)
     character(*)      :: filename
     integer, optional :: ID
     integer :: NC_ERR
     file_is_open=.false.
     if (trim(filename)=='') return
     if (present(ID)) then
       NC_ERR=nf90_inquire(io_unit(ID))
       file_is_open=NC_ERR==NF90_NOERR
     else
       inquire(file=filename,opened=file_is_open)
     endif
     !
   end function
   !
   !
   subroutine netcdf_call(status,ID,VAR)
     use com,           ONLY:error
     integer,      intent (in)           :: status
     integer,      intent (in)           :: ID
     character(*), intent (in), optional :: VAR
     character(lchlen) :: msg
     character(schlen) :: VARNAME,FILENAME
     integer           :: local_error
     if(status /= NF90_NOERR) then
       FILENAME=io_file(ID)
       VARNAME=' NOT DEFINED '
       if(.not.present(VAR)) then
         local_error=nf90_inquire_variable(io_unit(ID),io_netcdf_var(ID),VARNAME)
         if(local_error/=0) VARNAME=' NOT DEFINED '
       endif
       if(present(VAR)) VARNAME=VAR
       write (msg,'(6a)') ' File ',trim(FILENAME),'; Variable ',trim(VARNAME),'; ',trim(nf90_strerror(status))
       call error(trim(msg))
     end if
   end subroutine
   !
   integer function netcdf_dim(ID,DIM)
     integer, intent ( in) :: ID,DIM
     integer       :: dim_found
     character(12) :: dim_strng
     netcdf_dim=0
     write (dim_strng,'(a,i10.10)') 'D_',dim
     dim_found=nf90_inq_dimid(io_unit(ID),dim_strng,netcdf_dim)
     if (dim_found/=nf90_noerr) call netcdf_call(nf90_def_dim(io_unit(ID),dim_strng,dim,netcdf_dim),ID)
   end function
   !
   logical function variable_exist(ID,var_name,var_ID)
     integer,           intent ( in) :: ID
     character(*),      intent ( in) :: var_name
     integer, optional, intent (out) :: var_ID
     integer                    :: var_check,var_ID_
     var_check=nf90_inq_varid(io_unit(ID),var_name,var_ID_)
     if (var_check==NF90_NOERR) variable_exist=.TRUE.
     if (var_check/=NF90_NOERR) variable_exist=.FALSE.
     if (present(var_ID)) then
       if(     variable_exist) var_ID=var_ID_
       if(.not.variable_exist) var_ID=-1
     endif
   end function
   !
   subroutine deliver_IO_error_message(i_err,DB,STOP_it)
     !
     use com,        ONLY:warning,error
     !
     integer          :: i_err
     logical,optional :: STOP_it
     character(*)     :: DB
     !
     ! ws
     logical          :: STOP_
     !
     if (i_err==0) return
     !
     STOP_=.FALSE.
     if (present(STOP_it)) STOP_=STOP_it
     !
     if (STOP_) then
       if (i_err==IO_NO_DATABASE     ) call error('Missing all or part of '//DB//' DB')
       if (i_err==IO_INCOMPATIBLE_VAR) call error('Variable not compatible in '//DB//' DB')
       if (i_err==IO_OUTDATED_DB)      call error('Too old '//DB//' DB')
     else
       if (i_err==IO_NO_DATABASE     ) call warning('Missing all or part of '//DB//' DB')
       if (i_err==IO_INCOMPATIBLE_VAR) call warning('Variable not compatible in '//DB//' DB')
       if (i_err==IO_OUTDATED_DB)      call warning('Too old '//DB//' DB')
     endif
     !
   end subroutine
   !
end module IO_m
