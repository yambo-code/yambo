!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
 use pars,    ONLY:SP,lchlen,schlen
 use LOGO,    ONLY:code_version,code_revision
#if defined _NETCDF_IO
 use netcdf
#endif
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
 logical :: dbs_fragment
 !
 ! Actions
 !
 integer, parameter:: RD_CL=1,OP_RD_CL=2,OP_WR_CL=3,OP_APP_WR_CL=4,OP_RD=5,OP_APP=6,&
&                     OP_WR=7,RD=8,WR=9,RD_CL_IF_END=10,WR_CL=11,WR_CL_IF_END=12
 !
 ! Modes
 !
 integer, parameter:: DUMP=31,VERIFY=32
 !
 ! COM
 !
 integer, parameter:: REP=41,LOG=42,NONE=43
 !
 ! Units
 !
 integer, parameter:: max_io_units=10
 integer, parameter:: max_active_sections=10
 integer :: io_action(max_io_units)
 integer :: io_com(max_io_units)
 integer :: io_mode(max_io_units)
 integer :: io_status(max_io_units)
 integer :: io_sec(max_io_units,max_active_sections)
 integer :: io_rec_pos(max_io_units)
 integer :: io_unit(max_io_units)
 integer :: io_restart(max_io_units)
 integer :: io_code_version(max_io_units,3)
 integer :: io_code_revision(max_io_units)
 integer :: io_serial_number(max_io_units)
 integer :: io_netcdf_var(max_io_units)
 logical :: io_fragmented(max_io_units)
 logical :: io_resetable(max_io_units)
 logical :: io_netcdf_support(max_io_units)
 character(lchlen)          :: io_file(max_io_units)
 !
 ! Private variables
 !
 character(schlen), private :: io_extension(max_io_units)
 integer,           private :: io_type(max_io_units)
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
&                          DB_R0,R0,R1,DB_R1,CH0,CH1,L0,DB_L0,UNIT,MENU,&
&                          DESCRIPTION)
     !
     use pars
     integer      :: ID
     character(*),optional :: VAR,OP(:)
     integer,     optional :: VAR_SZ,MENU
     integer,     optional :: I0,DB_I0,I1(:),DB_I1(:)
     real(SP),    optional :: R0,DB_R0,R1(:),DB_R1(:),UNIT
     logical     ,optional :: L0,DB_L0,CHECK,WARN
     character(*),optional :: CH0,CH1(:)
     character(lchlen),optional :: DESCRIPTION
     !
   end subroutine
   !
   subroutine io_bulk(ID,VAR,VAR_SZ,I0,I1,I2,I3,R0,R1,R2,R3,C1,IPOS)
     !
     use pars
     integer      :: ID
     character(*),optional :: VAR
     integer,     optional :: VAR_SZ(:),IPOS(:)
     integer,     optional :: I0,I1(:),I2(:,:),I3(:,:,:)
     real(SP),    optional :: R0,R1(:),R2(:,:),R3(:,:,:)
     complex(SP), optional :: C1(:)
     !
   end subroutine
   !
   integer function io_header(ID,QPTS,R_LATT,WF,IMPOSE_SN,&
&                             T_EL,KPTS,D_LATT,XC_KIND)
     integer               :: ID
     logical,optional      :: QPTS,R_LATT,WF,IMPOSE_SN,T_EL,KPTS,D_LATT
     character(*),optional :: XC_KIND
   end function
   !
   logical function ver_is_gt_or_eq(ID,version,revision)
     integer          :: ID
     integer,optional :: version(3),revision
   end function
   !
 end interface
 !
 contains
   ! 
   logical function open_is_on(ID)
     !
     ! Note that an identical IF statement must be defined in 
     ! io_control.
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
&                     io_action(ID)==RD_CL_IF_END/))
   end function
   !
   logical function write_is_on(ID)
     !
     integer :: ID
     write_is_on=any((/io_action(ID)==OP_WR_CL,io_action(ID)==OP_APP_WR_CL,&
&                      io_action(ID)==WR      ,io_action(ID)==OP_APP,&
&                      io_action(ID)==OP_WR   ,io_action(ID)==WR_CL,&
&                      io_action(ID)==WR_CL_IF_END/))
   end function
   !
   integer function io_connect(desc,subfolder,type,ENABLE_LARGE_FILE,ID)
     !
     use stderr,        ONLY:string_pack
     use com,           ONLY:get_name,jobstr,file_exists,more_io_path
     character(*)           :: desc
     integer                :: type,ID
     character(*), optional :: subfolder
     logical,      optional :: ENABLE_LARGE_FILE
     !
     ! Work Space
     !
     integer          ::CREATE_MODE
     character(lchlen)::alternative_file(8),folder
     character(schlen)::subfolder_
     !
     io_connect=0
     if ( (read_is_on(ID).and..not.IO_read).or.&
&         (write_is_on(ID).and..not.IO_write) ) then
       io_connect=-1
       call io_reset(ID)
       return
     endif
     !
     ! Check if file is already open
     !
     if (file_is_open(io_file(ID),ID)) return
     !
     ! Keep the extension for the io_fragment routine
     !
     io_extension(ID)=desc
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
         io_file(ID)=get_name(desc,subfolder_,type,CORE_IO=.FALSE.,&
&                             MORE_IO=.TRUE.,COM_IO=.FALSE.,NETCDF=io_netcdf_support(ID))
         !
       else if (read_is_on(ID)) then
         !
         io_file(ID)=get_name(desc,subfolder_,type,CORE_IO=.FALSE.,&
&                             MORE_IO=.TRUE.,COM_IO=.FALSE.,NETCDF=io_netcdf_support(ID))
         !
         if (type/=-2.and.type/=1.and.type/=2) then
           !
           ! Alternative is the same as when write_is_on(ID)=.TRUE. with(/out) NETCDF support
           !
           alternative_file(1)=get_name(desc,subfolder_,type,CORE_IO=.FALSE.,&
&                                       MORE_IO=.TRUE.,COM_IO=.FALSE.,NETCDF=.not.io_netcdf_support(ID))
         endif
         !
         if (type==1.or.type==2) then
           !
           ! OLD databases are searched in the following order:
           !
           ! 1/2. MORE_IO=.TRUE.  CORE_IO=.FALSE. COM_IO=.FALSE. type=2 (with JOBSTR)
           ! 3/4. MORE_IO=.TRUE.  CORE_IO=.FALSE. COM_IO=.FALSE. type=1 (no   JOBSTR)
           ! 5/6. MORE_IO=.FALSE. CORE_IO=.TRUE. COM_IO=.FALSE.  type=2 (with JOBSTR)
           ! 7/8. MORE_IO=.FALSE. CORE_IO=.TRUE. COM_IO=.FALSE.  type=1 (no   JOBSTR)
           !
           alternative_file(1)=get_name(desc,subfolder_,2,CORE_IO=.FALSE.,&
&                                       MORE_IO=.TRUE.,COM_IO=.FALSE.,NETCDF=.not.io_netcdf_support(ID))
           alternative_file(2)=get_name(desc,subfolder_,2,CORE_IO=.FALSE.,&
&                                       MORE_IO=.TRUE.,COM_IO=.FALSE.,NETCDF=io_netcdf_support(ID))
           alternative_file(3)=get_name(desc,subfolder_,1,CORE_IO=.FALSE.,&
&                                       MORE_IO=.TRUE.,COM_IO=.FALSE.,NETCDF=.not.io_netcdf_support(ID))
           alternative_file(4)=get_name(desc,subfolder_,1,CORE_IO=.FALSE.,&
&                                       MORE_IO=.TRUE.,COM_IO=.FALSE.,NETCDF=io_netcdf_support(ID))
           alternative_file(5)=get_name(desc,subfolder_,2,CORE_IO=.TRUE.,&
&                                       MORE_IO=.FALSE.,COM_IO=.FALSE.,NETCDF=.not.io_netcdf_support(ID))
           alternative_file(6)=get_name(desc,subfolder_,2,CORE_IO=.TRUE.,&
&                                       MORE_IO=.FALSE.,COM_IO=.FALSE.,NETCDF=io_netcdf_support(ID))
           alternative_file(7)=get_name(desc,subfolder_,1,CORE_IO=.TRUE.,&
&                                       MORE_IO=.FALSE.,COM_IO=.FALSE.,NETCDF=.not.io_netcdf_support(ID))
           alternative_file(8)=get_name(desc,subfolder_,1,CORE_IO=.TRUE.,&
&                                       MORE_IO=.FALSE.,COM_IO=.FALSE.,NETCDF=io_netcdf_support(ID))
           !
         endif
         !
       endif
       !
     endif
     !
     ! Create Directories 
     !
     if (len_trim(jobstr)>0) then
       write (folder,'(3a)') trim(more_io_path),'/',trim(jobstr)
     else
       write (folder,'(2a)') trim(more_io_path),'/SAVE'
     endif
     if (write_is_on(ID).and.(type==2.or.len_trim(jobstr)==0)) then
       call mk_dir(folder)
       if (present(subfolder)) call mk_dir(string_pack(folder,"/",subfolder))
     endif
     !
     ! Open if to open and to write
     !
     if (open_is_on(ID).and.write_is_on(ID)) then
       if (io_netcdf_support(ID)) then
#if defined _NETCDF_IO
         !
         ! Setting NF90_64BIT_OFFSET causes netCDF to create a 64-bit 
         ! offset format file, instead of a netCDF classic format file. 
         ! The 64-bit offset format imposes far fewer restrictions on very large 
         ! (i.e. over 2 GB) data files. See Large File Support.
         !
         ! http://www.unidata.ucar.edu/software/netcdf/docs/netcdf/Large-File-Support.html
         ! http://www.unidata.ucar.edu/software/netcdf/faq-lfs.html
         !
         CREATE_MODE=nf90_share
         if ( present(ENABLE_LARGE_FILE)) CREATE_MODE=ior(nf90_share,nf90_64bit_offset)
         !
         if ( (io_action(ID)==OP_APP_WR_CL.or.io_action(ID)==OP_APP) ) then
           !
           if( file_exists(trim(io_file(ID))) ) then
             call netcdf_call(nf90_open(trim(io_file(ID)),&
&                             ior(nf90_write,nf90_share),io_unit(ID)))
           else
             call netcdf_call(nf90_create(trim(io_file(ID)),CREATE_MODE,io_unit(ID)))
             call netcdf_call(nf90_enddef(io_unit(ID)))
             if (io_action(ID)==OP_APP_WR_CL) io_action(ID)=OP_WR_CL
             if (io_action(ID)==OP_APP) io_action(ID)=OP_WR
           endif
           !
         else
           !
           call netcdf_call(nf90_create(trim(io_file(ID)),CREATE_MODE,io_unit(ID)))
           call netcdf_call(nf90_enddef(io_unit(ID)))
           !
         endif
#endif
       else
         !
         if ( (io_action(ID)==OP_APP_WR_CL.or.io_action(ID)==OP_APP) ) then
           if( file_exists(trim(io_file(ID))) ) then
             open(unit=io_unit(ID),file=trim(io_file(ID)),&
&                 form='unformatted',position='append')
           else
             open(unit=io_unit(ID),file=trim(io_file(ID)),form='unformatted')
             if (io_action(ID)==OP_APP_WR_CL) io_action(ID)=OP_WR_CL
             if (io_action(ID)==OP_APP) io_action(ID)=OP_WR
           endif
         else
           open(unit=io_unit(ID),file=trim(io_file(ID)),form='unformatted')
         endif
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
         if (file_exists(trim(alternative_file(1)))) then
           io_file(ID)=alternative_file(1)
           io_netcdf_support(ID)=.not.io_netcdf_support(ID)
         else if (file_exists(trim(alternative_file(2)))) then
           io_file(ID)=alternative_file(2)
         else if (file_exists(trim(alternative_file(3)))) then
           io_file(ID)=alternative_file(3)
           io_netcdf_support(ID)=.not.io_netcdf_support(ID)
         else if (file_exists(trim(alternative_file(4)))) then
           io_file(ID)=alternative_file(4)
         else if (file_exists(trim(alternative_file(5)))) then
           io_file(ID)=alternative_file(5)
           io_netcdf_support(ID)=.not.io_netcdf_support(ID)
         else if (file_exists(trim(alternative_file(6)))) then
           io_file(ID)=alternative_file(6)
         else if (file_exists(trim(alternative_file(7)))) then
           io_file(ID)=alternative_file(7)
           io_netcdf_support(ID)=.not.io_netcdf_support(ID)
         else if (file_exists(trim(alternative_file(8)))) then
           io_file(ID)=alternative_file(8)
         else
           io_connect=-1
           call io_reset(ID)
           io_type(ID)=0
           return
         endif
       endif
       if (io_netcdf_support(ID)) then
#if defined _NETCDF_IO
         call netcdf_call(nf90_open(trim(io_file(ID)),&
&                         nf90_nowrite,io_unit(ID)))
#else
         io_connect=-1
         call io_reset(ID)
         io_type(ID)=0
         return
#endif
       else
         open(unit=io_unit(ID),file=trim(io_file(ID)),form='unformatted')
       endif
       !
       io_type(ID)=type
       !
     endif
     !
   end function
   !
   subroutine io_fragment(ID,i_pointer,j_pointer,k_pointer,ierr)
     !
     use stderr, ONLY:intc
     !
     ! This routine checks if the DB is fragmented. If it is
     ! it closes the DB and opens the fragment corresponding
     ! to the i_pointer (j_pointer) pointers.
     !
     ! Note that, as io_disconnect is called with a negative
     ! argument io_reset is no called. This is needed to save
     ! the prperties of the DB that have been read previously 
     ! (like io_nectdf_support or io_code_version/revision).
     !
     implicit none
     integer           :: ID
     integer, optional :: i_pointer,j_pointer,k_pointer,ierr
     !
     ! Work Space
     !
     integer           :: ierr_,i_fragment
     character(schlen) :: subfolder
     logical           :: close_is_on_save
     !
     if (.not.io_fragmented(ID)) return
     !
     close_is_on_save=close_is_on(ID)
     !
     if (read_is_on(ID))  io_action(ID)=OP_RD_CL
     if (write_is_on(ID)) io_action(ID)=OP_WR_CL
     !
     if (present(i_pointer)) i_fragment=i_pointer
     if (.not.present(i_pointer)) i_fragment=maxval(io_sec(ID,:))
     !
     ! Disconnect without calling io_reset
     !
     io_resetable(ID)=.false.
     !
     call io_disconnect(ID)
     !
     ! io_file is redefined by io_connect
     !
     io_file(ID)=' '
     !
     write (subfolder,'(2a,i5.5)') trim(io_extension(ID)),'_',i_fragment
     if (present(j_pointer)) write (subfolder,'(2a,i5.5,a,i5.5)') &
&                            trim(io_extension(ID)),'_',i_fragment,'_',j_pointer
     !
     ! Note that the fragment DB keeps the same type of the DB core (io_type(ID))
     !
     if (present(k_pointer)) then
       ierr_=io_connect('fragment_'//trim(intc(k_pointer)),subfolder=subfolder,type=io_type(ID),ID=ID)
     else
       ierr_=io_connect('fragment',subfolder=subfolder,type=io_type(ID),ID=ID)
     endif
     if (present(ierr)) ierr=ierr_
     !
     ! io_action must be saved otherwise any io_disconnect call would
     ! call io_reset (like in ioWF) 
     !
     if (close_is_on_save) io_resetable(ID)=.true.
     !
   end subroutine
   !
   subroutine io_disconnect(ID)
     !
     integer :: ID
     !
     if (.not.close_is_on(ID).and.io_status(ID)>=0) return
     !
     if (file_is_open(io_file(ID),ID)) then
       if (io_netcdf_support(ID)) then
#if defined _NETCDF_IO
         call netcdf_call(nf90_close(io_unit(ID)))
#endif
       else
         close(unit=io_unit(ID))
       endif
     endif
     !
     if (io_resetable(ID)) call io_reset(ID)
     !
   end subroutine
   !
   subroutine io_check_restart(desc,ID,now,todo)
     !
     use drivers, ONLY:list_dbs
     use stderr,  ONLY:string_pack
     use com,     ONLY:get_name,file_exists,more_io_path
     use timing,  ONLY:date_and_time_string
     use com,     ONLY:msg
     character(*)::desc
     integer     ::ID,i1
     integer, optional::now,todo
     character(lchlen)::restart_file,restart_folder,ch
     !
     io_restart(ID)=0
     !
     if (present(now)) then
       !
       if (.not.write_is_on(ID)) return
       !
       restart_file  =get_name(desc=desc,type=4,CORE_IO=.false.,MORE_IO=.true.,COM_IO=.FALSE.,NETCDF=.false.)
       !
       restart_folder=string_pack(trim(more_io_path),"/RESTART")
       !
       if (now==todo) then
         call rm_file(restart_file)
         call rm_file(restart_folder)
         return
       endif
       !
       if (.not.file_exists(trim(restart_folder))) call mk_dir(restart_folder)
       !
       open(unit=81,file=trim(restart_file))
       write (81,'(/2x,a,3(i2.2,a),a,i3.3)') &
&            'Restart file for YAMBO ',code_version(1),&
&            '.',code_version(2),'.',code_version(3),' -- ',&
&            'revision ',code_revision
       ch=date_and_time_string()
       write (81,'(/2x,a)') trim(ch)
       write (81,'(/2x,2(a,i6,1x))') 'Section Completed ',now,'. To reach ',todo
       close(81)
       !
     else
       !
       if (io_mode(ID)/=VERIFY.and.io_mode(ID)/=DUMP) return
       !
       restart_file=get_name(desc=desc,type=4,CORE_IO=.false.,MORE_IO=.true.,COM_IO=.FALSE.,NETCDF=.false.)
       if (.not.file_exists(trim(restart_file))) return
       !
       open(unit=81,file=trim(restart_file))
       read (81,1) ch(:18),io_restart(ID)
       close(81)
       io_restart(ID)=io_restart(ID)+1
       !
       if (list_dbs) then
         call msg('s',trim(string_pack('[RESTARTer] Section(s) completed for ',&
&                       trim(io_file(ID)),' :')),io_restart(ID)-1)
    
       else if (io_com(ID)/=NONE) then
         call msg('rns',trim(string_pack('[RESTARTer] Section(s) completed for ',&
&                       trim(io_file(ID)),' :')),io_restart(ID)-1)
       endif
     endif
     !
1    format(/////2x,a,i6)
     !
   end subroutine
   !
   subroutine io_control(ACTION,MODE,COM,SEC,ID,NETCDF)
     !
     integer :: ACTION,ID
     integer,optional :: MODE,COM,SEC(:)
     logical,optional :: NETCDF
     !
     ! Work Space
     !
     integer :: i1
     !
     ! Assign a new unit if the unit is not alreadu open
     !
     if ( any((/ACTION==OP_RD_CL,ACTION==OP_WR_CL,&
&               ACTION==OP_APP_WR_CL,ACTION==OP_RD,&
&               ACTION==OP_APP,ACTION==OP_WR/)) ) then
       do i1=1,max_io_units
         if (io_unit(i1)==0) then
           ID=i1
           io_unit(ID)=40+i1
           exit
         endif
       enddo
     endif
     !
     io_action(ID)=ACTION
     if (present(MODE)) io_mode(ID)=MODE
     if (present(COM )) io_com(ID)=COM
     if (present(SEC)) then
       io_sec(ID,:)=0
       io_sec(ID,:size(SEC))=SEC
     endif
     if (write_is_on(ID)) io_fragmented(ID)=dbs_fragment
     !
#if defined _NETCDF_IO
     if (present(NETCDF)) io_netcdf_support(ID)=NETCDF
#endif
     !
   end subroutine
   !
   subroutine manage_RD_WR_CL_IF_END(ID,start_,end_)
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
     io_extension(ID)=' ' 
     io_rec_pos(ID)=1
     io_restart(ID)=1
     io_code_version(ID,:)=code_version
     io_code_revision(ID)=code_revision
     io_serial_number(ID)=serial_number
     io_resetable(ID)=.true.
     io_netcdf_support(ID)=.false.
#if defined _NETCDF_IO
     io_netcdf_support(ID)=.true.
#endif
   end subroutine
   !
   subroutine mk_dir(dirname)
     use pars
     use stderr,         ONLY:cstr
     implicit none
     character(*)      :: dirname
     if (len_trim(dirname)==0) return
     call imkdir( cstr(trim(dirname)) )
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
   logical function file_is_open(filename,ID)
     character(*)      :: filename
     integer, optional :: ID
     integer :: NC_ERR
     file_is_open=.false.
     if (trim(filename)=='') return
     if (present(ID)) then
       if (io_netcdf_support(ID))  then
#if defined _NETCDF_IO
         NC_ERR=nf90_inquire(io_unit(ID))
         file_is_open=NC_ERR==NF90_NOERR
#endif
       else
         inquire(file=filename,opened=file_is_open)
       endif
     else
       inquire(file=filename,opened=file_is_open)
     endif
     !
   end function
   !
   subroutine netcdf_call(status)
     use com,           ONLY:error
     integer, intent ( in) :: status
#if defined _NETCDF_IO
     character(schlen) :: msg
     if(status /= nf90_noerr) then
       write (msg,'(2a)') '[NetCDF] ',trim(nf90_strerror(status))
       call error(trim(msg))
     end if
#endif
   end subroutine
   !
   integer function netcdf_dim(ID,DIM)
     integer, intent ( in) :: ID,DIM
     integer       :: dim_found
     character(12) :: dim_strng
     netcdf_dim=0
#if defined _NETCDF_IO
     write (dim_strng,'(a,i10.10)') 'D_',dim
     dim_found=nf90_inq_dimid(io_unit(ID),dim_strng,netcdf_dim)
     if (dim_found/=nf90_noerr) call netcdf_call(&
&                               nf90_def_dim(io_unit(ID),dim_strng,dim,netcdf_dim))
#endif
   end function
   !
   integer function variable_is_found(ID,var_name)
     integer,      intent ( in) :: ID
     character(*), intent ( in) :: var_name
     integer :: var_ID ! Work Space
     variable_is_found=-1
#if defined _NETCDF_IO
     if (nf90_inq_varid(io_unit(ID),var_name,var_ID)==nf90_NoErr) variable_is_found=1  
     if (nf90_inq_varid(io_unit(ID),var_name,var_ID)/=nf90_NoErr) variable_is_found=0  
#endif
   end function
   !
end module IO_m
