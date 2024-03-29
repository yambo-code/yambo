!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): CH DS
!
integer function e2y(np,pid,lnstr,iinf,iind,iod,icd,ijs,instr,inf,ind,od,com_dir,js)
 !
 use etsf_io
 use etsf_io_low_level
 use etsf_data
 use pars,                ONLY: SP,lchlen
 use LOGO,                ONLY: pickup_a_random
 use com,                 ONLY: msg,write_to_report,core_io_path
 use parallel_m,          ONLY: ncpu,myid
 use electrons,           ONLY: levels,E_reset,n_spinor,n_sp_pol
 use D_lattice,           ONLY: n_atoms_species_max
 use pseudo,              ONLY: PP_alloc_abinit, PP_free
 use R_lattice,           ONLY: bz_samp,bz_samp_reset,nkibz
 use wave_func,           ONLY: wf_ncx,io_WF,wf_nb_io_groups,wf_nb_io
 use IO_int,              ONLY: io_control
 use IO_m,                ONLY: OP_WR_CL,NONE,OP_APP_CL,serial_number
 use mod_com2y,           ONLY: interface_presets, force_noWFs
 use mod_wf2y,            ONLY: WF_splitter
 !
 implicit none
 integer,          intent(in) :: lnstr,iind,iod,ijs,np,pid,icd
 integer,          intent(in) :: iinf
 character(lnstr), intent(in) :: instr
 character(iinf),  intent(in) :: inf
 character(iind),  intent(in) :: ind
 character(iod),   intent(in) :: od
 character(ijs),   intent(in) :: js
 character(icd),   intent(in) :: com_dir   
 !
 type(levels)                 :: en
 type(bz_samp)                :: k
 !
 character(lchlen)            :: KSS_filename,WFK_filename
 integer                      :: ID,io_err,i_sp_pol,ik,ifrag,ib_grp
 integer,     external        :: io_DB1, io_KB_abinit
 complex(SP), allocatable     :: wf_disk(:,:,:)
 ! 
 ! Work Space
 !
 logical                     :: lstat      ! to get informed on error
 type(etsf_io_low_error)     :: error_data ! to store informations about errors
 integer                     :: ncid
 !
 logical, external           :: file_exists
 !
 ! Presets
 !=========
 e2y   = 0
 ncpu  = np
 myid  = pid
 !
 ! ... Internal Defaults
 !
 call SET_defaults(instr,ind,od,'')
 !
 ! ... C_driver -> YPP information transfer
 !
 call C_driver_transfer()
 !
 call SET_job_strings_and_dirs(js)
 call IO_make_directories("MORE LOG")
 call interface_presets(instr)
 call bz_samp_reset(k)
 call E_reset(en)
#if defined _MEM_CHECK 
 call MEM_defaults( )
 call MEM_treshold_estimate( )
#endif
 !
 ! S/N
 !======
 serial_number=pickup_a_random(10000._SP)
 !
 ! Switch off report file support
 !================================
 write_to_report=.FALSE.
 !
 ! LOGO
 !
 call write_the_logo(6,' ')
 !
 call msg('s','E(TSF) 2 Y(ambo)')
 !
 ! Check input filenames 
 !
 call msg('s','Checking input file ...')
 !
 if(index(trim(inf),'KSS')/=0) ETSF_kind='KSS'
 if(index(trim(inf),'WFK')/=0) ETSF_kind='WFK'
 !
 if(trim(ETSF_kind)=='KSS') then
   KSS_filename=trim(inf)
   call msg('s','ETSF KSS file ',trim(KSS_filename))
 endif
 !
 if(trim(ETSF_kind)=='WFK') then
   WFK_filename=trim(inf)
   call msg('s','ETSF WFK file ',trim(WFK_filename))
 endif
 !
 if(trim(ETSF_kind)/='KSS'.and.trim(ETSF_kind)/='WFK') then
   call msg('ln','File kind not supported! Use either KSS or WFK filename.')
   return
 endif

 if(.not.file_exists(trim(inf))) then
   call msg('ln','File not found! Specify -F filename.')
   return
 endif
 !
 ! Open ETSF file for reading
 !
 if(trim(ETSF_kind)=='KSS') call etsf_io_low_open_read(ncid, trim(KSS_filename), lstat, error_data = error_data)
 if(trim(ETSF_kind)=='WFK') call etsf_io_low_open_read(ncid, trim(WFK_filename), lstat, error_data = error_data)
 !
 if (.not. lstat) call etsf_long_error(error_data)
 call msg('s','DBs path set to ',trim(core_io_path))
 !
 ! DB1
 !==========================================
 !
 call e2y_db1(en,k,ncid)
 !
 if (force_noWFs) then
   !
   ! Write the data to YAMBO DB1
   !
   call msg('s',' == Writing DB1 ...')
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
   io_err=io_DB1(en,k,ID)
   call msg('ln','done ==')
   return
 endif
 !
 call WF_splitter()
 !
 ! WF & PP
 !==========================================
 !
 if(trim(ETSF_kind)=='KSS') then
   call msg('s',' == Writing DB2 (wavefunctions) + nlPP ...') 
   call PP_alloc_abinit()
 endif
 !
 if(trim(ETSF_kind)=='WFK') &
&  call msg('s',' == Writing DB2 (wavefunctions) ...') 
 !
 allocate(wf_disk(wf_ncx,n_spinor,wf_nb_io))
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=1,nkibz
     !
     ifrag=ik+(i_sp_pol-1)*nkibz
     !
     do ib_grp=1,wf_nb_io_groups
       !
       ! Read from the etsf-nc file
       !
       call e2y_wf(wf_disk,i_sp_pol,ik,ib_grp,ncid)
       ! 
       ! Write to the YAMBO WF database
       ! 
       if (ifrag==1.and.ib_grp==1) call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2,1/),ID=ID)
       if (ifrag> 1.or. ib_grp> 1) call io_control(ACTION=OP_APP_CL,COM=NONE,SEC=(/ifrag+1,ib_grp/),ID=ID)
       io_err=io_WF(ID,wf_disk)
       !
       if(trim(ETSF_kind)=='KSS') then
         call e2y_kb_pp(ik,ncid) 
         if (n_atoms_species_max>0.and.ib_grp==1) then
           if (ifrag==1) call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
           if (ifrag> 1) call io_control(ACTION=OP_APP_CL,COM=NONE,SEC=(/ifrag+1/),ID=ID)
           io_err=io_KB_abinit(ID) 
         endif
       endif
       !
     enddo
     !
   enddo
   !
 enddo
 !
 deallocate(wf_disk)
 if(trim(ETSF_kind)=='KSS') call PP_free()
 !
 call msg('l','done ==')
 !
 ! Write the data to YAMBO DB1
 !
 call msg('s',' == Writing DB1 ...')
 call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
 io_err=io_DB1(en,k,ID)
 call msg('ln','done ==')
 !
 ! Close file
 !
 call etsf_io_low_close(ncid, lstat, error_data = error_data)
 !
end function

 subroutine etsf_long_error(error_data)
   use etsf_io_low_level, only : etsf_io_low_error, etsf_io_low_error_handle
   type(etsf_io_low_error), intent(in)     :: error_data
   call etsf_io_low_error_handle(error_data)
   call error('YAMBO dies')
 end subroutine etsf_long_error
