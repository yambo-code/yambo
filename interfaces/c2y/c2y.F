!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DV
!
integer function c2y(np,pid,lnstr,iinf,iind,iod,icd,ijs,instr,inf,ind,od,com_dir,js)
 !
 use pars,                ONLY:SP,lchlen
 use LOGO,                ONLY:pickup_a_random
 use com,                 ONLY:msg,write_to_report,core_io_path
 use parallel_m,          ONLY:ncpu,myid
 use electrons,           ONLY:levels,E_reset,n_spinor,n_sp_pol
 use R_lattice,           ONLY:bz_samp,bz_samp_reset,nkibz
 use wave_func,           ONLY:wf_ncx,io_WF,wf_nb_io_groups,wf_nb_io
 use IO_int,              ONLY:io_control
 use IO_m,                ONLY:OP_WR_CL,NONE,OP_APP_CL,serial_number
 use mod_com2y,           ONLY:interface_presets,force_noWFs
 use mod_wf2y,            ONLY:WF_splitter
 use parallel_int,        ONLY:PARALLEL_live_message
 !
#include<memory.h>
 type(levels)                 :: en 
 type(bz_samp)                :: k 
 integer,          intent(in) :: lnstr,iind,iod,ijs,np,pid,icd
 integer,          intent(in) :: iinf
 character(lnstr), intent(in) :: instr
 character(iinf),  intent(in) :: inf
 character(iind),  intent(in) :: ind
 character(iod),   intent(in) :: od
 character(ijs),   intent(in) :: js
 character(icd),   intent(in) :: com_dir   
 !
 character(lchlen) :: file_name_
 integer           :: ID,io_err,i_sp_pol,ik,ifrag,ib_grp
 integer,     external :: c2y_file_name,io_DB1
 complex(SP), allocatable :: wf_disk(:,:,:)
 !
 ! Presets
 !
 c2y   =0
 ncpu  =np
 myid  =pid
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
 !
 serial_number=pickup_a_random(10000._SP)
 !
 ! Switch off report file support
 !
 write_to_report=.FALSE.
 !
 ! LOGO
 !
 call write_the_logo(6,' ')
 !
 call PARALLEL_live_message('C(pmd) 2 Y(ambo)')
 !
 call msg('s','Checking input file ...')
 !
 if ( c2y_file_name(inf,file_name_) < 0 ) then
   call msg('ln','failed')
   return
 endif
 call msg('l',trim(file_name_))
 !
 call msg('s','DBs path set to ',trim(core_io_path))
 !
 call c2y_db1(en,k,trim(file_name_))
 !
 call msg('s','== DB1 ...')
 !
 call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
 io_err=io_DB1(en,k,ID)
 !
 if (force_noWFs) then
   call msg('ln','done ==')
   return
 else
   call msg('l','done ==')
 endif
 !
 call WF_splitter()
 !
 call msg('s','== DB2 ...')
 !
 YAMBO_ALLOC(wf_disk,(wf_ncx,n_spinor,wf_nb_io))
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=1,nkibz
     !
     ifrag=ik+(i_sp_pol-1)*nkibz
     !
     do ib_grp=1,wf_nb_io_groups
       !
       call c2y_wf(wf_disk,i_sp_pol,ik,ib_grp,trim(file_name_))
       !
       if (ifrag==1.and.ib_grp==1) call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2,1/),ID=ID)
       if (ifrag> 1.or. ib_grp> 1) call io_control(ACTION=OP_APP_CL,COM=NONE,SEC=(/ifrag+1,ib_grp/),ID=ID)
       io_err=io_WF(ID,wf_disk)
       !
     enddo
     !
   enddo
   !
 enddo
 !
 YAMBO_FREE(wf_disk)
 !
#if defined _MEM_CHECK 
 !
 ! Memory Overview
 !=================
 call MEM_report("final")
 !
#endif
 !
 call msg('ln','done ==')
 !
end function
