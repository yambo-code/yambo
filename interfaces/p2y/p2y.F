!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CH, AF
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
integer function p2y(np,pid,lnstr,iinf,iind,iod,icd,ijs,instr,inf,ind,od,com_dir,js)
 !
 use P2Ym,                ONLY:pw_init, pw_close, dftdata_fmt
 use LOGO,                ONLY:pickup_a_random
 use pars,                ONLY:SP,lchlen,schlen
 use com,                 ONLY:msg,core_io_path,write_the_logo,com_path,file_exists
 use LIVE_t,              ONLY:live_timing
 use parallel_m,          ONLY:ncpu,myid,PARALLEL_wait,PP_indexes,PP_indexes_reset
 use electrons,           ONLY:levels,E_reset,n_bands,n_sp_pol,n_spinor
 use R_lattice,           ONLY:bz_samp,bz_samp_reset,nkibz
 use wave_func,           ONLY:wf_ncx,io_WF, wf_nb_io_groups,wf_nb_io
 use mod_wf2y,            ONLY:WF_splitter
 use IO_m,                ONLY:io_control,OP_WR_CL,NONE,OP_APP_CL,serial_number,OP_RD_CL,DUMP,&
&                              IO_and_Messaging_switch,mk_dir,IO_write,IO_read
 use mod_com2y,           ONLY:interface_presets,force_noWFs,verboseIO
 use stderr,              ONLY:intc,logfile,log_as_a_file
 use interfaces,          ONLY:PARALLEL_index,PARALLEL_live_message
 use qexml_module
#if defined _MEM_CHECK 
 use memory,              ONLY : MEM_manager
#endif
 !
#include<memory.h>
 !
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
 character(lchlen)     :: lch
 character(schlen)     :: atm_file
 integer               :: ID,io_err,i_sp_pol,ik,ifrag,ib_grp
 complex(SP), allocatable :: wf_disk(:,:,:)
 type(PP_indexes)      :: PAR_IND_wfc_io
 integer, external     :: io_DB1
 logical               :: IO_write_save,IO_read_save
 !
 ! Presets
 !
 p2y   =0
 ncpu  =np
 myid  =pid
 call SET_defaults(instr,od,od,com_dir)
 call SET_job_strings_and_dirs(js)
 call interface_presets(instr)
 call bz_samp_reset(k)
 call E_reset(en)
#if defined _MEM_CHECK 
 call MEM_defaults( )
#endif
 !
 if (log_as_a_file) then
   if (len_trim(com_path)==0) com_path="./"
   write (logfile,'(2a)') trim(com_path),'/l-p2y'
   !
   if (ncpu>1) then
     call mk_dir(trim(com_path)//"/LOG")
     write (logfile,'(2a)') trim(com_path)//'/LOG/l-p2y'
     logfile=trim(logfile)//"_CPU_"//trim(intc(myid+1))
   endif
   !
 endif
 !
 ! Switch off report file support & Handle logs
 !
 call IO_and_Messaging_switch("-report +log +fragments_IO_log " )
 !
 ! LOGO
 !
 call write_the_logo(6,' ')
 !
 ! S/N
 !
 serial_number=pickup_a_random(10000._SP)
 !
 lch='P(W) 2 Y(ambo) - multi-fmt version'
 !
 if (ncpu>1) lch=trim(lch)//' @ '//trim(intc(ncpu))//' CPUs'
 !
 call PARALLEL_live_message(trim(lch))
 !
 call msg('s','DBs path set to ',trim(core_io_path))
 !
 ! Open XML index files and units
 !
 call pw_init(instr,inf)
 !
 ! Make db1 database
 !
 call p2y_db1(en,k)
 !
 call msg('s','== DB1 (Gvecs and more) ... ')
 !
 call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
 io_err=io_DB1(en,k,ID)
 !
 call msg('s','... Database done')
 !
 if (force_noWFs) then 
   call msg('s',' == P2Y completed ==')
   call msg('s',' ')
   return
 endif
 ! 
 ! Some definitions are done during the I/O in DB1. As only the
 ! cpu 0 has IO_write=TRUE all cpu's need to re-read the database
 !
 if (ncpu>1) then
   !
   call PARALLEL_wait()
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID)
   io_err=io_DB1(en,k,ID)
   !
 endif
 !
 ! Wavefunction blocks
 !
 wf_nb_io_groups=1
 wf_nb_io=n_bands
 call WF_splitter()
 !
 ! Make db2 database
 !             
 call msg('s','== DB2 (wavefunctions)  ... ')
 !
 ! setup MPI parallelism if required
 !
 call PP_indexes_reset(PAR_IND_wfc_io)
 ! 
 call PARALLEL_index(PAR_IND_wfc_io,(/wf_nb_io_groups,nkibz/))
 call PARALLEL_live_message("[p2y] wfc_io",LOADED=PAR_IND_wfc_Io%n_of_elements(myid+1),TOTAL=wf_nb_io_groups*nkibz)
 ! 
 call live_timing("[p2y] wfc_io",PAR_IND_wfc_Io%n_of_elements(myid+1))
 !
 IO_write_save=IO_write
 IO_read_save =IO_read
 IO_write=.true.
 IO_read=.true.
 !
 ! IO
 !
 YAMBO_ALLOC(wf_disk,(wf_ncx,n_spinor,wf_nb_io))
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=1,nkibz
     !
     if (.not.any(PAR_IND_wfc_io%element_2D(:,ik))) cycle
     !
     ifrag=ik+(i_sp_pol-1)*nkibz
     !
     ! Print verbose wfc read message
     !
     if(verboseIO.and.(any( (/1,2,nkibz/)-ik==0 ).or.mod(ik,max(k%nibz/4,1))==0)) then
       write(lch,'(" :: K-point:",i5,"/",i5," Spin pol ",i1)') ik,nkibz,n_sp_pol
       call msg('s',trim(lch))
     endif
     ! 
     do ib_grp=1,wf_nb_io_groups
        !
        if (.not.PAR_IND_wfc_io%element_2D(ib_grp,ik)) cycle
        !
        if (ifrag==1.and.ib_grp==1) then
          call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID)
          io_err=io_WF(ID)
        endif
        !
        ! Read a single wfc component from disk
        !
        call p2y_wf(wf_disk,i_sp_pol,ik,ib_grp)
        !
        ! Write the standard wf header
        !
        call io_control(ACTION=OP_APP_CL,COM=NONE,SEC=(/ifrag+1,ib_grp/),ID=ID)
        io_err=io_WF(ID,wf_disk)
        !
     enddo
     !
     call live_timing(steps=1)
     !
   enddo
 enddo
 !
 IO_write=IO_write_save
 IO_read=IO_read_save
 !
 if (ncpu>1) call PARALLEL_wait()
 call live_timing()
 !
 YAMBO_FREE(wf_disk)
 !
 if(verboseIO) then
   call msg('s','== DB2 (wavefunctions)  ... done ==')
 else
   call msg('l',' done ==')
 endif
 !
 if (trim(dftdata_fmt) /= "pw_export") then
   !
   call msg('s','== DB3 (PseudoPotential) ... ')
   call p2y_pseudo(k)
   if(verboseIO) then
     call msg('s','== DB3 (PseudoPotential) ... done ==')
   else
     call msg('l',' done ==')
   endif
 else
   call msg('s','Vnl support only with QE v5.0 or later (QEXML, QEXSD).')
 endif
 !
 if ( file_exists('atomic_proj.xml') .or. file_exists('atomic_proj.dat') ) then
   call msg('s','== DB4 (Atomproj parameters) ... ')
   if ( file_exists('atomic_proj.xml') ) atm_file='atomic_proj.xml'
   if ( file_exists('atomic_proj.dat') ) atm_file='atomic_proj.dat'
   call p2y_atmproj(atm_file,k)
   if(verboseIO) then
     call msg('s','== DB4 (Atomproj parameters) ... done ==')
   else
     call msg('l',' done ==')
   endif
 endif
 !
 call PARALLEL_wait()
 !
 call pw_close
 !
 ! Memory Overview
 !=================
#if defined _MEM_CHECK 
 !
 call MEM_manager("",REPORT="final")
 !
#endif
 !
 call msg('s',' == P2Y completed ==')
 call msg('s' ,' ')
 !
end function p2y
