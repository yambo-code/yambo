!
!        Copyright (C) 2000-2017 the YAMBO team
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
integer function p2y_i(np,pid,lnstr,iinf,iind,iod,icd,ijs,instr,inf,ind,od,com_dir,js)
 !
 use P2Y,                 ONLY : pw_init, pw_close 
 use LOGO,                ONLY : pickup_a_random
 use pars,                ONLY : SP,lchlen
 use com,                 ONLY : msg,write_to_report,core_io_path,error,write_the_logo
 use parallel_m,          ONLY : ncpu,myid,PARALLEL_wait
 use electrons,           ONLY : levels,E_reset,n_bands,n_sp_pol,n_spinor
 use R_lattice,           ONLY : bz_samp,bz_samp_reset,nkibz
 use wave_func,           ONLY : wf_ncx,io_WF, wf_nb_io_groups,wf_nb_io
 use IO_m,                ONLY : io_control,OP_WR_CL,NONE,OP_APP_WR_CL,serial_number,OP_RD_CL,DUMP
 use mod_com2y,           ONLY : interface_presets,force_noWFs,verboseIO
 use mod_wf2y,            ONLY : wf_splitter
 use stderr,              ONLY : intc
 use interfaces,          ONLY : PARALLEL_live_message
#if !defined _P2Y_EXPORT
 use qexml_module
#endif
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
 integer               :: ID,io_err,i_sp_pol,ik,ifrag,ib_grp
 integer, external     :: io_DB1
 complex(SP), allocatable :: wf_disk(:,:,:)
 !
 ! Presets
 !
 p2y_i =0
 ncpu  =np
 myid  =pid
 call SET_defaults(instr,od,od,'','')
 call interface_presets(instr)
 call bz_samp_reset(k)
 call E_reset(en)
#if defined _MEM_CHECK 
 call MEM_defaults( )
#endif
 !
 ! LOGO
 !
 call write_the_logo(6,' ')
 !
 ! S/N
 !
 serial_number=pickup_a_random(10000._SP)
 !
 ! Switch off report file support
 !
 write_to_report=.FALSE.
 !
#if defined _P2Y_V31
 lch='P(W) 2 Y(ambo) Ver(s). 3.1 '
#elif defined _P2Y_V311
 lch='P(W) 2 Y(ambo) Ver(s). 3.1.1'
#elif defined _P2Y_V32
 lch='P(W) 2 Y(ambo) Ver. 3.2'
#elif defined _P2Y_V40
 lch='P(W) 2 Y(ambo) Ver. 4.0'
#elif defined _P2Y_V50
 lch='P(W) 2 Y(ambo) Ver. 5.0'
#elif defined _P2Y_QEXSD
 lch='P(W) 2 Y(ambo) - QEXSD version'
#else
 call error('Unknown version of P2Y. Stopping.')
#endif
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
 YAMBO_ALLOC(wf_disk,(wf_ncx,n_spinor,wf_nb_io))
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=1,nkibz
     !
     ifrag=ik+(i_sp_pol-1)*nkibz
     !
     ! Print verbose wfc read message
     !
     if(verboseIO.and.(any( (/1,2,nkibz/)-ik.eq.0 ).or.mod(ik,max(k%nibz/4,1)).eq.0)) then
       write(lch,'(" :: K-point:",i5,"/",i5," Spin pol ",i1)') ik,nkibz,n_sp_pol
       call msg('s',trim(lch))
     endif
     ! 
     do ib_grp=1,wf_nb_io_groups
        !
        ! Read a single wfc component from disk
        !
        call p2y_wf(wf_disk,i_sp_pol,ik,ib_grp)
        !
        ! Write the standard wf header
        !
        if (ifrag==1.and.ib_grp==1) call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2,1/),ID=ID)
        if (ifrag> 1.or. ib_grp> 1) call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/ifrag+1,ib_grp/),ID=ID)
        io_err=io_WF(ID,wf_disk)
        !
     enddo
     !
   enddo
 enddo
 !
 YAMBO_FREE(wf_disk)
 !
 if(verboseIO) then
   call msg('s','== DB2 (wavefunctions)  ... done ==')
 else
   call msg('l',' done ==')
 endif
 !
#if defined _P2Y_V50
 call msg('s','== DB3 (PseudoPotential) ... ')
 call PP_PWscf_driver(k)
 if(verboseIO) then
   call msg('s','== DB3 (PseudoPotential) ... done ==')
 else
   call msg('l',' done ==')
 endif
#else
 call msg('s','Vnl support only with PWscf v5.0 or later.')
#endif
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
 end function p2y_i

