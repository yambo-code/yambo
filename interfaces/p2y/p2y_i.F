!
! Copyright (C) 2000-2008 C. Hogan and the YAMBO team 
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
integer function p2y_i(instr,lnstr,inf,iinf,ind,iind,od,iod,&
&                      com_dir,icd,js,ijs,np,pid)
 !
 use P2Y,                 only : pw_init, pw_close, verboseIO
 use LOGO,                only : pickup_a_random
 use pars,                only : SP,lchlen
 use memory_m,            only : mem_est
 use com,                 only : msg,write_to_report,more_io_path, error
 use par_proc_m,          only : ncpu,myid,p_finalize
 use electrons,           only : levels,E_reset,n_bands, n_spin, n_sp_pol
 use D_lattice,           only : n_atoms_max
 use R_lattice,           only : bz_samp,bz_samp_reset,nkibz
 use timing,              only : live_timing_is_on
 use wave_func,           only : wf_ncx,ioWF, wf_nb_io_groups,wf_nb_io
 use IO_m,                only : io_control,OP_WR_CL,NONE,OP_APP_WR_CL,serial_number
 use mod_com2y,           only : interface_presets,force_noWFs
 use mod_wf2y,            only : wf_splitter
 !
 implicit none
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
 character(lchlen) :: index_filename, lch
 integer           :: ID,io_err,ioPP_err,ik,icycle, ib_grp
 integer, external :: ioDB1,ioKB_PP
 real(SP), allocatable :: wf_disk(:,:,:,:)
 !
 ! Presets
 !
 p2y_i =0
 ncpu  =np
 myid  =pid
 call std_presets(instr,ind,od,'','')
 call interface_presets(instr)
 call bz_samp_reset(k)
 call E_reset(en)
 !
 ! S/N
 !
 serial_number=pickup_a_random(10000.)
 !
 ! Switch off report file support
 !
 write_to_report=.FALSE.
 !live_timing_is_on=.FALSE.
 !
#if defined _P2Y_EXPORT
 call section('*','P(W) 2 Y(ambo) - pw_export.x version')
#elif defined _P2Y_V31
 call section('*','P(W) 2 Y(ambo) Ver(s). 3.1 ')
#elif defined _P2Y_V311
 call section('*','P(W) 2 Y(ambo) Ver(s). 3.1.1')
#elif defined _P2Y_V32
 call section('*','P(W) 2 Y(ambo) Ver. 3.2')
#elif defined _P2Y_V40
 call section('*','P(W) 2 Y(ambo) Ver. 4.0')
#else
 call error('Unknown version of P2Y. Stopping.')
#endif
 call msg('s','DBs path set to ',trim(more_io_path))
 !
 ! Verbose I/O
 !
 verboseIO = .false.
 if(index(instr,'verb').ne.0) verboseIO = .true.
 !
 ! Open XML index files and units
 !
 call pw_init(instr,inf)
 !
 ! Make db1 database                                              *
 !
 call p2y_db1(en,k)
 !
 call msg('s','== Writing DB1 ...')
 !
 call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
 io_err=ioDB1(en,k,ID)
 !
 if (force_noWFs) then 
   call msg('ln','done ==')
   call p_finalize
   return
 else
   call msg('l','done ==')
 endif
 !
 ! Make db2 database                                              *
 !
 call msg('s','== DB2 (wavefunctions) ... ')
 !
 ! Wavefunction blocks
 !
 wf_nb_io_groups=1
 wf_nb_io=n_bands
#if !defined _P2Y_EXPORT
 call wf_splitter
#endif
 !
 allocate(wf_disk(2,wf_nb_io,wf_ncx,n_spin))
 call mem_est("wf_disk",(/size(wf_disk)/),(/SP/))
 !
 do ik=1,nkibz
   !
   ! Print verbose wfc read message
   !
   if(verboseIO.and.(any( (/1,2,nkibz/)-ik.eq.0 ) &
&  .or.mod(ik,k%nibz/4).eq.0)) then
     write(lch,'(" :: K-point:",i5,"/",i5," Spinors ",i1)') ik,nkibz,n_spin
     call msg('s',trim(lch))
   endif
   
   do ib_grp=1,wf_nb_io_groups
      !
      ! Read a single wfc component from disk
      !
      call p2y_wf(wf_disk,ik,ib_grp)
      !
      ! Write the standard wf header
      !
      if (ik==1.and.ib_grp==1) call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2,1/),ID=ID)
      if (ik> 1.or. ib_grp> 1) call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/ik+1,ib_grp/),ID=ID)
      io_err=ioWF(ID,wf=wf_disk)
   enddo
   !
 enddo
 !
 deallocate(wf_disk)
 !
 call pw_close
 if(verboseIO) then
   call msg('s','== DB2 (wavefunctions) ... done ==')
 else
   call msg('ln','done ==')
 endif
 !
 call p_finalize
 !
 end function p2y_i

