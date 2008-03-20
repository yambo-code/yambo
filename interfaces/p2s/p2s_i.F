!
! Copyright (C) 2000-2005 C. Hogan and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 integer function p2s_i(instr,lnstr,inf,iinf,ind,iind,od,iod,&
&                       com_dir,icd,js,ijs,np,pid)
 !
 use P2S,                 only : pw_init, pw_close, verboseIO
 use LOGO,                only : pickup_a_random
 use pars,                only : SP,lchlen
 use memory_m,            only : mem_est
 use com,                 only : msg,write_to_report,more_io_path, error
 use par_proc_m,          only : ncpu,myid,p_finalize
 use electrons,           only : levels,E_reset,n_bands, n_spin, n_sp_pol
 use D_lattice,           only : n_atoms_max
 use R_lattice,           only : bz_samp,bz_samp_reset,nkibz
 use timing,              only : live_timing_is_on
 use wave_func,           only : wf_ncx,ioWF
 use IO_m,                only : io_control,OP_WR_CL,NONE,OP_APP_WR_CL,serial_number
 use interfaces,          only : interface_presets,force_noWFs
 !
 ! PW MODULES
 !
#if defined _P2S_EXPORT
!use pw_export,        only : pw_openindex, pw_closeindex
#else
 use qexml_module
#endif
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
 integer           :: ID,io_err,ioPP_err,ik,icycle
 integer, external :: ioDB1,ioKB_PP
 real(SP), allocatable :: wf_disk(:,:,:,:)
 !
 !PW WS
 !
 integer           :: pwunit, isec, ispinor
 integer           :: rhounit_, qexml_err
 logical           :: use_qexml, use_pwexport, deffile, lexist
 !
 ! Presets
 !
 p2s_i =0
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
#if defined _P2S_EXPORT
 call section('*','P(W) 2 S(elf) - pw_export.x version')
#elif defined _P2S_V31
 call section('*','P(W) 2 S(elf) Ver(s). 3.1 ')
#elif defined _P2S_V311
 call section('*','P(W) 2 S(elf) Ver(s). 3.1.1')
#elif defined _P2S_V32
 call section('*','P(W) 2 S(elf) Ver. 3.2')
#else
 call error('Unknown version of P2S. Stopping.')
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
 call p2s_db1(en,k)
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
 call wf_disk_split
 !
 allocate(wf_disk(2,n_bands,wf_ncx,n_spin))
 call mem_est("wf_disk",(/size(wf_disk)/),(/SP/))
 !
 isec = 1
 do ik=1,nkibz
   !
   ! Print verbose wfc read message
   !
   if(verboseIO.and.(any( (/1,2,nkibz/)-ik.eq.0 ) &
&  .or.mod(ik,k%nibz/4).eq.0)) then
     write(lch,'(" :: K-point:",i5,"/",i5," Spinors ",i1)') ik,nkibz,n_spin
     call msg('s',trim(lch))
   endif
   !
   ! Read a single wfc component from disk
   !
   call p2s_wf(wf_disk,ik)
   !
   ! Write the standard wf header
   !
   if (ik==1) then
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID)
     io_err=ioWF(ID)
   endif
   !
   ! Section title (SEC = ik+1). 
   !
   isec = isec + 1 ! check
   call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/ik+1/),ID=ID)
   io_err=ioWF(ID,wf_disk)
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
 end function p2s_i

 subroutine wf_disk_split
   use stderr,              only : intc
   use pars,                only : SP,DP, schlen
   use electrons,           only : n_bands
   use wave_func,           only : wf_ncx
   use com,                 only : msg
   use interfaces,          only : n_band_block,blocks, maxband
   use memory_m,            only : mem_est
   real(SP),parameter    :: max_array = 400.0_SP ! Mb limit for single WF array in memory
!  real(SP),parameter    :: max_array = 0.01_SP ! Mb limit, for testing small systems
   real(SP),parameter    :: kb = 1024.0_SP
   integer               :: i
   character(schlen)     :: sch


   n_band_block = 1
   array_sz = DP*n_bands*real(wf_ncx)/kb/kb ! wf_disk_DP
   !
   ! No memory problems: no blocks
   !
   if(array_sz.lt.max_array &
#if !defined _BIGSYS
      .or. .true. &   ! Switch off variable blocks unless _BIGSYS flag enabled at compilation
#endif
&    ) then
!    call msg('s','Wavefunctions fully stored in memory.')

     n_band_block = 1
     allocate(blocks(2,n_band_block))
     blocks(:,1) = (/ 1, n_bands /)
     maxband = n_bands
   else
   !
   ! Split the WFs read from disk into blocks
   !
     n_band_block = array_sz/max_array + 1
     maxband = n_bands/real(n_band_block)  ! rough size of each block
     allocate(blocks(2,n_band_block))
     do i=1,n_band_block-1
       blocks(1,i) = (i-1)*maxband+1
       blocks(2,i) = blocks(1,i)+maxband-1
     enddo
     blocks(1,n_band_block) = blocks(2,n_band_block-1) + 1
     blocks(2,n_band_block) = n_bands

     do i=1,n_band_block
       write(sch,'(a,i2,a,i4,a,i4,a)') &
&      "  Block ",i," bands: [",blocks(1,i),":",blocks(2,i),"]"
       call msg('s',trim(sch))
     enddo

     maxband = 1+maxval(blocks(2,:)-blocks(1,:)) ! The maximum block size

   endif

   return
 end subroutine wf_disk_split
