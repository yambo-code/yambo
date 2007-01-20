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
&                       js,ijs,np,pid)
!=============================================================
 use pars
 use memory_m,            only : mem_est
 use com,                 only : msg,write_to_report,outpath, error
 use par_proc_m,          only : ncpu,myid,p_finalize
 use electrons,           only : levels,E_reset,n_bands, n_spin, n_sp_pol
 use D_lattice,           only : n_atoms_max
 use R_lattice,           only : bz_samp,bz_samp_reset,nkibz
 use timing,              only : live_timing_is_on
 use wave_func,           only : wf_ncx,ioWF
 use IO_m,                only : io_control,OP_WR_CL,NONE,OP_APP_WR_CL,serial_number
!
! PW MODULES
!
 use pw_interface,        only : pw_openindex, pw_closeindex
 use qexml_module
!
 implicit none
 type(levels)                 :: en
 type(bz_samp)                :: k
 integer,          intent(in) :: lnstr,iind,iod,ijs,np,pid
 integer,          intent(in) :: iinf
 character(lnstr), intent(in) :: instr
 character(iinf),  intent(in) :: inf
 character(iind),  intent(in) :: ind
 character(iod),   intent(in) :: od
 character(ijs),   intent(in) :: js
!
 character(lchlen) :: index_filename, lch
 integer           :: ID,io_err,ioPP_err,ik,icycle
 integer, external :: ioDB1,ioKB_PP,rand_identifier
 real(SP), allocatable :: wf_disk(:,:,:,:)
!
!PW WS
!
 integer           :: pwunit, isec, ispinor
 integer           :: rhounit_, qexml_err
 logical           :: use_qexml, use_pwexport, deffile, lexist, verboseIO, force_noTR
!----------------------------------------------------------------
!
! Presets
!=========
 p2s_i =0
 ncpu  =np
 myid  =pid
 call std_presets(instr,ind,od,'')
 call bz_samp_reset(k)
 call E_reset(en)
!
! S/N
!======
 serial_number=rand_identifier()

! Switch off report file support
!================================
 write_to_report=.FALSE.
! live_timing_is_on=.FALSE.
!
 call section('*','P(W) 2 S(elf)')
 call msg('s','DBs path set to ',trim(outpath))

! pw_export/qexml version
!=================
 use_pwexport = .true.
 if(index(instr,'qexml').ne.0) use_pwexport = .false.
 use_qexml = .not.use_pwexport
 if(index(instr,'31').ne.0) pwversion = 31
 if(index(instr,'311').ne.0) pwversion = 311

! Verbose I/O
!============
 verboseIO = .false.
 if(index(instr,'verb').ne.0) verboseIO = .true.

! Forcing non generation of TR symmetries
!============
 force_noTR = .false.
 if(index(instr,'notr').ne.0) force_noTR = .true.

! Check input filenames (p2s.in is default)
!==========================================
 deffile = .false.
 index_filename = trim(inf)
 if(index(index_filename,'p2s.in').ne.0) deffile = .true.
 if(deffile.and.use_pwexport) index_filename = 'index.xml'
 if(deffile.and.use_qexml)    index_filename = 'data-file.xml'
 call msg('s','Index file set to ',trim(index_filename))
 inquire(file=trim(index_filename),exist=lexist) 
 if(.not.lexist) then
   call msg('s','Index file not found! Perhaps you need -Q <ver>...?')
   stop ' '
 endif

! Open XML index files and units
!==========================================
 if(use_pwexport) then
   
   call msg('s','== Reading pw_export generated data ...')
   pwunit = 10
 
   call pw_openindex(pwunit,index_filename) ! open index.xml file

 else if(use_qexml) then

   write(lch,'(a,i3,a)') '== Reading PWscf v.',pwversion,' generated data ...'
   call msg('s',trim(lch))
   pwunit = 10
   rhounit_ = 12
   call qexml_init(pwunit,rhounit_,.true.) ! sets unit numbers
   call qexml_openfile( index_filename, "read", .false., qexml_err)
!
!  qexml_init(_,_,.true.)               : rho file is binary
!  qexml_openfile(_,"read"/"write",_,_) : read only from existing files
!  qexml_openfile(_,_,.false.,_)        : data-file.xml is not binary
!
 endif

!---------------------------------------------------------------------*
!    Make s.db1 database                                              *
!---------------------------------------------------------------------*

 if(use_pwexport) call p2s_db1(pwunit,en,k)
 if(use_qexml)    call p2s_qexml_db1(pwunit,verboseIO,force_noTR,en,k)

 call msg('s','== Writing DB1 ...')
!===================================
 call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
 io_err=ioDB1(en,k,ID)
 call msg('l','done ==')

!---------------------------------------------------------------------*
!    Make s.db2 database                                              *
!---------------------------------------------------------------------*

 call msg('s','== DB2 (wavefunctions) ... ')

 allocate(wf_disk(2,n_bands,wf_ncx,n_spin))
 call mem_est("wf_disk",(/size(wf_disk)/),(/SP/))

 isec = 1
 do ik=1,nkibz
     !
     ! Print verbose wfc read message
     !
     if(verboseIO.and.(any( (/1,2,nkibz/)-ik.eq.0 ) &
&    .or.mod(ik,k%nibz/4).eq.0)) then
       write(lch,'(" :: K-point:",i5,"/",i5," Spinors ",i1)') ik,nkibz,n_spin
       call msg('s',trim(lch))
     endif
     !
     ! Read a single wfc component from disk
     !
     if(use_pwexport) call p2s_wf(wf_disk,ik, pwunit)
     if(use_qexml)    call p2s_qexml_wf(wf_disk,ik, pwunit)
     !
     ! Write the standard wf header
     !
     if (ik==1) then
       call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID)
       io_err=ioWF(ID)
     endif
     !
     ! Section title (SEC = ik+1). NB use of ispinor in ioWF
     !
     isec = isec + 1 ! check
     call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/ik+1/),ID=ID)
!    call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/isec/),ID=ID)
!    if(n_spin.eq.1) io_err=ioWF(ID,wf_disk)
!    if(n_spin.eq.4) io_err=ioWF(ID,wf_disk,ispinor)
     io_err=ioWF(ID,wf_disk)

 enddo

 deallocate(wf_disk)

 if(use_pwexport) then
   call pw_closeindex(pwunit) ! close index.xml file
 endif
 if(use_qexml) then
   call qexml_closefile("read",qexml_err) ! close index.xml file
 endif
 if(verboseIO) then
   call msg('s','== DB2 (wavefunctions) ... done ==')
 else
   call msg('ln','done ==')
 endif

 end function

!
! Error routine called inside pw_interface and qexml_module
!

subroutine errore(subname,msgin,ierr)
  use com,      ONLY : error
  implicit none
  character*(*), intent(in) :: subname,msgin
  integer,       intent(in) :: ierr
  character(3)              :: merr

  write(merr,'(i3)') ierr 
  call error('Error in '//trim(subname)//' '//trim(msgin)//' ierr:'//trim(merr))
  stop ' ' ! not called  
end subroutine errore

