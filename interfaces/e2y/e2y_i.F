!
! Copyright (C) 2000-2013 C. Hogan and the YAMBO team
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
integer function e2y_i(np,pid,lnstr,iinf,iind,iod,icd,ijs,instr,inf,ind,od,com_dir,js)
 use etsf_io
 use etsf_io_low_level
 use pars,                only : SP,lchlen
 use LOGO,                only : pickup_a_random
 use com,                 only : msg,write_to_report,core_io_path, file_exists,write_the_logo
 use par_proc_m,          only : ncpu,myid,p_finalize
 use electrons,           only : levels,E_reset,n_bands,n_spin
 use D_lattice,           only : n_atoms_species_max
 use pseudo,              only : PP_alloc, PP_free
 use R_lattice,           only : bz_samp,bz_samp_reset,nkibz
 use timing,              only : live_timing_is_on
 use wave_func,           only : wf_ncx,ioWF,wf_nb_io_groups,wf_nb_io
 use IO_m,                only : io_control,OP_WR_CL,NONE,OP_APP_WR_CL,serial_number
 use mod_com2y,           only : interface_presets, force_noWFs
 use mod_wf2y,            only : wf_splitter
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
 character(lchlen)            :: ETSF_file_name
 integer                      :: ID,io_err,ik,icycle, ib_grp
 integer,     external        :: ioDB1, ioKB_PP
 real(SP), allocatable        :: wf_disk(:,:,:,:)
 logical                      :: letsf_file, lwrite_PP, force_kbpp
 ! 
 ! Work Space
 !
 ! The local variables to handle ETSF data.
 type(etsf_dims)             :: dims  ! The dimensions of our system
 logical                     :: lstat ! to get informed on error
 type(etsf_io_low_error)     :: error_data ! to store informations about errors
 integer                     :: ncid
 !
 ! Presets
 !=========
 e2y_i = 0
 ncpu  = np
 myid  = pid
 call std_presets(instr,od,od,'','')
 call interface_presets(instr)
 call bz_samp_reset(k)
 call E_reset(en)
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

 ETSF_file_name = trim(inf)
 call msg('s','ETSF format file set to ',trim(ETSF_file_name))
 if(.not.file_exists(ETSF_file_name)) then
   call msg('ln','File not found! Specify -F filename.')
   call p_finalize
   return
 endif
 !
 ! Open ETSF file for reading
 !
 call etsf_io_low_open_read(ncid, trim(ETSF_file_name), lstat, error_data = error_data)
 if (.not. lstat) call etsf_long_error(error_data)
 call msg('s','DBs path set to :',trim(core_io_path))
 !
 ! DB1
 !==========================================
 !
 call e2y_db1(en,k,ncid)
 !
 ! Write the data to YAMBO DB1
 !
 call msg('s',' == Writing DB1 ...')
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
 call wf_splitter()
 !
 ! WF & PP
 !==========================================
 !
 call msg('s',' == Writing DB2 (wavefunctions) + nlPP ...') 
 allocate(wf_disk(2,wf_nb_io,wf_ncx,n_spin))
 call PP_alloc()

 do ik=1,nkibz
   !
   do ib_grp=1,wf_nb_io_groups
     !
     ! Read from the etsf-nc file
     !
     call e2y_wf(wf_disk,ik,ib_grp,ncid)
     call e2y_kb_pp(ik,ncid) 
     ! 
     ! Write to the YAMBO database
     ! 
     if (n_atoms_species_max>0.and.ib_grp==1) then
       if (ik==1) call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
       if (ik> 1) call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/ik+1/),ID=ID)
       io_err=ioKB_PP(ID) 
     endif
     if (ik==1.and.ib_grp==1) call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2,1/),ID=ID)
     if (ik> 1.or. ib_grp> 1) call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/ik+1,ib_grp/),ID=ID)
     io_err=ioWF(ID,wf=wf_disk)
     !
   enddo
   !
 enddo
 deallocate(wf_disk)

 call PP_free()

 call msg('ln','done ==')
 !
 ! Close file
 !
 call etsf_io_low_close(ncid, lstat, error_data = error_data)
 call p_finalize
 !
end function

 subroutine etsf_long_error(error_data)
   use etsf_io_low_level, only : etsf_io_low_error, etsf_io_low_error_handle
   use com, only : error
   type(etsf_io_low_error), intent(in)     :: error_data
   call etsf_io_low_error_handle(error_data)
   call error('YAMBO dies')
 end subroutine etsf_long_error
