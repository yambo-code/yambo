!
! Copyright (C) 2000-2005 A. Marini and the SELF team
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
integer function e2s_i(instr,lnstr,inf,iinf,ind,iind,od,iod,&
&                       js,ijs,np,pid)
  use etsf_io
  use etsf_io_low_level
  use pars,                only : SP,lchlen
  use com,                 only : msg,write_to_report,out_path, file_exists
  use par_proc_m,          only : ncpu,myid,p_finalize
  use electrons,           only : levels,E_reset,n_bands,n_spin
  use D_lattice,           only : PP_alloc, PP_free, n_atoms_max
  use R_lattice,           only : bz_samp,bz_samp_reset,nkibz
  use timing,              only : live_timing_is_on
  use wave_func,           only : wf_ncx,ioWF
  use IO_m,                only : io_control,OP_WR_CL,NONE,OP_APP_WR_CL,serial_number
  implicit none
  integer,          intent(in) :: lnstr,iind,iod,ijs,np,pid
  integer,          intent(in) :: iinf
  character(lnstr), intent(in) :: instr
  character(iinf),  intent(in) :: inf
  character(iind),  intent(in) :: ind
  character(iod),   intent(in) :: od
  character(ijs),   intent(in) :: js

  type(levels)                 :: en
  type(bz_samp)                :: k
  character(lchlen)            :: ETSF_file_name
  integer                      :: ID,io_err,ik,icycle
  integer,     external        :: ioDB1,rand_identifier, ioKB_PP
  real(SP), allocatable        :: wf_disk(:,:,:,:)
  logical                      :: letsf_file, lwrite_PP, force_kbpp

  ! The local variable to handle ETSF data.
  type(etsf_dims)             :: dims ! The dimensions of our system
  logical                     :: lstat ! to get informed on error
  type(etsf_io_low_error)     :: error_data ! to store informations about errors

  integer :: i, dummy
  integer :: ncid
  double precision, allocatable, target :: my_kpt(:,:) 
  double precision, allocatable :: pp_section(:,:,:) ! also one for wf_section?
!
! Presets
!=========
  e2s_i = 0
  ncpu  = np
  myid  = pid
  call std_presets(instr,ind,od,'')
  call bz_samp_reset(k)
  call E_reset(en)
!
! S/N
!======
  serial_number=rand_identifier()
!
! Switch off report file support
!================================
  write_to_report=.FALSE.
! live_timing_is_on=.FALSE.

  call section('*','E(TSF) 2 S(elf)')
  !
  ! Check for input file
  !
  call msg('s','Checking input file ...')
! ETSF_file_name = "sih4-output_DS1_WFK-etsf.nc"
! inquire( file=ETSF_file_name, exist = letsf_file)
! if ( .not.letsf_file ) then
!   call msg('ln','failed')
!   call p_finalize
!   return
! endif

! Check input filenames (e2s.in is default)
!==========================================
!deffile = .false.
 ETSF_file_name = trim(inf)
! if(index(ETSF_filename,'e2s.in').ne.0) deffile = .true.
!if(deffile.and.use_pwexport) index_filename = 'index.xml'
!if(deffile.and.use_qexml)    index_filename = 'data-file.xml'
 call msg('s','ETSF format file set to ',trim(ETSF_file_name))
!inquire(file=trim(ETSF_file_name),exist=lexist)
 if(.not.file_exists(ETSF_file_name)) then
   call msg('s','ETSF format file not found! Specify -F filename.')
   stop ' '
 endif



  !
  ! Check for KP PP forced creation
  !
  force_kbpp = .false.
! if(index(instr,'forcepp').ne.0) force_kbpp = .true.

  

!etsf_io_data_read direct  all in one go
! Open file for reading
! folder%reduced_coordinates_of_kpoints%array%data2d => my_kpt  splitting (wf)
! folder%reduced_coordinates_of_kpoints%data2d => my_kpt  unshaped etsf_io_var_o
! 
  call etsf_io_low_open_read(ncid, trim(ETSF_file_name), lstat, error_data = error_data)
  if (.not. lstat) call etsf_long_error(error_data)
  call msg('s','DBs path set to :',trim(out_path))

! etsf_io_XXX_def for output netcdf 
  !
  ! Run the ETSF library for the DB1 information
  !
  call e2s_db1(en,k,ncid)
  !
  ! Write the data to DB1
  !
  call msg('s',' == Writing DB1 ...')
  call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
  io_err=ioDB1(en,k,ID)
  call msg('l','done ==')
  !
  ! Run the ETSF library for the PP information
  !
  lwrite_PP = .true.
! call msg('s',' == Extracting KBPP information ...')
! call e2s_kb_pp(ncid,lwrite_PP)
! call msg('l','done ==')
! lwrite_PP = lwrite_PP.or.force_kbpp
! if(.not.lwrite_PP) call msg('s',' == Writing DB2 (wavefunctions) ...')
! if(lwrite_PP) call msg('s',' == Writing DB2 (wavefunctions) + nlPP ...')
! if(force_kbpp) call msg('s','[WARNING] KB PP file contains only atomic information...')
  !
  ! Local wavefunction storage
  !
  allocate(wf_disk(2,n_bands,wf_ncx,n_spin))
  allocate(pp_section( dims%max_number_of_coefficients, &
&                   dims%max_number_of_angular_momenta, &
&                   dims%number_of_atom_species ) )
  call PP_alloc()

  do ik=1,nkibz
    !
    ! Run the ETSF library for the WF and PP information
    !
    call e2s_wf(wf_disk,ik,ncid)
!   call e2s_kb_pp(pp_section,ik,ncid)
    ! 
    ! Write to the SELF database
    ! 
    do icycle=1,2
      if (icycle==2.and.n_atoms_max==0) cycle
      if (ik==1) call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
      if (ik> 1) call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/ik+1/),ID=ID)
      if (icycle==1) io_err=ioWF(ID,wf=wf_disk)
!     if (icycle==2) io_err=ioKB_PP(ID)
    enddo
  enddo

  deallocate(wf_disk)
  deallocate(pp_section)
  call PP_free()
  call msg('ln','done ==')
!
 ! Close file
  call etsf_io_low_close(ncid, lstat, error_data = error_data)
  call p_finalize

end function

 subroutine etsf_long_error(error_data)
   use etsf_io_low_level, only : etsf_io_low_error, etsf_io_low_error_handle
   use com, only : error
   type(etsf_io_low_error), intent(in)     :: error_data
   call etsf_io_low_error_handle(error_data)
   call error('SELf dies')
 end subroutine etsf_long_error
