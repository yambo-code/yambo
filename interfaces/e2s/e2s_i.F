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
  use com,                 only : msg,write_to_report,outpath
  use par_proc_m,          only : ncpu,myid,p_finalize
  use electrons,           only : levels,E_reset,n_bands,n_spin
  use D_lattice,           only : n_atoms_max
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
  integer,     external        :: ioDB1,rand_identifier
  real(SP), allocatable        :: wf_disk(:,:,:,:)
  logical                      :: letsf_file

  ! The main program memory
  ! Creating a silane molecule with a 10x10x10 mesh for the density
  double precision, target    :: rhor(1000)
  double precision, target    :: rprimd(3, 3)
  integer, target             :: reduced_symmetry_matrices(3 ,3 ,1)
  double precision, target    :: reduced_symmetry_translations(3, 1)
  double precision, target    :: reduced_atom_positions(3, 5)
  double precision, target    :: valence_charges(2)
  double precision, target    :: atomic_numbers(2)
  character(len = 80), target :: atom_species_names(2)
  character(len = 2), target  :: chemical_symbols(2)
  character(len = 80), target :: pseudopotential_types(2)
  integer, target             :: ntypat(5)
  integer, target             :: space_group => 1

  ! The local variable to handle ETSF data.
  type(etsf_dims)             :: dims ! The dimensions of our system
  type(etsf_geometry), target :: geometry ! the description of our system
  type(etsf_main)             :: main ! a container (will store the density)
  type(etsf_groups)           :: groups ! a container (will store the geometry)
  logical                     :: lstat ! to get informed on error
  type(etsf_io_low_error)     :: error_data ! to store informations about errors

  integer :: i, dummy
  integer :: ncid
  type(etsf_kpoints) :: k_folder
  double precision, allocatable, target :: my_kpt(:,:) 
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
  ETSF_file_name = "sih4-output_DS1_WFK-etsf.nc"
  inquire( file=ETSF_file_name, exist = letsf_file)
  if ( .not.letsf_file ) then
    call msg('ln','failed')
    call p_finalize
    return
  endif

!etsf_io_data_read direct  all in one go
! Open file for reading
! folder%reduced_coordinates_of_kpoints%array%data2d => my_kpt  splitting (wf)
! folder%reduced_coordinates_of_kpoints%data2d => my_kpt  unshaped etsf_io_var_o
! 
  call etsf_io_low_open_read(ncid, trim(ETSF_file_name), lstat, error_data = error_data)
  if (.not. lstat) call etsf_long_error(error_data)
  call msg('l',trim(ETSF_file_name))
  call msg('s','DBs path set to :',trim(outpath))

! etsf_io_XXX_def for output netcdf 
  !
  ! Run the ETSF library for the DB1 information
  !
  call e2s_db1(en,k,trim(ETSF_file_name),ncid)
  !
  ! Write the data to DB1
  !
  call msg('s',' == Writing DB1 ...')
  call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
  io_err=ioDB1(en,k,ID)
  !
  ! Wavefucntions...
  !
  call msg('s',' == Writing DB2 (wavefunctions) + nlPP ...')
 allocate(wf_disk(2,n_bands,wf_ncx,n_spin))
 do ik=1,nkibz
!------------------------------------------------
   call e2s_wf(wf_disk,ik,ncid)
!------------------------------------------------
   if (ik==1) call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
   if (ik> 1) call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/ik+1/),ID=ID)
   io_err=ioWF(ID,wf=wf_disk)
!  if(n_atoms_max.ne.0) io_err=ioKB_PP(ID)
 enddo
 deallocate(wf_disk)
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
