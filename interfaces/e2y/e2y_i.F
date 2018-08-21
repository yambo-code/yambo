!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CH DS
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
 use netcdf
 use etsf_data
 use pars,                only : SP,lchlen
 use LOGO,                only : pickup_a_random
 use com,                 only : msg,write_to_report,core_io_path, file_exists,write_the_logo
 use parallel_m,          only : ncpu,myid
 use electrons,           only : levels,E_reset,n_bands,n_spinor,n_sp_pol
 use D_lattice,           only : n_atoms_species_max
 use pseudo,              only : PP_alloc_abinit, PP_free, pp_n_l_times_proj_max, pp_n_l_max
 use R_lattice,           only : bz_samp,bz_samp_reset,nkibz
 use LIVE_t,              only : live_timing_is_on
 use wave_func,           only : wf_ncx,io_WF,wf_nb_io_groups,wf_nb_io
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
 !
 integer                      :: ID,io_err,i_sp_pol,ik,ifrag,icycle, ib_grp
 integer,     external        :: io_DB1, io_KB_abinit
 complex(SP), allocatable     :: wf_disk(:,:,:)
 ! 
 ! Work Space
 !
 integer                     :: ncid, netcdf_error
 !
 ! Presets
 !=========
 e2y_i = 0
 ncpu  = np
 myid  = pid
 call SET_defaults(instr,od,od,'','')
 call interface_presets(instr)
 call bz_samp_reset(k)
 call E_reset(en)
#if defined _MEM_CHECK 
 call MEM_defaults( )
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
 call msg('s','E(TSF) 2 Y(ambo)')
 !
 ! Check input filenames 
 !
 call msg('s','Checking input file ...')
 !
 if      (index(trim(inf),'KSS')/=0) then
   ETSF_kind='KSS'
 else if (index(trim(inf),'WFK')/=0) then
   ETSF_kind='WFK'
 else
   call msg('ln','File kind not supported! Use either KSS or WFK filename.')
 end if
 !
 call msg('s','ETSF file ',trim(inf))
 !
 if(.not.file_exists(trim(inf))) then
   call msg('ln','File not found! Specify -F filename.')
   return
 endif
 !
 ! Open ETSF file for reading
 !
 netcdf_error = nf90_open(path = trim(inf), mode = NF90_NOWRITE, ncid = ncid)
 !
 call msg('s','DBs path set to :',trim(core_io_path))
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
 call wf_splitter()
 !
 ! WF & PP
 !==========================================
 !
 if(trim(ETSF_kind)=='KSS') then
   call msg('s',' == Writing DB2 (wavefunctions) + nlPP ...') 
 endif
 !
 if(trim(ETSF_kind)=='WFK') then
   call msg('s',' == Writing DB2 (wavefunctions) ...') 
 endif
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
       if (ifrag> 1.or. ib_grp> 1) call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/ifrag+1,ib_grp/),ID=ID)
       io_err=io_WF(ID,wf_disk)
       !
       if(trim(ETSF_kind)=='KSS') call e2y_kb_pp(ik,ncid) 
       if(trim(ETSF_kind)=='WFK') call e2y_kb_pp_wfk(ik,ncid) 
       if (n_atoms_species_max>0.and.ib_grp==1) then
         if (ifrag==1) call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
         if (ifrag> 1) call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/ifrag+1/),ID=ID)
         io_err=io_KB_abinit(ID) 
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
 netcdf_error = nf90_close(ncid)
 !
end function
