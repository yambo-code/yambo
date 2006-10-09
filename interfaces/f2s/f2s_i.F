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
 integer function f2s_i(instr,lnstr,inf,iinf,ind,iind,od,iod,&
&                      js,ijs,np,pid)
!===========================================================
 use pars,                ONLY:SP,lchlen
 use com,                 ONLY:msg,write_to_report,outpath
 use par_proc_m,          ONLY:ncpu,myid,p_finalize
 use electrons,           ONLY:levels,E_reset,n_bands,n_spin
 use D_lattice,           ONLY:n_atoms_max
 use R_lattice,           ONLY:bz_samp,bz_samp_reset,nkibz
 use timing,              ONLY:live_timing_is_on
 use wave_func,           ONLY:wf_ng_disk,ioWF
 use IO_m,                ONLY:io_control,OP_WR_CL,NONE,OP_APP_WR_CL,serial_number
 use f2s_datatypes
 implicit none
 type(levels)                 :: en       ! Energies
 type(bz_samp)                :: k        ! K/Q points
 integer,          intent(in) :: lnstr,iind,iod,ijs,np,pid
 integer,          intent(in) :: iinf
 character(lnstr), intent(in) :: instr
 character(iinf),  intent(in) :: inf
 character(iind),  intent(in) :: ind
 character(iod),   intent(in) :: od
 character(ijs),   intent(in) :: js

!ws
 logical                      :: sdir
 character(lchlen)            :: file_name
!
 integer           :: ID,io_err,ioPP_err,ik,icycle
 integer,     external :: f2s_file_name,ioDB1,ioKB_PP,rand_identifier
 real(SP), allocatable :: wf_disk(:,:,:,:)
! FHI data passed between db1 and wf
 type(finp_type)::fhi_inp
 type(fwvf_type)::fhi_wvf
!----------------------------------------------------------------
! S/N
!======
 serial_number=rand_identifier()
!
! Presets
!=========
 f2s_i =0
 ncpu  =np
 myid  =pid
 call std_presets(instr,ind,od,'')
 call bz_samp_reset(k)
 call E_reset(en) 
!
! Switch off report file support
!================================
 write_to_report=.FALSE.
! live_timing_is_on=.FALSE.

 call section('*','FHI(96SPIN) 2 S(elf)')

 call msg('s','Checking input file ...')
!=======================================
 if ( f2s_file_name(inf,file_name) < 0 ) then
   call msg('ln','')
   call msg('ln','failed')
   call p_finalize
   return
 endif
 call msg('l',trim(file_name))

 call msg('s','DBs path set to :',trim(outpath))
!
! read DFT-LDA file
!      
!     - k-points G-vectors & tables
!     - atomic positions
!     - Kohn-Sham eigenstates
!     - symmetry operations
!
! adjust units and transform vector coordinates 
!
 call f2s_db1(en,k,fhi_inp,fhi_wvf,file_name)
 call msg('s',' == Writing DB1 ...')
!===================================
 call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
 io_err=ioDB1(en,k,ID)
 call msg('l','done ==')
!
 call msg('s',' == Writing DB2 (wavefunctions) + nlPP ...')
 
 allocate(wf_disk(2,n_bands,wf_ng_disk,n_spin))

 do ik=1,nkibz
!------------------------------------------------
   call f2s_wf(wf_disk,ik,fhi_inp,fhi_wvf)
!------------------------------------------------
   if (ik==1) call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
   if (ik> 1) call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/ik+1/),ID=ID)
   io_err=ioWF(ID,wf_disk)
 enddo
 deallocate(wf_disk)
 call msg('ln','done ==')
!
 call p_finalize

 call deallocate_finp(fhi_inp)
 call deallocate_fwvf(fhi_wvf)

 end function
