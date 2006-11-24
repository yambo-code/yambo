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
 integer function a2s_i(instr,lnstr,inf,iinf,ind,iind,od,iod,&
&                       js,ijs,np,pid)
!=============================================================
 use pars,                ONLY:SP,lchlen
 use com,                 ONLY:msg,write_to_report,outpath
 use par_proc_m,          ONLY:ncpu,myid,p_finalize
 use electrons,           ONLY:levels,E_reset,n_bands,n_spin
 use D_lattice,           ONLY:n_atoms_max
 use R_lattice,           ONLY:bz_samp,bz_samp_reset,nkibz
 use timing,              ONLY:live_timing_is_on
 use wave_func,           ONLY:wf_ncx,ioWF
 use IO_m,                ONLY:io_control,OP_WR_CL,NONE,OP_APP_WR_CL,serial_number
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
 character(lchlen) :: KSS_file_name
 integer           :: ID,io_err,ioPP_err,ik,icycle
 integer,     external :: a2s_KSS_file_name,ioDB1,ioKB_PP,rand_identifier
 real(SP), allocatable :: wf_disk(:,:,:,:)
!----------------------------------------------------------------
!
! Presets
!=========
 a2s_i =0
 ncpu  =np
 myid  =pid
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

 call section('*','A(binit) 2 S(elf)')

 call msg('s','Checking input file ...')
!=======================================
 if ( a2s_KSS_file_name(inf,KSS_file_name) < 0 ) then
   call msg('ln','failed')
   call p_finalize
   return
 endif
 call msg('l',trim(KSS_file_name))

 call msg('s','DBs path set to :',trim(outpath))

 call a2s_db1(en,k,trim(KSS_file_name))
 call msg('s',' == Writing DB1 ...')
!===================================
 call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
 io_err=ioDB1(en,k,ID)

 if (index(instr,'bzrim')>0) then
   call msg('ln','done ==')
   call p_finalize
   return
 else
   call msg('l','done ==')
 endif
!
 call msg('s',' == Writing DB2 (wavefunctions) + nlPP ...')
 
 allocate(wf_disk(2,n_bands,wf_ncx,n_spin))
 do ik=1,nkibz
!------------------------------------------------
   call a2s_wf(wf_disk,ik,trim(KSS_file_name))
!------------------------------------------------
   do icycle=1,2
     if (icycle==2.and.n_atoms_max==0) cycle
     if (ik==1) call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
     if (ik> 1) call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/ik+1/),ID=ID)
     if (icycle==1) io_err=ioWF(ID,wf=wf_disk)
     if (icycle==2) io_err=ioKB_PP(ID)
   enddo
 enddo
 deallocate(wf_disk)
 call msg('ln','done ==')
!
 call p_finalize

 end function

