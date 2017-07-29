!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
integer function a2y_i(np,pid,lnstr,iinf,iind,iod,icd,ijs,instr,inf,ind,od,com_dir,js)
 !
 use pars,                ONLY:SP,lchlen
 use LOGO,                ONLY:pickup_a_random
 use com,                 ONLY:msg,write_to_report,core_io_path,write_the_logo
 use parallel_m,          ONLY:ncpu,myid
 use electrons,           ONLY:levels,E_reset,n_spin
 use D_lattice,           ONLY:n_atoms_species_max
 use R_lattice,           ONLY:bz_samp,bz_samp_reset,nkibz
 use wave_func,           ONLY:wf_ncx,io_WF,wf_nb_io_groups,wf_nb_io
 use IO_m,                ONLY:io_control,OP_WR_CL,NONE,OP_WR_CL,serial_number
 use interfaces,          ONLY:PARALLEL_live_message
 use mod_com2y,           ONLY:interface_presets,force_noWFs
 use mod_wf2y,            ONLY:WF_splitter
 use pseudo,              ONLY:PP_free
#if defined _MEM_CHECK 
 use memory,              ONLY:MEM_manager
#endif
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
 character(lchlen) :: KSS_file_name
 integer           :: ID,io_err,ik,ib_grp
 integer,     external :: a2y_KSS_file_name,io_DB1,io_KB_abinit
 real(SP), allocatable :: wf_disk(:,:,:,:)
 !
 ! Presets
 !
 a2y_i =0
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
 ! S/N
 !
 serial_number=pickup_a_random(10000._SP)
 !
 ! Switch off report file support
 !
 write_to_report=.FALSE.
 !
 ! LOGO
 !
 call write_the_logo(6,' ')
 !
 call PARALLEL_live_message('A(binit) 2 Y(ambo)')
 !
 call msg('s','Checking input file ...')
 !
 if ( a2y_KSS_file_name(inf,KSS_file_name) < 0 ) then
   call msg('ln','failed')
   return
 endif
 call msg('l',trim(KSS_file_name))
 !
 call msg('s','DBs path set to :',trim(core_io_path))
 !
 call a2y_db1(en,k,trim(KSS_file_name))
 !
 call msg('s','Writing main Database ...')
 !
 call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
 io_err=io_DB1(en,k,ID)
 !
 call msg('s','... Database done')
 !
 if (force_noWFs) then
   call msg('s',' == A2Y completed ==')
   call msg('s',' ')
   return
 endif
 !
 ! Splitting + SPIN support still not implemented (see a2y_wf.F for more details)
 ! Davide: barrier removed. I do not see any reason for keeping it
 !
 !
 ! GPL_EXCLUDE_START
 !
 call WF_splitter()
 !
 ! GPL_EXCLUDE_END
 !
 !
 ! GPL_INCLUDE_START
 !
 !if(n_spin==1) call WF_splitter()
 !
 ! GPL_INCLUDE_END
 !
 !
 call msg('s','Writing WFs and (eventually) nlPP ...')
 !
 YAMBO_ALLOC(wf_disk,(2,wf_nb_io,wf_ncx,n_spin))
 !
 do ik=1,nkibz
   !
   do ib_grp=1,wf_nb_io_groups
     !
     call a2y_wf(wf_disk,ik,ib_grp,trim(KSS_file_name))
     !
     if (n_atoms_species_max>0.and.ib_grp==1) then
       if (ik==1) call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
       if (ik> 1) call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/ik+1/),ID=ID)
       io_err=io_KB_abinit(ID)
     endif
     !
     if (ik==1.and.ib_grp==1) call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2,1/),ID=ID)
     if (ik> 1.or. ib_grp> 1) call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/ik+1,ib_grp/),ID=ID)
     io_err=io_WF(ID,wf=wf_disk)
     !
   enddo
   !
 enddo
 YAMBO_FREE(wf_disk)
 call PP_free()
 !
 ! Memory Overview
 !=================
#if defined _MEM_CHECK 
 !
 call MEM_manager("",REPORT="final")
 !
#endif
 !
 call msg('s','... WF and PP done')
 call msg('s',' == A2Y completed ==')
 call msg('s',' ')
 !
end function
