!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine RT_OBSERVABLES_IO_paths(what,en)
 !
 use pars,           ONLY:lchlen
 use YPP_real_time,  ONLY:Probe_Keyword,Pump_Keyword,max_n_of_paths,Pump_path,Probe_path,RT_OBSERVABLES_IO,&
&                         N_pumps,N_probes,N_probe_frequencies
 use com,            ONLY:core_io_path,jobstr,file_exists,msg,error,warning
 use IO_m,           ONLY:NONE
 use fields,         ONLY:Efield
 use stderr,         ONLY:string_split,cstr,intc
 use electrons,      ONLY:levels
 use parser_m,       ONLY:parser
 !
 implicit none
 !
 character(*)      :: what
 type(levels)      :: en
 !
 ! Work Space
 !
 character(lchlen) :: jobstr_save
 integer           :: i_f,i_fp,nf,N_pump_frequencies,i_Probe,i_Pump
 logical           :: IO_error,l_many_paths=.false.
 character(10*max_n_of_paths) :: folder_list
 !
 call parser('CheckPaths',l_many_paths)
 !
 i_Probe=1
 i_Pump =2
 !
 if (l_many_paths) then
   !
   call section("=",'Checking for Probe and Pump paths')
   !====================================================
   !
   N_pumps =0
   !
   ! Get the list of ALL folders in the current I/O directory (defult is .)
   !
   call ifolder_list(cstr(trim(core_io_path)),folder_list,nf)
   !
   call string_split(folder_list(:nf),Probe_path)
   !
   nf=count( len_trim(Probe_path(:)) > 0 ) 
   !
   call warning(" Checking all "//trim(intc(nf))//" folders in core_io_path")
   !
   ! Remove "." and ".."
   !
   i_f=1
   do while (i_f<nf)
     if ( trim(Probe_path(i_f)) == "." .or. trim(Probe_path(i_f)) == ".." ) then
       forall (i_fp=i_f+1:nf) Probe_path(i_fp-1)=Probe_path(i_fp)
       nf=nf-1
     endif
     i_f=i_f+1
   enddo
   !
   Pump_path="none"
   !
   ! Define the probes and pump on the basis of the existence of (n)db.RT_G or (n)db.RT_J_and_P.
   ! Use the Pump_Keyword unless it is not "none". The Probe, instead, is defined also 
   ! when Probe_Keyword=none by using the provided jobstr.
   !
   i_f=1
   do while (i_f<=nf)
     !
     if (file_exists(trim(core_io_path)//"/"//trim(Probe_path(i_f))//'/ndb.RT_G').or.&
&        file_exists(trim(core_io_path)//"/"//trim(Probe_path(i_f))//'/ndb.RT_OBSERVABLES').or.&
&        file_exists(trim(core_io_path)//"/"//trim(Probe_path(i_f))//'/db.RT_G').or.&
&        file_exists(trim(core_io_path)//"/"//trim(Probe_path(i_f))//'/db.RT_OBSERVABLES')) then
       !
       if(index(trim(Probe_path(i_f)),trim(Probe_Keyword))>0.or.&
&         (trim(Probe_Keyword)=="none".and.index(trim(Probe_path(i_f)),trim(jobstr))>0)) then
         N_probes =N_probes+1
         i_f=i_f+1
       else if ( index(trim(Probe_path(i_f)),trim(Pump_Keyword))>0 ) then
         Pump_path=Probe_path(i_f)
         N_pumps=1
       endif
       !
     endif
     !
     forall (i_fp=i_f+1:nf) Probe_path(i_fp-1)=Probe_path(i_fp)
     nf=nf-1
     !
   enddo
   !
   jobstr_save=jobstr
   !
   ! Probe Databases
   !=================
   !
   do i_f=1,N_probes
     jobstr=trim(Probe_path(i_f))
     call RT_OBSERVABLES_IO(what=what,IO_error=IO_error,first_sec_only=.TRUE.,COM_=NONE)
     N_probe_frequencies= Efield(i_Probe)%n_frequencies
   enddo
   !
   ! In case of several probe_frequencies only one probe is supported
   !
   if (N_probes>1.and.N_probe_frequencies>1) N_probes=0
   !
   ! Pump only
   !===========
   !
   if (N_pumps==1) then
     jobstr=trim(Pump_path)
     call RT_OBSERVABLES_IO(what=what,IO_error=IO_error,first_sec_only=.TRUE.,COM_=NONE)
     if (.not.IO_error) then
       N_pump_frequencies= Efield(i_Pump)%n_frequencies
       if (N_pump_frequencies>1) call error('Pumps with multiple frequencies still not supported')
     else
       N_pumps=0
     endif
   endif
   !
   jobstr=jobstr_save
   !
   !
 else
   !
   N_probes=0
   N_pumps=0
   !
 endif
 !
 ! Single Probe 
 !=============
 !
 if (N_probes==0.and.N_pumps==0) then
   !
   call RT_OBSERVABLES_IO(what=what,IO_error=IO_error,first_sec_only=.TRUE.,COM_=NONE)
   !
   ! I can use ANTIRES/RES probes with multiple frequencies only by using
   ! ProbeKey in the input file. This is because of the procedures defined in X_effective/X_inversion.
   !
   if (Efield(i_Probe)%n_frequencies>1) IO_error=.TRUE.
   if (.not.IO_error)                   N_probes=1
   !
   Probe_path(1)=jobstr
   !
 endif
 !
 call msg('s',':: Pumps  found      :',N_pumps)
 call msg('s',':: Probes found      :',N_probes)
 call msg('s',':: Probe  frequencies:',N_probe_frequencies)
 !
 if (N_pumps>1  ) call error('Multiple Pumps still not supported')
 if (N_probes==0) call error('No probes found')
 !
end subroutine
