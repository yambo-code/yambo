!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
subroutine RT_paths(en)
 !
 use pars,           ONLY:lchlen
 use YPP,            ONLY:Probe_Keyword,Pump_Keyword,max_n_of_paths,Pump_path,Probe_path,RT_databases,&
&                         N_pumps,N_probes
 use com,            ONLY:core_io_path,jobstr,file_exists,msg
 use IO_m,           ONLY:NONE,LOG
 use stderr,         ONLY:string_split,cstr
 use electrons,      ONLY:levels
 implicit none
 !
 type(levels)      :: en
 !
 character(lchlen) :: jobstr_save
 integer           :: i_f,i_fp,nf
 logical           :: IO_error
 character(10*max_n_of_paths) :: folder_list
 !
 call section("=",'Checking for Probe and Pump paths')
 !====================================================
 !
 N_pumps =0
 !
 call ifolder_list(cstr(trim(core_io_path)),folder_list,nf)
 !
 call string_split(folder_list(:nf),Probe_path)
 !
 nf=count( len_trim(Probe_path(:)) > 0 ) 
 !
 Pump_path="none"
 !
 i_f=1
 do while (i_f<nf)
   if (file_exists(trim(core_io_path)//"/"//trim(Probe_path(i_f))//'/ndb.RT_GreenF').and.&
&      index(trim(Probe_path(i_f)),trim(Probe_Keyword))>0  ) then
     N_probes =N_probes+1
     i_f=i_f+1
   else
     if ( index(trim(Probe_path(i_f)),trim(Pump_Keyword))>0 ) then
       Pump_path=Probe_path(i_f)
       N_pumps=1
     endif
     forall (i_fp=i_f+1:nf) Probe_path(i_fp-1)=Probe_path(i_fp)
     nf=nf-1
   endif
 enddo
 !
 jobstr_save=jobstr
 !
 ! Probe Databases
 !=================
 !
 do i_f=1,N_probes
   jobstr=trim(Probe_path(i_f))
   call RT_databases(en,IO_error=IO_error,first_sec_only=.TRUE.,COM_=NONE)
 enddo
 !
 ! Pump only
 !===========
 !
 if (N_pumps==1) then
   jobstr=trim(Pump_path)
   call RT_databases(en,IO_error=IO_error,first_sec_only=.TRUE.,COM_=NONE)
 endif
 !
 jobstr=jobstr_save
 !
 ! Single Probe 
 !=============
 !
 if (N_probes==0.and.N_pumps==0) then
   call RT_databases(en,IO_error=IO_error,first_sec_only=.TRUE.,COM_=NONE)
   if (.not.IO_error) N_probes=1
 endif
 !
 call msg('s',':: Pump   found:',N_pumps)
 call msg('s',':: Probes found:',N_probes)
 !
end subroutine
