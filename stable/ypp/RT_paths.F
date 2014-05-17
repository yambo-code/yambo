!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
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
&                         N_pumps,N_probes,N_probe_frequencies
 use com,            ONLY:core_io_path,jobstr,file_exists,msg,error
 use IO_m,           ONLY:NONE
 use fields,         ONLY:Efield,i_Probe,i_Pump
 use stderr,         ONLY:string_split,cstr
 use electrons,      ONLY:levels
 implicit none
 !
 type(levels)      :: en
 !
 ! Work Space
 !
 character(lchlen) :: jobstr_save
 integer           :: i_f,i_fp,nf,N_pump_frequencies
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
 i_f=1
 do while (i_f<=nf)
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
   call RT_databases(en,IO_error=IO_error,first_sec_only=.TRUE.,COM_=NONE)
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
 ! Single Probe 
 !=============
 !
 if (N_probes==0.and.N_pumps==0) then
   call RT_databases(en,IO_error=IO_error,first_sec_only=.TRUE.,COM_=NONE)
   !
   ! I can use ANTIRES/RES probes with multiple frequencies only by using
   ! ProbeKey in the input file. This is because of the procedures defined in X_effective/X_inversion.
   !
   if (Efield(i_Probe)%n_frequencies>1) IO_error=.TRUE.
   if (.not.IO_error)                   N_probes=1
 endif
 !
 call msg('s',':: Pump   found      :',N_pumps)
 call msg('s',':: Probes found      :',N_probes)
 call msg('s',':: Probe  frequencies:',N_probe_frequencies)
 !
 if (N_pumps>1  ) call error('Multiple Pumps still not supported')
 if (N_probes==0) call error('No probes found')
 !
end subroutine
