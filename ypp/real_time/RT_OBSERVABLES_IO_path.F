!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine RT_OBSERVABLES_IO_paths(what,en)
 !
 use YPP_interfaces, ONLY:RT_OBSERVABLES_IO
 use YPP_real_time,  ONLY:Probe_Keyword,Pump_Keyword,max_n_of_paths,Pump_path,Probe_path,&
&                         N_pumps,N_probes,N_probe_frequencies
 use RT_output_m,    ONLY:RT_desc
 use com,            ONLY:core_io_path,msg,jobstr
 use IO_m,           ONLY:NONE
 use fields,         ONLY:Efield,n_ext_fields
 use stderr,         ONLY:STRING_split,cstr,intc,STRING_same
 use electrons,      ONLY:levels
 use interfaces,     ONLY:DESC_compare
 use descriptors,    ONLY:IO_desc,IO_desc_duplicate,IO_desc_reset
 !
 implicit none
 !
 character(*)      :: what
 type(levels)      :: en
 !
 ! Work Space
 !
 integer           :: i_f,i_fp,nf,N_pump_frequencies,i_Probe,i_Pump,i_d,i_desc_err(2)
 logical           :: IO_error
 character(100*max_n_of_paths) :: folder_list
 type(IO_desc)     :: Pump_RT_desc
 logical, external :: file_exists
 !
 i_Probe=1
 i_Pump =2
 !
 call section("+",'Pump and Probe(s)')
 !====================================
 !
 if (.not.STRING_same(Pump_Keyword,"none").or..not.STRING_same(Probe_Keyword,"none")) then
   !
   N_pumps =0
   !
   ! Get the list of ALL folders in the current I/O directory (default is .)
   !
   call ifolder_list(cstr(trim(core_io_path)),folder_list,nf)
   !
   if (nf>100*max_n_of_paths) call error(" too many folders found")
   !
   call STRING_split(folder_list(:nf),Probe_path)
   !
   nf=count( len_trim(Probe_path(:)) > 0 ) 
   !
   call warning(" Searching for Probe(s) and Pump(s) databases among "//trim(intc(nf))//" folders in "//trim(core_io_path))
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
&        file_exists(trim(core_io_path)//"/"//trim(Probe_path(i_f))//'/ndb.RT_OBSERVABLES')) then
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
   ! If no Probes using the Keyword try using jobstr
   !
   if (N_probes==0) then
     N_probes=1
     Probe_path(1)="default"
   endif
   !
   ! Jobstring/dirs re-direction
   !
   call IO_temporary_jobdir_and_strings("ALL","SAVE")
   !
   ! Pump 
   !======
   if (N_pumps==1) then
     call IO_temporary_jobdir_and_strings("JOBSTR",Pump_path)
     call RT_OBSERVABLES_IO(what=what,IO_error=IO_error,first_sec_only=.TRUE.,COM_=NONE)
     if (.not.IO_error) then
       N_pump_frequencies= Efield(i_Pump)%n_frequencies
       call IO_desc_reset(Pump_RT_desc)
       call IO_desc_duplicate(RT_desc,Pump_RT_desc)
       if (N_pump_frequencies>1) call error('Pumps with multiple frequencies still not supported')
     else
       N_pumps=0
     endif
   endif
   !
   ! Probe Databases
   !=================
   do i_f=1,N_probes
     if (.not.STRING_same(Probe_path(i_f),"default")) call IO_temporary_jobdir_and_strings("JOBSTR",Probe_path(i_f))
     if (     STRING_same(Probe_path(i_f),"default")) call IO_temporary_jobdir_and_strings("ALL","RESTORE")
     call RT_OBSERVABLES_IO(what=what,IO_error=IO_error,first_sec_only=.TRUE.,COM_=NONE)
     N_probe_frequencies= Efield(i_Probe)%n_frequencies
   enddo
   !
   ! Pump & Probe comparison
   !
   if (N_pumps>0) then
     call DESC_compare(RT_desc,Pump_RT_desc,i_desc_err,exclude="FIELDs")
     if (any(i_desc_err/=0)) call error("Probe and Pump runs are not compatible")
     if (n_ext_fields/=2) call warning("Probe run has only one field. Pump ignored")
   endif
   !
   ! In case of several probe_frequencies only one probe is supported
   !
   if (N_probes>1.and.N_probe_frequencies>1) N_probes=0
   !
   call IO_temporary_jobdir_and_strings("ALL","RESTORE")
   !
 else
   !
   N_probes=0
   N_pumps=0
   !
 endif
 !
 ! No pump & Probe passed by -J strings
 !======================================
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
   ! If no Probes using the Keyword try using jobstr
   !
   Probe_path(1)="default"
   !
 endif
 !
 call msg('s',' Pumps  found      ',N_pumps)
 call msg('s',' Probes found      ',N_probes)
 call msg('s',' Probe  frequencies',N_probe_frequencies)
 !
 if (N_pumps>1  ) call error('Multiple Pumps still not supported')
 if (N_probes==0) call error('No probes found')
 !
end subroutine
