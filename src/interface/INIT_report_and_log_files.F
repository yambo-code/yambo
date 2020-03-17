!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine INIT_report_and_log_files()
 !
 use pars,           ONLY:lchlen,repfile_index,logfile_index
 use stderr,         ONLY:logfile,logfile_unit,log_as_a_file,intc,write_to_log,write_to_log_default
 use it_m,           ONLY:rstatus,nrnlvls,rnlvls
 use com,            ONLY:repfile,repfile_unit,write_to_report,com_path,jobstr,of_unit
 use parallel_m,     ONLY:myid,ncpu
 use LIVE_t,         ONLY:live_timing_is_on
#if defined _YAML_OUTPUT
 use f_utils,        ONLY:f_get_free_unit
 use com,            ONLY:of_yaml_IDs
 use yaml_output,    ONLY:yaml_set_stream,yaml_stream_connected
#endif
 !
 implicit none
 !
 integer          :: i_r
 character(lchlen)::jch,rch
 !
 write_to_log      = write_to_log_default
 live_timing_is_on = write_to_log_default
 ! 
 if (trim(jobstr)=='') write (repfile,'(2a)') trim(com_path),'/r'
 if (trim(jobstr)/='') write (repfile,'(4a)') trim(com_path),'/','r-',trim(jobstr)
 if (log_as_a_file) then
  if (trim(jobstr)=='') write (logfile,'(2a)') trim(com_path),'/l'
  if (trim(jobstr)/='') write (logfile,'(4a)') trim(com_path),'/','l-',trim(jobstr)
  if (ncpu>1) then
    if (trim(jobstr)=='') write (logfile,'(a)')  trim(com_path)//'/LOG/l'
    if (trim(jobstr)/='') write (logfile,'(2a)') trim(com_path)//'/LOG/l-',trim(jobstr)
  endif
 endif
 do i_r=1,nrnlvls
   rch=repfile
   if (rstatus(i_r)/=0) then
     write (rch,'(3a)') trim(repfile),'_',trim(rnlvls(i_r,1))
     repfile=rch
   endif
   if (log_as_a_file) then
     jch=logfile
     if (rstatus(i_r)/=0) write (jch,'(3a)') trim(logfile),'_',trim(rnlvls(i_r,1))
     logfile=jch
   endif
 enddo
 if (ncpu>1.and.log_as_a_file) then
   jch=logfile
   logfile=trim(jch)//"_CPU_"//trim(intc(myid+1))
 endif
 !
 if (write_to_report) call rename_file(repfile)
 !
 if (log_as_a_file)   call rename_file(logfile)
 !
 ! Init flib
 !
#if defined _YAML_OUTPUT
 call f_lib_initialize()
#endif
 !
 if (write_to_report) then
   !
   ! Report file
   !
#if defined _YAML_OUTPUT
   repfile_unit=f_get_free_unit()
#else
   repfile_unit=11
#endif
   of_unit(repfile_index) = repfile_unit
   open(unit=repfile_unit,file=trim(repfile))
#if defined _YAML_OUTPUT
   call yaml_set_stream(filename=trim(repfile)//".yaml",record_length=200)
   call yaml_stream_connected(trim(repfile)//".yaml",of_yaml_IDs(repfile_index))
#endif
   call write_the_logo(repfile_unit,' ')
   !
 endif
 !
 if (log_as_a_file) then
   !
   ! Log file
   !
#if defined _YAML_OUTPUT
   logfile_unit=f_get_free_unit()
#else
   logfile_unit=13
#endif
   of_unit(repfile_index) = repfile_unit
   open(unit=logfile_unit,file=trim(logfile))
#if defined _YAML_OUTPUT
   call yaml_set_stream(filename=trim(logfile)//".yaml",record_length=200)
   call yaml_stream_connected(trim(logfile)//".yaml",of_yaml_IDs(logfile_index))
#endif
   !
 endif
 !
end subroutine
