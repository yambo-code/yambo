!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use pars,           ONLY:lchlen
 use stderr,         ONLY:logfile,log_as_a_file,intc,write_to_log,write_to_log_default
 use it_m,           ONLY:rstatus,nrnlvls,rnlvls
 use com,            ONLY:repfile,com_path,jobstr
 use parallel_m,     ONLY:myid,ncpu
 use LIVE_t,         ONLY:live_timing_is_on
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
end subroutine
