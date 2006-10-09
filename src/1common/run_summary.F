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
 subroutine run_summary(en,k)
!==================================
 use stderr,         ONLY:logfile
 use com,            ONLY:inpath,outpath,repfile,jobstr,msg
 use it_m,           ONLY:infile
 use IO_m,           ONLY:io_control,OP_RD_CL,REP
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 implicit none
 type(levels)  :: en
 type(bz_samp) :: k
 integer       :: ID,io_err
 integer, external :: ioDB1

 call section('*','Game Summary')
   
 call msg('r','IN  directory    : ',trim(inpath))
 call msg('r','OUT directory    : ',trim(outpath))
 call msg('r','Input file       : ',trim(infile))
 call msg('r','Report file      : ',trim(repfile))
 if (len_trim(jobstr)>0)  call msg('r','Job string       : ',trim(jobstr))
 if (len_trim(logfile)>0) call msg('r','Log file         : ',trim(jobstr))
 call msg('r','')

 call io_control(ACTION=OP_RD_CL,SEC=(/1/),COM=REP,ID=ID)
 io_err=ioDB1(en,k,ID)

 end subroutine
