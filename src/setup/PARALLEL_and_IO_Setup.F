!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team
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
subroutine PARALLEL_and_IO_Setup(en,k)
 !
 use stderr,         ONLY:log_as_a_file,intc
 use com,            ONLY:core_io_path,more_io_path,repfile,jobstr,msg,com_path,&
&                         alt_jobstr,n_alt_jobstr
 use it_m,           ONLY:infile
 use IO_m,           ONLY:io_control,OP_RD_CL,REP,Fragmented_IO
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use parallel_m,     ONLY:n_nodes,ncpu,n_MPI,n_chains,PARALLEL_message,n_CPU_str_max
 use openmp,         ONLY:n_threads,OPENMP_initialize,n_user_threads
 implicit none
 !
 type(levels)  :: en
 type(bz_samp) :: k
 ! 
 ! Work Space
 !
 integer           :: ID,io_err,i_s
 integer, external :: ioDB1
 !
 call section('*','CPU structure, Files & I/O Directories') 
 !
 call OPENMP_initialize(.FALSE.)
 !
 ! CPU structure
 !===============
 !
 if (ncpu>1) then
   !
   do i_s=0,n_CPU_str_max
     if (len_trim(PARALLEL_message(i_s))==0) cycle
     call msg( 's','CPU structure:',trim(PARALLEL_message(i_s)))
     call msg( 'r','* CPU structure :',trim(PARALLEL_message(i_s)))
   enddo
   !
   call msg( 'r','* MPI CHAINS    :',n_chains)
   call msg( 'r','* MPI CPU       :',n_MPI)
   !
 else
   !
   call msg ('r','* CPU           :',ncpu)
   !
 endif
 !
 call msg('r', '* THREADS  (tot):',n_threads)
 if (n_user_threads>0) then
   call msg('r', '*         (user):',n_user_threads)
 endif
 call msg('r', '* I/O NODES     :',n_nodes)
 call msg('rn','* Fragmented I/O:',Fragmented_IO)
 call msg('r','CORE databases in ',trim(core_io_path))
 call msg('r','Additional I/O in ',trim(more_io_path))
 call msg('r','Communications in ',trim(com_path))
 call msg('r','Input file     is ',trim(infile))
 call msg('r','Report file    is ',trim(repfile))
 if (len_trim(jobstr)>0        ) call msg('r','Job string(main): ',trim(jobstr))
 do i_s=1,n_alt_jobstr
   call msg('r','            (#'//trim(intc(i_s))//'): ',trim(alt_jobstr(i_s)))
 enddo
 if (log_as_a_file      ) call msg('r','Log files      in ',trim(trim(com_path)//"/LOG"))
 call msg('r','')
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),COM=REP,ID=ID)
 io_err=ioDB1(en,k,ID)
 !
end subroutine
