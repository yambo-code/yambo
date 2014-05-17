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
subroutine IO_Setup(en,k)
 use pars,           ONLY:lchlen
 use stderr,         ONLY:logfile,tty_size,intc
 use com,            ONLY:core_io_path,more_io_path,repfile,jobstr,msg,com_path,warning,second_jobstr
 use it_m,           ONLY:infile
 use IO_m,           ONLY:io_control,OP_RD_CL,REP,IO_write,Fragmented_IO
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use parallel_m,     ONLY:n_nodes,ncpu,check_for_a_redundant_IO,PP_redux_wait,myid,&
&                         local_master_cpu,master_cpu
 use WF_distribute,  ONLY:Distributed_Memory
 implicit none
 !
 type(levels)  :: en
 type(bz_samp) :: k
 ! 
 ! Work Space
 !
 integer           :: ID,io_err,n_cpu_per_node
 integer(8)        :: cpu_seen_by_me(ncpu),cpu_seen_by_any(ncpu)
 character(lchlen) :: cluster_msg
 integer, external :: ioDB1
 !
 call section('*','Files & I/O Directories') 
 !
 ! CPU's structure
 !
 local_master_cpu(myid+1)=master_cpu
 !
 ! GPL_EXCLUDE_START
 !
 if (ncpu>1.and.more_IO_path/=core_IO_path) then
   !
   call check_for_a_redundant_IO(trim(more_IO_path),cpu_seen_by_me,cpu_seen_by_any)
   call PP_redux_wait()
   n_cpu_per_node=cpu_seen_by_any(1)
   !
   if (.not.all(cpu_seen_by_any==n_cpu_per_node)) then
     !
     call warning('Impossible to setup a distributed I/O configuration')
     more_IO_path=core_IO_path
     !
   else
     n_nodes=ncpu/n_cpu_per_node
     if (n_nodes>1) then
       cluster_msg='I/O is clustered in '//trim(intc(n_nodes))//&
&                  ' node(s) in a '//trim(intc(n_cpu_per_node))//' cpu/node configuration'
       call msg('rns',trim(cluster_msg))
       !
       ! Parallel I/O: 
       !
       ! In each node I pick 1 cpu that is allowed to write in the "MIRRORING"
       ! mode, that is the default. The local_master_cpu logical is indeed used
       ! to restore the default in IO_switch.
       !
       local_master_cpu(myid+1)=sum(cpu_seen_by_me(myid+1:))==n_cpu_per_node
       !
       Fragmented_IO=.true.
       !
     endif
   endif
   !
 endif
 !
 IO_write=local_master_cpu(myid+1)
 ! 
 ! GPL_EXCLUDE_END 
 !
 if(Distributed_Memory) call msg('rn','** Distributed Memory Run **')
 ! 
 call msg('r','CORE databases in ',trim(core_io_path))
 call msg('r','Additional I/O in ',trim(more_io_path))
 call msg('r','Communications in ',trim(com_path))
 call msg('r','Input file     is ',trim(infile))
 call msg('r','Report file    is ',trim(repfile))
 if (len_trim(jobstr)>0 )        call msg('r','Job string     is ',trim(jobstr))
 if (len_trim(second_jobstr)>0 ) call msg('r','2nd Job string is ',trim(second_jobstr))
 if (tty_size<0         ) call msg('r','Log file       is ',trim(logfile))
 call msg('r','')
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),COM=REP,ID=ID)
 io_err=ioDB1(en,k,ID)
 !
end subroutine
