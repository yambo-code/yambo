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
subroutine PARALLEL_and_IO_Setup(en,k)
 !
 use pars,           ONLY:lchlen
 use stderr,         ONLY:log_as_a_file,intc
 use com,            ONLY:core_io_path,more_io_path,repfile,jobstr,msg,com_path,&
&                         alt_jobstr,n_alt_jobstr,jobdir,alt_jobdir
 use it_m,           ONLY:infile
 use IO_m,           ONLY:io_control,OP_RD_CL,REP,frag_WF
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use parallel_m,     ONLY:n_nodes,ncpu,PARALLEL_message,n_CPU_str_max,PAR_COM_WORLD,&
&                         mpi_comm_world,myid,COMM_reset,PAR_COM_NULL
 use parser_m,       ONLY:parser
 use wave_func,      ONLY:WF_buffered_IO
 use openmp,         ONLY:n_threads_X,n_threads_SE,n_threads_RT,n_threads_DIP,n_threads_NL,n_threads
 use LIVE_t,         ONLY:USER_wall_time_string
 implicit none
 !
 type(levels)  :: en
 type(bz_samp) :: k
 ! 
 ! Work Space
 !
 integer           :: ID,io_err,i_s,n_max_threads
 character(lchlen) :: dumb_ch
 integer, external :: io_DB1
 !
 call section('*','CPU structure, Files & I/O Directories') 
 !
 ! Buffered I/O ?
 !===============
 !
 call parser('WFbuffIO'   ,WF_buffered_IO)
 !
 ! WORLD communicator setup
 !==========================
 !
 call COMM_reset(PAR_COM_WORLD)
 call COMM_reset(PAR_COM_NULL)
 !
#if defined _MPI
 !
 PAR_COM_WORLD%COMM  =mpi_comm_world
 PAR_COM_WORLD%CPU_id=myid
 PAR_COM_WORLD%n_CPU =ncpu
 !
 ! GET ENVIROMENT definitions
 !============================
 !
 call PARALLEL_get_ENVIRONMENT_structure("ALL")
 !
#endif
 !
 ! CPU structure REPORT
 !======================
 !
 n_max_threads=maxval((/n_threads,n_threads_X,n_threads_SE,n_threads_RT,n_threads_DIP,n_threads_NL/))
 !
 if (ncpu>1.or.n_max_threads>1) then
   !
   do i_s=0,n_CPU_str_max
     if (len_trim(PARALLEL_message(i_s))==0) cycle
     call msg( 's','CPU-Threads:',trim(PARALLEL_message(i_s)))
     call msg( 'r','* CPU-Threads     :',trim(PARALLEL_message(i_s)))
   enddo
   !
   call msg( 'r','* MPI CPU         :',ncpu)
   !
 else
   !
   call msg ('r','* CPU             :',ncpu)
   !
 endif
 !
 call msg('r', '* THREADS    (max):',n_max_threads)
 call msg('r', '* THREADS TOT(max):',n_max_threads*ncpu)
 call msg('r', '* I/O NODES       :',n_nodes)
 !
 ! I/O and more
 !===============
 !
 call msg('rn','* Fragmented WFs  :',frag_WF)
 call msg('r','CORE databases in   ',trim(core_io_path))
 call msg('r','Additional I/O in   ',trim(more_io_path))
 call msg('r','Communications in   ',trim(com_path))
 call msg('r','Input file     is   ',trim(infile))
 call msg('r','Report file    is   ',trim(repfile))
#if defined _DOUBLE
 call msg('r','Precision      is   ','DOUBLE')
#else
 call msg('r','Precision      is   ','SINGLE')
#endif
 if (log_as_a_file      ) call msg('r','Log files      in   ',trim(trim(com_path)//"/LOG"))
 !
 if (len_trim(USER_wall_time_string)>0)  call msg('r','User WALL time is   ',trim(USER_wall_time_string))
 !
 ! Strings & Dirs
 !-----------------
 !
 dumb_ch=" "
 if (len_trim(jobstr) >0) dumb_ch='Job string(s)-dir(s) (main): '//trim(jobstr)
 if (len_trim(jobdir) >0) dumb_ch=trim(dumb_ch)//" - "//trim(jobdir)
 if (len_trim(dumb_ch)>0) call msg('nr',trim(dumb_ch))
 do i_s=1,n_alt_jobstr
   if (len_trim(alt_jobstr(i_s))>0) then
     dumb_ch='                    (alt#'//trim(intc(i_s))//'): '//trim(alt_jobstr(i_s))
   else
     dumb_ch='                    (alt#'//trim(intc(i_s))//'): none'
   endif
   if (len_trim(alt_jobdir(i_s))>0) dumb_ch=trim(dumb_ch)//" - "//trim(alt_jobdir(i_s))
   call msg('r',trim(dumb_ch))
 enddo
 call msg('r','')
 !
 ! DB1 I/O
 !===============
 !
#if !defined _YPP_RT
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),COM=REP,ID=ID)
 io_err=io_DB1(en,k,ID)
 !
#endif
 !
end subroutine
