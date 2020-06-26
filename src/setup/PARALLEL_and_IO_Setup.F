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
subroutine PARALLEL_and_IO_Setup(en,k)
 !
 use pars,           ONLY:lchlen,SP,DP
 use stderr,         ONLY:log_as_a_file,intc
 use com,            ONLY:core_io_path,more_io_path,repfile,jobstr,msg,com_path,&
&                         alt_jobstr,n_alt_jobstr,jobdir,alt_jobdir,fat_log
 use it_m,           ONLY:infile
 use IO_m,           ONLY:io_control,OP_RD_CL,REP,frag_WF
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use parallel_m,     ONLY:n_nodes,ncpu,PARALLEL_message,n_CPU_str_max,PAR_COM_WORLD,&
&                         mpi_comm_world,myid,COMM_reset,PAR_COM_NULL,host_name,&
&                         master_cpu,PP_redux_wait,n_IO_nodes,PP_bcast
 use parser_m,       ONLY:parser
 use wave_func,      ONLY:WF_buffered_IO
 use openmp,         ONLY:n_threads_X,n_threads_SE,n_threads_RT,n_threads_DIP,n_threads_NL,n_threads
 use LIVE_t,         ONLY:USER_wall_time_string
 use cuda_m,         ONLY:cuda_setup,cuda_visible_devices,have_cuda_devices
 implicit none
 !
 type(levels)  :: en
 type(bz_samp) :: k
 ! 
 ! Work Space
 !
 integer           :: ID,i_err,i_s,n_max_threads,i_cpu
 character(lchlen) :: dumb_ch
 integer, external :: io_DB1
 !
 !
 call section('*','MPI/OPENMP structure, Files & I/O Directories') 
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
 ! Nodes
 !==========================
 if (master_cpu) dumb_ch=host_name
 call PP_bcast(dumb_ch,0)
 do i_cpu=2,ncpu
   if (myid+1==i_cpu) then
     if (trim(host_name)==trim(dumb_ch)) n_nodes=0
     dumb_ch=host_name
   endif
   call PP_bcast(dumb_ch,i_cpu-1)
 enddo
 call PP_redux_wait(n_nodes)
 !
 ! GET ENVIROMENT definitions
 !============================
 !
 call PARALLEL_get_ENVIRONMENT_structure("ALL")
 !
#endif
 !
 ! GET CUDA ENVIROMENT defs 
 !============================
 !
 call cuda_setup()
 !
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
     call msg( 's','MPI Cores-Threads   ',trim(PARALLEL_message(i_s)))
     call msg( 'r','Cores-Threads       ',trim(PARALLEL_message(i_s)))
   enddo
   !
   call msg( 'r','MPI Cores           ',ncpu)
   !
 else
   !
   call msg ('r','Cores               ',ncpu)
   !
 endif
 !
 call msg('r', 'Threads per core    ',n_max_threads)
 call msg('r', 'Threads total       ',n_max_threads*ncpu)
 call msg('r', 'Nodes Computing     ',n_nodes(1))
 call msg('r', 'Nodes IO            ',n_IO_nodes)
#ifdef _CUDA
 call msg('r', 'CUDA support        ',"yes")
 call msg('r', 'CUDA devices        ',have_cuda_devices)
 call msg('r', 'CUDA visible dev    ',trim(cuda_visible_devices))
#endif
 !
 ! I/O and more
 !===============
 !
 call msg('nr','Fragmented WFs      ',frag_WF)
 call msg('r', 'CORE databases      ',trim(core_io_path))
 call msg('r', 'Additional I/O      ',trim(more_io_path))
 call msg('r', 'Communications      ',trim(com_path))
 call msg('r', 'Input file          ',trim(infile))
 call msg('r', 'Report file         ',trim(repfile))
 call msg('r', 'Verbose log/report  ',fat_log)
 if (log_as_a_file) then
   call msg('r','Log files           ',trim(trim(com_path)//"/LOG"))
 endif
 !
 if (len_trim(USER_wall_time_string)>0)  call msg('r','User WALL time      ',trim(USER_wall_time_string))
 !
 ! Strings & Dirs
 !-----------------
 !
 if (len_trim(jobstr) >0) then
   if ( len_trim(jobdir)>0 ) then
     call msg('nr',"Job   dir + string  ",(/jobdir,jobstr/))
   else
     call msg('nr',"Job   string        ",jobstr)
   endif
 endif
 do i_s=1,n_alt_jobstr
   if (len_trim(alt_jobdir(i_s)) >0) then
     call msg('r',"Alt#"//trim(intc(i_s))//" dir + string  ",(/alt_jobdir(i_s),alt_jobstr(i_s)/))
   else
     call msg('r',"Alt#"//trim(intc(i_s))//" string        ",alt_jobstr(i_s))
   endif
 enddo
 !
 if(SP/=DP) call msg('nr', 'Precision           ','SINGLE')
 if(SP==DP) call msg('nr', 'Precision           ','DOUBLE')
 !
 call msg('r','')
 !
 ! DB1 I/O
 !===============
 !
#if !defined _YPP_RT
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),COM=REP,ID=ID)
 i_err=io_DB1(en,k,ID)
 !
#endif
 !
end subroutine
