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
subroutine SET_logicals()
 !
 use pars,       ONLY:schlen
 use parallel_m, ONLY:IO_write_default,master_cpu,myid,master_cpu,myid,INTER_CHAIN,n_log_CPUs,ncpu
 use com,        ONLY:write_to_of,write_to_report
 use fragments,  ONLY:Parallel_Fragments
 use IO_m,       ONLY:IO_write,IO_read,Fragmented_IO,Fragmented_IO_save,IO_write_not_fragmented_DB,&
&                     DBs_IO_control_string,io_DIP,io_X,io_HF
 use stderr,     ONLY:write_to_log,write_fragments_IO_log,string_split
 use LIVE_t,     ONLY:live_timing_is_on
 !
 ! Work Space
 !
 integer          ::i_chain,i_ch
 character(schlen)::ch_piece(10)
 !
 master_cpu                =myid==0
 !
 if (n_log_CPUs==0) then
   write_to_log            =.TRUE.
 else
   i_chain=nint(log(real(ncpu)/real(n_log_CPUs))/log(2.))
   if (i_chain==0) write_to_log=.TRUE.
   if (i_chain> 0) write_to_log=INTER_CHAIN(i_chain)%CPU_id==0
 endif
 !
 write_fragments_IO_log    =write_to_log
 IO_write_default(myid+1)  =master_cpu
 IO_write                  =IO_write_default(myid+1)
 write_to_of               =master_cpu
 write_to_report           =master_cpu
 IO_read                   =.TRUE.
 Fragmented_IO             =Fragmented_IO_save
 live_timing_is_on         =write_to_log
 Parallel_Fragments        =.TRUE.
 IO_write_not_fragmented_DB=master_cpu
 !
 call string_split(DBs_IO_control_string,ch_piece)
 !
 do i_ch=1,10
   if (trim(ch_piece(i_ch))=="DIP") io_DIP=.FALSE.
   if (trim(ch_piece(i_ch))=="X")   io_X=.FALSE.
   if (trim(ch_piece(i_ch))=="HF")  io_HF=.FALSE.
 enddo
 !
end subroutine
