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
subroutine SET_logicals()
 !
 use pars,       ONLY:schlen,n_max_DB_IO_off_fields
 use drivers,    ONLY:l_eval_collisions
 use parallel_m, ONLY:IO_write_default,master_cpu,myid,master_cpu,myid,n_log_CPUs,ncpu
 use com,        ONLY:write_to_of,write_to_report
 use IO_m,       ONLY:IO_write,IO_read,DBs_IO_control_string,DBs_FRAG_control_string, &
&                     io_DIP,io_RESPONSE,io_HF,io_COLLs,io_GF,io_OBSERVABLES,io_CARRIERs,io_SC,io_BS_K,io_SCREEN, &
&                     frag_DIP,frag_RESPONSE,frag_HF,frag_SC,io_MULTIPOLE, &
&                     frag_BS_K,frag_SCREEN,frag_QINDX,frag_ELPH,frag_RT
 use stderr,     ONLY:write_to_log,write_fragments_IO_log,STRING_split,write_to_log_default
 use LIVE_t,     ONLY:live_timing_is_on
 !
 implicit none
 !
 ! Work Space
 !
 integer            ::i_ch
 character(schlen)  ::ch_piece(n_max_DB_IO_off_fields)
 !
 ! Parallel Master & CPU log
 !===========================
 !
 master_cpu                =myid==0
 !
 if (n_log_CPUs==0) write_to_log_default = .TRUE.
 if (n_log_CPUs> 0) write_to_log_default = (mod(myid,(ncpu-1)/n_log_CPUs+1)==0)
 !
 ! Write permissions 
 !===================
 !
 write_fragments_IO_log    =write_to_log_default
 IO_write_default(myid+1)  =master_cpu
 IO_write                  =master_cpu
 write_to_of               =master_cpu
 write_to_report           =master_cpu
 IO_read                   =.TRUE.
 !
 ! Both set to write_to_log_default after the log file is initialized
 !
 write_to_log              =.FALSE. 
 live_timing_is_on         =.FALSE.
 !
 ! DBs IO
 !========
 io_DIP         =.TRUE.
 io_RESPONSE    =.TRUE.
 io_MULTIPOLE   =.TRUE.
 io_SCREEN      =.FALSE.
 io_HF          =.TRUE.
 io_COLLs       =.TRUE.
 io_GF          =.TRUE.
 io_OBSERVABLES =.TRUE.
 io_CARRIERs    =.TRUE.
 io_SC          =.TRUE.
 io_BS_K        =.TRUE.
 !
 ch_piece=' '
 call STRING_split(DBs_IO_control_string,ch_piece)
 !
 do i_ch=1,n_max_DB_IO_off_fields
   if (str_check(4,(/"DIP","dip","ALL","all"/),ch_piece(i_ch)) ) io_DIP     =.FALSE.
   if (str_check(4,(/"X  ","x  ","ALL","all"/),ch_piece(i_ch)) ) io_RESPONSE=.FALSE.
   if (str_check(4,(/"W  ","w  ","ALL","all"/),ch_piece(i_ch)) ) io_SCREEN  =.FALSE.
   if (str_check(4,(/"HF ","hf ","ALL","all"/),ch_piece(i_ch)) ) io_HF      =.FALSE.
   if (str_check(5,(/"COLLS","COLLs","colls","ALL  ","all  "/),&
&                                               ch_piece(i_ch)) ) io_COLLs      =.FALSE.
   if (str_check(4,(/"GF ","gf ","ALL","all"/),ch_piece(i_ch)) ) io_GF          =.FALSE.
   if (str_check(4,(/"J  ","j  ","ALL","all"/),ch_piece(i_ch)) ) io_OBSERVABLES =.FALSE. ! Back Compatibility
   if (str_check(4,(/"OBS","obs","ALL","all"/),ch_piece(i_ch)) ) io_OBSERVABLES =.FALSE. 
   if (str_check(5,(/"CARRIERs","carriers","CARRIERS","ALL     ","all     "/),&
&                                               ch_piece(i_ch)) ) io_CARRIERs=.FALSE.
   if (str_check(4,(/"SC ","sc ","ALL","all"/),ch_piece(i_ch)) ) io_SC      =.FALSE.
   if (str_check(4,(/"k  ","K  ","ALL","all"/),ch_piece(i_ch)) ) io_BS_K    =.FALSE.
   if (str_check(4,(/"BS ","bs ","ALL","all"/),ch_piece(i_ch)) ) io_BS_K    =.FALSE.
 enddo
 !
 ! Override input in some cases
 !
 io_RESPONSE=io_RESPONSE.or.l_eval_collisions
 !
 ! DBs fragmentation
 !===================
 frag_DIP     =.TRUE.
 frag_RESPONSE=.TRUE.
 frag_SCREEN  =.TRUE.
 frag_HF      =.FALSE.
 frag_SC      =.TRUE.
 frag_RT      =.TRUE.
 frag_ELPH    =.TRUE.
 frag_QINDX   =.FALSE.
 frag_BS_K    =.FALSE.
 !
 ch_piece=' '
 call STRING_split(DBs_FRAG_control_string,ch_piece)
 !
 do i_ch=1,n_max_DB_IO_off_fields
   !
   if (str_check(4,(/"+DIP","+dip","+ALL","+all"/),ch_piece(i_ch)) ) frag_DIP     =.TRUE.
   if (str_check(4,(/"+X  ","+x  ","+ALL","+all"/),ch_piece(i_ch)) ) frag_RESPONSE=.TRUE.
   if (str_check(4,(/"+W  ","+w  ","+ALL","+all"/),ch_piece(i_ch)) ) frag_SCREEN  =.TRUE.
   if (str_check(4,(/"+HF ","+hf ","+ALL","+all"/),ch_piece(i_ch)) ) frag_HF      =.TRUE.
   if (str_check(4,(/"+k  ","+K  ","+ALL","+all"/),ch_piece(i_ch)) ) frag_BS_K    =.TRUE.
   if (str_check(4,(/"+BS ","+bs ","+ALL","+all"/),ch_piece(i_ch)) ) frag_BS_K    =.TRUE.
   if (str_check(4,(/"+QINDX","+qindx","+ALL  ","+all  "/),ch_piece(i_ch)) ) frag_QINDX   =.TRUE.
   !
   if (str_check(4,(/"-DIP","-dip","-ALL","-all"/),ch_piece(i_ch)) ) frag_DIP     =.FALSE.
   if (str_check(4,(/"-X  ","-x  ","-ALL","-all"/),ch_piece(i_ch)) ) frag_RESPONSE=.FALSE.
   if (str_check(4,(/"-W  ","-w  ","-ALL","-all"/),ch_piece(i_ch)) ) frag_SCREEN  =.FALSE.
   if (str_check(4,(/"-HF ","-hf ","-ALL","-all"/),ch_piece(i_ch)) ) frag_HF      =.FALSE.
   if (str_check(4,(/"-k  ","-K  ","-ALL","-all"/),ch_piece(i_ch)) ) frag_BS_K    =.FALSE.
   if (str_check(4,(/"-BS ","-bs ","-ALL","-all"/),ch_piece(i_ch)) ) frag_BS_K    =.FALSE.
   if (str_check(4,(/"-QINDX","-qindx","-ALL  ","-all  "/),ch_piece(i_ch)) ) frag_QINDX   =.FALSE.
   !
 enddo 
 !
contains
 !
 logical function str_check(n,strings,val)
    integer      :: n
    character(*) :: strings(n)
    character(*) :: val
    !
    integer :: i
    str_check=.false.
    !
    do i = 1, n
      if ( trim(strings(i))==trim(val) ) then
        str_check=.true.
        return
      endif
    enddo
 end function    
 !
end subroutine
