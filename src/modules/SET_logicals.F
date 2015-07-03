!
!        Copyright (C) 2000-2015 the YAMBO team
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
 use pars,       ONLY:schlen
 use drivers,    ONLY:l_eval_collisions
 use parallel_m, ONLY:IO_write_default,master_cpu,myid,master_cpu,myid,INTER_CHAIN,n_log_CPUs,ncpu
 use com,        ONLY:write_to_of,write_to_report
 use IO_m,       ONLY:IO_write,IO_read,DBs_IO_control_string,DBs_FRAG_control_string,                   &
&                     io_DIP,io_RESPONSE,io_HF,io_COLLs,io_GF,io_J,io_CARRIERs,io_SC,io_BS_K,io_SCREEN, &
&                     frag_DIP,frag_RESPONSE,frag_HF,frag_COLLs,frag_SC,   &
&                     frag_BS_K,frag_SCREEN,frag_QINDX,frag_ELPH,frag_RT
 use stderr,     ONLY:write_to_log,write_fragments_IO_log,string_split,write_to_log_default
 use LIVE_t,     ONLY:live_timing_is_on
 implicit none
 !
 ! Work Space
 !
 integer            ::i_chain,i_ch
 integer, parameter ::n_max=20
 character(schlen)  ::ch_piece(n_max)
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
 write_to_log_default      =write_to_log
 write_fragments_IO_log    =write_to_log
 IO_write_default(myid+1)  =master_cpu
 IO_write                  =IO_write_default(myid+1)
 write_to_of               =master_cpu
 write_to_report           =master_cpu
 IO_read                   =.TRUE.
 live_timing_is_on         =write_to_log
 !
 ! DBs IO
 !========
 io_DIP     =.TRUE.
 io_RESPONSE=.TRUE.
 io_SCREEN  =.FALSE.
 io_HF      =.TRUE.
 io_COLLs   =.TRUE.
 io_GF      =.TRUE.
 io_J       =.TRUE.
 io_CARRIERs=.TRUE.
 io_SC      =.TRUE.
 io_BS_K    =.TRUE.
 !
 ch_piece=' '
 call string_split(DBs_IO_control_string,ch_piece)
 !
 do i_ch=1,n_max
   if (any( (/"DIP","dip"/)                     ==trim(ch_piece(i_ch)) )) io_DIP     =.FALSE.
   if (any( (/"X","x"/)                         ==trim(ch_piece(i_ch)) )) io_RESPONSE=.FALSE.
   if (any( (/"W","w"/)                         ==trim(ch_piece(i_ch)) )) io_SCREEN  =.FALSE.
   if (any( (/"HF","hf"/)                       ==trim(ch_piece(i_ch)) )) io_HF      =.FALSE.
   if (any( (/"COLLS","COLLs","colls"/)         ==trim(ch_piece(i_ch)) )) io_COLLs   =.FALSE.
   if (any( (/"GF","gf"/)                       ==trim(ch_piece(i_ch)) )) io_GF      =.FALSE.
   if (any( (/"J","j"/)                         ==trim(ch_piece(i_ch)) )) io_J       =.FALSE.
   if (any( (/"CARRIERs","carriers","CARRIERS"/)==trim(ch_piece(i_ch)) )) io_CARRIERs=.FALSE.
   if (any( (/"SC","sc"/)                       ==trim(ch_piece(i_ch)) )) io_SC      =.FALSE.
   if (any( (/"k","K"/)                         ==trim(ch_piece(i_ch)) )) io_BS_K    =.FALSE.
   if (any( (/"BS","bs"/)                       ==trim(ch_piece(i_ch)) )) io_BS_K    =.FALSE.
 enddo
 !
 ! Override input in some cases
 !
 io_RESPONSE=io_RESPONSE.or.l_eval_collisions
 !
 ! DBs fragmentation
 !===================
 frag_DIP     =.FALSE.
 frag_RESPONSE=.TRUE.
 frag_SCREEN  =.TRUE.
 frag_HF      =.FALSE.
 frag_COLLs   =.TRUE.
 frag_SC      =.TRUE.
 frag_RT      =.TRUE.
 frag_ELPH    =.TRUE.
 frag_QINDX   =.FALSE.
 frag_BS_K    =.FALSE.
 !
 ch_piece=' '
 call string_split(DBs_FRAG_control_string,ch_piece)
 !
 do i_ch=1,n_max
   !
   if (any( (/"+DIP","+dip"/)                   ==trim(ch_piece(i_ch)) )) frag_DIP     =.TRUE.
   if (any( (/"+X","+x"/)                       ==trim(ch_piece(i_ch)) )) frag_RESPONSE=.TRUE.
   if (any( (/"+W","+w"/)                       ==trim(ch_piece(i_ch)) )) frag_SCREEN  =.TRUE.
   if (any( (/"+HF","+hf"/)                     ==trim(ch_piece(i_ch)) )) frag_HF      =.TRUE.
   if (any( (/"+COLLS","+COLLs","+colls"/)      ==trim(ch_piece(i_ch)) )) frag_COLLs   =.TRUE.
   if (any( (/"+k","+K"/)                       ==trim(ch_piece(i_ch)) )) frag_BS_K    =.TRUE.
   if (any( (/"+BS","+bs"/)                     ==trim(ch_piece(i_ch)) )) frag_BS_K    =.TRUE.
   if (any( (/"+QINDX","+qindx"/)               ==trim(ch_piece(i_ch)) )) frag_QINDX   =.TRUE.
   !
   if (any( (/"-DIP","-dip"/)                   ==trim(ch_piece(i_ch)) )) frag_DIP     =.FALSE.
   if (any( (/"-X","-x"/)                       ==trim(ch_piece(i_ch)) )) frag_RESPONSE=.FALSE.
   if (any( (/"-W","-w"/)                       ==trim(ch_piece(i_ch)) )) frag_SCREEN  =.FALSE.
   if (any( (/"-HF","-hf"/)                     ==trim(ch_piece(i_ch)) )) frag_HF      =.FALSE.
   if (any( (/"-COLLS","-COLLs","-colls"/)      ==trim(ch_piece(i_ch)) )) frag_COLLs   =.FALSE.
   if (any( (/"-k","-K"/)                       ==trim(ch_piece(i_ch)) )) frag_BS_K    =.FALSE.
   if (any( (/"-BS","-bs"/)                     ==trim(ch_piece(i_ch)) )) frag_BS_K    =.FALSE.
   if (any( (/"-QINDX","-qindx"/)               ==trim(ch_piece(i_ch)) )) frag_QINDX   =.FALSE.
   !
 enddo 
 !
end subroutine
