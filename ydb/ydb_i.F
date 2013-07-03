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
integer function ydb_i(lnstr,instr)
 !
 use pars,                ONLY:schlen,lchlen
 use IO_m,                ONLY:mk_dir 
 use com,                 ONLY:file_exists
 use stderr,              ONLY:string_split,write_to_log,log_as_a_file
 use LIVE_t,              ONLY:LIVE_message
 use parallel_m,          ONLY:ncpu
 !
 implicit none
 integer,          intent(in) :: lnstr
 character(lnstr), intent(in) :: instr
 !
 ! Work Space
 !
 integer,parameter::n_max_pieces=100
 character(schlen)::rstr_piece(n_max_pieces)
 character(lchlen)::PATH,file_name
 integer          ::n_pieces,i1
 !
 ! Presets
 !
 ydb_i =0
 ncpu  =1
 write_to_log =.TRUE.
 log_as_a_file=.FALSE.
 !
 ! INPUT string splitter
 !
 call string_split(instr(:lnstr),rstr_piece)
 n_pieces=0
 do i1=1,n_max_pieces
   if (len_trim(rstr_piece(i1))>0) n_pieces=n_pieces+1
 enddo
 if (n_pieces==0) return
 !
 ! PATH
 !
 if (file_exists('~/.ydb/PATH')) then
   open(unit=1,file='~/.ydb/PATH',form='formatted')
   read (1,'(a)') PATH
   close(1)
 else 
   call LIVE_message('[ERROR] DB not found ',"n","n","%s")
   stop
 endif
 !
 ! ADD
 !
 if (check_for_word("add")>0) then
   call LIVE_message('Adding ... '//trim(file_name),"n"," ","%s")
   if (check_for_word("in")>0) then
     read(rstr_piece(check_for_word("in")+1),'(a)') file_name
     call LIVE_message('... INPUT file '//trim(file_name),"n"," ","%s")
     call YDB_scan_file(trim(file_name))
   endif
 endif
 !
!,rstr_piece(check_for_word("db")+1)
! call mk_dir("ydb")
 call LIVE_message(' ',"n","n","%s")
 !
 contains
   !
   integer function check_for_word(word)
     character(*) :: word
     integer      :: ic
     check_for_word=-1
     do ic=1,n_max_pieces
       if (trim(rstr_piece(ic))==word) check_for_word=ic
     enddo
   end function
end function
