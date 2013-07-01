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
 use pars,                ONLY:schlen
 use IO_m,                ONLY:mk_dir 
 use stderr,              ONLY:string_split
 !
 implicit none
 integer,          intent(in) :: lnstr
 character(lnstr), intent(in) :: instr
 !
 ! Work Space
 !
 integer,parameter::n_max_pieces=100
 character(schlen)::rstr_piece(n_max_pieces)
 integer          ::n_pieces,i1
 !
 call string_split(instr(:lnstr),rstr_piece)
 n_pieces=0
 do i1=1,n_max_pieces
   if (len_trim(rstr_piece(i1))>0) n_pieces=n_pieces+1
 enddo
 if (n_pieces==0) return
 !
 write (*,*) check_for_word("db"),rstr_piece(check_for_word("db")+1)
! call mk_dir("ydb")
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
