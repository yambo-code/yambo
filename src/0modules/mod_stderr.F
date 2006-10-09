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
 module stderr
  use pars
  integer          :: tty_size
  logical          :: write_to_log
  character(lchlen):: logfile

  interface 
    subroutine win_size(wdim)
      integer :: wdim
    end subroutine
    subroutine c_fprintf(lfmt,msg,rfmt,sfmt)
      character(*) :: lfmt,rfmt,msg,sfmt
    end subroutine
  end interface

  contains

 subroutine c_print(lfmt,msg,rfmt,sfmt)
!===============================================================
 character(*) :: lfmt,rfmt,msg,sfmt
 call c_fprintf(cstr(lfmt),cstr(msg),cstr(rfmt),cstr(sfmt))
 end subroutine

 character(lchlen) function cstr(si) result(so)
!===============================================================
 character(*), intent(IN) :: si
 integer :: i 
 i = len(trim(si))
 call clear_str(so)
 so(1:i) = si(1:i)
 so(i+1:i+1) = achar(0)
 end function cstr

 subroutine clear_str(str)
!===============================================================
 character(*), intent(out) :: str
 integer :: i
 do i = 1, len(str)
   str(i:i) = " " 
 end do
 end subroutine clear_str

 character(lchlen) function string_pack(str1,str2,str3,str4,str5)
!===============================================================
 character(*)          :: str1
 character(*),optional :: str2,str3,str4,str5
!ws
 character(lchlen) :: lch
!
 string_pack=str1
 if (present(str2)) then
   write (lch,'(2a)') trim(string_pack),trim(str2)
   string_pack=lch
 endif
 if (present(str3)) then
   write (lch,'(2a)') trim(string_pack),trim(str3)
   string_pack=lch
 endif
 if (present(str4)) then
   write (lch,'(2a)') trim(string_pack),trim(str4)
   string_pack=lch
 endif
 if (present(str5)) then
   write (lch,'(2a)') trim(string_pack),trim(str5)
   string_pack=lch
 endif
 end function 
!
 subroutine string_split(string_in,string_out)
!===============================================================
 character(*)     :: string_in
 character(schlen):: string_out(:)
!ws
 integer          :: i_pos(2),is

 i_pos=(/1,1/)
 is=0
 string_out=""
 do while (i_pos(1)<=len_trim(string_in))
!
! Here I go to the first non null characater " AB  C"
!                                              |
   do while (string_in(i_pos(1):i_pos(1)) == ' ')
     if (i_pos(1)==len_trim(string_in)) exit
     i_pos(1)=i_pos(1)+1
   enddo
!
! Here I go to the last non-null characater before a space " AB  C"
!                                                             |   
   i_pos(2)=i_pos(1)
   do while (string_in(i_pos(2):i_pos(2)) /= ' ')
     if (i_pos(2)==len_trim(string_in)) exit
     i_pos(2)=i_pos(2)+1
   enddo
   if(i_pos(2)<len_trim(string_in)) i_pos(2)=i_pos(2)-1
! 
   is=is+1
   string_out(is)=trim(string_in(i_pos(1):i_pos(2)))
   i_pos(1)=i_pos(2)+1
 
   if (i_pos(2)==len_trim(string_in)) exit

 enddo
!
 end subroutine
!
 character(schlen) function string_remove(string_in,what)
!===============================================================
 character(*)     :: string_in
 character(*)     :: what
!ws
 integer          :: i_pos

 string_remove=string_in
 i_pos=index(string_in,what)
 if (i_pos==0) return
 write (string_remove,'(2a)') string_in(:i_pos-1),string_in(i_pos+1:)

 end function

 end module stderr

