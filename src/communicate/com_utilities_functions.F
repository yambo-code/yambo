!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
logical function OF_exists(name)
 !
 use pars,      ONLY:max_open_ofs
 use com,       ONLY:opened_of
 !
 implicit none
 !
 character(*) ::name
 integer      ::i_f
 OF_exists=.false. 
 do i_f=1,max_open_ofs
   if (index(opened_of(i_f),name)/=0) OF_exists=.TRUE.
 enddo
 !
end function OF_exists
!
logical function file_exists(name)
 !
 implicit none
 !
 character(*) name
 !
 file_exists=.false.
 if (len_trim(name)==0) return
 inquire(file=name,exist=file_exists)
 !
end function file_exists
!
function composed_fmt(basic_fmt,n,USE_TABS,INDENT)
 !
 use pars,   ONLY:lchlen
 use com,    ONLY:terminator,depth
 use stderr, ONLY:STRING_pack,of_tabs
 !
 implicit none
 !
 character(lchlen) :: composed_fmt
 !
 character(*)     :: basic_fmt
 integer          :: n,INDENT
 logical          :: USE_TABS
 ! 
 ! Work Space
 !
 integer           :: i1
 character(lchlen) :: lch
 !
 if (USE_TABS) then
   composed_fmt=STRING_pack(terminator(1),'a,')
   lch=composed_fmt
   do i1=1,n-1
     write (lch,'(2a,i3.3,3a)') trim(composed_fmt),'t',of_tabs(i1)+2+INDENT,',',basic_fmt,','
     composed_fmt=lch
   enddo
   write (composed_fmt,'(2a,i3.3,3a)') trim(lch),'t',of_tabs(n)+2+INDENT,',',basic_fmt,trim(terminator(2))
 else
   write (composed_fmt,'(a,i2.2,a,i5.4,4a)') trim(terminator(1)),depth+1,'x,a,',n,'(',basic_fmt,',1x)',trim(terminator(2))
   if (INDENT==0) write (composed_fmt,'(2a,i5.4,4a)')  trim(terminator(1)),'a,',n,'(',basic_fmt,',1x)',trim(terminator(2))
 endif
 if (n<0) write (composed_fmt,'(a,i5.4,3a)') '(a,',iabs(n),'(',basic_fmt,',1x))'
 !
end function composed_fmt
!
function filename(name)
 !
 use stderr,     ONLY:STRING_split,STRING_remove
 use pars,       ONLY:schlen,lchlen
 !
 implicit none
 !
 character(lchlen) :: filename
 character(*)      :: name
 integer           :: N
 character(schlen) :: names(20)
 character(lchlen) :: name_
 name_=name
 name_=STRING_remove(name_,"/",replace=" ")
 call STRING_split(name_,names,n_non_empty_strings=N)
 filename=trim(names(N))
 !
end function filename
!
integer function get_file_index(of_name_complete)
 !
 use pars,      ONLY:max_n_ofs,lchlen
 use com,       ONLY:n_ofs_defined,of_name
 !
 implicit none
 !
 character(lchlen), intent(in) :: of_name_complete
 integer :: i2
 !
 get_file_index=-1
 !
 do i2=1,n_ofs_defined
   ! the two can differ only by the _0n appendend in case it is not the first run
   if (index(trim(of_name(i2)),trim(of_name_complete))==0) cycle
   get_file_index=i2
   exit
 enddo
 !
 if(i2==n_ofs_defined+1) then
   n_ofs_defined=n_ofs_defined+1
   get_file_index=n_ofs_defined
 endif
 !
 if(n_ofs_defined>max_n_ofs) call error(" Maximum number of file names exceeded")
 if(get_file_index==-1)      call error(" Negative file index, something went wrong")
 !
end function get_file_index
