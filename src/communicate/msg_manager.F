!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine msg_manager(how,OUT_msg,REP_msg,LOG_msg,STD_msg,NEW_lines)
 !
 ! how = '(n)s(n)' '(n)r(n)' '(n)l(n)' 'o PATTERN'
 !
 ! r(eport)
 ! s(tderr)
 ! oN=msgunits(N) 
 !
 ! This subroutine switches to positive the value of of_unit just before writing
 ! plus extra operations
 !
 use pars,    ONLY:schlen,lchlen,repfile_index
 use stderr,  ONLY:STRING_split,STRING_remove
 use com,     ONLY:terminator,of_opened,of_name,of_unit,n_ofs_defined,jobstr,&
&                  write_to_report,write_to_of
 !
 implicit none
 !
 character(*), intent(in)  :: how
 logical,      intent(out) :: OUT_msg,REP_msg,LOG_msg,NEW_lines(2),STD_msg
 ! 
 ! Work Space
 !
 integer          ::i1,i2
 character(schlen)::ch_piece(10),adjusted_how
 character(lchlen)::stripped_file_name
 !
 terminator(1)='('
 terminator(2)=')'
 !
 OUT_msg   =.false.
 REP_msg   =.false.
 LOG_msg   =.false.
 STD_msg   =.false.
 NEW_lines =.false.
 !
 ! o. files
 !
 if (how(:1)=="o") then
   if(write_to_of) then
     call STRING_split(how,ch_piece)
     do i2=2,10
       if (len_trim(ch_piece(i2))==0) cycle
       do i1=1,n_ofs_defined
         if (len_trim(of_name(i1))==0) cycle
         stripped_file_name=STRING_remove(of_opened(i1),"-"//trim(jobstr))
         if (index( stripped_file_name,trim(ch_piece(i2)) )/=0) then
           if (len_trim(of_opened(i1))==0) call error(' Trying to write on closed file '//trim(of_name(i1)))
           of_unit(i1) =-of_unit(i1)
         endif
       enddo
     enddo
   endif
   OUT_msg=.true.
   return
 endif
 !
 ! Characters to control the report/log are in the group
 ! before a space
 !
 i2=len_trim(how)
 adjusted_how=adjustl(how)
 do i1=1,len_trim(adjusted_how)
   if (adjusted_how(i1:i1)==" ") then
     i2=i1
     exit
   endif
 enddo
 !
 ! Report
 !
 if (index(adjusted_how(:i2),'r' )/=0.and.write_to_report) then
   of_unit(repfile_index) =-of_unit(repfile_index)
   REP_msg=.true.
   if (index(adjusted_how(:i2),'nr')/=0) then
     terminator(1)='(/'
     NEW_lines(1)=.true.
   endif
   if (index(adjusted_how(:i2),'rn')/=0) then
     terminator(2)='/)'
     NEW_lines(2)=.true.
   endif
 endif
 !
 ! Log 
 !
 if (index(adjusted_how(:i2),'l' )/=0) then
   LOG_msg=.true.
   terminator(3:4)=' '
   if (index(adjusted_how(:i2),'nl')/=0) then
     terminator(3)='n'
     NEW_lines(1)=.true.
   endif
   if (index(adjusted_how(:i2),'ln')/=0) then
     terminator(4)='n'
     NEW_lines(2)=.true.
   endif
 endif
 !
 ! STD 
 !
 if (index(adjusted_how(:i2),'s' )/=0) then
   STD_msg=.true.
   terminator(3:4)=' '
   if (index(adjusted_how(:i2),'ns')/=0) then
     terminator(3)='n'
     NEW_lines(1)=.true.
   endif
   if (index(adjusted_how(:i2),'sn')/=0) then
     terminator(4)='n'
     NEW_lines(2)=.true.
   endif
 endif
 !
end subroutine msg_manager
