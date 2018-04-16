!
!        Copyright (C) 2000-2018 the YAMBO team
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
integer function parse_character_arrays(CH_v,N,VAR_NAME,VAR_DESC,READ_it)
 !
 use pars,      ONLY:lchlen
 use parser_m,  ONLY:parser
 use it_m,      ONLY:flines,nflines,infile
 !
 implicit none
 !
 character(*)      :: VAR_NAME,VAR_DESC
 integer           :: N
 character(lchlen) :: CH_v(N,1)
 logical           :: READ_it
 !
 ! Work Space
 !
 integer :: i_check,i_pointer,nflines_save,i1
 !
 parse_character_arrays=0
 !
 if (READ_it) then
   i_check=1
   i_pointer=1
   CH_v= " "
   do while(i_check/=0)
     call parser(VAR_NAME,CH_v(1:i_pointer,:))
     i_check=len_trim(CH_v(i_pointer,1))
     if (i_check/=0) i_pointer=i_pointer+1
   enddo
   parse_character_arrays=i_pointer-1
 else
   nflines_save=nflines
   write (flines(nflines+1),'(a,t32,a)') '%'//trim(VAR_NAME),'#'//VAR_DESC
   nflines=nflines+1
   do i1=1,N
     flines(nflines+1)='"'//trim(CH_v(i1,1))//'"'
     nflines=nflines+1
   enddo
   flines(nflines+1)="%"
   nflines=nflines+1
   open(unit=12,file=trim(infile),position='append')
   do i1=nflines_save+1,nflines
     write (12,'(a)') trim(flines(i1))
   enddo 
   close(12)
 endif
 !
end function
