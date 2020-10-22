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
subroutine PARSER_force_command_line_value(defs,name,I1,R1,CHAR)
 !
 use it_m,       ONLY:initdefs,ni1v,i1vars,nr1v,r1vars,nchv,chvars
 use pars,       ONLY:SP
 !
 implicit none
 !
 type(initdefs)        :: defs
 character(*)          :: name
 integer, optional     :: I1
 real(SP),optional     :: R1
 character(*),optional :: CHAR
 !
 ! Work Space
 !
 integer :: i_var
 !
 !
 if (present(I1)) then
   do i_var=1,ni1v
     if (trim(i1vars(i_var,1))==trim(name)) then
       defs%i1def(i_var)=I1
     endif
   enddo
 endif
 if (present(R1)) then
   do i_var=1,nr1v
     if (trim(r1vars(i_var,1))==trim(name)) then
       defs%r1def(i_var)=R1
     endif
   enddo
 endif
 if (present(CHAR)) then
   do i_var=1,nchv
     if (trim(chvars(i_var,1))==trim(name)) then
       defs%chdef(i_var)=CHAR
     endif
   enddo
 endif
 !
end subroutine
