!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine OUTPUT_init(what)
 !
 use stderr,         ONLY:STRING_match
 use OUTPUT,         ONLY:OUTPUT_driver
 use X_m,            ONLY:X_obs,N_X_obs
 !
 implicit none
 !
 character(*)           :: what
 !
 ! Workspace
 integer            ::it
 !
 if (STRING_match(what,"open").or.STRING_match(what,"append").or.STRING_match(what,"overwrite")) then
   do it=1,N_X_obs
     if (.not.X_obs(it)%active) cycle
     if (.not.STRING_match(what,trim(X_obs(it)%what)).and..not.STRING_match(what,"ALL")) cycle
     if (STRING_match(what,"open")) call OUTPUT_driver(trim(X_obs(it)%prefix),action="open")  
     if (STRING_match(what,"overwrite")) call OUTPUT_driver(trim(X_obs(it)%prefix),action="overwrite")  
     if (STRING_match(what,"append")) call OUTPUT_driver(trim(X_obs(it)%prefix),action="append")  
   enddo
 endif
 !
 if (STRING_match(what,"close")) then
   do it=1,N_X_obs
     if (.not.X_obs(it)%active) cycle
     if (.not.STRING_match(what,trim(X_obs(it)%what)).and..not.STRING_match(what,"ALL")) cycle
     call OUTPUT_driver(trim(X_obs(it)%prefix),action="close")
   enddo
 endif
 !
end subroutine
