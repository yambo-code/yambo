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
subroutine PARALLEL_live_message(WHAT,ENVIRONMENT,LOADED,TOTAL,NC)
 !
 use pars,       ONLY:lchlen
 use parallel_m, ONLY:ncpu
 use LIVE_t,     ONLY:LIVE_message
 use stderr,     ONLY:intc
 !
 implicit none
 !
 character(*)          :: WHAT
 character(*),optional :: ENVIRONMENT
 integer,optional      :: LOADED,TOTAL,NC
 character(lchlen)     :: local_message,local_env
 !
 local_message=WHAT
 !
 if (ncpu==1) return
 !
 local_env="distribution"
 if (present(ENVIRONMENT)) then
   local_env=ENVIRONMENT
 endif
 ! 
 if (present(LOADED).and.present(NC)) then
   local_message="[PARALLEL "//trim(local_env)//" for "//WHAT//&
&                            " on "//trim(intc(NC))//" CPU] Loaded-Total-Percentual: "//&
&                            trim(intc(LOADED))//"-"//&
&                            trim(intc(TOTAL))//"-"//&
&                            trim(intc(int(real(LOADED)/real(TOTAL)*100)))//"%"
 else if (present(LOADED)) then
   local_message="[PARALLEL "//trim(local_env)//" for "//WHAT//&
&                            "] Loaded-Total-Percentual: "//trim(intc(LOADED))//"-"//&
&                            trim(intc(TOTAL))//"-"//&
&                            trim(intc(int(real(LOADED)/real(TOTAL)*100)))//"%"
 endif
 !
 if (present(LOADED)) then
   if (LOADED/=TOTAL) call LIVE_message(message=trim(local_message),lfmt="n")
 else
   call LIVE_message(message=trim(local_message),lfmt="n")
 endif
 !
end subroutine
