!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine PARALLEL_live_message(WHAT,ENVIRONMENT,LOADED,TOTAL,LOADED_r,TOTAL_r,NC)
 !
 use pars,       ONLY:lchlen,SP
 use parallel_m, ONLY:ncpu
 use LIVE_t,     ONLY:LIVE_message
 use stderr,     ONLY:intc,real2ch
 !
 implicit none
 !
 character(*)          :: WHAT
 character(*),optional :: ENVIRONMENT
 integer,optional      :: LOADED,TOTAL,NC
 real(SP),optional     :: LOADED_r,TOTAL_r
 ! Work Space
 integer               :: PERCENTUAL
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
 if(present(LOADED).or.present(LOADED_r)) then
   !
   local_message="[PARALLEL "//trim(local_env)//" for "//WHAT
   if (     present(NC)) local_message=trim(local_message)//" on "//trim(intc(NC))//" CPU] Loaded/Total (Percentual): "
   if (.not.present(NC)) local_message=trim(local_message)//"] Loaded/Total(Percentual): "
   !
   if(present(LOADED_r).and.present(TOTAL_r)) then
     PERCENTUAL=nint(LOADED_r/TOTAL_r*100._SP)
     local_message=trim(local_message)//trim(real2ch(LOADED_r))//"/"//trim(real2ch(TOTAL_r))//" ("//trim(intc(PERCENTUAL))//"%)"
   else if(present(LOADED).and.present(TOTAL)) then
     PERCENTUAL=nint(real(LOADED,SP)/real(TOTAL,SP)*100._SP)
     local_message=trim(local_message)//trim(intc(LOADED))//"/"//trim(intc(TOTAL))//"("//trim(intc(PERCENTUAL))//"%)"
   endif
   !
 endif
 !
 call LIVE_message(message=trim(local_message),lfmt="n")
 !
end subroutine
