!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details):
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
subroutine RT_Commensurable_Time(TIME,DEFINE_TOTAL_TIME)
 !
 ! Return TIME To be commensurable with RT_dyn_step
 !
 use pars,           ONLY:SP
 use real_time,      ONLY:NE_steps,NE_tot_time,RT_step
 !
 implicit none
 !
 real(SP) :: TIME
 logical  :: DEFINE_TOTAL_TIME
 !
 ! Work Space
 !
 integer :: rest,STEPS
 !
 if (TIME<0) TIME = max(NE_tot_time/1000._SP,RT_step)
 !
 STEPS = nint(TIME/RT_step)
 TIME  = STEPS*RT_step
 !
 if (STEPS == 0 ) STEPS = 1
 !
 if (DEFINE_TOTAL_TIME) then
   rest        = mod(NE_steps-1,STEPS)
   if (rest>0) then
     NE_steps    = NE_steps+STEPS-rest
     NE_tot_time = RT_step*(NE_steps-1)
   endif
 endif
 !
end subroutine RT_Commensurable_Time
