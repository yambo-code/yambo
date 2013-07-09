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
subroutine RT_Lifetimes_Interpolation(Life,Life_SAVED)
 !
 use pars,           ONLY:SP
 use SC,             ONLY:it_now
 use real_time,      ONLY:RT_step,Life_MEM_steps,RT_dynamics_jumps
 !
 implicit none
 !
 real(SP)  :: Life,Life_SAVED(Life_MEM_steps)
 !
 ! Work Space
 !
 integer   :: it_first
 real(SP)  :: T_first,T_now,M(Life_MEM_steps-1)
 !
 it_first=int(real(it_now-1)/real(Life_MEM_steps+RT_dynamics_jumps))*(Life_MEM_steps+RT_dynamics_jumps)+1
 T_first=(it_first-1)*RT_step
 T_now=(it_now-1)*RT_step
 !
 M(1)=(4._SP*Life_SAVED(2)-3._SP*Life_SAVED(1)-Life_SAVED(3))/2._SP/RT_step
 M(2)=(Life_SAVED(3)-2._SP*Life_SAVED(2)+Life_SAVED(1))/2._SP/RT_step**2
 !
 Life=Life_SAVED(1)+M(1)*(T_now-T_first)+M(2)*(T_now-T_first)**2
 !
end subroutine RT_Lifetimes_Interpolation
