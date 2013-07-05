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
 use real_time,      ONLY:RT_step,Life_MEM_steps,Thermal_steps,RT_dynamics_jumps
 use matrix_operate, ONLY:m3inv
 use wrapper,        ONLY:M_by_V
 !
 implicit none
 !
 real(SP)  :: Life,Life_SAVED(Life_MEM_steps)
 !
 ! Work Space
 !
 integer   :: it,it_absolute
 real(SP)  :: Mat(Life_MEM_steps,Life_MEM_steps),X(Life_MEM_steps),M(Life_MEM_steps)
 !
 do it=1,Life_MEM_steps
   it_absolute=int(real(it_now-1)/real(Life_MEM_steps+RT_dynamics_jumps))*(Life_MEM_steps+RT_dynamics_jumps)+it
   X(it)=(it_absolute-1)*RT_step
   Mat(it,:)=(/1.,(X(it)-,X(it)**2./)
 enddo
 !
 if (Life_MEM_steps==3) call m3inv(Mat)
 !
 call M_by_V('n',Life_MEM_steps,Mat,Life_SAVED,M)
 !
 Life=0.
 do it=1,Life_MEM_steps
  Life=Life+M(it)*X(it)**(it-1)
 enddo
 !
 write (*,*) Life_SAVED,Life
 stop
 !
end subroutine RT_Lifetimes_Interpolation
