!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
subroutine RT_generalized_KBA(L,ik,ikp,Time,left_kind,right_kind)
 !
 use pars,           ONLY:SP
 use SC,             ONLY:SC_bands
 use real_time,      ONLY:G_lesser,NE_MEM_steps,RT_step
 implicit none
 !
 complex(SP)   :: L(SC_bands(2),SC_bands(2),NE_MEM_steps)
 integer       :: ik,ikp
 character(1)  :: left_kind,right_kind
 real(SP)      :: Time
 !
 ! Work Space
 !
 integer               :: it,it_now
 complex(SP), external :: RT_G_retarded
 !
 it_now=Time/RT_step
 !
 ! L^{><}_{kp,nm}(t,t')=-i G^r_nk(t-t')(delta_{nm}+i G^<_{n n' k}(t')) G^<_{n' m p}(t') [G^r_mp(t-t')]^*
 !
 if (left_kind==">".and.right_kind=="<") then
 !
 do it=max(it_now-NE_MEM_steps+1,0),it_now
 enddo
 !
 endif
 !
 stop
 !
end subroutine RT_generalized_KBA
