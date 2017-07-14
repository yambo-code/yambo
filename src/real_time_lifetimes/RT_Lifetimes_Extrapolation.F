!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine RT_Lifetimes_Extrapolation(Life,Time,Life_SAVED,Times_SAVED)
 !
 use pars,           ONLY:SP
 use real_time,      ONLY:Life_MEM_steps,l_RT_lifetimes_extrapolation
 use POL_FIT_m,      ONLY:POL_LK_fit
 !
 implicit none
 !
 real(SP)  :: Life,Time,Life_SAVED(Life_MEM_steps),Times_SAVED(Life_MEM_steps)
 !
 ! Work Space
 !
 real(SP)  :: M,Q,dT(Life_MEM_steps),dG(Life_MEM_steps),P(Life_MEM_steps),EPS,Life_SUM
 integer   :: POL_order,i_o
 !
 if (.not.l_RT_lifetimes_extrapolation) return
 !
 Life_SUM=sum(abs(Life_SAVED))
 !
 if (Life_MEM_steps==3.or.Life_SUM<1.E-5) then
   dT(2)=Times_SAVED(2)-Times_SAVED(1)
   dG(2)=Life_SAVED(2)-Life_SAVED(1)
   dT(3)=Times_SAVED(3)-Times_SAVED(1)
   dG(3)=Life_SAVED(3)-Life_SAVED(1)
   Q=(dT(3)/dT(2)*dG(2)-dG(3))/(dT(3)*(dT(2)-dT(3)))
   M=(dG(3)-Q*(dT(3))**2)/dT(3)
   Life=Life_SAVED(1)+M*(Time-Times_SAVED(1))+Q*(Time-Times_SAVED(1))**2
 else
   POL_order=2
   EPS=0.d0
   call POL_LK_fit(Life_MEM_steps,Times_SAVED-Times_SAVED(1),Life_SAVED,POL_order,P(:POL_order+1),EPS,0._SP)
   Life=0.
   do i_o=1,POL_order+1
     Life=Life+(Time-Times_SAVED(1))**(i_o-1)*P(i_o)
   enddo
 endif
 !
end subroutine RT_Lifetimes_Extrapolation
