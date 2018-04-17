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
subroutine RT_remove_background(F,N,ZERO_at_reference)
 !
 use YPP_real_time,  ONLY:RT_time,RT_background_T_ref
 use pars,           ONLY:SP,DP
 use units,          ONLY:AUT2FS
 implicit none
 !
 integer,      intent(in)      :: N
 complex(DP),  intent(inout)   :: F(N)
 logical,      intent(in)      :: ZERO_at_reference
 !                          
 ! Work Space 
 !
 integer             :: i_t,i_To(3),i_To_delta
 real(SP), parameter :: delta_T=.5/AUT2FS !fs
 complex(SP)         :: M_fac,DELTA,R,Q
 !
 i_To(1) =GET_point(RT_background_T_ref-2*delta_T)
 i_To(2) =GET_point(RT_background_T_ref-delta_T)
 i_To(3) =GET_point(RT_background_T_ref)
 !
 if (i_To(1)<=0) return
 !
 if (ZERO_at_reference) then
   M_fac=F(i_To(3))/RT_time(i_To(3))
   do i_t=i_To(3)+1,N
     F(i_t)=F(i_t)-F(i_To(3))
   enddo
   do i_t=1,i_To(3)
     F(i_t)=F(i_t)-M_fac*RT_time(i_t)
   enddo
 endif
 !
 M_fac=(F(i_To(2))-F(i_To(1)))/(RT_time(i_To(2))-RT_time(i_To(1)))
 !
 Q=F(i_To(3))
 DELTA=2*(F(i_To(3))-F(i_To(2)))/M_fac
 R=-M_fac/2./DELTA
 i_To_delta=GET_point(RT_time(i_To(2))+real(DELTA))
 !
 do i_t=i_To(2),i_To_delta
   F(i_t)=Q+R*(RT_time(i_t)-RT_time(i_To(2))-DELTA)**2
 enddo
 do i_t=i_To_delta+1,N
   F(i_t)=Q
 enddo
 !
 contains
   !
   integer function GET_point(TIME)
     real(SP) :: TIME
     integer  :: it
     GET_point=0
     do it=1,N-1
       if (RT_time(it+1)>TIME) then
         GET_point=it
         return
       endif
     enddo
   end function
   !
end subroutine RT_remove_background
