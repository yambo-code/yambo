!
! Copyright (C) 2000-2011 M. Gruning, A. Marini and the YAMBO team 
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
subroutine RT_Coefficients_inv(nt,P,X,N_order,Wo)
 !
 use pars,           ONLY:SP,cI,pi
 use YPP,            ONLY:i_t_start
 use units,          ONLY:FS2AUT,HARTREE,HBAR_eVfs
 use real_time,      ONLY:RT_step
 use matrix_operate,  ONLY:mat_dia_inv,INV,USE_LK
 implicit none
 !
 integer, intent(in)       :: N_order,nt
 real(SP),intent(in)       :: P(nt),Wo
 complex(SP), intent(out)  :: X(N_order+1)
 !
 ! Work space
 !
 integer           :: i_t,iT_range,it_interval,max_order,max_it,i_order
 real(SP)          :: T_range,T_i
 complex,allocatable :: M(:,:), P_i(:)
 !
 max_order = N_order+1 
 max_it = 2*max_order-1
 !
 T_range=2.*pi*HBAR_eVfs/(Wo*HARTREE)*FS2AUT
 iT_range=T_range/RT_step
 it_interval=iT_range/2/max_order
 !
 allocate(M(max_it,max_it),P_i(max_it))
 do i_t=1,max_it
   T_i=(i_t_start+it_interval*(i_t-1))*RT_step
   P_i(i_t) = P(i_t_start+it_interval*(i_t-1))
   M(i_t,1) = 2.
   do i_order=2, max_order
     M(i_t,i_order) = exp(cI*Wo*(i_order-1)*T_i)
     M(i_t,i_order+N_order) = exp(-cI*Wo*(i_order-1)*T_i)
   end do
 end do
 !
 call mat_dia_inv(INV,USE_LK,M)
 !
 X = (0._SP,0._SP)
 do i_order =1,max_order
   do i_t = 1,max_it
     X(i_order) =  X(i_order)+M(i_order,i_t)*P_i(i_t)
   end do
 end do
 !
 deallocate(M)
 ! 
end subroutine RT_Coefficients_inv

